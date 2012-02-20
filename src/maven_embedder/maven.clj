(ns maven-embedder.maven
  "Maven embedder

   Other maven embedders:
     See m2e eclipse project for MavenImpl.java
     https://github.com/jenkinsci/lib-jenkins-maven-embedder"
  (:use
   [maven-embedder.plexus :only [lookup]])
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.logging :as logging])
  (:import
   java.io.BufferedInputStream
   java.util.Properties
   org.apache.maven.Maven
   org.apache.maven.cli.MavenCli
   org.apache.maven.execution.MavenExecutionRequest
   org.apache.maven.repository.RepositorySystem
   org.apache.maven.settings.Settings
   [org.apache.maven.settings.building
    DefaultSettingsBuildingRequest SettingsBuilder SettingsBuildingException]
   [org.apache.maven.execution
    DefaultMavenExecutionRequest DefaultMavenExecutionResult
    MavenExecutionRequest MavenExecutionRequestPopulationException
    MavenExecutionRequestPopulator MavenExecutionResult MavenSession]
   org.apache.maven.lifecycle.LifecycleExecutor
   [org.apache.maven.lifecycle.internal
    MojoExecutor LifecycleExecutionPlanCalculator]
   [org.apache.maven.model.building
    DefaultModelBuildingRequest ModelBuildingRequest]
   org.apache.maven.model.io.ModelReader
   [org.apache.maven.plugin
    MojoExecution BuildPluginManager]
   [org.apache.maven.project
    MavenProject ProjectBuilder]
   org.sonatype.aether.RepositorySystemSession
   org.codehaus.plexus.logging.Logger))

(defn settings [container {:keys [global-settings user-settings maven-home]}]
  (try
    (let [env (reduce
               (fn [p v]
                 (doto p
                   (.setProperty
                    (str "env." (string/upper-case (.getKey v)))
                    (.getValue v))))
               (Properties.)
               (.. (System/getenv) entrySet))
          properties (doto (System/getProperties) (.putAll env))
          _ (when maven-home (.put properties "maven.home" maven-home))
          request (doto (DefaultSettingsBuildingRequest.)
                    (.setSystemProperties properties)
                    (.setGlobalSettingsFile
                     (or (and global-settings (.exists global-settings)
                              global-settings)
                         MavenCli/DEFAULT_GLOBAL_SETTINGS_FILE))
                    (.setUserSettingsFile
                     (or (and user-settings (.exists user-settings)
                              user-settings)
                         MavenCli/DEFAULT_USER_SETTINGS_FILE)))]
      (.. (lookup container SettingsBuilder)
          (build request)
          getEffectiveSettings))
    (catch SettingsBuildingException e
      (logging/errorf
       e "Could not build settings [%s %s]. Using default settings."
       user-settings global-settings)
      (Settings.))))

(defn local-repository-path
  [^Settings settings]
  (or (.getLocalRepository settings)
      (.. RepositorySystem/defaultUserLocalRepository getAbsolutePath)))

(defn local-repository
  [container ^Settings settings]
  (.. (lookup container RepositorySystem)
      (createLocalRepository (io/file (local-repository-path settings)))))

(defn execution-request
  [container settings {:keys [offline log-level]
                       :or {offline false
                            log-level MavenExecutionRequest/LOGGING_LEVEL_INFO}
                       :as options}]
  (let [request (DefaultMavenExecutionRequest.)
        local-repo (local-repository container settings)]
    (.. (lookup container MavenExecutionRequestPopulator)
        (populateFromSettings request settings))
    (.. (lookup container Logger) (setThreshold log-level))
    (doto request
      (.setLocalRepository local-repo)
      (.setLocalRepositoryPath (.getBasedir local-repo))
      (.setOffline offline)
      ;;(.setUpdateSnapshots false)
      (.setCacheNotFound true)
      (.setCacheTransferError true)
      (.setLoggingLevel log-level))))

(defn ^RepositorySystemSession repository-session
  [container ^MavenExecutionRequest request]
  (.. (lookup container Maven) (newRepositorySession request)))

(defn maven-session
  [container ^MavenExecutionRequest request ^MavenProject project]
  (let [repo-session (repository-session container request)
        maven-session (MavenSession.
                       container repo-session request
                       (DefaultMavenExecutionResult.))]
    (when project
      (.setProjects maven-session [project]))
    maven-session))

(defn setup-execution
  [container ^MavenSession session ^MavenProject project
   ^MojoExecution execution]
  (.. (lookup container LifecycleExecutionPlanCalculator)
      (setupMojoExecution session project execution)))

(defn execute
  ([container ^MavenExecutionRequest request]
     (try
       (.. (lookup container MavenExecutionRequestPopulator)
           (populateDefaults request))
       (.. (lookup container Maven) (execute request))
       (catch Exception e
         (doto (DefaultMavenExecutionResult.) (.addException e)))))
  ([container ^MavenSession session ^MojoExecution execution]
     (let [artifacts (reduce
                      (fn [m p] (assoc m p (.getArtifacts p)))
                      {} (.getProjects session))
           ;; snapshots (reduce
           ;;            (fn [m p]
           ;;              (assoc m p (MavenProjectMutableState/takeSnapshot p)))
           ;;            {} (.getProjects session))
           restore (fn [] (doseq [project (.getProjects session)]
                            (.setArtifactFilter project nil)
                            (.setResolvedArtifacts project nil)
                            (.setArtifacts project (artifacts project))
                            ;; (when-let [s (snapshots project)]
                            ;;   (.restore s project))
                            ))
           executor (lookup container MojoExecutor)]
       (try
         (let [depend-ctxt (.newDependencyContext
                        executor session
                        (java.util.ArrayList. [execution]))]
           (.ensureDependenciesAreResolved
            executor (.getMojoDescriptor execution) session depend-ctxt))
         (.. (lookup container BuildPluginManager)
             (executeMojo session execution))
         (catch Exception e
           (.. session getResult (addException e)))
         (finally
          (restore))))))

(defn calculate-execution-plan
  [container ^MavenSession session ^MavenProject project goals setup-flag]
  (.. (lookup container LifecycleExecutor)
      (calculateExecutionPlan session setup-flag (into-array String goals))))

(defn read-model
  "Read a model from some input clojure.java.io can process."
  [container input]
  (with-open [is (BufferedInputStream. (io/input-stream input))]
    (.. (lookup container ModelReader) (read is nil))))

(defn read-project
  "Read project from pom-file"
  [container ^MavenExecutionRequest request
   ^RepositorySystemSession repo-session ^java.io.File pom-file]
  (let [cl (.. (Thread/currentThread) getContextClassLoader)]
    (try
      (.. (Thread/currentThread)
          (setContextClassLoader (.getContainerRealm container)))
      (.. (lookup container MavenExecutionRequestPopulator)
          (populateDefaults request))
      (let [project-request (doto (.getProjectBuildingRequest request)
                              (.setValidationLevel
                               ModelBuildingRequest/VALIDATION_LEVEL_MINIMAL)
                              (.setRepositorySession repo-session)
                              (.setProcessPlugins true)
                              (.setResolveDependencies true))]
        (.. (lookup container ProjectBuilder)
            (build pom-file project-request)
            getProject))
      (finally
       (.. (Thread/currentThread) (setContextClassLoader cl))))))


(comment
(def plexus (maven-embedder.plexus/plexus-container))
(def settings (maven-embedder.maven/settings
               plexus
               {:user-settings (clojure.java.io/file "/Users/duncan/.m2/settings.xml")}))
(def er (maven-embedder.maven/execution-request plexus settings {:offline true}))
(def repo-session (maven-embedder.maven/repository-session plexus er))
(def project (maven-embedder.maven/read-project
              plexus er repo-session (clojure.java.io/file "pom.xml")))
(def maven-session (maven-embedder.maven/maven-session
                    plexus
                    (maven-embedder.maven/execution-request plexus settings {})
                    project))
(def exec-plan (maven-embedder.maven/calculate-execution-plan
                plexus maven-session project ["test"] false))
(doseq [exec (.getMojoExecutions exec-plan)]
  (maven-embedder.maven/setup-execution plexus maven-session project exec)
  (maven-embedder.maven/execute plexus maven-session exec))
(.. maven-session getResult getExceptions)
)
