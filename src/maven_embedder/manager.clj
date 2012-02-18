(ns maven-embedder.manager
  "Simplified maven embedding"
  (:require
   [clojure.java.io :as io]
   [maven-embedder.plexus :as plexus]
   [maven-embedder.maven :as maven])
  (:import
   java.io.File))

(def ^:dynamic *plexus-container*
  (atom nil))

(defn plexus-container
  "Return the default plexus container"
  []
  (when-not @*plexus-container*
    (compare-and-set! *plexus-container* nil (plexus/plexus-container)))
  @*plexus-container*)

(def user-home (System/getProperty "user.home"))

(defn default-options
  "Default options for maven requests."
  []
  {:user-settings (io/file user-home ".m2" "settings.xml")})

(defn settings
  "Create a maven settings object"
  [options]
  (maven/settings
   (plexus-container)
   (merge
    (default-options)
    options)))

(defn execution-request
  [settings options]
  (maven/execution-request (plexus-container) settings options))

(defn repository-session
  ([execution-request]
     (maven/repository-session (plexus-container) execution-request))
  ([settings options]
     (repository-session (execution-request settings options))))

(defn project
  "Read a maven project."
  ([^File pom-file options settings execution-request repo-session]
     (maven/read-project
      (plexus-container) execution-request repo-session pom-file))
  ([pom-file options settings execution-request]
     (project
      pom-file options settings execution-request
      (repository-session execution-request)))
  ([pom-file options settings]
     (project pom-file options settings (execution-request settings options)))
  ([pom-file options]
     (project pom-file options (settings options)))
  ([pom-file]
     (project pom-file (default-options)))
  ([]
     (project (clojure.java.io/file "pom.xml"))))

(defn maven-session
  "Create a maven session"
  ([project execution-request]
     (maven/maven-session
      (plexus-container)
      execution-request
      project))
  ([project options settings]
     (maven-session project (execution-request settings options)))
  ([project]
     (maven-session project (default-options) (settings (default-options)))))

(defn execute
  "Execute goals on a project. Goals is a sequence of goal names as strings."
  ([project goals maven-session]
     (let [exec-plan (maven/calculate-execution-plan
                      (plexus-container)
                      maven-session project goals true)]
       (doseq [exec (.getMojoExecutions exec-plan)]
         ;; (maven/setup-execution plexus maven-session project exec)
         (maven/execute (plexus-container) maven-session exec))
       (.. maven-session getResult getExceptions)))
  ([project goals options settings]
     (execute project goals (maven-session project options settings)))
  ([project goals]
     (execute project goals (default-options) (settings (default-options)))))

(comment
  (execute (project) ["test"])
  (maven-embedder.manager/execute (maven-embedder.manager/project) ["test"]))
