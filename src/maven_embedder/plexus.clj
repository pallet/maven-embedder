(ns maven-embedder.plexus
  (:require
   [clojure.java.io :as io])
  (:import
   java.io.File
   org.codehaus.plexus.DefaultContainerConfiguration
   org.codehaus.plexus.DefaultPlexusContainer
   org.codehaus.plexus.PlexusContainer
   org.codehaus.plexus.classworlds.ClassWorld
   org.codehaus.plexus.classworlds.realm.ClassRealm))

(defn class-realm
  "build a class realm with all jars from ${maven-home}/lib"
  ([^File maven-home ^ClassWorld world ^ClassLoader parent-classloader]
     {:pre [maven-home world parent-classloader]}
     (when-not (.exists maven-home)
       (throw (IllegalArgumentException.
               (str "Maven home at " (.getPath maven-home) "can not be read"))))
     (let [lib (io/file maven-home "lib")]
       (when-not (and (.exists lib) (.isDirectory lib))
         (throw (IllegalArgumentException.
                 (str "Maven home at " (.getPath maven-home)
                      "does not contain a readable lib directory."))))
       (let [realm (ClassRealm. world "plexus.core" parent-classloader)
             jar? (fn [f] (.endsWith (.name f) ".jar"))]
         (doseq [f (filter jar? (file-seq lib))]
           (.addURL realm (.. f toURI toURL)))
         realm)))
  ([^File maven-home ^ClassWorld world]
     (class-realm
      maven-home world (.getContextClassLoader (Thread/currentThread) ))))

(defn class-world
  "Build a class world."
  []
  (ClassWorld. "plexus.core" (.getClassLoader ClassWorld)))

(defn ^PlexusContainer plexus-container
  "Construct a plexus container"
  ([^ClassWorld world ^ClassRealm realm]
     (let [config (.. (DefaultContainerConfiguration.)
                      (setClassWorld world)
                      (setName "mavenCore")
                      (setAutoWiring true))] ;; might need to use doto for this
       (when realm
         (.setRealm config realm))
       (DefaultPlexusContainer. config)))
  ([^ClassWorld world]
     (plexus-container world nil))
  ([]
     (plexus-container (class-world))))

(defn lookup
  "Lookup the implementation of class in container"
  [^PlexusContainer container ^Class class]
  (.lookup container class))
