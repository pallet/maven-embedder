(ns maven-embedder.data
  "Data mappings"
  (:import
   [org.apache.maven.model Dependency Exclusion]))

(def ^:private translated-keys
  {:groupId :group-id
   :artifactId :artifact-id
   :baseVersion :base-version
   :constitutesBuildPath :constitutes-build-path
   :includesDependencies :includes-dependencies
   :managementKey :management-key
   :systemPath :system-path})

(defn- discard-keys
  [m v]
  (apply dissoc m v))

(defn- translate-keys
  "Beans generate camel cased keys, which are translated here. Along with
   removal of keys without sensible values."
  [m]
  (reduce
   (fn [r k]
     (let [v (get m k ::notfound)]
       (if (= v ::notfound)
         r
         (assoc r (get translated-keys k) v))))
   (discard-keys m (keys translated-keys))
   (keys translated-keys)))

(defn- sanitise-keys
  "Beans generate camel cased keys, which are translated here. Along with
   removal of keys without sensible values."
  [m]
  (letfn [(remove-asterisk [m k]
            (let [v (get m k)]
              (if (or (nil? v) (#{"" "*"} v)) (dissoc m k) m)))
          (remove-empty [m k]
            (let [v (get m k)]
              (if (or (nil? v) (and (coll? v) (not (seq v)))) (dissoc m k) m)))
          (remove-false [m k]
            (if-let [v (get m k)] m (dissoc m k)))]
    (->
     m
     translate-keys
     (dissoc :class)
     (remove-asterisk :extension)
     (remove-asterisk :classifier)
     (remove-empty :file)
     (remove-empty :exclusions)
     (remove-empty :properties)
     (remove-empty :system-path)
     (remove-false :snapshot)
     (remove-false :optional)
     (remove-false :includes-dependencies))))

(defn- exclusion-spec
  "Given an Aether Exclusion, returns a map describing the exclusion with the
   :exclusion in its metadata."
  [^Exclusion ex]
  (with-meta (-> ex bean sanitise-keys) {:exclusion ex}))

(defn dep-spec
  "Given an Aether Dependency, returns a map describing the dependency with the
   :dependency and its corresponding artifact's :file in its metadata."
  [^Dependency dep]
  (let [;;artifact (.getArtifact dep)
        t-f {"true" true "false" false}]
    (-> (merge
         (dissoc (bean dep) :artifact)
         ;; (dissoc (bean artifact) :file)
         )
        (update-in [:exclusions] #(map exclusion-spec %))
        (update-in [:properties] (comp
                                  sanitise-keys
                                  #(zipmap
                                    (map keyword (keys %))
                                    (map (fn [v] (get t-f v v)) (vals %)))
                                  (partial into {})))
        (update-in [:file] #(when % (.getPath %)))
        sanitise-keys
        (with-meta {:dependency dep ;; :file (.getFile artifact)
                    }))))
