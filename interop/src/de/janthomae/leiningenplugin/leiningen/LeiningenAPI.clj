(ns de.janthomae.leiningenplugin.leiningen.LeiningenAPI
  "This namespace provides our interop access point so that we can get into Clojure world from Java - basically so we can
  leverage the leiningen core library for introspection of project details."
  (:require [leiningen.core.project :as p]
             [leiningen.core.classpath :as cp])
  (:gen-class
    :methods [#^{:static true}[loadProject [String] java.util.Map]
              #^{:static true}[loadDependencies [String] java.util.List]]))

(defn find-all-artifacts
  "Traverses a nested map of dependencies as given by leiningen.core.classpath/dependency-hierarchy
    - Args: h - the dependency hierarchy
    - Returns: A sequence of maps containing the following keys:
        :artifactid: String: the artifactid coordinate
        :groupid: String: the group coordinates
        :version: String: the version number
        :scope: String: the scope of the dependency
        :dependency: org.sonatype.aether.graph.Dependency - The dependency itself in case you want to have access to anything that we haven't returned
        :file: java.io.File - The file containing the downloaded artifact (usually in your .m2 directory)

   Thanks to cemerick, clgv, and ivaraasen in #clojure for helping me figure this out in such a concise way."
  [h]
  (->> (tree-seq map? vals h)
    (map #(when (map? %) (keys %)))
    (reduce #(into %1 %2) [])
    (map #(let[[artifact version] %] (with-meta [artifact version] (meta %))))
    (map #(let[[artifact version] %
               {:keys [dependency file]} (meta %)]
            {:artifactid (name artifact) :version version :groupid (.getGroupId (.getArtifact dependency)) :scope (.getScope dependency) :dependency dependency :file file}))))

(defn -loadProject
   "Map a Java Static Function call to the project/read function.
     args: prj-file-path - path to the project.clj file - appears to work with relative or absolute"
   [prj-file-path]
   (let [m (p/read prj-file-path)]
     (zipmap (map name (keys m)) (vals m))))


(defn remove-modules
  "Remove any modules from s that are in the sequence to-remove.

  deps - in the format of the :dependencies values in the leiningen project file.  [[groupid/artifact  \"version\"]]
  to-remove - the dependencies to remove, in the format [groupid/artifact1 groupid/artifact2] - the version is explicitly not included.

  What this is doing is allowing us to specify a set of artifacts which we don't want aether to retrieve.
  This is usually the case when we have another leiningen module created in IntelliJ that isn't in a repository.
  See issue 18 for more details and discussion"
  [deps to-remove]
  (if (empty? to-remove)
    deps
    (remove nil?
      (for [d deps
          r to-remove]
        (if (= (first d)  r)
          nil
          d)))))


(defn group
  "Copied from the private aether/group."
  [group-artifact]
  (or (namespace group-artifact) (name group-artifact)))

(defn dep-vec-to-map
  "Convert a dependency vector into a map.

  Arguments:
     A vector in the format of a leiningen dependency declaration."
  [[group-artifact version & {:keys [scope optional exclusions] :as opts}]]
  (merge {:groupid (group group-artifact) :artifactid (name group-artifact) :version version} opts))

(defn dep-map-to-vec
  "Convert a dependency map back into a dependency declaration vector as used in leiningen"
  [{:keys [groupid artifactid version] :as m}]
  (reduce #(conj %1 (first %2) (second %2))
    [(if (= groupid artifactid)
       (symbol groupid)
       (symbol groupid artifactid))
     version]
    (seq (dissoc m :groupid :artifactid :version))))

(defn add-exclusions
  "Add exclusions to any dependencies so that aether won't attempt to retrieve them.  This is handy when
  we are working with artifacts which are not in a remote repository.  The particular use case that inspired this
  was in Issue 18 where we had multiple modules in a project that reference each other.

  This implicitly adds other modules that are loaded in IntelliJ into the exclusion list and right now supports only the
  groupname/artifactid specification for an exclusion.

  Arguments:
    deps - in the vector of vectors format of the :dependencies values in the leiningen project file.  [[groupid.artifact  \"version\"]]
    exclusions - the dependencies to add to each of deps exclusions list, in the format [groupid/artifact1 groupid/artifact2].
                 At this time we don't support any more complex exclusion definitions as we don't need them."
  [deps exclusions]
    (if (empty? exclusions)
       deps
      (for [d deps]
        (let [m (dep-vec-to-map d)
            e (:exclusions m)
            e (if (nil? e) [] e)
            exs (reduce conj e exclusions)]
            (-> (assoc m :exclusions exs)
                dep-map-to-vec)))))

(defn -loadDependencies
  "Retrieve all of the dependencies (including transitive) which are in the :dependencies list in the project file.
     - args: prj-file-path - path to the project.clj file - appears to work with relative or absolute
     - Returns: A sequence of maps containing the following string keys:
        \"artifactid\": String: the name of the artifact in leiningen format (ie. group/artifact)
        \"groupid\" String: the group coordinates
        \"version\": String: the version number
        \"scope\": String: the scope of the dependency
        \"dependency\": org.sonatype.aether.graph.Dependency - The dependency itself in case you want to have access to anything that we haven't returned
        \"file\": java.io.File - The file containing the downloaded artifact (usually in your .m2 directory)"
  [prj-file-path]
  (let [prj (p/read prj-file-path)
        dep-hier (cp/dependency-hierarchy :dependencies prj)
        deps (find-all-artifacts dep-hier)]
    (into []
      (for [m deps]
      (zipmap (map name (keys m)) (vals m))))))
