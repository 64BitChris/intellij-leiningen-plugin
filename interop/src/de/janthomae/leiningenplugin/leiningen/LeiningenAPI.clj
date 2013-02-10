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

  What this is doing is allowing us to specify a set of artifacts which we don't want aether to retrieve.
  This is usually the case when we have another leiningen module created in IntelliJ that isn't in a repository.
  See issue 18 for more details and discussion"
  [deps to-remove]
  (if (empty? to-remove)
    deps
    (remove nil?
      (for [d deps
          r to-remove]
        (if (= (take 2 d) r)
          nil
          d)))))

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
