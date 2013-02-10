(ns de.janthomae.leiningenplugin.leiningen.LeinigenAPITest
  "Test the LeiningenAPI functions"
  (:use midje.sweet
        de.janthomae.leiningenplugin.leiningen.LeiningenAPI)
  (:require [leiningen.core.project :as p]
            [leiningen.core.classpath :as cp]))


(facts
  "About the way we flatten the dependency tree"
  (let [project (assoc p/defaults :dependencies '[[midje "1.4.0"]])
        hierarchy (cp/dependency-hierarchy :dependencies project)
        result (find-all-artifacts hierarchy)]
    (count result) => 12
    (map #(contains? % :dependency ) result) => (has every? true?)
    (map #(contains? % :version ) result) => (has every? true?)
    (map #(contains? % :scope ) result) => (has every? true?)
    (map #(contains? % :artifactid ) result) => (has every? true?)
    (map #(contains? % :file ) result) => (has every? true?)
    (map #(contains? % :groupid ) result) => (has every? true?)))

;Test for the scenario when we have one leiningen module dependent on another.
(def module-a '[com.example/module "0.0.1-SNAPSHOT"])
(def module-b '[midje "1.4.0"])
(def deps [module-a module-b])
(def to-remove [module-a])

(facts
  "About how we remove modules from the dependencies list "
  (let [result (remove-modules deps to-remove)]
     (count result) => 1
     (first (take 2 result)) =>  module-b)
  (let [result (remove-modules deps [])]
     (count result) => 2))

;(def project-1 (assoc p/defaults :dependencies deps))
;(cp/dependency-hierarchy :dependencies project-1)