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
;Some of these module declarations were taken from the leiningen sample project file to ensure compatibility.
(def module-a '[com.example/module "0.0.1-SNAPSHOT" :exclusions [midje]])
(def module-b '[midje "1.4.0"])
(def module-c '[org.jclouds/jclouds "1.0" :classifier "jdk15" :scope "test"])
(def module-d '[net.sf.ehcache/ehcache "2.3.1" :extension "pom"])
(def module-e '[log4j "1.2.15" :exclusions [[javax.mail/mail :extension "jar"] [javax.jms/jms :classifier "*"] com.sun.jdmk/jmxtools com.sun.jmx/jmxri]])

(facts
  "About our dependency vector to map conversions and vice versa.  Basically, making sure I can round trip convert these dependency vectors."
    (let [a (dep-vec-to-map module-a)
          b (dep-vec-to-map module-b)
          c (dep-vec-to-map module-c)
          d (dep-vec-to-map module-d)
          e (dep-vec-to-map module-e)]
    a => (contains {:groupid "com.example" :artifactid "module" :version "0.0.1-SNAPSHOT" :exclusions '[midje]})
    (count a) => 4
    b => (contains {:groupid "midje" :artifactid "midje" :version "1.4.0"})
    (count b) => 3
    c => (contains {:groupid "org.jclouds" :artifactid "jclouds" :version "1.0" :classifier "jdk15" :scope "test"})
    (count c) => 5
    d => (contains {:groupid "net.sf.ehcache" :artifactid "ehcache" :version "2.3.1" :extension "pom"})
    (count d) => 4
    e => (contains {:groupid "log4j" :artifactid "log4j" :version "1.2.15" :exclusions '[[javax.mail/mail :extension "jar"] [javax.jms/jms :classifier "*"] com.sun.jdmk/jmxtools com.sun.jmx/jmxri]})
    (dep-map-to-vec a) => module-a
    (dep-map-to-vec b) => module-b
    (dep-map-to-vec c) => module-c
    (dep-map-to-vec d) => module-d
    (dep-map-to-vec e) => module-e))


(def exclusion-a '[exclude/a])
(def exclusion-b '[b])
(def exclusion-c '[com.example/module])
(def to-remove [exclusion-a exclusion-b])

(facts
  "About how we remove modules from the dependencies list "
  (let [result (remove-modules [module-a module-b] [exclusion-c])]
    (count result) => 1
    (first (take 2 result)) =>  module-b)
  (let [result (remove-modules deps [])]
    (count result) => 2))

(facts
  "About how we add exclusions to a list of dependencies"
  (let [results (add-exclusions [module-a] [exclusion-a])
        r (first results)
        m (dep-vec-to-map r)
        exs (:exclusions m)]
    (count results) => 1
    (count exs) => 1
    exs => (contains exclusion-a)))

(facts
  "Testing with empty exclusions vector."
  (let [results (add-exclusions [module-a] [])
        r (first results)
        m (dep-vec-to-map r)
        exs (:exclusions m)]
    (count results) => 1
    exs => '[midje]))

;(def project-1 (assoc p/defaults :dependencies deps))
;(cp/dependency-hierarchy :dependencies project-1)