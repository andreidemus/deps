(ns deps.core
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.string :as str])
  (:gen-class))

(defn log [& args]
  (println (apply str args)))

(defn split [s]
  (str/split s #":"))

(defn parse [v]
  (let [[group artifact type version scope] v]
    {:group    group
     :artifact artifact
     :type     type
     :version  version
     :scope    scope}))

(defn parse-short [v]
  (let [[group artifact version] v]
    {:group    group
     :artifact artifact
     :version  version}))

(defn parse-file [fname]
  (->> fname
       slurp
       str/split-lines
       (drop 2)
       (map #(-> % str/trim split parse))))

(defn find-dep [col {:keys [group artifact]}]
  (some #(if (and (= group (:group %))
                  (= artifact (:artifact %))) %) col))

(defn differs [{:keys [left right]}]
  (and
    right
    (not= (:version left)
          (:version right))))

(defn diff [left right]
  (log "Calculating diff...")
  (->> left
       (map (fn [i] {:left i :right (find-dep right i)}))
       (filter differs)))

(defn diff-to-str [{:keys [left right]}]
  (str (:group left) ":" (:artifact left) "(" (:version left) " <-> " (:version right) ") [" (:scope left) "]"))

(defn str->dep [s]
  (let [m (-> s split parse-short)]
    (str "<dependency>"
         "<groupId>" (:group m) "</groupId>"
         "<artifactId>" (:artifact m) "</artifactId>"
         "<version>" (:version m) "</version>"
         "</dependency>")))

(defn pom [lib]
  (str "<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
  xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\">
  <modelVersion>4.0.0</modelVersion>
  <groupId>demus</groupId>
  <artifactId>simple-pom</artifactId>
  <version>1.0</version>
  <packaging>pom</packaging>
  <dependencies>"
       (str->dep lib)
       "</dependencies></project>"))

(defn deps-from-artifact [a]
  (log "Calculating dependencies for " a "...")
  (spit "pom.xml" (pom a))
  (sh "sh" "-c" "mvn dependency:list -DoutputFile=deps.txt -Dsort=true") ;todo exclude
  (parse-file "deps.txt"))

(defn -main
  [& args]
  (if (< (count args) 2)
    (log "Params: <left-group:left-artifact:left-version> <right-group:right-artifact:right-version>\n"
         "Out: left-group:left-artifact(left-version <-> right-version) [left-scope]")
    (let [[left right] args]
    (->> (diff (deps-from-artifact left) (deps-from-artifact right))
         (map diff-to-str)
         (str/join "\n")
         log)))
  (System/exit 0))
