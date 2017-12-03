(ns analyze-this.core
  (:import java.io.PushbackReader)
  (:require [clojure.java.io :refer [file reader]]
            [clojure.string :refer [trim]]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.tools.reader.edn :as edn])
  (:gen-class))

(defn files [directory path-re skip]
  (let [skip (set (map file skip))]
    (for [path (file-seq (file directory))
          :when (not (or (.isDirectory path)
                         (skip path)))
          :when (re-matches path-re (str path))]
      path)))

(defn count-nodes [data] 
  (if (seq? data) (+ 1 (reduce + 0 (map count-nodes data))) 1))

(defn branch-form? [form]
  (contains? #{'if 'if-not 'if-let 'when 'when-not 'when-let 'when-first 'case 'cond 'condp 'cond->>} form))

(defn count-branches [data] 
  (if (seq? data) 
    (reduce + 0 (map count-branches data))
  (if (branch-form? data) 1 0)))

(defn pprint-complexity [complexity-vec fname]
  (let [sum (first complexity-vec)
        nodes (second complexity-vec)
        branches (nth complexity-vec 2)]
    (str fname " has complexity " sum " (" nodes " nodes/" branches " branches)")))

(defn first-symbol [form]
  (keyword (first form)))

(defmulti analyze #'first-symbol)

(defmethod analyze :ns [form ]
  {:ns (second form)})

(defmethod analyze :defn [form]
  (let [nodes (count-nodes form)
        branches (count-branches form)
        fname (second form)]
    {:functions {fname {:nodes nodes :branches branches :public true}}}))

(defmethod analyze :defn- [form]
  (let [nodes (count-nodes form)
        branches (count-branches form)
        fname (second form)]
    {:functions {fname {:nodes nodes :branches branches :public false}}}))

(defmethod analyze :defmacro [form]
  (let [nodes (count-nodes form)
        branches (count-branches form)
        fname (second form)]
    {:macros {fname {:nodes  nodes :branches branches}}}))

(defmethod analyze :default [form]
  {:others (first form)})

(defn analyze-file [opts r]
  (doall (->> (repeatedly #(binding [r/*read-eval* false
                                     r/*alias-map* identity]
                             (r/read r false ::eof)))
              (take-while #(not= % ::eof))
              (map analyze)
              (reduce (fn [acc v]
                        (cond-> acc
                          (:ns v) (update :ns conj (:ns v))
                          (:macros v) (update :macros conj (:macros v))
                          (:functions v) (update :functions conj (:functions v))
                          (:others v) (update :others conj (:others v)))) {:others []
                                                                           :functions []
                                                                           :macros []}))))

(defn count-complexity [directory {:keys [skip] :or {skip #{}}}]
  (for [path (files directory #".*\.clj" skip)]
    (with-open [r (rt/push-back-reader (reader path))]
      (println  (analyze-file {} r)))))

(def default-opts {:threshold 60
     		   :branch-penalty 30
		   :macro-penalty 2
		   :source-dir "./src"})

(defn uncomplexor
  "running complexity analysis.."
  [project & args]
  (println "analyzing " (:name project))
  (let [opts (merge default-opts (:uncomplexor project))
       threshold (:threshold opts)
       branch-penalty (:branch-penalty opts)
       macro-penalty (:macro-penalty opts)
       source-dir (:source-dir opts)
        _ (println (str "functions or macros with complexity over threshold " threshold))
        complexity-results (count-complexity source-dir branch-penalty macro-penalty)
	overly-complex (filter #(< threshold (first (second %))) complexity-results)]
	
	(doseq [c overly-complex]
	  (println (pprint-complexity (second c) (first c))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
