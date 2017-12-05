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
  
  {:ns {:name (second form)
        :requires (filter identity  (mapcat (fn [f]  (when (and (seq? f) (= :require (first f)))
                                                       (rest f))) form))}})

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

(defmethod analyze :s/defn [form]
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
  {:others {(first form) 1}})

(defn analyze-file [opts r]
  (doall (->> (repeatedly #(binding [r/*read-eval* false
                                     r/*alias-map* identity]
                             (r/read r false ::eof)))
              (take-while #(not= % ::eof))
              (map analyze)
              (reduce (fn [acc v]
                        (cond-> acc
                          (:ns v) (assoc :ns (:ns v))
                          (:macros v) (update :macros merge (:macros v))
                          (:functions v) (update :functions merge (:functions v))
                          (:others v) (update :others (partial merge-with +) (:others v))))
                      {:others {}
                       :functions {}
                       :macros {}}))))

(defn count-complexity [directory {:keys [skip] :or {skip #{}}}]
  (for [path (files directory #".*\.clj" skip)]
    (with-open [r (rt/push-back-reader (reader path))]
      (analyze-file {} r))))

(def penalty-for-branch 30)

(defn complexity
  ([fn-or-macro] (complexity fn-or-macro 1))
  ([fn-or-macro penalty]
   (+ (:nodes fn-or-macro)
      (* penalty penalty-for-branch
         (:branches fn-or-macro)))))

(defn total-complexity [ns]
  (letfn [(total [acc [_ data]] (+ acc (:complexity data)))]
    (-> ns
        (assoc-in  [:ns :total-complexity]  (+ (reduce total 0 (:functions ns))
                                                 (reduce total 0 (:macros ns))
                                                 (* 3 (count (get-in ns [:ns :requires])))))
        (assoc-in [:ns :requires-count] (count (get-in ns [:ns :requires]) )))))

(defn stats [{:keys [ns macros functions others] :as objects}]
  (-> objects
      (update :functions (fn [fns] fns
                           (into {}
                                 (map (fn [[name data]]
                                        [name (assoc data :complexity (complexity data))])fns ))))
      (update :macros (fn [fns] fns
                        (into {}
                              (map (fn [[name data]]
                                     [name (assoc data :complexity (complexity data 2))])
                                   fns))))

      total-complexity
      (update :ns (fn [n] (-> n (dissoc :requires))))))

(defn do-math [nss]
  (->> nss
       (map stats)
       (map :ns)
       (map #(select-keys % [:name :total-complexity :requires-count]))
       (sort-by (juxt :total-complexity :requires-count))))

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
