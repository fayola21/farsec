(ns farsec.tfidf
  (:use [farsec context utils])
  (:use (incanter core stats io))
  (require [clojure.java.io :as io]))

;--------------TFIDF-------------------
(defn dict-inc
  [m word]
  (update-in m [word] (fnil inc 0)))

(defn fast-tfidf
  "res = [{} {}]"
  [res input-dir]
  (let [mylst (rest (list-of-files input-dir 10))]
    (loop [lst mylst result res]
      (if (empty? lst)
        result
        (recur
          (rest lst)
          (let [words
                (remove #(or (= % "_") (= % ""))
                 (remove is-unwanted
                  (remove is-stopword
                    (normalize
                      (tokenize
                        (slurp (str my-home input-dir (first lst))))))))]
            [(reduce dict-inc (first result) words) (reduce dict-inc (second result) (distinct words))]))))))

(declare get-tfidf-terms-fast) 

(defn fast-tfidf1
  "res = [{} {}]"
  [res input-data]
  (let [dat         (read-as-csv input-data)
        n           (reduce + (map #(read-string %) (rest (last (transpose dat)))))
        wds-freqall (map #(vector (first %) (map read-string (rest %))) (butlast (rest (transpose dat))))
        wds         (map first wds-freqall)
        freq-fnc    (fn [lst] (count (filter #(not= % 0) (drop n lst))))
        wds-freq    (into {} (map #(vector (first %) (reduce + (map read-string (drop n (rest %))))) 
                               (butlast (rest (transpose dat)))))
        doc-freq    (into {} (map #(vector (first %) (freq-fnc (second %))) wds-freqall))]
    [[wds-freq doc-freq] (into {} (get-tfidf-terms-fast [wds-freq doc-freq]))]))        
;(read-as-csv (str my-home "/resources/pdata/wicket/wicket-fat/wicketfatwithintfidf-train.csv")) 

(defn tfidf-fnc
  [term res]
  (let [n-docs (count (map first (first res)))
        tf    (get (first res) term 0)
        df    (get (second res) term 0)
        idf   (if (not= df 0) (Math/log (/ n-docs df)) 0)]
    (vector term (* tf idf))))

(defn get-tfidf-terms-fast
  [res]
  (let [words (map first (first res))]
    (sort-by second > (map #(tfidf-fnc % res) words))))

(defn get-tfidf-term
  [corpus-text header-term]
  (let [ans    (map #(get (frequencies (normalize (tokenize %))) header-term 0) corpus-text)
        n-docs (count ans)
        tf     (reduce + ans)
        df     (count (filter #(not= % 0) ans))
        idf    (if (not= df 0) (Math/log (/ n-docs df)) 0)]
    (vector header-term (* tf idf))))

(defn get-tfidf-terms
  [corpus-text header]
  (into {} (pmap #(get-tfidf-term corpus-text %) header)))

(defn run-tfidf-terms
  [old-input-dir header]
  (let [mylst       (rest (list-of-files old-input-dir 10))
        corpus-text (map #(slurp (str my-home old-input-dir %)) mylst)]
    (get-tfidf-terms corpus-text (distinct header))))
  
(defn run-tfidf-terms2
  [old-input-dir new-input-dir header]
  (let [mylstold        (rest (list-of-files old-input-dir 10))
        mylstnew        (rest (list-of-files new-input-dir 10))
        corpus-text-old (map #(slurp (str my-home old-input-dir %)) mylstold)
        corpus-text-new (map #(slurp (str my-home new-input-dir %)) mylstnew)
        corpus-text     (apply concat [corpus-text-old corpus-text-new])]
    (get-tfidf-terms corpus-text (distinct header))))
  