(ns farsec.context
  (:use [opennlp.nlp])
  (:use [opennlp.tools.filters])
  (:use [farsec utils])
  (:use (incanter core stats io))
  (require [clojure.java.io :as io]))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn normalize [token-seq]
  (map #(.toLowerCase %) token-seq))

(defn load-stopwords [filename]
  (with-open [r (io/reader filename)]
    (set (doall (line-seq r)))))
(def is-stopword (load-stopwords "stopwords/english"))

; Requires you to run this from the root opennlp directory or have the
; models downloaded into a "models" folder
(def get-sentences (make-sentence-detector "models/en-sent.bin"))
(def tokenize (make-tokenizer "models/en-token.bin"))
(def detokenize (make-detokenizer "models/english-detokenizer.xml"))
(def pos-tag (make-pos-tagger "models/en-pos-maxent.bin"))
