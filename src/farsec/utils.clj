(ns farsec.utils
  (require ;[opennlp.nlp :as nlp]
           [clojure.java.io :as io]
           [clojure.data.csv :as csv]
           [clojure.set :as set]
           [incanter.core :as i]
           [clojure.xml :as xml]
           [clojure.zip :as zip]))

; Source http://writequit.org/blog/index.html%3Fp=351.html
(def my-home (System/getProperty "user.dir"))
;(def path-name "/results")

(defn member? [val lst]
  (get (set lst) val))

(defn files-to-csv
  "
   Take the content of each file, apply stem, label remaining words as security
   or nonsecurity. Save output in a text file and use to build a model. Start
   with the first 10 files. Will need to append output file and each need file
   is added.

  "
  [filename] ())

(defn list-of-files
  "
  Returns list of files from dir.

  Arguments:
  dirname -- dir containing files
  n -- num of files to use

  "
  [dirname n]
  (.list (io/file (str my-home dirname))))

(defn append-to-file
  "Uses spit to append to a file specified with its name as a string, or
   anything else that writer can take as an argument.  s is the string to
   append.
   http://clojuredocs.org/clojure_core/clojure.core/spit"
  [file-name s]
  (let [wrtr (io/writer file-name :append true)]
    (.write wrtr s)
    (.close wrtr)))

(defn read-as-csv [filename]
  (with-open [in-file (io/reader filename)]
    (doall
      (csv/read-csv in-file))))

(defn save-as-csv [data filename header-names]
  (with-open [f-out (io/writer filename)]
    (csv/write-csv f-out  [header-names])
    (csv/write-csv f-out data)))

(defn count-substring 
  "Use a sequence of regexp matches to count occurrences.
   Source http://www.rosettacode.org/wiki/Count_occurrences_of_a_substring#Clojure"
  [txt sub]
  (count (re-seq (re-pattern sub) txt)))

(defn transpose [m]
  (apply mapv vector m))

(defn filter-att [data-path att]
  (let [data (read-as-csv data-path)]
    (transpose (filter #(not= (first %) att) (transpose data)))))

;---text
(defn remove-punctuation [word]
  "Removes all non-alphanumeric characters, aside from single quotation
   mark ('), from word. XXXX made change to pattern, remove quote from w 
   https://github.com/zolrath/Cameron-Clojure-Class/blob/master/src/cameron/piglatin.clj"
  (clojure.string/replace word #"(?i)[^\w]+" ""))

(defn is-word-punctuation [word]
  "Returns nil if there is punctuation in the token."
  (if (empty? (clojure.string/replace word #"(?i)[\w]+" "")) word nil))

(defn is-word-digit [word]
  "Returns nil if there no digits in the token."
  (re-find #"\d+" word))

(defn is-underscore [word]
  "Returns nil if there no digits in the token."
  (re-find #"_" word))

(defn is-unwanted "Returns true if word contains digits and/or punctuation." [word] (if (is-word-digit word) true (if (is-word-punctuation word) false true)))

(defn normalize-number [num small big]
  (/ (- num small) (- big small)))

(defn normalize-numbers [nums small big]
  (map #(normalize-number % small big) nums))
  