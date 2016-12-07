(ns farsec.examples
  (:use (seer naive-bayes random-forest logistic-regression ib-k 
          multilayer-perceptron cpdp))
  (:use [farsec evaluate farsec-filter tfidf utils])
  (:use (incanter core stats io charts))
  (require 
    [clojure.java.io :as io]
    [clojure.java.io :as io]))
    
(defn exp-1
  "
   Accepts path to directory containing SBRs. 
   Returns list of top n security related keywords. Sorted from highest to
   lowest tf-idf values.
  
   Example: 
      
            (def data-name-pathway (subs (str :/resources/data1/wicket/wicket-sbr-old) 1))
            (exp-1 data-name-pathway 100)
  "
  [data-path n]
  (->> (str data-path "/")
       (fast-tfidf [{} {}])
       (get-tfidf-terms-fast)
       (take n)
       (map first)))

(defn exp-2
  "
   Accepts path to a directory and returns hashmap of keywords mapped 
   to tfidf values.
  
   Example: 
      
            (def data-name-pathway (subs (str :/resources/data1/wicket/wicket-sbr-old) 1))
            (exp-2 data-name-pathway)
  "
  [data-path]
  (->> (str data-path "/")
       (fast-tfidf [{} {}])
       (get-tfidf-terms-fast)
       (into {})))

(defn exp-4
  "
   Accepts a file of keywords and frequencies of keywords for each bug report. 
   Returns a hashmap with keywords mapped to tfidf values.
  
   Used mainly on farsec filtered train data. 
  "
  [file-path]
  (->> file-path
       (fast-tfidf1 [{} {}])
       (second)))

;-----------setup experiments------------
(defn experiment-1
  "
   Returns the tfidf of top n security related keywords based on sbrs, nsbrs,
   and farsec filtered bug reports.
  "
  [sbr-data-path nsbr-data-path file-path n]
  (let [words      (exp-1 sbr-data-path n)
        sbr-map    (exp-2 sbr-data-path)
        nsbr-map   (exp-2 nsbr-data-path)
        farsec-map (exp-4 file-path)]
    (->> (map #(vector 
                 % 
                 (get nsbr-map % 0.0) 
                 (get sbr-map % 0.0) 
                 (get farsec-map % 0.0)) 
           words) 
         (sort-by second >))))

(defn experiment-2a
  "Returns prediction results for a vector of data sets (original) and a 
   machine learning algorithm. WPP experiment."
  [data-name-vec learner]
  (let [header (append-to-file (str my-home "/results1/wpp-output.csv")
                 (str "train," "test," "learner," 
                  "tn," "tp," "fn," "fp," "err,"
                  "pd," "pf," "prec," "g-measure" "\n"))]        
    (loop [dat data-name-vec result 'done]
      (if (empty? dat)
        result
        (recur
          (rest dat)
          (run-results 
            (str my-home "/resources/data1/"(first dat)"/"(first dat)"-mat-train.csv") 
            (str my-home "/resources/data1/"(first dat)"/"(first dat)"-mat-test.csv") 
            :file-cpdp (str my-home "/results1/wpp-output.csv")
            :learner learner))))))

(defn experiment-2b 
  "Returns prediction results for a vector of data sets (farsec) and a 
   machine learning algorithm. WPPx experiment."
  [data-name-vec learner]
  (let [header (append-to-file (str my-home "/results1/wppx-output.csv")
                 (str "train," "test," "learner," 
                  "tn," "tp," "fn," "fp," "err,"
                  "pd," "pf," "prec," "g-measure" "\n"))]       
    (loop [dat data-name-vec result 'done]
      (if (empty? dat)
        result
        (recur
          (rest dat)
          (run-results 
            (str my-home "/resources/data1/"(first dat)"/"(first dat)"-farsec-train.csv") 
            (str my-home "/resources/data1/"(first dat)"/"(first dat)"-farsec-test.csv") 
            :file-cpdp (str my-home "/results1/wppx-output.csv")
            :learner learner))))))

(defn experiment-2c 
  "Returns prediction results for a vector of data sets and a 
   machine learning algorithm. TPP experiment."
  [data-name-vec cross-name learner]
  (let [header (append-to-file (str my-home "/results1/tpp-output.csv")
                 (str "train," "test," "learner," 
                  "tn," "tp," "fn," "fp," "err,"
                  "pd," "pf," "prec," "g-measure" "\n"))]       
    (loop [dat data-name-vec result 'done]
      (if (empty? dat)
        result
        (recur
          (rest dat)
          (run-results 
            (str my-home "/resources/data1/"(first dat)"/"(first dat) cross-name"-cross-train.csv") 
            (str my-home "/resources/data1/"(first dat)"/"(first dat) cross-name"-cross-test.csv") 
            :file-cpdp (str my-home "/results1/tpp-output.csv")
            :learner learner))))))

(defn experiment-2d 
  "Returns prediction results for a vector of data sets (farsec) and a 
   machine learning algorithm. TPPx experiment."
  [data-name-vec cross-name learner]
  (let [header (append-to-file (str my-home "/results1/tppx-output.csv")
                 (str "train," "test," "learner," 
                  "tn," "tp," "fn," "fp," "err,"
                  "pd," "pf," "prec," "g-measure" "\n"))]       
    (loop [dat data-name-vec result 'done]
      (if (empty? dat)
        result
        (recur
          (rest dat)
          (run-results 
            (str my-home "/resources/data1/"cross-name"/"cross-name"-farsec-train.csv") 
            (str my-home "/resources/data1/"(first dat)"/"(first dat) cross-name"-farsec-test.csv") 
            :file-cpdp (str my-home "/results1/tppx-output.csv")
            :learner learner))))))

(defn wpp-experiment-2
  "Returns prediction results for a vector of data sets (original) and  
   machine learning algorithms. WPP experiment."
  [exp-fnc]
  (exp-fnc ["chromium" "wicket" "ambari" "camel" "derby"] naive-bayes)
  (exp-fnc ["chromium" "wicket" "ambari" "camel" "derby"] logistic-regression)
  (exp-fnc ["chromium" "wicket" "ambari" "camel" "derby"] random-forest)
  (exp-fnc ["chromium" "wicket" "ambari" "camel" "derby"] ib-k)
  (exp-fnc ["chromium" "wicket" "ambari" "camel" "derby"] multilayer-perceptron))

(defn tpp-experiment-2
  "Returns prediction results for a vector of data sets (farsec) and  
   machine learning algorithms."
  [exp-fnc]
  (exp-fnc ["wicket" "ambari" "camel" "derby"] "chromium"  naive-bayes)
  (exp-fnc ["chromium" "ambari" "camel" "derby"] "wicket"  naive-bayes)
  (exp-fnc ["chromium" "wicket" "camel" "derby"] "ambari"  naive-bayes)
  (exp-fnc ["chromium" "wicket" "ambari" "derby"] "camel"  naive-bayes)
  (exp-fnc ["chromium" "wicket" "ambari" "camel"] "derby"  naive-bayes)
  (exp-fnc ["wicket" "ambari" "camel" "derby"] "chromium"  logistic-regression)
  (exp-fnc ["chromium" "ambari" "camel" "derby"] "wicket"  logistic-regression)
  (exp-fnc ["chromium" "wicket" "camel" "derby"] "ambari"  logistic-regression)
  (exp-fnc ["chromium" "wicket" "ambari" "derby"] "camel"  logistic-regression)
  (exp-fnc ["chromium" "wicket" "ambari" "camel"] "derby"  logistic-regression)
  (exp-fnc ["wicket" "ambari" "camel" "derby"] "chromium"  random-forest)
  (exp-fnc ["chromium" "ambari" "camel" "derby"] "wicket"  random-forest)
  (exp-fnc ["chromium" "wicket" "camel" "derby"] "ambari"  random-forest)
  (exp-fnc ["chromium" "wicket" "ambari" "derby"] "camel"  random-forest)
  (exp-fnc ["chromium" "wicket" "ambari" "camel"] "derby"  random-forest)
  (exp-fnc ["wicket" "ambari" "camel" "derby"] "chromium"  ib-k)
  (exp-fnc ["chromium" "ambari" "camel" "derby"] "wicket"  ib-k)
  (exp-fnc ["chromium" "wicket" "camel" "derby"] "ambari"  ib-k)
  (exp-fnc ["chromium" "wicket" "ambari" "derby"] "camel"  ib-k)
  (exp-fnc ["chromium" "wicket" "ambari" "camel"] "derby"  ib-k)
  (exp-fnc ["wicket" "ambari" "camel" "derby"] "chromium"  multilayer-perceptron)
  (exp-fnc ["chromium" "ambari" "camel" "derby"] "wicket"  multilayer-perceptron)
  (exp-fnc ["chromium" "wicket" "camel" "derby"] "ambari"  multilayer-perceptron)
  (exp-fnc ["chromium" "wicket" "ambari" "derby"] "camel"  multilayer-perceptron)
  (exp-fnc ["chromium" "wicket" "ambari" "camel"] "derby"  multilayer-perceptron))

(defn experiment-3a ; baseline
  "
   Returns test bug labels of bug reports, sorted by report ids. 
   This is the baseline.

   Example: 
            (def testid-data-path (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)) 
            (experiment-3a (str my-home test-data-path))
  "
  [testid-data-path]
  (let [test      (read-as-csv testid-data-path)
        bugids    (first (transpose (rest test)))
        buglabels (last (transpose (rest test)))
        baseline  (map #(vector %1 (read-string %2)) bugids buglabels)]
    (->> (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) baseline)
         (sort-by first)
         (transpose)
         (rest)
         (second))))
  
(defn experiment-3b ; wpp and tpp
  "
   Returns test bug labels of bug reports, sorted by report ids then prediction 
   results. Used for WPP and TPP results.

   Example: WPP
            (def train-data-path  (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path   (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (experiment-3b train-data-path test-data-path testid-data-path logistic-regression)
  
   Example: TPP, source = ambari, target = wicket
            (def train-data-path  (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path   (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (experiment-3b train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path test-data-path testid-data-path best-ml-fnc]
  (let [predict (cr train-data-path test-data-path testid-data-path best-ml-fnc)]
    (->> (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) predict)
         (sort-by first)   ; sort-by report ids
         (sort-by last >)  ; sort-by prediction in decending order
         (transpose)
         (rest)
         (second))))       ; returns the actual labels of sorted test reports
  
(defn experiment-3c
  "
   Returns test bug labels of bug reports, sorted by 
   1) report ids then 
   2) bug report scores then 
   3) prediction results. 
   Used for WPPx and TPPx results.

   Example: WPPx
            (def train-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (experiment-3c train-data-path test-data-path testid-data-path logistic-regression)

   Example: TPPx, source = ambari, target = wicket
            (def train-data-path (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (experiment-3c train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path test-data-path testid-data-path for-trainmap best-ml-fnc]
  (let [score-reports       (score-bug-reports for-trainmap testid-data-path)
        score-dummy-ids     (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) score-reports))
        predict             (cr train-data-path test-data-path testid-data-path best-ml-fnc)
        predict-dummy-ids   (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) predict))]
    (->> (map #(flatten (vector (rest %1) (rest (rest (rest %2))))) score-dummy-ids predict-dummy-ids) 
         (fk4)))) 

(defn experiment-3d
  "
   Returns test bug labels of bug reports, sorted by 
   1) report ids then 
   [xxx] 2) bug report scores then 
   2) prediction results. 
   Used for WPPx and TPPx results.

   Example: WPPx
            (def train-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (experiment-3c train-data-path test-data-path testid-data-path logistic-regression)

   Example: TPPx, source = ambari, target = wicket
            (def train-data-path (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (experiment-3c train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path test-data-path testid-data-path for-trainmap best-ml-fnc]
  (let [score-reports       (score-bug-reports for-trainmap testid-data-path)
        score-dummy-ids     (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) score-reports))
        predict             (cr train-data-path test-data-path testid-data-path best-ml-fnc)
        predict-dummy-ids   (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) predict))]
    (->> (map #(flatten (vector (rest %1) (rest (rest (rest %2))))) score-dummy-ids predict-dummy-ids) 
         (fk5)))) 

(defn experiment-3e
  "
   Returns test bug labels of bug reports, sorted by 
   1) report ids then 
   2) bug report scores then 
   3) prediction results pre-FARSEC
   4) prediction results post-FARSEC 
   Used for WPPx and TPPx results.

   Example: WPPx
            (def train-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (experiment-3c train-data-path test-data-path testid-data-path logistic-regression)

   Example: TPPx, source = ambari, target = wicket
            (def train-data-path (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (experiment-3c train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path train-data-path2 test-data-path test-data-path2 testid-data-path testid-data-path2 for-trainmap best-ml-fnc]
  (let [score-reports       (score-bug-reports for-trainmap testid-data-path)
        score-dummy-ids     (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) score-reports)) ; [numid,id,buglabel,score]
        predict             (cr train-data-path test-data-path testid-data-path best-ml-fnc) ; [id,want,got]
        predict2            (cr train-data-path2 test-data-path2 testid-data-path2 best-ml-fnc) ; [id,want,got2]
        predict-dummy-ids   (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) predict)) ; [numid,id,want,got]
        predict2-dummy-ids  (sort-by first (map #(flatten (vector (read-string (first (clojure.string/split (first %) #"\."))) %)) predict2))] ; [numid,id,want,got2]
    (->> (map #(flatten (vector (rest %1) (rest (rest (rest %2))) (rest (rest (rest %3))))) score-dummy-ids predict-dummy-ids predict2-dummy-ids) ; [id,buglabel,score,got,got2]
         (fk6)))) 
         

;-----------run experiments------------
(defn run-experiment-1
  "
   Using only the training data. Returns (normalized) tfidf data for making 
   charts for data-name-str. Figure 5 in paper.
  
   Example:
            (run-experiment-1 'wicket)
  "
  [data-name-str]
  (let [tfidf-vals (experiment-1
                     (str "/resources/data1/"data-name-str"/"data-name-str"-sbr-old")
                     (str "/resources/data1/"data-name-str"/"data-name-str"-nsbr-old")
                     (str my-home "/resources/data1/"data-name-str"/"data-name-str"-farsec-train.csv")
                     100)
        tfidf-norm (let [big1   (second (first tfidf-vals))
                         small1 (second (last tfidf-vals))
                         big    (apply max (map #(apply max %) (rest (transpose tfidf-vals))))
                         small  (apply min (map #(apply min %) (rest (transpose tfidf-vals))))]
                    (loop [i tfidf-vals result []]
                     (if (empty? i)
                      result
                      (recur
                       (rest i)
                       (conj result (apply vector (first (first i)) 
                                      (normalize-numbers (rest (first i)) small big)))))))]
    [(save-as-csv tfidf-vals (str my-home "/resources/data1/"data-name-str"/"data-name-str"-tfidf-vals.csv") ["word"  "NSBRs tf-idf"  "SBRs tf-idf"  "FARSEC tf-idf"]) 
     (save-as-csv tfidf-norm (str my-home "/resources/data1/"data-name-str"/"data-name-str"-tfidf-norm.csv") ["word"  "NSBRs tf-idf"  "SBRs tf-idf"  "FARSEC tf-idf"])]))

(defn run-experiment-2a []
  (wpp-experiment-2 experiment-2a))
  
(defn run-experiment-2b []
  (wpp-experiment-2 experiment-2b))

(defn run-experiment-2c []
  (tpp-experiment-2 experiment-2c))

(defn run-experiment-2d 
  ""
  []
  (tpp-experiment-2 experiment-2d))

(defn run-experiment-3a 
  "
   Returns mean average precision of test bug labels, sorted by report ids. 
   This is the baseline.

   Example: 
            (def testid-data-path (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)) 
            (run-experiment-3a (str my-home test-data-path))
  "
  [testid-data-path]
  (let [baseline  (experiment-3a testid-data-path)
        test      (read-as-csv testid-data-path)
        nbaseline (conj 
                    (apply vector 
                      (rest 
                        (range 0 (count (rest test)) 
                         (int (Math/ceil (/ (count (rest test)) 10)))))) 
                    (count (rest test)))]
    (map #(mean-avg-precision baseline %) nbaseline)))
        
(defn run-experiment-3b 
  "
   Returns mean average precision of test bug labels, sorted by report ids then prediction 
   results. Used for WPP and TPP results.

   Example: WPP
            (def train-data-path  (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path   (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (run-experiment-3b train-data-path test-data-path testid-data-path logistic-regression)
  
   Example: TPP, source = ambari, target = wicket
            (def train-data-path  (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path   (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (run-experiment-3b train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path test-data-path testid-data-path best-ml-fnc]
  (let [wpp-tpp   (experiment-3b train-data-path test-data-path testid-data-path best-ml-fnc)
        test      (read-as-csv testid-data-path)
        nbaseline (conj 
                    (apply vector 
                      (rest 
                        (range 0 (count (rest test)) 
                         (int (Math/ceil (/ (count (rest test)) 10)))))) 
                    (count (rest test)))]
    (map #(mean-avg-precision wpp-tpp %) nbaseline)))

(defn run-experiment-3c 
  "
   Returns mean average precision of test bug labels, sorted by 
   1) report ids then 
   2) bug report scores then 
   3) prediction results. 
   Used for WPPx and TPPx results.

   Example: WPPx
            (def train-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (run-experiment-3c train-data-path test-data-path testid-data-path logistic-regression)

   Example: TPPx, source = ambari, target = wicket
            (def train-data-path (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (run-experiment-3c train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path test-data-path testid-data-path for-trainmap best-ml-fnc]
  (let [wppx-tppx (experiment-3c train-data-path test-data-path testid-data-path for-trainmap best-ml-fnc)
        test      (read-as-csv testid-data-path)
        nbaseline (conj 
                    (apply vector 
                      (rest 
                        (range 0 (count (rest test)) 
                         (int (Math/ceil (/ (count (rest test)) 10)))))) 
                    (count (rest test)))]
    (map #(mean-avg-precision (take % wppx-tppx) %) nbaseline)))


(defn run-experiment-3d 
  "
   Returns mean average precision of test bug labels, sorted by 
   1) report ids then 
   2) bug report scores then 
   3) prediction results. 
   Used for WPPx and TPPx results.

   Example: WPPx
            (def train-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (run-experiment-3c train-data-path test-data-path testid-data-path logistic-regression)

   Example: TPPx, source = ambari, target = wicket
            (def train-data-path (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (run-experiment-3c train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path test-data-path testid-data-path for-trainmap best-ml-fnc]
  (let [wppx-tppx (experiment-3d train-data-path test-data-path testid-data-path for-trainmap best-ml-fnc)
        test      (read-as-csv testid-data-path)
        nbaseline (conj 
                    (apply vector 
                      (rest 
                        (range 0 (count (rest test)) 
                         (int (Math/ceil (/ (count (rest test)) 10)))))) 
                    (count (rest test)))]
    (map #(mean-avg-precision (take % wppx-tppx) %) nbaseline)))

(defn run-experiment-3e 
  "
   Returns mean average precision of test bug labels, sorted by 
   1) report ids then 
   2) bug report scores then 
   3) prediction results. 
   Used for WPPx and TPPx results.

   Example: WPPx
            (def train-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-mat-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicket-matid-test.csv) 1)))
            (run-experiment-3c train-data-path test-data-path testid-data-path logistic-regression)

   Example: TPPx, source = ambari, target = wicket
            (def train-data-path (str my-home (subs (str :/resources/data1/ambari/ambari-mat-train.csv) 1)))
            (def test-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-cross-test.csv) 1)))
            (def testid-data-path (str my-home (subs (str :/resources/data1/wicket/wicketambari-crossid-test.csv) 1)))
            (run-experiment-3c train-data-path test-data-path testid-data-path naive-bayes)
  "
  [train-data-path train-data-path2 test-data-path test-data-path2 testid-data-path testid-data-path2 for-trainmap best-ml-fnc]
  (let [wppx-tppx (experiment-3e train-data-path train-data-path2 test-data-path test-data-path2 testid-data-path testid-data-path2 for-trainmap best-ml-fnc)
        test      (read-as-csv testid-data-path)
        nbaseline (conj 
                    (apply vector 
                      (rest 
                        (range 0 (count (rest test)) 
                         (int (Math/ceil (/ (count (rest test)) 10)))))) 
                    (count (rest test)))]
    (map #(mean-avg-precision (take % wppx-tppx) %) nbaseline)))



;----------make charts for popularity of security related keywords-----XXXXXXX
(defn chart-experiment-1
  "
   Examples:
   
   (chart-experiment-1 \"wicket\" 'Wicket)
   (chart-experiment-1 'ambari 'Ambari)
   (chart-experiment-1 'camel 'Camel)
   (chart-experiment-1 'derby 'Derby)
   (chart-experiment-1 'chromium 'Chromium)
  "
  [data-name-str title]
  (let [norm-data (transpose (read-as-csv (str my-home "/resources/data1/"data-name-str"/"data-name-str"-tfidf-norm.csv")))
        x1        (rest (first norm-data))
        x         (range 1 101)
        y1        (map read-string (rest (second norm-data)))
        y2        (map read-string (rest (nth norm-data 2)))
        y3        (map read-string (rest (last norm-data)))]
    (doto (set-background-alpha  (xy-plot x y3 :legend true :title (str title " Keyword Popularity") :x-label "Top 100 Security Related Keywords" :y-label "Normalized tf-idf" :points false :series-label "FARSEC tf-idf") 1)
      set-theme-bw
      (add-lines x y2 :points false :series-label "SBRs tf-idf")
      (add-lines x y1 :points false :series-label "NSBRs tf-idf")
      (set-stroke :width 5 :dataset 0)
      (set-stroke-color java.awt.Color/lightGray :dataset 0)
      (set-stroke :width 2 :dash 5 :dataset 1)
      (set-stroke-color java.awt.Color/red :dataset 1)
      (set-stroke :width 2 :dash 1 :dataset 2)
      (set-stroke-color java.awt.Color/green :dataset 2)
      (save (str my-home "/results1/"data-name-str"-tfidf-incanter.png")))))



;----------boxplots for experiment 2 WPP and WPPx----------------------
(defn chart-experiment-2
  "
   Generate boxplots for g-measures of WPP and WPPx. \"output-boxplot.csv\") 
  "
  [result-file-path]
  (let [result (read-dataset result-file-path)
        g-wpp  (map int (rest (sel result :cols 0)))
        g-wppx (map int (rest (sel result :cols 1)))]
   (doto (set-background-alpha (box-plot g-wpp
                                :title "G-measures Before and After Filtering"
                                :legend true
                                :y-label "g-measures (%)"
                                :series-label "Before") 0.3)
         (set-stroke-color java.awt.Color/red :series 0)
         (add-box-plot g-wppx :series-label "After")
         (set-stroke-color java.awt.Color/green :series 1)
         (save (str my-home "/results1/output-boxplot.png")))))



;----------run experiments for mean-avg-precision charts---------------
(defn map-charts-data
  "
   Get the mean average precisions and y-range from table.
  "
  [map-data-path]
  (let [map-data    (transpose (rest (read-as-csv map-data-path)))
        x           (range 1 11)
        tppx        (map read-string (nth map-data 0))
        wppx        (map read-string (nth map-data 1))
        tpp         (map read-string (nth map-data 2))
        wpp         (map read-string (nth map-data 3))
        baseline    (map read-string (nth map-data 4))
        the-y-range (fn [lsts] (apply max (apply concat lsts)))]
    [(the-y-range [tppx wppx tpp wpp baseline]) 
     x tppx wppx tpp wpp baseline]))

(defn map-chart
  "
   Generate one chart.
  "
  [y-max x tppx wppx tpp wpp baseline target title outputname ranking?]
  (doto (set-background-alpha (xy-plot x tppx :legend true :title (str title " MAP " ranking?) :x-label "Decile" :y-label "MAP" :points true :series-label "tppx") 1)
      set-theme-bw
      (add-lines x wppx :points true :series-label "wppx") 
      (add-lines x tpp  :points true :series-label "tpp") 
      (add-lines x wpp  :points true :series-label "wpp") 
      (add-lines x baseline :points true :series-label "baseline") 
      (set-stroke-color java.awt.Color/darkGray :dataset 0)
      (set-stroke-color java.awt.Color/green :dataset 1)
      (set-stroke-color java.awt.Color/magenta :dataset 2)
      (set-stroke-color java.awt.Color/red :dataset 3)
      (set-stroke-color java.awt.Color/blue :dataset 4)
      (set-y-range 0 y-max)
      (save (str my-home "/results1/"target"-map-farsec-incanter-"outputname".png"))))

(defn map-charts-tables
  "
   Generates the mean average precisions for each treatment.
  
   Examples: For 'Without-Ranking, before
   
   (map-charts-tables 'wicket wicket-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'ambari ambari-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'camel camel-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'derby derby-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'chromium chromium-experiment-3 run-experiment-3d 'before)
  "
  [target target-exp-fnc run-exp-fnc outputname]
  (let [map-data (target-exp-fnc target run-exp-fnc outputname)]
    (save-as-csv map-data 
      (str my-home "/results1/"target"-map-farsec-gnuplot-"outputname".csv") 
      (if (= outputname "tpp")
        ["best tppx" "tpp" "best tpp" "tppx" "baseline"]
        ["best wppx" "wpp" "best wpp" "wppx" "baseline"]))))

(defn map-charts-tables2
  "
   Generates the mean average precisions for each treatment.
  
   Examples: For 'Without-Ranking, before
   
   (map-charts-tables2 'wicket wicket-experiment-3 run-experiment-3d 'before)
   (map-charts-tables2 'ambari ambari-experiment-3 run-experiment-3d 'before)
   (map-charts-tables2 'camel camel-experiment-3 run-experiment-3d 'before)
   (map-charts-tables2 'derby derby-experiment-3 run-experiment-3d 'before)
   (map-charts-tables2 'chromium chromium-experiment-3 run-experiment-3d 'before)
  "
  [target target-exp-fnc run-exp-fnc outputname]
  (let [map-data (target-exp-fnc target run-exp-fnc outputname)]
    (save-as-csv map-data 
      (str my-home "/results1/"target"-map-farsec-gnuplot-"outputname".csv") 
      (if (= outputname "tpp")
        ["best tppx" "tpp" "best tpp" "tppx" "baseline"]
        ["best wppx" "wpp" "best wpp" "wppx" "baseline"]))))

(defn map-charts
  "
   Generate charts from the mean average precisions.
  "
  [target title] ; (map-charts 'wicket 'Wicket)
  (let [before-results  (map-charts-data (str my-home "/results1/"target"-map-farsec-incanter-wpp.csv"))
        after-results   (map-charts-data (str my-home "/results1/"target"-map-farsec-incanter-tpp.csv"))
        y-max           (max (nth before-results 0) (nth after-results 0))]
    (map-chart 
      y-max
      (nth before-results 1) ;x
      (nth before-results 2) ;tppx
      (nth before-results 3) ;wppx
      (nth before-results 4) ;tpp
      (nth before-results 5) ;wpp
      (nth before-results 6) ;baseline
      target
      title
      "before"
      "Without Ranking")
    (map-chart 
      y-max
      (nth after-results 1) ;x
      (nth after-results 2) ;tppx
      (nth after-results 3) ;wppx
      (nth after-results 4) ;tpp
      (nth after-results 5) ;wpp
      (nth after-results 6) ;baseline
      target
      title
      "after"
      "With Ranking")))

(defn wicket-experiment-3 [target exp-fnc] ;exp-fnc = run-experiment-3c (after) or run-experiment-3d (before)
  (->> [(exp-fnc (str my-home "/resources/data1/camel/camel-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"camel-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"camel-farsecid-test.csv") (str my-home "/resources/data1/camel/camel-mat-train.csv") naive-bayes)
        (exp-fnc (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") (str my-home "/resources/data1/"target"/"target"-mat-train.csv") logistic-regression)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") naive-bayes)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
        (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
       (transpose)
       (matrix)))

(defn ambari-experiment-3 [target exp-fnc]
  (->> [(exp-fnc (str my-home "/resources/data1/derby/derby-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"derby-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"derby-farsecid-test.csv") (str my-home "/resources/data1/derby/derby-mat-train.csv") random-forest)
        (exp-fnc (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") (str my-home "/resources/data1/"target"/"target"-mat-train.csv") naive-bayes)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") multilayer-perceptron)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
        (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
       (transpose)
       (matrix)))

(defn camel-experiment-3 [target exp-fnc]
  (->> [(exp-fnc (str my-home "/resources/data1/derby/derby-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"derby-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"derby-farsecid-test.csv") (str my-home "/resources/data1/derby/derby-mat-train.csv") multilayer-perceptron)
        (exp-fnc (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") (str my-home "/resources/data1/"target"/"target"-mat-train.csv") naive-bayes)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") logistic-regression)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
        (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
       (transpose)
       (matrix)))
  
(defn derby-experiment-3 [target exp-fnc]
  (->> [(exp-fnc (str my-home "/resources/data1/ambari/ambari-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") (str my-home "/resources/data1/ambari/ambari-mat-train.csv") logistic-regression)
        (exp-fnc (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") (str my-home "/resources/data1/"target"/"target"-mat-train.csv") logistic-regression)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") naive-bayes)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") naive-bayes)
        (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
       (transpose)
       (matrix)))  

(defn chromium-experiment-3 [target exp-fnc]
  (->> [(exp-fnc (str my-home "/resources/data1/ambari/ambari-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") (str my-home "/resources/data1/ambari/ambari-mat-train.csv") logistic-regression)
        (exp-fnc (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") (str my-home "/resources/data1/"target"/"target"-mat-train.csv") logistic-regression)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") logistic-regression)
        (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") naive-bayes)
        (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
       (transpose)
       (matrix)))



;----------------------Match-----------------------

(defn wicket-experiment-match-3 [target exp-fnc treatment] ;exp-fnc = run-experiment-3c (after) or run-experiment-3d (before), treatment wpp tpp
  (if (= treatment "tpp")
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"camel-cross-train.csv")
            (str my-home "/resources/data1/camel/camel-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"camel-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"camel-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"camel-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"camel-farsecid-test.csv") 
            (str my-home "/resources/data1/camel/camel-mat-train.csv") 
            naive-bayes)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"camel-cross-train.csv") (str my-home "/resources/data1/"target"/"target"camel-cross-test.csv") (str my-home "/resources/data1/"target"/"target"camel-crossid-test.csv") naive-bayes)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") naive-bayes)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv")
            (str my-home "/resources/data1/ambari/ambari-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") 
            (str my-home "/resources/data1/ambari/ambari-mat-train.csv") 
            naive-bayes)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") multilayer-perceptron)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            multilayer-perceptron)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))))

(defn ambari-experiment-match-3 [target exp-fnc treatment]
  (if (= treatment "tpp")
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv")
            (str my-home "/resources/data1/derby/derby-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"derby-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"derby-farsecid-test.csv") 
            (str my-home "/resources/data1/derby/derby-mat-train.csv") 
            random-forest)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") random-forest)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") multilayer-perceptron)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv")
            (str my-home "/resources/data1/derby/derby-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"derby-farsec-test.csv")
            (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"derby-farsecid-test.csv") 
            (str my-home "/resources/data1/derby/derby-mat-train.csv") 
            multilayer-perceptron)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            naive-bayes)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") naive-bayes)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv")
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            logistic-regression)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))))

(defn camel-experiment-match-3 [target exp-fnc treatment]
  (if (= treatment "tpp")
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") 
            (str my-home "/resources/data1/derby/derby-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-farsecid-test.csv") 
            (str my-home "/resources/data1/derby/derby-mat-train.csv") 
            multilayer-perceptron)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") multilayer-perceptron)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") logistic-regression)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"derby-cross-train.csv") 
            (str my-home "/resources/data1/derby/derby-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-cross-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-crossid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"derby-farsecid-test.csv") 
            (str my-home "/resources/data1/derby/derby-mat-train.csv") 
            logistic-regression)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            naive-bayes)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") naive-bayes)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            logistic-regression)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))))
  
(defn derby-experiment-match-3 [target exp-fnc treatment]
  (if (= treatment "tpp")
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv")
            (str my-home "/resources/data1/ambari/ambari-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") 
            (str my-home "/resources/data1/ambari/ambari-mat-train.csv") 
            logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") naive-bayes)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv")
            (str my-home "/resources/data1/ambari/ambari-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") 
            (str my-home "/resources/data1/ambari/ambari-mat-train.csv") 
            naive-bayes)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") naive-bayes)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            naive-bayes)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))))  

(defn chromium-experiment-match-3 [target exp-fnc treatment]
  (if (= treatment "tpp")
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv")
            (str my-home "/resources/data1/ambari/ambari-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") 
            (str my-home "/resources/data1/ambari/ambari-mat-train.csv") 
            logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv") (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv") (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv") logistic-regression)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-train.csv")
            (str my-home "/resources/data1/ambari/ambari-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-cross-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"ambari-crossid-test.csv")
            (str my-home "/resources/data1/"target"/"target"ambari-farsecid-test.csv") 
            (str my-home "/resources/data1/ambari/ambari-mat-train.csv") 
            logistic-regression)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))
    (->> [(exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv")
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv")
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") logistic-regression)
          (run-experiment-3b (str my-home "/resources/data1/"target"/"target"-mat-train.csv") (str my-home "/resources/data1/"target"/"target"-mat-test.csv") (str my-home "/resources/data1/"target"/"target"-matid-test.csv") naive-bayes)
          (exp-fnc 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-train.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsec-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-matid-test.csv")
            (str my-home "/resources/data1/"target"/"target"-farsecid-test.csv") 
            (str my-home "/resources/data1/"target"/"target"-mat-train.csv") 
            naive-bayes)
          (run-experiment-3a (str my-home "/resources/data1/"target"/"target"-matid-test.csv"))]
         (transpose)
         (matrix))))