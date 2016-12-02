(ns farsec.evaluate
  (:use (seer naive-bayes random-forest logistic-regression ib-k multilayer-perceptron cpdp))
  (:use [farsec utils])
  (:use (incanter core stats io))
  (require [clojure.java.io :as io]))

;---------------prediction------------------
(defn run-results
  [src tar & {:keys [learner file-cpdp]
                :or {learner naive-bayes file-cpdp ""}}]  
  (let [result    (cpdp src tar learner)
        g-measure (get result :g)
        pd        (get result :pd)
        pf        (get result :pf)
        f         (get result :f)
        prec      (get result :prec)
        err       (get result :err)
        tn        (get result :tn)
        tp        (get result :tp)
        fn        (get result :fn)
        fp        (get result :fp)
        pattern0   #"\w+\.csv"
        pattern1   #"\w+\-\w+\.csv"
        pattern2   #"\w+\-\w+\-\w+\.csv"
        pattern3   #"\w+\-\w+\-\w+\-\w+\.csv"
        learn      (re-find (re-pattern #"\w+\_\w+") (str learner))]        
    (append-to-file 
      file-cpdp 
      (str (re-find pattern2 src)","(re-find pattern2 tar)","learn","tn","tp","fn","fp","err","pd","pf","prec","g-measure "\n"))))

(defn cr-old [source-file target-file sbr-dir-new nsbr-dir-new learner]
  (let [;mysbrlst  (rest (list-of-files "/resources/data/derby/derby-sbr-new/" 10))
        ;mynsbrlst (rest (list-of-files "/resources/data/derby/derby-nsbr-new/" 10))
        mysbrlst  (rest (list-of-files sbr-dir-new 10))
        mynsbrlst (rest (list-of-files nsbr-dir-new 10))
        source    (format-data source-file)
        target    (format-data target-file)
        buildc    (learner source)
        cmatrix   (map #(vector (.classValue (.get target %)) (.classifyInstance buildc (.get target %))) (range (.numInstances target)))
        bmatrix   (apply concat [mysbrlst mynsbrlst])]
    (apply vector (map #(apply vector (flatten (vector %1 %2))) bmatrix cmatrix))))

(defn cr [source-file target-file targetid-file learner]
  (let [source    (format-data source-file)
        target    (format-data target-file)
        targetid  (rest (first (transpose (read-as-csv targetid-file))))
        buildc    (learner source)
        cmatrix   (map #(vector (.classValue (.get target %)) (.classifyInstance buildc (.get target %))) (range (.numInstances target)))]
    (apply vector (map #(apply vector (flatten (vector %1 %2))) targetid cmatrix))))

;---------------mean average precision------------------
(defn mean-avg-precision [want val]
  (let [precision-at (fn [] 
                       (loop [w want i 1 result []]
                         (if (= i (inc (count w)))
                           result
                           (recur
                             w
                             (inc i)
                             (conj result (* 100 (float (/ (reduce + (take i w)) (count (take i w))))))))))
        mean-precision-at (fn [] (float (/ (reduce + (take val (precision-at))) val)))]
    (mean-precision-at)))

(defn fk4 [dat]
  (let [one (sort-by #(nth % 2) > dat)
        two (second (transpose (sort-by #(nth % 3) > one)))]
    (map #(read-string %) two)))

(defn fk5 [dat]
  (let [;one (sort-by #(nth % 2) > dat)
        two (second (transpose (sort-by #(nth % 3) > dat)))]
    (map #(read-string %) two)))
