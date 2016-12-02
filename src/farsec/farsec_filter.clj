(ns farsec.farsec-filter
  (:use [farsec context utils])
  (:use (incanter core stats)))

(defn score-keywords 
  "Score the topn security related keywords."
  [trainpath]
  (let [data        (read-as-csv trainpath)
        header      (first data)
        sbr         (filter #(= (last %) "1") (rest data))
        nsbr        (filter #(= (last %) "0") (rest data))
        numsbr      (count sbr)
        numnsbr     (count nsbr)
        farsec-sbr  (fn [X] (reduce + (map read-string (rest X)))) 
        farsec-nsbr (fn [Y] (reduce + (map read-string (rest Y))))
        farsec-fnc  (fn [X Y] 
                       (if (= (farsec-nsbr Y) 0) 
                         0.0 
                         (max 0.01 
                          (min 0.99 
                            (float 
                              (/ (min 1 (/ (farsec-sbr X) numsbr)) 
                                 (+ (min 1 (/ (farsec-nsbr Y) numnsbr))
                                    (min 1 (/ (farsec-sbr X) numsbr)))))))))]
    (butlast (map #(vector %1 (farsec-fnc %2 %3)) header (transpose sbr) (transpose nsbr)))))

(defn filter-bug-reports 
  "Score the train bug reports and remove any NSBRs with a score greater than 
   0.9. Also remove keywords with scores less than the 25th percentile of all 
   keyword scores. 

   Returns new train and test data with the same security related keywords for 
   each matrix."
  [trainpath testpath trainmap outputname-train outputname-test]
  (let [data         (read-as-csv trainpath)
        data-test    (read-as-csv testpath)
        header       (first data)
        sbr          (filter #(= (last %) "1") (rest data))
        nsbr         (filter #(= (last %) "0") (rest data))
        test-wds     (fn [x]
                       (let [text-map    (map #(vector %1 %2) header x)
                             text        (fn [] (filter #(and (not= (first %) "id") (not= (second %) "0")) text-map))
                             text-expand (if (empty? (text)) nil (map #(repeat (read-string (get (into {} (text)) % "0")) %) (map first (text))))] 
                         (flatten text-expand)))
        farsec-head  (fn []
                       (let [lower (first (quantile (filter #(not= % 0.0) (map second trainmap)) :probs [0.25]))]
                         (conj (apply vector (cons (first header) (filter #(>= (get trainmap % 0.0) lower) (butlast header)))) (last header)))) 
        data-new     (fn [dat] (transpose (filter #(member? (first %) (farsec-head)) (transpose dat))))
        farsec-score (fn [x] ;row by row
                       (let [probs  (map #(get trainmap %) (test-wds x)) 
                             prod    (reduce * probs)
                             prodneg (reduce * (map #(- 1 %) probs))] 
                         (if (and (= prod 0.0) (= prodneg 0.0)) 0.0 (/ prod (+ prod prodneg)))))]
       [(save-as-csv (rest (data-new data-test)) outputname-test (farsec-head))
        (save-as-csv
         ;(rest
          (rest 
           (data-new 
             (cons header 
              (apply concat 
                [sbr
                 (filter #(< (farsec-score %) 0.9) nsbr)])))) outputname-train (farsec-head))]))

(defn run-filter-bug-reports1 [dataname]
  (filter-bug-reports 
    (str my-home "/resources/data1/"dataname"/"dataname"-matid-train.csv")
    (str my-home "/resources/data1/"dataname"/"dataname"-matid-test.csv")
    (into {} (score-keywords (str my-home "/resources/data1/"dataname"/"dataname"-mat-train.csv")))
    (str my-home "/resources/data1/"dataname"/"dataname"-farsecid-train.csv")
    (str my-home "/resources/data1/"dataname"/"dataname"-farsecid-test.csv")))

(defn run-filter-bug-reports2 [dataname]
  (let [train  (filter-att (str my-home "/resources/data1/"dataname"/"dataname"-farsecid-train.csv") "id")
        test   (filter-att (str my-home "/resources/data1/"dataname"/"dataname"-farsecid-test.csv") "id")
        header (first train)]
    [(save-as-csv (rest train) (str my-home "/resources/data1/"dataname"/"dataname"-farsec-train.csv") header)
     (save-as-csv (rest test)  (str my-home "/resources/data1/"dataname"/"dataname"-farsec-test.csv") header)]))                 

(defn score-bug-reports
  "Score the test bug reports."
  [source-data-file targetid-data-file]
  (let [trainmap     (into {} (score-keywords source-data-file))
        targetid     (read-as-csv targetid-data-file)
        tarid-header (first targetid)
        tarid-data   (rest targetid)
        header-data  (fn [report]
                       (let [output (map #(vector %1 %2) tarid-header report)] 
                        (remove #(and (= (second %) "0") (not= (first %) "buglabel")) output))) 
        expand-vec0  (fn [report]
                       (if (= (count (header-data report)) 2) 
                         nil
                         (flatten 
                           (map #(repeat (read-string (second %)) (first %)) (butlast (rest (header-data report)))))))
        expand-vec   (fn [report]
                       (if (= (count (header-data report)) 2) 
                         nil
                         (map first (butlast (rest (header-data report))))))
        score-report (fn [report]
                       (if (= (expand-vec report) nil) 
                         0.0
                         (let [probs0  (map #(get trainmap % 0.0) (expand-vec report))
                               probs   (remove #(= % 0.0) probs0)
                               prod    (reduce * probs)
                               prodneg (reduce * (map #(- 1 %) probs))] 
                           (if (and (= prod 0.0) (= prodneg 0.0)) 0.0 (float (/ prod (+ prod prodneg)))))))] 
    (map #(vector (first %) (last %) (score-report %)) tarid-data)))
