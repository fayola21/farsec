(ns farsec.data
  (:use [farsec context examples farsec-filter utils])
  (:use (incanter core stats io))
  (require [clojure.java.io :as io]))

;-------------------------------------
; Create data files from bug reports.
;-------------------------------------

;-----------------------------------------------
; Putting reports into directories.
;-----------------------------------------------
(defn exp-0
  "
   Takes original data files and split them into train and test set. For each 
   set, split into SBRs and NSBRs. Then turn each set into directory with a
   file for each bug report. The experiments will only use the data in the 
   directories.

   Example: 

            (def data-name-pathway (str my-home (subs (str :/resources/data1/wicket/wicket-rest.csv) 1)))
            (exp-0 data-name-pathway 'wicket)
  "
  [outdir data-name-path data-name-str split] ;split = 0.5, 0.75 (for ambari)
  (let [data        (read-as-csv data-name-path)
        dummy-att   (map read-string (map first (rest data))) ; to make sure the sort is correct.
        data-rows   (sort-by first (map #(flatten (vector %1 %2)) dummy-att (rest data)))
        nsplit      (int (* (count data-rows) split))
        train       (take nsplit data-rows)
        test        (drop nsplit data-rows)
        train-sbrs  (filter #(= (last %) "1") train)
        train-nsbrs (filter #(= (last %) "0") train)
        test-sbrs   (filter #(= (last %) "1") test)
        test-nsbrs  (filter #(= (last %) "0") test)
        dir-fnc     (fn [one dir-path] (spit (str dir-path (str (second one)".txt")) (nth one 2)))]
     [(map #(dir-fnc % (str my-home "/resources/"outdir"/" data-name-str "/" data-name-str"-sbr-old/")) train-sbrs)
      (map #(dir-fnc % (str my-home "/resources/"outdir"/" data-name-str "/" data-name-str"-nsbr-old/")) train-nsbrs)
      (map #(dir-fnc % (str my-home "/resources/"outdir"/" data-name-str "/" data-name-str"-sbr-new/")) test-sbrs)
      (map #(dir-fnc % (str my-home "/resources/"outdir"/" data-name-str "/" data-name-str"-nsbr-new/")) test-nsbrs)]))

;-------------------------------------------------------------------
; Creating data matrices with the top n security related keywords.
; Used for WPP and TPP experiments.
;-------------------------------------------------------------------
(defn get-text-frequencies
  [header text]
  (let [text-words (detokenize (normalize (tokenize text)))]
    (map #(count (re-seq (re-pattern (clojure.string/replace % #"[\[\])(*?]" "")) text-words)) header)))

(defn get-frequency-tables ;gft
  [sbr-old-dir nsbr-old-dir sbr-new-dir nsbr-new-dir outputname outputpath header]
  (let [mysbroldlst  (rest (list-of-files sbr-old-dir 10))
        mynsbroldlst (rest (list-of-files nsbr-old-dir 10))
        mysbrnewlst  (rest (list-of-files sbr-new-dir 10))
        mynsbrnewlst (rest (list-of-files nsbr-new-dir 10))
        exp-data-fnc (fn [data-dir data-lst type]
                       (map #(conj 
                               (apply vector 
                                (cons (str %) 
                                 (get-text-frequencies 
                                  header 
                                   (slurp (str my-home data-dir %))))) type)
                         data-lst))
        train        (apply concat [(exp-data-fnc sbr-old-dir mysbroldlst 1) 
                                    (exp-data-fnc nsbr-old-dir mynsbroldlst 0)])
        test         (apply concat [(exp-data-fnc sbr-new-dir mysbrnewlst 1) 
                                    (exp-data-fnc nsbr-new-dir mynsbrnewlst 0)])]
    [(save-as-csv train (str my-home outputpath outputname"id-train.csv") 
      (conj (apply vector (cons "id" header)) "buglabel"))
     (save-as-csv test (str my-home outputpath outputname"id-test.csv") 
      (conj (apply vector (cons "id" header)) "buglabel"))
     (save-as-csv (transpose (rest (transpose train))) (str my-home outputpath outputname"-train.csv") 
      (conj (apply vector header) "buglabel"))
     (save-as-csv (transpose (rest (transpose test))) (str my-home outputpath outputname"-test.csv") 
      (conj (apply vector header) "buglabel"))]))
  
(defn run-gft 
  "
   Returns csv files containing the data matrix for the frequencies of security
   related keywords in each bug report. For each project, they are separated 
   into train and test data sets. The format is [id, keywords, buglabel].
  
   Examples: 
  
            (run-gft 'wicket 'wicket-mat 100)
            (run-gft 'ambari 'ambari-mat 100)
            (run-gft 'camel 'camel-mat 100)
            (run-gft 'derby 'derby-mat 100)
            (run-gft 'chromium 'chromium-mat 100)
  "
  [dataname outputname n]
  (get-frequency-tables 
    (str "/resources/data1/"dataname"/"dataname"-sbr-old/")
    (str "/resources/data1/"dataname"/"dataname"-nsbr-old/") 
    (str "/resources/data1/"dataname"/"dataname"-sbr-new/") 
    (str "/resources/data1/"dataname"/"dataname"-nsbr-new/")
    outputname
    (str "/resources/data1/"dataname"/")
    (exp-1 (str "/resources/data1/"dataname"/"dataname"-sbr-old") n))) 

(defn run-gft-cross
  "
   Returns csv files containing the data matrix for the frequencies of security
   related keywords in each bug report from another source. For each project, 
   they are separated into train and test data sets. The format is 
   [id, keywords, buglabel].
  
   Example: 
  
           (run-gft-cross 'wicket 'ambari 'wicket-cross 100))
   
   Comment: 
  
           Can run multiple cross data sets with exp-5 below.
  "
  [dataname crossdataname outputname n]
  (get-frequency-tables 
    (str "/resources/data1/"crossdataname"/"crossdataname"-sbr-old/")
    (str "/resources/data1/"crossdataname"/"crossdataname"-nsbr-old/") 
    (str "/resources/data1/"dataname"/"dataname"-sbr-new/") 
    (str "/resources/data1/"dataname"/"dataname"-nsbr-new/")
    outputname
    (str "/resources/data1/"dataname"/")
    (exp-1 (str "/resources/data1/"crossdataname"/"crossdataname"-sbr-old") n)))

(defn exp-5 [data-name cross-names] ; Example: (exp5 "wicket" ["ambari" "camel" "derby" "chromium"])
  "
   Returns cross (TPP) data sets. For example if wicket is the target 
   data set and ambari is the source data set, the security related keywords
   from ambari are used to build a test data matrix for wicket. Also ambari is
   training set and new test wicket is test set.
  "
  (map #(run-gft-cross data-name % (str data-name % "-cross") 100) cross-names))

;-------------------------------------------------------------------
; Creating data matrices with the top n security related keywords.
; Used for WPPx and TPPx experiments.
;-------------------------------------------------------------------
(defn exp-3
  "
   Apply farsec to train data. Making sure that the keywords also match with
   test data. For WPPx experiments.
  "
  [data-name]
  (run-filter-bug-reports1 data-name)
  (run-filter-bug-reports2 data-name))

(defn farsec-cross [crossdataname sbr-new-dir nsbr-new-dir outputname outputpath]
  "
   Returns test data for farsec source header.
  "
  (let [train  (read-as-csv (str my-home "/resources/data1/"crossdataname"/"crossdataname"-farsec-train.csv"))
        header (butlast (first train))
        mysbrnewlst  (rest (list-of-files sbr-new-dir 10))
        mynsbrnewlst (rest (list-of-files nsbr-new-dir 10))
        exp-data-fnc (fn [data-dir data-lst type]
                       (map #(conj 
                               (apply vector 
                                (cons (str %) 
                                 (get-text-frequencies 
                                  header 
                                   (slurp (str my-home data-dir %))))) type)
                         data-lst))
        test         (apply concat [(exp-data-fnc sbr-new-dir mysbrnewlst 1) 
                                    (exp-data-fnc nsbr-new-dir mynsbrnewlst 0)])]
    (save-as-csv test (str my-home outputpath outputname"id-test.csv") 
      (conj (apply vector (cons "id" header)) "buglabel"))))

(defn run-farsec-cross1 [dataname crossdataname outputname]
  (farsec-cross
    crossdataname
    (str "/resources/data1/"dataname"/"dataname"-sbr-new/") 
    (str "/resources/data1/"dataname"/"dataname"-nsbr-new/")
    outputname
    (str "/resources/data1/"dataname"/")))

(defn run-farsec-cross2 [dataname crossdataname]
  (let [test  (filter-att (str my-home "/resources/data1/"dataname"/"dataname crossdataname"-farsecid-test.csv") "id")
        header (first test)]
    (save-as-csv (rest test) (str my-home "/resources/data1/"dataname"/"dataname crossdataname"-farsec-test.csv") header)))

(defn exp-6 [data-name cross-names] ; Example (exp-6 "wicket" ["ambari" "camel" "derby" "chromium"])
  "Creates data sets for TPPx experiments."
  (map #(run-farsec-cross1 data-name % (str data-name % "-farsec")) cross-names))

(defn exp-7 [data-name cross-names]
  "Creates data sets for TPPx experiments. Must run exp-6 first."
  (map #(run-farsec-cross2 data-name %) cross-names))

