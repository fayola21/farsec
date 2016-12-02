(ns farsec.core
  (:use [opennlp.nlp])
  (:use [opennlp.tools.filters])
  (:use [clojure.tools.cli :only (cli)])
  (:use (seer naive-bayes random-forest logistic-regression ib-k multilayer-perceptron cpdp filtering))
  (:use (farsec context data evaluate examples farsec-filter tfidf utils))
  (:use (incanter core stats io charts))
  (require 
    [clojure.java.io :as io])
  (:gen-class))

(def learners [logistic-regression naive-bayes random-forest ib-k multilayer-perceptron])
    
(defn make-tbls [outdir dataname n]
  (get-frequency-tables 
    (str "/resources/data1/"dataname"/"dataname"-sbr-old/")
    (str "/resources/data1/"dataname"/"dataname"-nsbr-old/")
    (str "/resources/data1/"dataname"/"dataname"-sbr-new/") 
    (str "/resources/data1/"dataname"/"dataname"-nsbr-new/")
    (str dataname"-mat")
    (str "/"outdir"/")
    (exp-1 (str "/resources/data1/"dataname"/"dataname"-sbr-old") n)))

(defn make-cross-tbls [outdir crossdataname dataname n]
  (get-frequency-tables 
    (str "/resources/data1/"crossdataname"/"crossdataname"-sbr-old/")
    (str "/resources/data1/"crossdataname"/"crossdataname"-nsbr-old/")
    (str "/resources/data1/"dataname"/"dataname"-sbr-new/") 
    (str "/resources/data1/"dataname"/"dataname"-nsbr-new/")
    (str dataname crossdataname"-cross")
    (str "/"outdir"/")
    (exp-1 (str "/resources/data1/"crossdataname"/"crossdataname"-sbr-old") n)))

(defn make-farsec-tbls-0 [outdir crossdataname dataname type]
  (filter-bug-reports 
    (if type 
      (str my-home "/"outdir"/"dataname"-matid-train.csv")
      (str my-home "/"outdir"/" dataname crossdataname"-crossid-train.csv"))
    (if type
      (str my-home "/"outdir"/"dataname"-matid-test.csv")
      (str my-home "/"outdir"/" dataname crossdataname"-crossid-test.csv"))
    (if type 
      (into {} (score-keywords (str my-home "/"outdir"/"dataname"-mat-train.csv")))
      (into {} (score-keywords (str my-home "/"outdir"/" dataname crossdataname"-cross-train.csv"))))
    (if type
      (str my-home "/"outdir"/"dataname"-farsecid-train.csv")
      (str my-home "/"outdir"/"dataname crossdataname"-farsecid-train.csv"))
    (if type
      (str my-home "/"outdir"/"dataname"-farsecid-test.csv")
      (str my-home "/"outdir"/"dataname crossdataname"-farsecid-test.csv"))))

(defn make-farsec-tbls-1 [outdir crossdataname dataname type]
  (let [train  (if type 
                 (filter-att (str my-home "/"outdir"/"dataname"-farsecid-train.csv") "id")
                 (filter-att (str my-home "/"outdir"/"dataname crossdataname"-farsecid-train.csv") "id"))
        test   (if type
                 (filter-att (str my-home "/"outdir"/"dataname"-farsecid-test.csv") "id")
                 (filter-att (str my-home "/"outdir"/"dataname crossdataname"-farsecid-test.csv") "id"))
        header (first train)]
    [(save-as-csv (rest train) (if type 
                                 (str my-home "/"outdir"/"dataname"-farsec-train.csv")
                                 (str my-home "/"outdir"/"dataname crossdataname"-farsec-train.csv")) header)
     (save-as-csv (rest test)  (if type 
                                 (str my-home "/"outdir"/"dataname"-farsec-test.csv")
                                 (str my-home "/"outdir"/"dataname crossdataname"-farsec-test.csv")) header)]))

(defn make-farsec-tbls [outdir crossdataname dataname type]
  (make-farsec-tbls-0 outdir crossdataname dataname type)
  (make-farsec-tbls-1 outdir crossdataname dataname type))

(defn make-pop-tbls [outdir dataname n]
  (let [tfidf-vals (experiment-1
                     (str "/resources/data1/"dataname"/"dataname"-sbr-old")
                     (str "/resources/data1/"dataname"/"dataname"-nsbr-old")
                     (str my-home "/"outdir"/"dataname"-farsec-train.csv")
                     n)
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
    [(save-as-csv tfidf-vals (str my-home "/"outdir"/"dataname"-tfidf-vals.csv") ["word"  "NSBRs tf-idf"  "SBRs tf-idf"  "FARSEC tf-idf"]) 
     (save-as-csv tfidf-norm (str my-home "/"outdir"/"dataname"-tfidf-norm.csv") ["word"  "NSBRs tf-idf"  "SBRs tf-idf"  "FARSEC tf-idf"])]))

(defn make-pop-chart [outdir dataname]
  (let [norm-data (transpose (read-as-csv (str my-home "/"outdir"/"dataname"-tfidf-norm.csv")))
        x1        (rest (first norm-data))
        x         (range 1 101)
        y1        (map read-string (rest (second norm-data)))
        y2        (map read-string (rest (nth norm-data 2)))
        y3        (map read-string (rest (last norm-data)))]
    (doto (set-background-alpha  (xy-plot x y3 :legend true :title (str (clojure.string/capitalize dataname) " Keyword Popularity") :x-label "Top 100 Security Related Keywords" :y-label "Normalized tf-idf" :points false :series-label "FARSEC tf-idf") 1)
      set-theme-bw
      (add-lines x y2 :points false :series-label "SBRs tf-idf")
      (add-lines x y1 :points false :series-label "NSBRs tf-idf")
      (set-stroke :width 5 :dataset 0)
      (set-stroke-color java.awt.Color/lightGray :dataset 0)
      (set-stroke :width 2 :dash 5 :dataset 1)
      (set-stroke-color java.awt.Color/red :dataset 1)
      (set-stroke :width 2 :dash 1 :dataset 2)
      (set-stroke-color java.awt.Color/green :dataset 2)
      (save (str my-home "/"outdir"/"dataname"-tfidf-incanter.png")))))

(defn make-output-header [outdir treatment-output]
  (let [header (append-to-file (str my-home "/"outdir"/"treatment-output"-output.csv")
                 (str "train," "test," "learner," 
                  "tn," "tp," "fn," "fp," "err,"
                  "pd," "pf," "prec," "g-measure" "\n"))]
    header))

(defn make-prediction [outdir crossdataname dataname learner treatment]
  (cond
    (= treatment "wpp")  (run-results 
                          (str my-home "/"outdir"/"dataname"-mat-train.csv") ;source
                          (str my-home "/"outdir"/"dataname"-mat-test.csv")  ;target
                          :file-cpdp (str my-home "/"outdir"/"treatment"-output.csv")
                          :learner learner)
    (= treatment "wppx") (run-results 
                          (str my-home "/"outdir"/"dataname"-farsec-train.csv") ;source
                          (str my-home "/"outdir"/"dataname"-farsec-test.csv")  ;target
                          :file-cpdp (str my-home "/"outdir"/"treatment"-output.csv")
                          :learner learner)
    (= treatment "tpp")  (run-results 
                          (str my-home "/"outdir"/"dataname crossdataname"-cross-train.csv") ;source
                          (str my-home "/"outdir"/"dataname crossdataname"-cross-test.csv")  ;target
                          :file-cpdp (str my-home "/"outdir"/"treatment"-output.csv")
                          :learner learner)
    (= treatment "tppx") (run-results 
                          (str my-home "/"outdir"/"dataname crossdataname"-farsec-train.csv") ;source
                          (str my-home "/"outdir"/"dataname crossdataname"-farsec-test.csv")  ;target
                          :file-cpdp (str my-home "/"outdir"/"treatment"-output.csv")
                          :learner learner)
    :else "done"))

(defn make-predictions [outdir crossdataname dataname treatment]
  (make-output-header outdir treatment)
  (make-prediction outdir crossdataname dataname (nth learners 0) treatment)
  (make-prediction outdir crossdataname dataname (nth learners 1) treatment)
  (make-prediction outdir crossdataname dataname (nth learners 2) treatment)
  (make-prediction outdir crossdataname dataname (nth learners 3) treatment)
  (make-prediction outdir crossdataname dataname (nth learners 4) treatment))
   
(defn pick-learner [learner]
  (cond
    (= learner "logistic-regression") logistic-regression
    (= learner "naive-bayes") naive-bayes
    (= learner "random-forest") random-forest
    (= learner "ib-k") ib-k
    (= learner "multilayer-perceptron") multilayer-perceptron
    :else "done"))

; Example map for best prediction results with wicket and ambari. 
(defn make-wicket-experiment-3 [outdir target exp-fnc] ;exp-fnc = run-experiment-3c (after) or run-experiment-3d (before)
  (->> [(exp-fnc (str my-home "/"outdir"/"target"ambari-farsec-train.csv") (str my-home "/"outdir"/"target"ambari-farsec-test.csv") (str my-home "/"outdir"/"target"ambari-farsecid-test.csv") (str my-home "/"outdir"/"target"-mat-train.csv") naive-bayes)
        (exp-fnc (str my-home "/"outdir"/"target"-farsec-train.csv") (str my-home "/"outdir"/"target"-farsec-test.csv") (str my-home "/"outdir"/"target"-farsecid-test.csv") (str my-home "/"outdir"/"target"-mat-train.csv") logistic-regression)
        (run-experiment-3b (str my-home "/"outdir"/"target"ambari-cross-train.csv") (str my-home "/"outdir"/"target"ambari-cross-test.csv") (str my-home "/"outdir"/"target"ambari-crossid-test.csv") naive-bayes)
        (run-experiment-3b (str my-home "/"outdir"/"target"-mat-train.csv") (str my-home "/"outdir"/"target"-mat-test.csv") (str my-home "/"outdir"/"target"-matid-test.csv") logistic-regression)
        (run-experiment-3a (str my-home "/"outdir"/"target"-matid-test.csv"))]
       (transpose)
       (matrix)))

(defn make-map-tbls
  "
   Generates the mean average precisions for each treatment.
  
   Examples: For 'Without-Ranking, before
   
   (map-charts-tables 'wicket wicket-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'ambari ambari-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'camel camel-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'derby derby-experiment-3 run-experiment-3d 'before)
   (map-charts-tables 'chromium chromium-experiment-3 run-experiment-3d 'before)
  "
  [outdir target target-exp-fnc run-exp-fnc outputname] ; outputname = before or after
  (let [map-data (target-exp-fnc outdir target run-exp-fnc)]
    (save-as-csv map-data 
      (str my-home "/"outdir"/"target"-map-farsec-incanter-"outputname".csv") 
      ["tppx" "wppx" "tpp" "wpp" "baseline"])))

(defn make-map-chart
  "
   Generate one chart.
  "
  [outdir y-max x tppx wppx tpp wpp baseline target title outputname ranking?]
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
      (save (str my-home "/"outdir"/"target"-map-farsec-incanter-"outputname".png"))))


(defn make-map-charts
  "
   Generate charts from the mean average precisions.
  "
  [outdir target] ; (map-charts 'wicket 'Wicket)
  (let [before-results  (map-charts-data (str my-home "/"outdir"/"target"-map-farsec-incanter-before.csv"))
        after-results   (map-charts-data (str my-home "/"outdir"/"target"-map-farsec-incanter-after.csv"))
        y-max           (max (nth before-results 0) (nth after-results 0))]
    (make-map-chart 
      outdir
      y-max
      (nth before-results 1) ;x
      (nth before-results 2) ;tppx
      (nth before-results 3) ;wppx
      (nth before-results 4) ;tpp
      (nth before-results 5) ;wpp
      (nth before-results 6) ;baseline
      target
      (clojure.string/capitalize target)
      "before"
      "Without Ranking")
    (make-map-chart 
      outdir
      y-max
      (nth after-results 1) ;x
      (nth after-results 2) ;tppx
      (nth after-results 3) ;wppx
      (nth after-results 4) ;tpp
      (nth after-results 5) ;wpp
      (nth after-results 6) ;baseline
      target
      (clojure.string/capitalize target)
      "after"
      "With Ranking")))
      
(defn -main [& args]
  (let [[opts args banner]
        (cli args
          ["-h" "--help" "Show help" :flag true :default false]
          ["-d" "--data" "Path to corpus"]
          ["-t" "--tfidf" "Keywords with high tf-idf" :flag true :default false]
          ["--c1" "Popularity of keywords in different corpora" :flag true :default false]
          ["--target" "Project name as a string" :flag true :default false]
          ["-o" "--odir" "REQUIRED: Output directory for data" :default "wicket-data"]
          ["-p" "--proj" "REQUIRED: Project name" :default "wicket"]
          ["-c" "--cproj" "Source project for cross prediction" :default "ambari"]
          ["-n" "--num" "Number of keywords returned" :default "100"]
          ["-s" "--treatment" "wpp, wppx, tpp, or tppx" :default "tppx"]
          ["--wpp" "Make tables for WPP" :flag true :default false]
          ["--tpp" "Make tables for TPP" :flag true :default false]
          ["--wppx" "Make tables for WPPx" :flag true :default false]
          ["--tppx" "Make tables for TPPx" :flag true :default false]
          ["--tfidf" "Make tables for tfidf" :flag true :default false]
          ["--pop-chart" "Make chart for tfidf" :flag true :default false]
          ["--predict" "Predict SBRs" :flag true :default false]
          ["--map-after" "Find mean average precision" :flag true :default false]
          ["--map-before" "Find mean average precision" :flag true :default false]
          ["--map-chart" "Chart mean average precision" :flag true :default false])]                       
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (if (:odir opts)
      (cond
        (:wpp opts)        (make-tbls (:odir opts) (:proj opts) (read-string (:num opts)))
        (:tpp opts)        (make-cross-tbls (:odir opts) (:cproj opts) (:proj opts) (read-string (:num opts)))
        (:wppx opts)       (make-farsec-tbls-0 (:odir opts) (:cproj opts) (:proj opts) true)
        (:tppx opts)       (make-farsec-tbls (:odir opts) (:cproj opts) (:proj opts) false)
        (:tfidf opts)      (make-pop-tbls (:odir opts) (:proj opts) (read-string (:num opts)))
        (:pop-chart opts)  (make-pop-chart (:odir opts) (:proj opts))
        (:predict opts)    (make-predictions (:odir opts) (:cproj opts) (:proj opts) (:treatment opts))
        (:map-after opts)  (make-map-tbls (:odir opts) (:proj opts) make-wicket-experiment-3 run-experiment-3c "after")
        (:map-before opts) (make-map-tbls (:odir opts) (:proj opts) make-wicket-experiment-3 run-experiment-3d "before")
        (:map-chart opts)  (make-map-charts (:odir opts) (:proj opts))
       banner (System/exit 0))
      (println banner))))
