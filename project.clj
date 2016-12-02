(defproject farsec "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter/incanter-core "1.4.1"]
                 [incanter/incanter-io "1.4.1"]
                 [org.clojure/data.csv "0.1.2"]
                 [clojure-opennlp "0.3.3"]
                 [seer "0.1.2-SNAPSHOT"]
                 [org.clojure/tools.cli "0.3.5"]]
  :aot [farsec.core]
  :main farsec.core)
