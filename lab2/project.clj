(defproject lab2 "0.1.0-SNAPSHOT"
  :description "Lab #2: Web crawler"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
		 [clj-http "1.0.1"]
                 [enlive "1.1.5"]
                 [slingshot "0.12.1"]
                 [clojurewerkz/urly "1.0.0"]]
  :main ^:skip-aot lab2.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
