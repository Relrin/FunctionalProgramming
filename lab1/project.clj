(defproject lab1 "0.1.0-SNAPSHOT"
  :description "Lab #1 on functional programming"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.3.1"]]
  :main ^:skip-aot lab1.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
