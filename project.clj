(defproject clj-graph "0.1.0-SNAPSHOT"
  :description "Graph data structures and algorithms"
  :url "http://github.com/kanej/clj-graph"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.2"]]
                   :plugins [[lein-kibit "0.0.8"]]}})
