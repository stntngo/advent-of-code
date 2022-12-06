(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/algo.generic "0.1.3"]]
  :repl-options {:init-ns aoc.core}
  :plugins [[lein-cloverage "1.2.2"]]
  :main aoc.core
  :aot [aoc.core])


