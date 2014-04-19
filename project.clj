(defproject ittybit "0.1.0-SNAPSHOT"
  :description "An itty BitTorrent client."
  :url "https://github.com/happy4crazy/ittybit"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]]

  :node-dependencies [[bncode "*"]
                      [ip "*"]]

  :plugins [[lein-cljsbuild "1.0.3"]
            [lein-npm "0.4.0"]]

  :source-paths ["src"]

  :cljsbuild { 
    :builds [{:id "ittybit"
              :source-paths ["src"]
              :compiler {
                :output-to "out/ittybit.js"
                :target :nodejs
                :optimizations :simple}}]})
