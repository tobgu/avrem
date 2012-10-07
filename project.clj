(defproject avrem "0.1.0-SNAPSHOT"
  :description "AVR emulator library"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [midje "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]]
  :dev-dependencies [[lein-midje "1.0.10"]]
  :profiles {:dev {:plugins [[lein-midje "2.0.0-SNAPSHOT"]]}})
