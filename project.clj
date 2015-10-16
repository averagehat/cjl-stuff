(defproject clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [instaparse "1.4.1"] 
                 [cheshire "5.5.0"]
                 [clojurewerkz/elastisch "2.1.0"] 
                 [org.clojure/core.typed "0.3.11"] 
                 [clj-biosequence "0.4.1"] 
                 [org.clojure/core.match "0.3.0-alpha4"] 


[org.clojure/data.xml "0.0.8"]

 [org.clojure/core.async "0.1.346.0-17112a-alpha"]

[clj-http "2.0.0"]
                 [defun "0.2.0"] ]
  :main ^:skip-aot clj.core
  :target-path "target/%s"


  :profiles {:uberjar {:aot :all}})
