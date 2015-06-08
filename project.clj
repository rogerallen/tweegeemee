(defproject tweegeemee "1.0.0"
  :description  "tweegeemee is an experiment in image creation and breeding via twitter."
  :url          "http://github.com/rogerallen/tweegeemee"
  :license      {:name "Eclipse Public License"
                 :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [twitter-api         "0.7.8"]
                 [environ             "1.0.0"]
                 [net.mikera/clisk    "0.10.0"]
                 [tentacles           "0.3.0"]
                 [im.chit/cronj       "1.4.3"]]
  :main         tweegeemee.core
  :plugins      [[lein-environ "1.0.0"]]
  :jvm-opts     ^:replace [] ;; Enable full optimizer
  )
