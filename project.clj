(defproject tweegeemee "1.1.11"
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
  :main         ^:skip-aot tweegeemee.core
  :min-lein-version "2.0.0"
  :plugins      [[lein-environ "1.0.0"]]
  ;; Enable full optimizer, don't let heap or metaspace get too big
  ;; 266+146=412 allowing 100mb for the process
  ;; finally adding the max direct mem size.  see what that does
  :jvm-opts     ^:replace ["-Xss512k" "-Xms128m" "-Xmx266m"
                           "-XX:MaxMetaspaceSize=146m"
                           "-XX:MaxDirectMemorySize=50m"]
  :target-path  "target/%s"
  :profiles     {:uberjar {:aot :all}}
  )
