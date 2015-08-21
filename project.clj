(defproject tweegeemee "1.3.2"
  :description  "tweegeemee is an experiment in image creation and breeding via twitter."
  :url          "http://github.com/rogerallen/tweegeemee"
  :license      {:name "Eclipse Public License"
                 :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [twitter-api         "0.7.8"]
                 [environ             "1.0.0"]
                 [net.mikera/clisk    "0.10.0"]
                 [tentacles           "0.3.0"]]
  :main         ^:skip-aot tweegeemee.core
  :min-lein-version "2.0.0"
  :plugins      [[lein-environ "1.0.0"]]
  ;; Enable full optimizer, don't let heap or metaspace get too big
  ;; 260+140=400 allowing 112mb for the process
  ;; finally adding the max direct mem size.  see what that does
  :jvm-opts     ^:replace ["-Xss512k" "-Xms128m" "-Xmx260m"
                           "-XX:MaxMetaspaceSize=140m"
                           "-XX:MaxDirectMemorySize=50m"]
  :target-path  "target/%s"
  :profiles     {:uberjar {:aot :all}}
  )
