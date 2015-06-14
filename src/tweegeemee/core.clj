(ns tweegeemee.core
  (:use [clisk live])
  (:require [twitter.api.restful :as tw]
            [twitter.oauth       :as tw-oauth]
            [twitter.request     :as tw-req]
            [environ.core        :refer [env]]
            [clojure.zip         :as zip]
            [clojure.set         :as set]
            [clojure.edn         :as edn]
            [clojure.java.io     :as io]
            [tentacles.gists     :as gists]
            [cronj.core          :as cj])
  (:import [java.io File]
           [javax.imageio ImageIO])
  (:gen-class))

;; ======================================================================
;; add these keys to your profiles.clj (AND DON'T CHECK THAT FILE IN!)
(defonce my-twitter-creds   (atom nil)) ;; oauth from api.twitter.com
(defonce my-screen-name     (atom nil)) ;; twitter screen name
(defonce my-gist-auth       (atom nil)) ;; gist username:password
(defonce my-gist-archive-id (atom nil)) ;; create this archive
(defn setup-env!
  "setup environment vars"
  []
  (reset! my-twitter-creds   (tw-oauth/make-oauth-creds
                              (env :app-consumer-key)
                              (env :app-consumer-secret)
                              (env :user-access-token)
                              (env :user-access-secret)))
  (reset! my-screen-name     (env :screen-name))
  (reset! my-gist-auth       (env :gist-auth))
  (reset! my-gist-archive-id (env :gist-archive-id))
  nil)

;; ======================================================================
(defonce min-color-value              36) ;; not too dark
(defonce min-color-difference         10) ;; not too similar
(defonce test-size                    16)
(defonce full-size                    720)
(defonce max-code-depth               10)
(defonce gist-archive-filename        "1_archive.edn")
(defonce num-tweets-for-parent-search 60) ;; 24 hrs/ every 3 = 8x * 6 imgs = 48
(defonce num-parents-to-breed         5)

;; Functions for use in creating imagery.
(declare random-value)
(defn- random-scalar [] (random-value))
(defn- random-vec2 [] [(random-value) (random-value)])
(defn- random-vec3 [] [(random-value) (random-value) (random-value)])
(defn- random-vec4 [] [(random-value) (random-value) (random-value) (random-value)])
;; use ` instead of ' in order to add the namespace so it can be found when eval'd
(def term-vals #{`pos random-scalar random-vec2 random-vec3 random-vec4})
(def term-fns #{`noise `snoise `plasma `splasma
                `vnoise `vsnoise `vplasma `vsplasma
                `grain `turbulence `vturbulence
                `spots `blotches})
(def unary-fns #{`vsin `vcos `vabs `vround `vfloor `vfrac
                 `square `vsqrt `sigmoid `max-component `min-component
                 `length `normalize `gradient
                 `hue-from-rgb `lightness-from-rgb `saturation-from-rgb
                 `hsl-from-rgb `red-from-hsl `green-from-hsl `blue-from-hsl
                 `rgb-from-hsl `x `y `z `t `alpha })
(def binary-fns #{`v+ `v* `v- `vdivide `vpow `vmod `dot `cross3
                  `vmin `vmax `checker `scale `offset
                  `adjust-hue `adjust-hsl `vconcat})
(def ternary-fns #{`lerp `clamp})
(def fns (set/union unary-fns binary-fns ternary-fns))
;; Probabilities
(def prob-term-fn    0.1)  ;; vs terminal values
(def prob-ternary-fn 0.02) ;; vs. binary or unary
(def prob-binary-fn  0.3)  ;; vs ternary or unary

;; ======================================================================
;; gist stuff
(declare image-hash)
(defn- write-str-to-gist-archive
  [data-str]
  (let [resp (gists/edit-gist
              @my-gist-archive-id
              {:auth @my-gist-auth
               :files { gist-archive-filename { :content data-str }}})]
    (if (nil? (:status resp))
      resp
      (throw (Exception. (str "gist error" (:message (:body resp))))))))

(defn- read-str-from-gist-archive
  []
  (let [resp (gists/specific-gist @my-gist-archive-id {:auth @my-gist-auth})]
    (if (nil? (:status resp))
      (-> (:files resp)
          ((keyword gist-archive-filename))
          :content)
      (throw (Exception. (str "gist error" (:message (:body resp))))))))

(defn- read-gist-archive-data
  []
  (edn/read-string (read-str-from-gist-archive)))

(defn- my-pr-str
  "take array of maps and output them.  only works for data I expect. :^)"
  [data]
  (str "[\n"
       (reduce (fn [a b]
                 (format "%s { :name \"%s\" :hash %d :image-hash %d\n   :code %s\n }\n"
                         a (:name b) (:hash b) (:image-hash b) (:code b)))
               "" data)
       "]\n"
       ))

(defn- update-gist-archive-data
  "add the data to the archive, return the line number for the info"
  [new-data]
  (let [old-archive-data (read-gist-archive-data)
        new-archive-data (conj old-archive-data new-data)
        line-number      (+ 2 (* 3 (dec (count new-archive-data))))
        new-archive-str  (my-pr-str new-archive-data)]
    (write-str-to-gist-archive new-archive-str)
    line-number))

(defn- append-to-gist
  [filename code]
  (update-gist-archive-data
   {:name       filename
    :hash       (hash code)
    :image-hash (image-hash (image (eval code) :size test-size))
    :code       (str code)
    }))
;; (append-to-gist "test6" 0.175)

;; ======================================================================
(defn- random-fn
  "return a random function.  Parameter n selects either 1 or 2 parameters."
  [n]
  (case n
    3 (rand-nth (seq ternary-fns))
    2 (rand-nth (seq binary-fns))
    1 (rand-nth (seq unary-fns))))
;;(random-fn 3)

(defn- random-value
  "return a random value in the range (-3,3) with only 4 significant digits
  to increase readability"
  []
  (let [x (* 3 (dec (rand 2)))
        x (/ (Math/floor (* x 10000)) 10000.0)]
    x))
;;(random-value)

(defn- random-terminal
  "return a random terminal value: vectors, position, or noise."
  []
  (if (< (rand) prob-term-fn)
    (rand-nth (seq term-fns))
    (let [x (rand-nth (seq term-vals))]
      (if (not= x `pos) (x) x))))
;;(random-terminal)

(defn- random-code
  "Recursively create & return a random s-expression made up of
  functions or terminals. When depth=0, create a terminal to control
  the size.  Create terminal fn with increasing probability as depth
  gets smaller.  Functions are not parameter-checked so runtime
  exceptions can be expected."
  ([depth]
   (if (and (pos? depth) (pos? (rand-int depth)))
     (if (< (rand) prob-ternary-fn)
       (cons (random-fn 3) (repeatedly 3 #(random-code (dec depth))))
       (if (< (rand) prob-binary-fn)
         (cons (random-fn 2) (repeatedly 2 #(random-code (dec depth))))
         (cons (random-fn 1) (repeatedly 1 #(random-code (dec depth))))))
     (random-terminal))))
;;(random-code 5)

(defn- locs
  "return all zip locations within the s-expression.  each location contains the
  full context within the tree for use in replacement later."
  [G]
  (let [zipper (zip/seq-zip G)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    all-locs))

(defn- locs-ex-fns
  "return all zip locations within the s-expression--but not
  functions, only s-expr and the operands.  each location contains the
  full context within the tree for use in replacement later."
  [G]
  (filter #(not (fns (zip/node %))) (locs G)))

(defn- replace-loc
  "replace the location loc1 with the location loc2, returning the root (full
  s-expression) of loc1."
  [loc1 loc2]
  (zip/root (zip/replace loc1 (zip/node loc2))))

(defn- replace-loc-with-node
  "replace the location loc1 with the location loc2, returning the root (full
  s-expression) of loc1."
  [loc1 node2]
  (zip/root (zip/replace loc1 node2)))

(defn- breed
  "find a random expression in L to replace with a random expression
  in R.  replace it within L and return a new L s-expression."
  [L R]
  (let [loc1 (rand-nth (locs-ex-fns L))
        loc2 (rand-nth (locs-ex-fns R))]
    ;;(println "\nLOC1" loc1)
    ;;(println "\nLOC2" loc2)
    (replace-loc loc1 loc2)))

(defn- mutate-just-a-node
  [node]
  (if (term-vals node)
    ;; must be pos--offset it
    (cons `v+ (cons (random-vec2) '(clisk.live/pos)))
    (if (term-fns node)
      (rand-nth (seq term-fns))
      (if (unary-fns node)
        (rand-nth (seq unary-fns))
        (if (binary-fns node)
          (rand-nth (seq binary-fns))
          (if (ternary-fns node)
            (rand-nth (seq ternary-fns))
            (println "UNEXPECTED NODE:" node)))))))

(defn- mutate-node
  [node]
  (when (< (rand) 0.95) ;; mostly mutate here, return nil 5%
    (condp = (type node)
      ;;* If the node is a scalar value, it can be adjusted by the
      ;;addition of some random amount.
      java.lang.Double
      (+ node (random-value))
      ;;* If the node is a vector, it can be adjusted by adding random
      ;;amounts to each element.
      clojure.lang.PersistentVector
      (vec (map #(+ % (random-value)) node))
      ;;* If the node is a function, it can mutate into a different
      ;;function. For example (abs X) might become (cos X). If this
      ;;mutation occurs, the arguments of the function are also adjusted
      ;;if necessary to the correct number and types.
      ;;[I will keep to same function type]
      clisk.node.Node
      (mutate-just-a-node node)
      clojure.lang.Symbol
      (mutate-just-a-node node)
      clojure.lang.PersistentList
      (if (< (rand) 0.5)
        ;; variation on above
        (cons (mutate-just-a-node (first node)) (rest node))
        ;;* An argument to a function can jump out and become the new value
        ;;for that node. For example (* X .3) might become X. This is the
        ;;inverse of the previous [next] type of mutation.
        (rand-nth (rest node)))
      (println "UNEXPECTED TYPE:" node (class node))
      )))
;;[See mutate fn]* Finally, a node can become a copy of another node from the
;;parent expression. For example (+ (abs X) (* Y .6)) might
;;become (+ (abs (* Y .6)) (* Y .6)). This causes effects similar to
;;those caused by mating an expression with itself. It allows for
;;sub-expressions to duplicate themselves within the overall
;;expression.

;;[TBD]* An expression can become the argument to a new random
;;function. Other arguments are generated at random if
;;necessary. For example X might become (* X .3).

(defn- mutate
  [L]
  (let [loc1      (rand-nth (locs L))
        loc2      (rand-nth (locs-ex-fns L))
        loc3      (rand-nth (locs-ex-fns L))
        new-node  (mutate-node (zip/node loc1))
        ;;_ (println "newnode" new-node)
        ]
    (if (nil? new-node)
      (replace-loc loc2 loc3)
      (replace-loc-with-node loc1 new-node))))

(defn- good-random-code?
  "does it have at least one paren?"
  [code]
  (let [code-str (str code)]
    (and (>= (count (filter #(= % \( ) code-str)) 1))))

(defn- good-colors?
  "are the values of a single color component too low or not different
  enough?"
  [colors]
  (let [min-v (apply min colors)
        max-v (apply max colors)]
    ;;(println (format "%x %x" min-v max-v))
    (and (> max-v min-color-value)
         (> (- max-v min-v) min-color-difference))))

(defn- third [x] (nth x 2))

(defn- good-image?
  "is the image not a constant color?"
  [img]
  (let [W (.getWidth img)
        H (.getHeight img)
        values (map #(vector
                      (bit-shift-right (bit-and % 0x00ff0000) 16)
                      (bit-shift-right (bit-and % 0x0000ff00) 8)
                      (bit-shift-right (bit-and % 0x000000ff) 0))
                    (for [x (range W) y (range H)]
                      (.getRGB img x y)))]
    (or (good-colors? (map first values))
        (good-colors? (map second values))
        (good-colors? (map third values)))))

(defn- image-hash
  "hash the colors in an image"
  [img]
  (let [W (.getWidth img)
        H (.getHeight img)]
    (hash (for [x (range W) y (range H)] (.getRGB img x y)))))

(defn- get-old-hashes
  []
  (let [data (read-gist-archive-data)
        old-hashes (set (map :hash data))
        old-image-hashes (set (map :image-hash data))]
    [old-hashes old-image-hashes]))
;; (get-old-hashes)

(defn- get-good-code*
  "given a code-creator-fn, get some code that creates non-boring images."
  [code-creator-fn]
  (let [cur-count  (atom 0)
        good-image (atom false)
        [old-hashes old-image-hashes] (get-old-hashes)
        good-code  (atom nil)]
    (while (and (< @cur-count 200)
                (not @good-image))
      (swap! cur-count inc)
      (try
        (let [cur-code (code-creator-fn)
              ;;_ (println "\n??" cur-code)
              _ (when-not (nil? (old-hashes (hash cur-code)))
                  (throw (Exception. "previously created code")))
              _ (when-not (good-random-code? cur-code)
                  (throw (Exception. "badly created code")))
              img (image (eval cur-code) :size test-size)
              _ (when-not (nil? (old-image-hashes (image-hash img)))
                  (throw (Exception. "previously created image")))
              _ (when-not (good-image? img)
                  (throw (Exception. "boring image")))]
          ;; no exception
          (println "\n" @cur-count "got:" cur-code)
          (reset! good-image true)
          (reset! good-code cur-code))
        (catch Exception e
          ;;(println @cur-count "Exception" (.getMessage e))
          (print "e")
          )
        (catch java.util.concurrent.ExecutionException e
          ;;(println @cur-count "execution exception")
          (print "E")
          )))
    @good-code))

(defn- get-random-code
  "get a good image-creation code created randomly"
  []
  (get-good-code* (fn [] (random-code max-code-depth))))

(defn- write-png
  "write the-image to filename (which should have a .png suffix)"
  [filename the-image]
  (ImageIO/write the-image "png" (File. filename)))

(defn- get-timestamp-str
  []
  (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss") (System/currentTimeMillis)))

(defn- get-our-file-seq
  []
  (filter #(re-matches #".*\.clj|.*\.png" (.getName %)) (file-seq (io/file "images"))))
;; (nth (get-our-file-seq) 5)

(defn- cleanup-our-files!
  []
  (dorun (map #(io/delete-file %) (get-our-file-seq))))

(defn- get-png-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".png"))

(defn- get-clj-filename
  [timestamp suffix]
  (str "images/" timestamp "_" suffix ".clj"))

(defn- get-clj-basename
  [timestamp suffix]
  (str timestamp "_" suffix ".clj"))

(defn- make-random-code-and-png
  "make random code and save as files. return the code, clj-filename and png-filename"
  [timestamp suffix]
  (let [my-code (get-random-code)]
    (if (nil? my-code)
      [nil nil nil]
      (let [png-filename (get-png-filename timestamp suffix)
            my-image (image (eval my-code) :size full-size)
            clj-filename (get-clj-filename timestamp suffix)
            clj-basename (get-clj-basename timestamp suffix)]
        (write-png png-filename my-image)
        (spit clj-filename (str my-code))
        [my-code clj-basename png-filename]))))

(defn- post-to-twitter
  [status-text the-image-filename]
  (try
    (if false
      (println "NOT posting to twitter" status-text)
      (tw/statuses-update-with-media
       :oauth-creds @my-twitter-creds
       :body [(tw-req/file-body-part the-image-filename)
              (tw-req/status-body-part status-text)]))
    (catch Exception e
      ;; FIXME -- why does this always have a remote-closed exception?
      (println "caught expected? twitter exception" (.getMessage e))))
  (println "waiting for a 5 seconds...")
  (Thread/sleep 5000))


(defn sanitize-url
  [url]
  (clojure.string/replace url "." "-"))

(defn- score-status
  "retweets count more than favorites"
  [status]
  (+ (* 3 (:retweet_count status)) (:favorite_count status)))

(defn- sanity-check-code
  "be careful with strange code you download from the web, son.  Only
  allow fns in the set of fns we know about."
  [code-str]
  (let [code-fns (map #(symbol (clojure.string/replace % #"[\( ]" ""))
                      (re-seq #"\(.+? " code-str))
        code-fn-set (set code-fns)]
    (set/subset? code-fn-set fns)))

(defn- get-parent-tweet-codes
  "return a sequence of the N highest-scoring two tweets code"
  [N]
  (let [archive  (read-gist-archive-data)
        statuses (->> (:body (tw/statuses-user-timeline
                              :oauth-creds @my-twitter-creds
                              :params {:count num-tweets-for-parent-search
                                       :screen-name @my-screen-name}))
                      (map #(update-in % [:text] clojure.string/replace #" http.*" ""))
                      (filter #(re-matches #"\d\d\d\d\d\d_\d\d\d\d\d\d_\w+.clj" (:text %)))
                      (map #(assoc % :score (score-status %)))
                      (sort-by :score)
                      (reverse)
                      (map :text)
                      (mapcat #(filter (fn [x] (= (:name x) %)) archive))
                      (map :code)
                      (map str)
                      (filter sanity-check-code)
                      (take N)
                      ;;((fn [x] (println "post-take:" x) x))
                      (map read-string)
                      )]
    statuses))

(defn- get-random-child
  "get a good image-creation code created via breeding two other
  codes"
  [code0 code1]
  (get-good-code* (fn [] (breed code0 code1))))

(defn- make-random-child-and-png
  "take 2 codes, breed them and save as files. return the code,
  clj-filename and png-filename"
  [code0 code1 timestamp suffix]
  (let [my-code (get-random-child code0 code1)]
    (if (nil? my-code)
      [nil nil nil]
      (let [png-filename (get-png-filename timestamp suffix)
            my-image (image (eval my-code) :size full-size)
            clj-filename (get-clj-filename timestamp suffix)
            clj-basename (get-clj-basename timestamp suffix)]
        (write-png png-filename my-image)
        (spit clj-filename (str my-code))
        [my-code clj-basename png-filename]))))

(defn- get-random-mutant
  "get a good image-creation code created via mutating a code"
  [code]
  (get-good-code* (fn [] (mutate code))))

(defn- make-random-mutant-and-png
  "take a code, mutate it and save as files. return the code,
  clj-filename and png-filename"
  [code timestamp suffix]
  (let [my-code (get-random-mutant code)]
    (if (nil? my-code)
      [nil nil nil]
      (let [png-filename (get-png-filename timestamp suffix)
            my-image (image (eval my-code) :size full-size)
            clj-filename (get-clj-filename timestamp suffix)
            clj-basename (get-clj-basename timestamp suffix)]
        (write-png png-filename my-image)
        (spit clj-filename (str my-code))
        [my-code clj-basename png-filename]))))

;; ======================================================================
;; Public API ===========================================================
;; ======================================================================

(defn post-to-web
  "Post the-code + clj-filename info to gist.github.com, post
  png-filename & a pointer to twitter."
  [the-code clj-filename png-filename]
  (let [gist-line-number (append-to-gist clj-filename the-code)
        gist-url         (str "https://gist.github.com/rogerallen/"
                              @my-gist-archive-id
                              "#file-" (sanitize-url gist-archive-filename)
                              "-L" gist-line-number "-L" (+ 2 gist-line-number))
        status-text (str clj-filename " " gist-url " #ProceduralArt")]
  (post-to-twitter status-text png-filename)))

;; FIXME - post-XXX-to-web routines below have a lot in common...

(defn post-random-batch-to-web
  "Post a batch of random codes & images to twitter and github."
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)]
    (dorun
     (doseq [suffix suffix-str]
       (let [[the-code clj-filename png-filename] (make-random-code-and-png timestamp-str suffix)]
         (if (nil? the-code)
           (println "!!! suffix" suffix "unable to create random image")
           (post-to-web the-code clj-filename png-filename)))))
    (println "done.")))

(defn post-children-to-web
  "Find the highest-scoring parents, post a batch of codes & images
  bred from those parents to twitter and github"
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)
        rents         (get-parent-tweet-codes num-parents-to-breed)
        c0            (rand-nth rents)
        c1            (rand-nth rents)]
    (dorun
     (doseq [suffix suffix-str]
       (let [[the-code clj-filename png-filename] (make-random-child-and-png c0 c1 timestamp-str suffix)]
         (if (nil? the-code)
           (println "!!! suffix" suffix "unable to create child image")
           (post-to-web the-code clj-filename png-filename)))))))

(defn post-mutants-to-web
  "Find the highest-scoring parents, post a batch of 5 codes & images
  mutated from the top parent to twitter and github"
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)
        rents         (get-parent-tweet-codes num-parents-to-breed)
        c0            (rand-nth rents)
        c1            (rand-nth rents)]
    (dorun
     (doseq [suffix suffix-str]
       (let [[the-code clj-filename png-filename] (make-random-mutant-and-png c0 timestamp-str suffix)]
         (if (nil? the-code)
           (println "!!! suffix" suffix "unable to create mutant image")
           (post-to-web the-code clj-filename png-filename)))))))

(defn post-a-set-to-web
  []
  (println "======================================================================")
  (println "posting a set of 6: random ab, children CD, mutant MN")
  (post-random-batch-to-web "ab")
  (post-children-to-web "CD")
  (post-mutants-to-web "MN")
  (cleanup-our-files!)
  (println "posting complete."))

(defn gen-handler [t opts]
  (println (:output opts) ": " t)
  (post-a-set-to-web))

(def cur-cronj
  (cj/cronj :entries [{:id "gen-task"
                       :handler gen-handler
                       :schedule "0 13 /3 * * * *"   ;; every 3 hours at 13 past
                       ;;:schedule "0 15 /1 * * * *" ;; every 1 hours at 15mins past...
                       ;;:schedule "0 /5 * * * * *"  ;; every 5 mins for testing
                       :opts {:output "posting every 3 hours"}}]))

(defn -main [& args]
  (println "Started version" (env :tweegeemee-version))
  (setup-env!)
  (cj/start! cur-cronj))

;; ======================================================================
;; ======================================================================
(comment ;; code below to test things out

  (env :tweegeemee-version)
  (setup-env!)

  ;; generate & show a random image
  (let [c (get-random-code)]
    (show (eval c)))

  ;; post an image to the web (careful!)
  (let [[the-code clj-filename png-filename]
        (make-random-code-and-png (get-timestamp-str) "s")]
    (post-to-web the-code clj-filename png-filename))

  ;; Careful!
  (post-random-batch-to-web "r")

  ;; breed things by hand...
  (def rents (get-parent-tweet-codes 5))
  (show (eval (nth rents 0)))
  (def dad (rand-nth rents))
  (def mom (rand-nth rents))
  (show (eval dad))
  (show (eval mom))
  (let [c (get-random-child dad mom)
        _ (println c)]
    (if (not (nil? c))
      (show (eval c))))

  ;; Careful!
  (post-children-to-web "ABCDE")

  (def rents (get-parent-tweet-codes 5))
  (def dad (rand-nth rents))
  (show (eval dad))
  (let [c (get-random-mutant dad)
        _ (println c)]
    (if (not (nil? c))
      (show (eval c))))

  ;; Careful!
  (post-mutants-to-web "VWXYZ")

  ;; to reproduce the heroku failure, do:
  ;;   lein with-profile production compile :all
  ;;   lein trampoline run

  (try (tw/statuses-update
        :oauth-creds @my-twitter-creds
        :params {:status "testing from home"})
       (catch Exception e (println "Oh no! " (.getMessage e))))

  )
