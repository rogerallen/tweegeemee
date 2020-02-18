(ns tweegeemee.core
  (:use [clisk live])
  (:require
   [tweegeemee.twitter :as twitter]
   [tweegeemee.gists   :as gists]
   [environ.core       :refer [env]]
   [clojure.zip        :as zip]
   [clojure.set        :as set]
   [clojure.java.io    :as io]
   )
  (:import [java.io File]
           [javax.imageio ImageIO])
  (:gen-class))

;; ======================================================================
(defonce my-twitter-creds   (atom nil)) ;; oauth from api.twitter.com
(defonce my-screen-name     (atom nil)) ;; twitter screen name
(defonce my-gist-auth       (atom nil)) ;; gist username:password
(defonce my-gist-archive-id (atom nil)) ;; create this archive
;; add these keys to your environment or profiles.clj (AND DON'T CHECK THAT FILE IN!)
;; # twitter stuff:
;; export APP_CONSUMER_KEY="FIXME"
;; export APP_CONSUMER_SECRET="FIXME"
;; export USER_ACCESS_TOKEN="FIXME"
;; export USER_ACCESS_SECRET="FIXME"
;; export SCREEN_NAME="FIXME"
;; # github gist stuff:
;; export GIST_AUTH="FIXME"
;; export GIST_ARCHIVE_ID="FIXME"
;; # just pick a random integer
;; export CLISK_RANDOM_SEED="FIXME"
(defn setup-env!
  "setup environment vars"
  []
  (reset! my-twitter-creds   (twitter/make-oauth-creds
                              (env :app-consumer-key)
                              (env :app-consumer-secret)
                              (env :user-access-token)
                              (env :user-access-secret)))
  (reset! my-screen-name     (env :screen-name))
  (reset! my-gist-auth       (env :gist-auth))
  (reset! my-gist-archive-id (env :gist-archive-id))
  (when-let [seed (Integer/parseInt (env :clisk-random-seed))]
    (println "clisk-random-seed" seed)
    (clisk.patterns/seed-perlin-noise! seed)
    (clisk.patterns/seed-simplex-noise! seed))
  nil)

;; ======================================================================
(defonce MAX-ALL-COMPONENT-VALUE   220)   ;; not too white
(defonce MIN-ALL-COMPONENT-VALUE   36)    ;; not too dark
(defonce MIN-ALL-COMPONENT-DELTA   10)    ;; not too similar
(defonce MIN-NUM-COLORS            3)     ;; not too few colors (checkers be gone!)
(defonce TEST-IMAGE-SIZE           16)    ;; size for boring & img-hash check
(defonce IMAGE-SIZE                720)   ;; size to post
(defonce MAX-RANDOM-CODE-DEPTH     10)    ;; emperically got to this...
(defonce NUM-PARENT-TWEETS         200)   ;; 24 posts/day * 2 imgs = 48 img/day = ~4 days
(defonce MAX-POSSIBLE-PARENTS      5)     ;; top 5 tweets become parents
(defonce MAX-GOOD-CODE-ATTEMPTS    200)   ;; don't want to give up too quickly
(defonce PROB-TERM-FN              0.1)   ;; probability of term-fn vs term-vals
(defonce PROB-TERNARY-FN           0.02)  ;; vs. binary or unary
(defonce PROB-BINARY-FN            0.3)   ;; vs ternary or unary
(defonce PROB-SINGLE-MUTATION      0.95)  ;; mostly mutate vs. copy

;; ======================================================================
;; Functions used in creating imagery.
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

;; ======================================================================
(defn- random-fn
  "return a random function.  Parameter n selects either 1 or 2
  parameters."
  [n]
  (case n
    3 (rand-nth (seq ternary-fns))
    2 (rand-nth (seq binary-fns))
    1 (rand-nth (seq unary-fns))))
;;(random-fn 3)

(defn- random-value
  "return a random value in the range (-3,3) with only 4 significant
  digits to increase readability"
  []
  (let [x (* 3 (dec (rand 2)))
        x (/ (Math/floor (* x 10000)) 10000.0)]
    x))
;;(random-value)

(defn- random-terminal
  "return a random terminal value: vectors, position, or noise."
  []
  (if (< (rand) PROB-TERM-FN)
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
     (if (< (rand) PROB-TERNARY-FN)
       (cons (random-fn 3) (repeatedly 3 #(random-code (dec depth))))
       (if (< (rand) PROB-BINARY-FN)
         (cons (random-fn 2) (repeatedly 2 #(random-code (dec depth))))
         (cons (random-fn 1) (repeatedly 1 #(random-code (dec depth))))))
     (random-terminal))))
;;(random-code 5)

(defn- locs
  "return all zip locations within the s-expression.  each location
  contains the full context within the tree for use in replacement
  later."
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
  "replace the location loc1 with the location loc2, returning the
  root (full s-expression) of loc1."
  [loc1 loc2]
  (zip/root (zip/replace loc1 (zip/node loc2))))

(defn- replace-loc-with-node
  "replace the location loc1 with the location loc2, returning the
  root (full s-expression) of loc1."
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

(defn- mutate-symbol
  "helper function for mutate-node.  Mutates symbols according to Karl
  Sims' SIGGRAPH paper."
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
  "Mutates code nodes according to Karl Sims' SIGGRAPH paper.  Returns
  nil sometimes to allow for copying other nodes in calling fn"
  [node]
  (when (< (rand) PROB-SINGLE-MUTATION) ;; mostly mutate here, return nil 5%
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
      (mutate-symbol node)
      clojure.lang.Symbol
      (mutate-symbol node)
      clojure.lang.PersistentList
      (if (< (rand) 0.5)
        ;; variation on above
        (cons (mutate-symbol (first node)) (rest node))
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
  "mutate the code string L according to Karl Sims' SIGGRAPH paper."
  [L]
  (let [loc1      (rand-nth (locs L))
        loc2      (rand-nth (locs-ex-fns L))
        loc3      (rand-nth (locs-ex-fns L))
        new-node  (mutate-node (zip/node loc1))
        ;;_ (println "newnode" new-node)
        ]
    (if (nil? new-node)
      ;; copy random nodes & sub-nodes
      (replace-loc loc2 loc3)
      ;; or, replace with new mutant
      (replace-loc-with-node loc1 new-node))))

(defn- good-random-code?
  "does code x have at least one paren?"
  [x]
  (= (first (pr-str x)) \( ))

(defn- good-components?
  "are the values of a single color component too dark or not
  different enough?"
  [colors]
  (let [min-v (apply min colors)
        max-v (apply max colors)]
    (and (< min-v MAX-ALL-COMPONENT-VALUE)
         (> max-v MIN-ALL-COMPONENT-VALUE)
         (> (- max-v min-v) MIN-ALL-COMPONENT-DELTA))))

(defn- third [x] (nth x 2)) ;; should be stdlib

(defn- good-image?
  "is img not a dark, boring image?  Does it have more than a few
  colors?"
  [img]
  (let [W (.getWidth img)
        H (.getHeight img)
        values (map #(vector
                      (bit-shift-right (bit-and % 0x00ff0000) 16)
                      (bit-shift-right (bit-and % 0x0000ff00) 8)
                      (bit-shift-right (bit-and % 0x000000ff) 0))
                    (for [x (range W) y (range H)]
                      (.getRGB img x y)))
        val-freq (frequencies values)]
    (and (> (count val-freq) MIN-NUM-COLORS)
         (or (good-components? (map first values))
             (good-components? (map second values))
             (good-components? (map third values))))))

(defn- image-hash
  "hash the colors in an image so we can figure out if we've created
  this one before."
  [img]
  (let [W (.getWidth img)
        H (.getHeight img)]
    (hash (for [x (range W) y (range H)] (.getRGB img x y)))))

(defn- get-old-hashes
  "return a vector of :hash and :image-hash data from the gist
  archive."
  []
  (let [data (gists/read-archive my-gist-auth my-gist-archive-id)
        old-hashes (set (map :hash data))
        old-image-hashes (set (map :image-hash data))]
    [old-hashes old-image-hashes]))

(defn- get-good-code*
  "The main code-generation workhorse function.  Given a
  code-creator-fn (random, breed or combine), return some code that
  creates non-boring images.  Tries for a while, but if it gives up,
  it returns nil."
  [code-creator-fn]
  (try
    (let [cur-count  (atom 0)
          good-image (atom false)
          [old-hashes old-image-hashes] (get-old-hashes)
          good-code  (atom nil)]
      (while (and (< @cur-count MAX-GOOD-CODE-ATTEMPTS)
                  (not @good-image))
        (swap! cur-count inc)
        (try
          (let [cur-code (code-creator-fn)
                ;;_ (println "\n??" cur-code)
                _ (when-not (nil? (old-hashes (hash cur-code)))
                    (throw (Exception. "previously created code")))
                _ (when-not (good-random-code? cur-code)
                    (throw (Exception. "badly created code")))
                img (image (eval cur-code) :size TEST-IMAGE-SIZE)
                _ (when-not (nil? (old-image-hashes (image-hash img)))
                    (throw (Exception. "previously created image")))
                _ (when-not (good-image? img)
                    (throw (Exception. "boring image")))]
            ;; no exception--got a good one
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
      @good-code)
    (catch Exception e
      (println "get-good-code* Setup Exception" (.getMessage e))
      nil)))

(defn- get-random-code
  "get a good image-creation code created randomly"
  []
  (get-good-code* (fn [] (random-code MAX-RANDOM-CODE-DEPTH))))

(defn- write-png
  "write the-image to filename (which should have a .png suffix)"
  [filename the-image]
  (ImageIO/write the-image "png" (File. filename)))

(defn- get-timestamp-str
  "return a timestamp for the current time"
  []
  (.format (java.text.SimpleDateFormat. "yyMMdd_HHmmss") (System/currentTimeMillis)))

(defn- get-our-file-seq
  "return a file-seq of our images/*.clj and images/*.png files"
  []
  (filter #(re-matches #".*\.clj|.*\.png" (.getName %)) (file-seq (io/file "images"))))

(defn- cleanup-our-files!
  "remove all of our images/*.clj and images/*.png files"
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

(defn- score-status
  "return a score for a image's tweet based on favorites and
  retweets. retweets count more than favorites"
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

;; ======================================================================
;; ideas for facebook/tumbler/etc posts
;; 1) last week's Top 3
;; 2) monthly mosaic image.  30 days x 24 images/day (32x32px?)
;; 3) album: ancestry of last week's post (or a post people talk about)
;; ======================================================================

;; Postprocess with e.g.:
;;   montage -tile 48x30 -geometry 32x32 1512*png a.png
(defn- render-a-months-statuses
  [year month]
  (let [statuses (twitter/get-a-months-statuses year month)
        data     (gists/read-archive my-gist-auth my-gist-archive-id)] ;; FIXME take archive name
    (doseq [s statuses]
      (let [name         (clojure.string/replace (:text s) #" http.*" "")
            my-code      (:code (gists/get-entry-by-name data name))
            _            (println name "::" my-code)
            my-image     (image (eval my-code) :size 32)
            png-filename (str "images/mosaic/" name ".png")]
        (write-png png-filename my-image)))))

(defn- render-statuses
  [names size]
  (let [data (gists/read-archive my-gist-auth my-gist-archive-id)] ;; FIXME take archive name
    (doseq [name names]
      (let [my-code      (:code (gists/get-entry-by-name data name))
            _            (println name "::" my-code)
            my-image     (image (eval my-code) :size size)
            png-filename (str "images/tiles/" name ".png")]
        (write-png png-filename my-image)))))

(defn- get-top-n
  "return a sequence of the N highest-scoring tweet image links."
  [fn N]
  (let [statuses (->> (fn);; (get-todays-statuses)
                      (filter #(re-matches #"\d\d\d\d\d\d_\d\d\d\d\d\d_\w+.clj .*" (:text %)))
                      (map #(assoc % :score (score-status %)))
                      (map #(dissoc % :user :extended_entities))
                      (sort-by :score)
                      (reverse)
                      (take N))]
    statuses))

;; (get-top-n get-last-weeks-statuses 3)
(defn print-top-n
  [fn N]
  (doseq [s (get-top-n fn N)]
    (let [score (:score s)
          url   (-> s :entities :media first :expanded_url)]
      (println score url))))
;;(print-top-n get-last-weeks-statuses 3)

;; geneaology
(defn- layout-generations
  [child parent-map]
  (let [layout (atom [])] ;; vector of vectors [x y num-parents name]
    (defn update-layout
      [child generation y]
      (let [parents (parent-map child)
            num-parents (count parents)]
        ;;(println generation y (count parents) child)
        ;; if I was smarter, maybe I'd be able to conj this below...
        (swap! layout #(conj % [generation y num-parents child]))
        (if (empty? parents)
          [(inc y) [generation y num-parents child]]
          (let [generation (inc generation)]
            (reduce #(do ;; (println "x" y %1 %2)
                       (update-layout (first %2) generation (max (first %1) (+ y (second %2)))))
                    [y []]
                    (map vector parents (range num-parents)))))))
    (update-layout child 0 0)
    (sort @layout)))

(defn get-parent-tweets
  "return a sequence of the N highest-scoring tweets.  Tweet data is
  from the gist file."
  [N]
  (let [archive  (gists/read-archive my-gist-auth my-gist-archive-id)
        statuses (->> (twitter/get-statuses my-twitter-creds my-screen-name NUM-PARENT-TWEETS)
                      (map #(update-in % [:text] clojure.string/replace #" http.*" ""))
                      (filter #(re-matches #"\d\d\d\d\d\d_\d\d\d\d\d\d_\w+.clj" (:text %)))
                      (map #(assoc % :score (score-status %)))
                      (sort-by :score)
                      (reverse)
                      (map :text)
                      (mapcat #(filter (fn [x] (= (:name x) %)) archive))
                      (filter #(sanity-check-code (str (:code %))))
                      (take N)
                      ;;((fn [x] (println "post-take:" x) x))
                      (map #(update-in % [:code] (fn [x] (read-string (str x)))))
                      )]
    statuses))

(defn- get-random-child
  "get a good image-creation code created via breeding two other
  codes"
  [code0 code1]
  (get-good-code* (fn [] (breed code0 code1))))

(defn- get-random-mutant
  "get a good image-creation code created via mutating a code"
  [code]
  (get-good-code* (fn [] (mutate code))))

(defn- make-code-and-png*
  "make code from code-fn and save as files. return the code,
  clj-filename and png-filename"
  [code-fn timestamp suffix]
  (let [my-code (code-fn)]
    (if (nil? my-code)
      [nil nil nil]
      (let [png-filename (get-png-filename timestamp suffix)
            my-image     (image (eval my-code) :size IMAGE-SIZE)
            clj-filename (get-clj-filename timestamp suffix)
            clj-basename (get-clj-basename timestamp suffix)]
        (write-png png-filename my-image)
        (spit clj-filename (pr-str my-code))
        [my-code clj-basename png-filename]))))

(defn- make-random-code-and-png
  "make random code and save as files. return the code, clj-filename
  and png-filename"
  [timestamp suffix]
  (make-code-and-png* get-random-code timestamp suffix))

(defn- make-random-child-and-png
  "take 2 codes, breed them and save as files. return the code,
  clj-filename and png-filename"
  [code0 code1 timestamp suffix]
  (make-code-and-png* (partial get-random-child code0 code1)
                      timestamp suffix))

(defn- make-random-mutant-and-png
  "take a code, mutate it and save as files. return the code,
  clj-filename and png-filename"
  [code timestamp suffix]
  (make-code-and-png* (partial get-random-mutant code)
                      timestamp suffix))

;; ======================================================================
;; Public API ===========================================================
;; ======================================================================

(defn post-to-web
  "Post the-code + clj-filename info to gist.github.com, post
  png-filename & a pointer to twitter."
  [the-code clj-filename png-filename parent-vec]
  (let [image-hash       (image-hash (image (eval the-code) :size TEST-IMAGE-SIZE))
        gist-line-number (gists/append-archive my-gist-auth my-gist-archive-id clj-filename the-code parent-vec image-hash)
        gist-url         (gists/get-url my-gist-archive-id gist-line-number)
        status-text      (str clj-filename " " gist-url
                              " #ProceduralArt #generative")]
  (twitter/post-to-twitter my-twitter-creds status-text png-filename)))

(defn post-batch-to-web*
  "Post a batch of random codes & images to twitter and github."
  [make-fn post-str suffix-str parent-vec]
  (dorun
   (doseq [suffix suffix-str]
     (let [_ (println post-str "begin" suffix)
           [the-code clj-filename png-filename] (make-fn suffix)]
       (if (nil? the-code)
         (println "!!! suffix" suffix "unable to create" post-str "image")
         (post-to-web the-code clj-filename png-filename parent-vec)))))
  (println post-str "done."))

(defn post-random-batch-to-web
  "Post a batch of random codes & images to twitter and github."
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)]
    (post-batch-to-web* (partial make-random-code-and-png timestamp-str)
                        "random" suffix-str [])))

(defn post-children-to-web
  "Find the highest-scoring parents, post a batch of codes & images
  bred from those parents to twitter and github"
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)
        rents         (get-parent-tweets MAX-POSSIBLE-PARENTS)
        c0            (rand-nth rents)
        c1            (rand-nth rents)]
    (post-batch-to-web* (partial make-random-child-and-png (:code c0) (:code c1) timestamp-str)
                        "repro" suffix-str [(:name c0) (:name c1)])))

(defn post-mutants-to-web
  "Find the highest-scoring parents, post a batch of 5 codes & images
  mutated from the top parent to twitter and github"
  [suffix-str]
  (let [timestamp-str (get-timestamp-str)
        rents         (get-parent-tweets MAX-POSSIBLE-PARENTS)
        c0            (rand-nth rents)]
    (post-batch-to-web* (partial make-random-mutant-and-png (:code c0) timestamp-str)
                        "mutant" suffix-str [(:name c0)])))

(defn cur-hour
  "return the current hour"
  []
  (.get (java.util.Calendar/getInstance) java.util.Calendar/HOUR_OF_DAY))

(defn -main [& args]
  (println "======================================================================")
  (println "Started version" (env :tweegeemee-version))
  (println "posting one of: random ab, children CD or mutant MN")
  (setup-env!)
  (case (mod (cur-hour) 3)
    0 (post-random-batch-to-web "ab")
    1 (post-children-to-web "CD")
    2 (post-mutants-to-web "MN"))
  (cleanup-our-files!)
  (println "posting complete.")
  (shutdown-agents) ;; quit faster
  0) ;; return 0 status so we don't look like we crashed

;; ======================================================================
;; Example usage to explore at the repl
;; ======================================================================
(comment

  ;; be sure to get this setup
  (setup-env!)

  ;; generate & show a random image
  (let [c (get-random-code)]
    (show (eval c)))

  ;; breed images by hand...
  (def rents (get-parent-tweets 5))
  (def dad (:code (rand-nth rents)))
  (def mom (:code (rand-nth rents)))
  (show (eval dad))
  (show (eval mom))
  (let [c (get-random-child dad mom)]
    (if (not (nil? c))
      (show (eval c))))

  ;; breed mutants by hand...
  (def rents (get-parent-tweets 5))
  (def dad (:code (rand-nth rents)))
  (show (eval dad))
  (let [c (get-random-mutant dad)]
    (if (not (nil? c))
      (show (eval c))))

  ;; Look at the frequency of instructions
  (def archive (gists/read-archive my-gist-auth my-gist-archive-id))
  (defn get-fns [s]
    (filter #(not= % "")
            (-> (clojure.string/replace s #"clisk.live/|\(|\)|\[|\]|\.|[0-9]" "")
                (clojure.string/replace #" -" "")
                (clojure.string/split #" "))))
  (println (sort-by val (frequencies (sort (mapcat #(get-fns (:code %)) archive)))))
  (println (sort-by val (frequencies (sort (mapcat #(get-fns (:code %)) (drop 200 archive))))))

  ;; ----------------------------------------------------------------------
  ;; NOTE that code below this point posts to the web

  ;; post a random image to the web
  (let [[the-code clj-filename png-filename]
        (make-random-code-and-png (get-timestamp-str) "s")]
    (post-to-web the-code clj-filename png-filename))

  ;; Careful!
  (post-random-batch-to-web "r")

  ;; Careful!
  (post-children-to-web "ABCDE")

  ;; Careful!
  (post-mutants-to-web "VWXYZ")

  (try (tw/statuses-update
        :oauth-creds @my-twitter-creds
        :params {:status "testing from home"})
       (catch Exception e (println "Oh no! " (.getMessage e))))

  ;; genealogy
  (def rents (get-parent-tweets 5))
  (def cur { :name "160215_233302_M.clj" :parents ["160215_073140_D.clj"] :hash -89036647 :image-hash 1882259542
            :code (clisk.live/vsin (clisk.live/vdivide (clisk.live/adjust-hue [1.009 0.4101 -0.8179] (clisk.live/v- [0.502 2.7223 -1.2887] (clisk.live/v* clisk.live/pos clisk.live/pos))) (clisk.live/blue-from-hsl (clisk.live/vmod (clisk.live/adjust-hsl (clisk.live/vfrac (clisk.live/adjust-hsl (clisk.live/y [-2.2811 -1.7858 1.5606]) clisk.live/pos)) (clisk.live/alpha [-0.1433 1.4431 -2.088])) (clisk.live/green-from-hsl (clisk.live/x (clisk.live/adjust-hue (clisk.live/max-component 0.1601) (clisk.live/length clisk.live/pos))))))))
            })
  (def data (gists/read-archive my-gist-auth my-gist-archive-id))
  (def parent-map (apply hash-map
                         (mapcat
                          #(let [v (select-keys % [:name :parents])]
                             [(:name v) (:parents v)])
                          data)))
  (defn get-by-name [name] (first (filter #(= name (:name %)) data)))

  (def cur (nth rents 0))
  (show (eval (:code cur)))
  (def cur (get-by-name (first (:parents cur))))
  (def cur (get-by-name (second (:parents cur))))

  (show (eval (:code (nth rents 1))) :width 900 :height 900)

  ;; Facebook update helpers
  (print-top-n get-last-weeks-statuses 5)

  ;; geneaology
  (def layout (layout-generations "160208_193216_C.clj" parent-map))
  ;; render tiles
  (render-statuses (sort (distinct (map #(nth % 3) layout))) 64)
  ;; print for use in ipython to create the final image
  (map #(println (apply format "[%d, %d, %d, '%s']," %)) layout)

  ;; rate limit issue?
  (-> (tw/application-rate-limit-status :oauth-creds @my-twitter-creds)
      :body
      :resources
      :statuses
      :/statuses/user_timeline)
  ;; {:limit 180, :remaining 180, :reset 1455205245}

  ) ;; comment

;; to reproduce the heroku environment, do:
;;   lein with-profile production compile :all
;;   lein trampoline run
