(ns tweegeemee.image
  (:use [clisk live])
  (:require
   ;;[clisk.live] tried this, but caused issues
   [clisk.patterns]
   [clojure.set      :as set]
   [clojure.string   :as string]
   [clojure.zip      :as zip]))

;; ======================================================================
(defonce MAX-ALL-COMPONENT-VALUE   220)   ;; not too white
(defonce MIN-ALL-COMPONENT-VALUE   36)    ;; not too dark
(defonce MIN-ALL-COMPONENT-DELTA   10)    ;; not too similar
(defonce MIN-NUM-COLORS            3)     ;; not too few colors (checkers be gone!)
(defonce IMAGE-SIZE                720)   ;; size to post
(defonce TEST-IMAGE-SIZE           16)    ;; size for boring & img-hash check
(defonce MAX-RANDOM-CODE-DEPTH     10)    ;; emperically got to this...
(defonce MAX-GOOD-CODE-ATTEMPTS    200)   ;; don't want to give up too quickly
(defonce PROB-TERM-FN              0.1)   ;; probability of term-fn vs term-vals
(defonce PROB-TERNARY-FN           0.02)  ;; vs. binary or unary
(defonce PROB-BINARY-FN            0.3)   ;; vs ternary or unary
(defonce PROB-SINGLE-MUTATION      0.95)  ;; mostly mutate vs. copy
(defonce PROB-STANDARD-MUTATE      0.84)  ;; standard vs. pan-zoom mutate

;; ======================================================================
;; Functions used in creating imagery.
(declare random-float)
(defn- random-scalar [] (random-float))
(defn- random-vec2   [] [(random-float) (random-float)])
(defn- random-vec3   [] [(random-float) (random-float) (random-float)])
(defn- random-vec4   [] [(random-float) (random-float) (random-float) (random-float)])
;; use ` instead of ' in order to add the namespace so it can be found when eval'd
(def term-vals   #{`pos `TAU `PI random-scalar random-vec2 random-vec3 random-vec4})
(def term-fns    #{`noise `snoise `plasma `splasma
                   `vnoise `vsnoise `vplasma `vsplasma
                   `grain `turbulence `vturbulence
                   `spots `blotches `agate `clouds `velvet `flecks `wood})
(def unary-fns   #{`vsin `vcos `vabs `vround `vfloor `vfrac
                   `square `vsqrt `sigmoid `max-component `min-component
                   `length `normalize `gradient
                   `theta `radius `polar `height `height-normal
                   `hue-from-rgb `lightness-from-rgb `saturation-from-rgb
                   `hsl-from-rgb `red-from-hsl `green-from-hsl `blue-from-hsl
                   `rgb-from-hsl `x `y `z `t `alpha})
(def binary-fns  #{`v+ `v* `v- `vdivide `vpow `vmod `dot `cross3
                   `vmin `vmax `turbulate `checker `rotate `scale `offset
                   `adjust-hue `adjust-hsl `vconcat `average})
(def ternary-fns #{`lerp `clamp `vconcat `vif `average})
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

(defn- random-float
  "return a random value in the range (-3,3) with only 4 significant
  digits to increase readability"
  []
  (let [x (* 3 (dec (rand 2)))
        x (/ (Math/floor (* x 10000)) 10000.0)]
    x))
;;(random-float)

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
      (+ node (random-float))
      ;;* If the node is a vector, it can be adjusted by adding random
      ;;amounts to each element.
      clojure.lang.PersistentVector
      (vec (map #(+ % (random-float)) node))
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
      (println "UNEXPECTED TYPE:" node (class node)))))
;;[See mutate fn]* Finally, a node can become a copy of another node from the
;;parent expression. For example (+ (abs X) (* Y .6)) might
;;become (+ (abs (* Y .6)) (* Y .6)). This causes effects similar to
;;those caused by mating an expression with itself. It allows for
;;sub-expressions to duplicate themselves within the overall
;;expression.

;;[TBD]* An expression can become the argument to a new random
;;function. Other arguments are generated at random if
;;necessary. For example X might become (* X .3).

(defn- mutate-pan-zoom
  "mutate the code string L with random scale & offset (aka pan & zoom)"
  [L]
  (let [rnd-scale (if (> (rand) 0.5) ; 50/50 zoom in/out
                    (+ 1.05 (rand 19))
                    (+ 0.05 (rand 0.90)))
        rnd-x (- (rand 10.0) 5.0)
        rnd-y (- (rand 10.0) 5.0)
        code `(clisk.live/offset [~rnd-x ~rnd-y]
                                 (clisk.live/scale ~rnd-scale
                                                   ~L))]
    code))

(defn- mutate
  "mutate the code string L according to Karl Sims' SIGGRAPH paper."
  [L]
  (let [loc1      (rand-nth (locs L))
        loc2      (rand-nth (locs-ex-fns L))
        loc3      (rand-nth (locs-ex-fns L))
        new-node  (mutate-node (zip/node loc1))
        ;;_ (println "newnode" new-node)
        ]
    (if (> (rand) PROB-STANDARD-MUTATE)
      ;; NEW! some of the time do a random pan/zoom
      ;; given errors found in Sim's mutation this needs
      ;; to be less probable or it will dominate.  Found 
      ;; rand > 75% led to ~50% pan-zoom as final choice.
      (mutate-pan-zoom L)
      ;; most of the time do Karl Sims' mutation
      (if (nil? new-node)
        ;; copy random nodes & sub-nodes
        (replace-loc loc2 loc3)
        ;; or, replace with new mutant
        (replace-loc-with-node loc1 new-node)))))

(defn- good-random-code?
  "does code x have at least one paren?"
  [x]
  (= (first (pr-str x)) \())

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

(declare image-hash)
(defn- get-good-code*
  "The main code-generation workhorse function.  Given a
  code-creator-fn (random, breed or combine), return some code that
  creates non-boring images.  Tries for a while, but if it gives up,
  it returns nil."
  [code-creator-fn my-old-hashes my-old-image-hashes]
  (try
    (let [cur-count  (atom 0)
          good-image (atom false)
          good-code  (atom nil)]
      (while (and (< @cur-count MAX-GOOD-CODE-ATTEMPTS)
                  (not @good-image))
        (swap! cur-count inc)
        (try
          (let [cur-code (code-creator-fn)
                ;;_ (println "\n??" cur-code)
                _ (when-not (nil? (@my-old-hashes (hash cur-code)))
                    (throw (Exception. "previously created code")))
                _ (when-not (good-random-code? cur-code)
                    (throw (Exception. "badly created code")))
                img (clisk.live/image (eval cur-code) :size TEST-IMAGE-SIZE)
                _ (when-not (nil? (@my-old-image-hashes (image-hash img)))
                    (throw (Exception. "previously created image")))
                _ (when-not (good-image? img)
                    (throw (Exception. "boring image")))]
            ;; no exception--got a good one
            (println "\n" @cur-count "got:" cur-code)
            (reset! good-image true)
            (reset! good-code cur-code))
          (catch Exception e
            ;;(println @cur-count "Exception" (.getMessage e))
            ;; intentionally NOT printing the exception e, we are
            ;; merely noting this as we attempt to generate good code
            (print "e"))
          (catch java.util.concurrent.ExecutionException e
            ;;(println @cur-count "execution exception")
            ;; intentionally NOT printing the exception e, we are
            ;; merely noting this as we attempt to generate good code
            (print "E"))))
      @good-code)
    (catch Exception e
      (println "get-good-code* Setup Exception" (.getMessage e))
      nil)))

;; ======================================================================
;; PUBLIC API

(defn get-random-code
  "get a good image-creation code created randomly"
  [my-old-hashes my-old-image-hashes]
  (get-good-code* (fn [] (random-code MAX-RANDOM-CODE-DEPTH)) my-old-hashes my-old-image-hashes))

(defn get-random-child
  "get a good image-creation code created via breeding two other
  codes"
  [my-old-hashes my-old-image-hashes code0 code1]
  (get-good-code* (fn [] (breed code0 code1)) my-old-hashes my-old-image-hashes))

(defn get-random-mutant
  "get a good image-creation code created via mutating a code"
  [my-old-hashes my-old-image-hashes code]
  (get-good-code* (fn [] (mutate code)) my-old-hashes my-old-image-hashes))

(defn sanity-check-code
  "be careful with strange code you download from the web, son.  Only
  allow fns in the set of fns we know about."
  [code-str]
  (let [code-fns (map #(symbol (string/replace % #"[\( ]" ""))
                      (re-seq #"\(.+? " code-str))
        code-fn-set (set code-fns)]
    (set/subset? code-fn-set fns)))

(defn image-hash
  "hash the colors in an image so we can figure out if we've created
  this one before."
  [img]
  (let [W (.getWidth img)
        H (.getHeight img)]
    (hash (for [x (range W) y (range H)] (.getRGB img x y)))))
