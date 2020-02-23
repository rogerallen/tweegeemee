(ns tweegeemee.gists
  (:require
   [clojure.edn     :as edn]
   [tentacles.gists :as gists]))

;; ======================================================================
(def     DEBUG-NO-POSTING false) ;; set true when you don't want to post
(defonce ARCHIVE-FILENAME "1_archive.edn")

;; ======================================================================
(defn- sanitize-filename
  [url]
  (clojure.string/replace url "." "-"))

(defn- write-str-to-archive
  "write data-str to the ARCHIVE-FILENAME within the
  @archive-id.  Throws an exception on failure."
  [auth archive-id data-str]
  (let [resp (gists/edit-gist
              @archive-id
              {:auth @auth
               :files { ARCHIVE-FILENAME { :content data-str }}})]
    (if (nil? (:status resp))
      resp
      (throw (Exception. (str "gist error" (:message (:body resp))))))))

(defn- read-str-from-archive
  "return str from the ARCHIVE-FILENAME within the
  @archive-id.  Throws an exception on failure."
  [auth archive-id]
  (let [resp (try
               (gists/specific-gist @archive-id {:auth @auth})
               (catch Exception e
                 (println "gist read exception: " (.getMessage e))
                 [] ;; return empty response
                 ))]
    (if (nil? (:status resp))
      (if (-> (:files resp)
              ((keyword ARCHIVE-FILENAME))
              :truncated)
        (throw (Exception. (str "truncated gist error")))
        (-> (:files resp)
            ((keyword ARCHIVE-FILENAME))
            :content))
      (throw (Exception. (str "gist error" (:message (:body resp))))))))

(defn- format-archive
  "take array of dicts and return as str.  dicts better be formatted
  as I expect."
  [data]
  (str "[\n"
       (reduce (fn [a b]
                 (format "%s { :name \"%s\" :parents %s :hash %d :image-hash %d%s\n   :code %s\n }\n"
                         a (:name b)
                         (:parents b)
                         (:hash b)
                         (:image-hash b)
                         (if-let [twitter-id (:twitter-id b)]
                           (format " :twitter-id %s" twitter-id)
                           "")
                         (:code b)))
               "" data)
       "]\n"))

(declare read-archive)
(defn- update-archive
  "append new-data dict data to the ARCHIVE-FILENAME within the
  @archive-id. Return the line number for the new-data entry
  in the file."
  [auth archive-id new-data]
  (let [old-archive-data (read-archive auth archive-id)
        new-archive-data (conj old-archive-data new-data)
        line-number      (+ 2 (* 3 (dec (count new-archive-data))))
        new-archive-str  (format-archive new-archive-data)]
    (write-str-to-archive auth archive-id new-archive-str)
    line-number))

;; ======================================================================
;; public api
(defn read-archive
  "parse edn-formatted data from the ARCHIVE-FILENAME within the
  @archive-id."
  [auth archive-id]
  (edn/read-string (read-str-from-archive auth archive-id)))

(defn write-archive
  "overwrite ARCHIVE-FILENAME within the @archive-id with new-data"
  [auth archive-id new-data]
  (let [new-archive-str (format-archive new-data)]
    (write-str-to-archive auth archive-id new-archive-str)))

(defn append-archive
  "Generate data dict from filename & code to append to the
  ARCHIVE-FILENAME within the @archive-id. Return the
  line number for the new-data entry in the file."
  [auth archive-id filename code parent-vec image-hash]
  (if DEBUG-NO-POSTING
    (do
      (println "DEBUG: NOT APPENDING CODE TO GIST ARCHIVE")
      0)
    (update-archive
     auth archive-id
     {:name       filename
      :hash       (hash code)
      :image-hash image-hash
      :code       (pr-str code)
      :parents    parent-vec
      })))

(defn get-entry-by-name
  [data name]
  (first (filter #(= name (:name %)) data)))

(defn get-url
  [archive-id gist-line-number]
   (str "https://gist.github.com/rogerallen/"
        @archive-id
        "#file-" (sanitize-filename ARCHIVE-FILENAME)
        "-L" gist-line-number "-L" (+ 2 gist-line-number)))
