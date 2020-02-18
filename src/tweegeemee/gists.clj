(ns tweegeemee.gists
  (:require
   [clojure.edn        :as edn]
   [tentacles.gists :as gists]))

(def     DEBUG-NO-POSTING      false) ;; set true when you don't want to post
(defonce GIST-ARCHIVE-FILENAME "1_archive.edn")

(defn- sanitize-filename
  [url]
  (clojure.string/replace url "." "-"))

(defn- write-str-to-gist-archive
  "write data-str to the GIST-ARCHIVE-FILENAME within the
  @my-gist-archive-id.  Throws an exception on failure."
  [my-gist-archive-id my-gist-auth data-str]
  (let [resp (gists/edit-gist
              @my-gist-archive-id
              {:auth @my-gist-auth
               :files { GIST-ARCHIVE-FILENAME { :content data-str }}})]
    (if (nil? (:status resp))
      resp
      (throw (Exception. (str "gist error" (:message (:body resp))))))))

(defn- read-str-from-gist-archive
  "return str from the GIST-ARCHIVE-FILENAME within the
  @my-gist-archive-id.  Throws an exception on failure."
  [my-gist-archive-id my-gist-auth]
  (let [resp (try
               (gists/specific-gist @my-gist-archive-id {:auth @my-gist-auth})
               (catch Exception e
                 (println "gist read exception: " (.getMessage e))
                 [] ;; return empty response
                 ))]
    (if (nil? (:status resp))
      (if (-> (:files resp)
              ((keyword GIST-ARCHIVE-FILENAME))
              :truncated)
        (throw (Exception. (str "truncated gist error")))
        (-> (:files resp)
            ((keyword GIST-ARCHIVE-FILENAME))
            :content))
      (throw (Exception. (str "gist error" (:message (:body resp))))))))

(defn read-gist-archive-data
  "parse edn-formatted data from the GIST-ARCHIVE-FILENAME within the
  @my-gist-archive-id."
  [my-gist-archive-id my-gist-auth]
  (edn/read-string (read-str-from-gist-archive my-gist-archive-id my-gist-auth)))

(defn- gist-print-data
  "take array of dicts and return as str.  dicts better be formatted
  as I expect."
  [data]
  (str "[\n"
       (reduce (fn [a b]
                 (format "%s { :name \"%s\" :parents %s :hash %d :image-hash %d\n   :code %s\n }\n"
                         a (:name b) (:parents b) (:hash b) (:image-hash b) (:code b)))
               "" data)
       "]\n"
       ))

(defn- update-gist-archive-data
  "append new-data dict data to the GIST-ARCHIVE-FILENAME within the
  @my-gist-archive-id. Return the line number for the new-data entry
  in the file."
  [my-gist-archive-id my-gist-auth new-data]
  (let [old-archive-data (read-gist-archive-data my-gist-archive-id my-gist-auth)
        new-archive-data (conj old-archive-data new-data)
        line-number      (+ 2 (* 3 (dec (count new-archive-data))))
        new-archive-str  (gist-print-data new-archive-data)]
    (write-str-to-gist-archive my-gist-archive-id my-gist-auth new-archive-str)
    line-number))

(defn append-to-gist
  "Generate data dict from filename & code to append to the
  GIST-ARCHIVE-FILENAME within the @my-gist-archive-id. Return the
  line number for the new-data entry in the file."
  [my-gist-archive-id my-gist-auth filename code parent-vec image-hash]
  (if DEBUG-NO-POSTING
    (do
      (println "DEBUG: NOT APPENDING CODE TO GIST")
      0)
    (update-gist-archive-data
     my-gist-archive-id my-gist-auth
     {:name       filename
      :hash       (hash code)
      :image-hash image-hash
      :code       (pr-str code)
      :parents    parent-vec
      })))

(defn get-gist-status-by-name
  [data name]
  (first (filter #(= name (:name %)) data)))

(defn get-gist-url
  [my-gist-archive-id gist-line-number]
   (str "https://gist.github.com/rogerallen/"
        @my-gist-archive-id
        "#file-" (sanitize-filename GIST-ARCHIVE-FILENAME)
        "-L" gist-line-number "-L" (+ 2 gist-line-number)))
