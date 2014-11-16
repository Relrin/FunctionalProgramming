(ns lab2.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [cli]])
  (:require [clojure.java.io   :as io])
  (:require [clojure.string    :as string])
  (:require [clj-http.client   :as client])  
  (:require [net.cgrand.enlive-html :as html])
  (:refer-clojure :exclude [resolve])
  (:use [clojurewerkz.urly.core])
  (:use [slingshot.slingshot])
)


; TODO: add multithreading (using atoms/vars/agents/etc.)
; TODO: extract all HREFs from page (recursively)

;-----------------------------------
(def tree (ref []))


(defn get-tree []
  ; Getting vectors with urls
  (deref tree))


(defn add-url-from-file [val]
  ; Add new URL at the end of vector
  (dosync
     (alter tree conj val)
     (alter tree conj  []))) 


(defn add-urls-on-index [item index]
  ; Inserting vector with URLs after element with some index
  (let [tmpTree (get-tree)]
    (dosync 
       (alter tree assoc index item))))
;------------------------------------

(defn read-urls [filepath]
  ; Reading URLs from *.txt file
  ; all URLs will be in one vector
  ; its seems like [link1 link2 ... linkN]
  (with-open [rdr (io/reader filepath)]
    (doseq [line (line-seq rdr)]
       (if (not= line "")
           (add-url-from-file line))))
  (get-tree))


(defn delete-element [vc pos]
  ; Delete element from vector by index
  (vec (concat 
         (subvec vc 0 pos) 
         (subvec vc (inc pos)))))


(defn relativeURL? [itemURL]
  ; checking URL on relativity
  ; NOTE: symbol '#' means, that its not URL, its anchor 
  (and (= (.indexOf itemURL "/") 0) (= (.indexOf itemURL "#") -1)))


(defn absoluteURL? [itemURL]
  ; checking URL on absoluteness
  (or (= (.indexOf itemURL "http://") 0) (= (.indexOf itemURL "https://") 0)))


(defn resolve-links [baseURL vector-links]
  ; Resolve all links
  ; See (example section): https://github.com/michaelklishin/urly
  (let [relative-links (vec (filter relativeURL? vector-links))
        absolute-links (vec (filter absoluteURL? vector-links))]
    (into [] (distinct (concat (vec (map #(str (resolve (java.net.URI. baseURL) %)) relative-links)) absolute-links)))))


(defn find-href [url-string]
  ; Finding all HREFs. Returns vector with corrected links
  (let [url (html/html-resource (java.net.URL. (str url-string)))]
    (resolve-links url-string (vec (keep #(-> % :attrs :href) (html/select url [:a]))))))


(defn parse-url [url-string]
  ; TODO: add count in vector for result value
  ; TODO: correct also with redirect version
  ; NOTE:
  ; if get "error 404/500/504", then returns "bad"
  ; if catch redirect, then return "redirect" + chain of links (but take last URL)
  (try+
    (let [downloaded-html (client/get url-string)]
      (if (empty? (delete-element (get downloaded-html :trace-redirects) 0))
        (find-href url-string)
        ; get redirects before downloading page
        (str url-string " redirect " (get (vec (take-last 1 (get downloaded-html :trace-redirects))) 0))
      )
    ) 
    ; exception handler for error 404/504
    (catch [:status 404] {:keys [trace-redirects]}
      (str url-string " bad"))
    (catch [:status 500] {:keys [trace-redirects]}
      (str url-string " bad"))
    (catch [:status 504] {:keys [trace-redirects]}
      (str url-string " bad"))))


(defn thread-parser [url-string index max-deep]
  ; TODO: Fill actions...
  (add-urls-on-index (parse-url url-string) index)
)


(defn print-tree
  ; Printing vector as a tree
  ([tree] 
   (apply str (map #(print-tree % "") tree)))
  ([tree prefix]
    (if (vector? tree)
      (->> (map #(print-tree % (str prefix "  ")) tree)
           (apply str))
      (str prefix tree "\n"))))


(defn parse-to-tree [filepath max-deep]
  ; Parsing urls from files to tree   
  (let [urls (read-urls filepath)  ; vector with URLs
        n-threads 25]              ; count of parallel threads (change me)
  ; some logic for multithreading...
  (thread-parser "http://dev.by/" 1 max-deep)
  (thread-parser "http://habrahabr.ru/" 3 max-deep)
  (println (print-tree (get-tree)))
))


(defn -main [& args]
  ; Main function for project
  ; Using: lein run -- --file=src/lab2/urls.txt
  ;        lein run -- --file=src/lab2/urls.txt --deep=20
  (let [[opts args banner]
        (cli args
          ["-h" "--help" "Print this help" :default false :flag true]
          ["-f" "--file" "REQUIRED: File with data"]
          ["-d" "--deep" "Parse deep of the three" :default 1]
          )]
    ; Print help, when no typed args
    (when (:help opts)
      (println banner)
      (System/exit 0))
    ; Or process args and start work
    (if (and (:file opts) (:deep opts))
      (println (string/join "\n" (parse-to-tree (:file opts) (:deep opts))))
      (println "Please, type path for file!"))))
