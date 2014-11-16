(ns lab2.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [cli]])
  (:require [clojure.java.io   :as io])
  (:require [clojure.string    :as string])
  (:require [clj-http.client   :as client])  
  (:require [net.cgrand.enlive-html :as html])
  (:use [slingshot.slingshot])
)


; TODO: add multithreading (using atoms/vars/agents/etc.)
; TODO: extract all HREFs from page (recursively)


(defn read-urls [filepath]
  ; Reading URLs from *.txt file
  ; all URLs will be in one vector
  ; its seems like [link1 link2 ... linkN]
  (let [data (atom [])]
  (with-open [rdr (io/reader filepath)]
    (doseq [line (line-seq rdr)]
       (if (not= line "")
           (swap! data conj line))))
  @data))


(defn delete-element [vc pos]
  ; Delete element from vector by index
  (vec (concat 
         (subvec vc 0 pos) 
         (subvec vc (inc pos)))))


(defn find-href [url-string]
  ; Finding all HREFs in <body> tag
  (let [url (html/html-resource (java.net.URL. (str url-string)))]
      (vec (map #(-> % :attrs :href) (html/select url [:a])))
  )
)


(defn parse-url [url-string deep max-deep]
  ; TODO: add count in vector for result value
  ; Processing URL
  ; NOTE:
  ; if get "error 404", then returns "bad"
  ; if catch redirect, then return "redirect" + chain of links (but take last URL)
  (try+
    (let [downloaded-html (client/get url-string)]
      (if (empty? (delete-element (get downloaded-html :trace-redirects) 0))
        (find-href url-string)
        ; get redirects before downloading page
        (str url-string " redirect " (get (vec (take-last 1 (get downloaded-html :trace-redirects))) 0))
      )
    ) 
    ; exception handler for error 404 
    (catch [:status 404] {:keys [trace-redirects]}
      (str url-string " bad"))))


(defn print-tree
  ; Printing vector as a tree
  ([tree] 
   (apply str (map #(print-tree % "") tree)))
  ([tree prefix]
    (if (vector? tree)
      (->> (map #(print-tree % (str prefix "  ")) tree)
           (apply str))
      (str prefix tree "\n"))))


(defn parse-to-tree [filepath deep]
  ; Parsing urls from files to tree   
  (let [urls (read-urls filepath)  ; vector with URLs
        n-threads 25]              ; count of parallel threads (change me)
  ;(println (print-tree test-tree))
  (println (parse-url "http://twitter.com" 0 deep-lvl))
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
