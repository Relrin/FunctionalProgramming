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


(defn uuid []
  ; generate random UUID
  (str (java.util.UUID/randomUUID)))


(defn create-node [id parent uris depth uri status children location]
  ; create node in the tree
  {:id id :parent parent :uris uris :depth depth :uri uri :status status :children children :location location})


(defn create-root-node [urls depth]
  (create-node (uuid) nil urls depth "root" nil (atom []) nil))
;------------------------------

(defn read-file [filepath]
  ; reading data from file
  ; scheme: read all file as string -> split by '\n'
  (clojure.string/split-lines (slurp filepath)))


(defn not-nil? [x]
  (not (nil? x)))


(defn remove-nils [col]
  ; remove nil from collection
  (filter not-nil? col))


(defn get-sutable-lines [filepath]
  (remove-nils (read-file filepath)))


(defn get-indent [n]
  ; repeat N time delimeter
  (apply str (repeat n " ")))


(defn get-status-message [node]
  (if (= (:status node) 404)
    " bad"
    (let [message (str " " (count (:uris node)) " link(s)")]
      (if (not-nil? (:location node))
        (str message " redirects " (:location node)))
        message)))


(defn print-formatted-line [node level]
  (let [indent (* 2 level)
        uri    (:uri node)
        status-message (get-status-message node)]
    (println (str (get-indent indent) uri status-message))))


(defn relativeURL? [itemURL]
  ; checking URL on relativity
  ; NOTE: symbol '#' means, that its not URL, its anchor 
  (and (= (.indexOf itemURL "/") 0) (= (.indexOf itemURL "#") -1)))


(defn absoluteURL? [itemURL]
  ; checking URL on absoluteness
  (or (= (.indexOf itemURL "http://") 0) (= (.indexOf itemURL "https://") 0)))


(defn resolve-links [baseURL vector-links]
  ; resolve all links
  ; see (example section): https://github.com/michaelklishin/urly
  (let [relative-links (vec (filter relativeURL? vector-links))
        absolute-links (vec (filter absoluteURL? vector-links))]
    (into [] (distinct (concat (vec (map #(str (resolve (java.net.URI. baseURL) %)) relative-links)) absolute-links)))))


(defn get-valid-urls [baseURL content]
  ; finding all HREFs. Returns vector with corrected links
  (let [data (->> (html/select content #{[:a]})
                  (map #(:href (:attrs %1)))
                  (remove-nils))]
  (resolve-links baseURL data)))


(defn get-parent-id [parent]
  ; return parent's ID
  (if (nil? parent)
    nil
    (:id parent)))


(defn fetch-url [url]
  ; try to download HTML
  (try+
    (client/get url {:throw-exceptions false})
    (catch Object _ {:status 404 :headers nil})))


(defn get-formatted-content [raw-content]
  (let [content-type (:content-type (:headers raw-content))]
    (if (and (not= 404 (:status raw-content)) (boolean (re-find #"text/html" content-type)))
      (html/html-snippet (:body raw-content))
      nil)))


(defn get-location-header [raw-content]
  ; checking on redirects
  (let [status (:status raw-content)]
    (if (boolean (some #(= status %) '(300 301 302 303 307)))
      (:location (:headers raw-content))
      nil)))


(defn parse-specific-page [parent url depth]
  ; parsing some URL 
  ; (println url)
  (let [raw-content (fetch-url url)
        status      (:status raw-content)
        id          (uuid)
        formatted-content (get-formatted-content raw-content)
        child (if (not-nil? formatted-content)
                (create-node id (get-parent-id parent) (get-valid-urls url formatted-content) depth url status (atom []) (get-location-header raw-content))
                (create-node id (get-parent-id parent) '() depth url status (atom []) (get-location-header raw-content)))]
    (swap! (:children parent) conj child)
    child))


(defn visit-node [node urls depth]
  ; parallel processing (using parallel map) URLs while depth > 1
  ; else return node
  (let [new-depth (dec depth)]
    (if (< depth 1)
      node
      (doseq [child (pmap #(parse-specific-page node %1 depth) urls)] (visit-node child (:uris child) new-depth)))))


(defn do-walk [node level]
  ; printing value in node and go inner
  (print-formatted-line node level)
  (doseq [child @(:children node)] (do-walk child (inc level))))


(defn walk-tree [root]
  ; walking over the tree from root
  (do-walk root 0))


(defn parse-to-tree [filepath depth]
  ; parsing urls from files to tree   
  (let [urls (get-sutable-lines filepath)
         parent (create-root-node urls depth)]
     (visit-node parent urls depth)
     parent))


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
      (walk-tree (parse-to-tree (:file opts) (Integer. (:deep opts))))
      (println "Please, type path for file!"))))
