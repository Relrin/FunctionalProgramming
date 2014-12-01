(ns lab2.core-test
  (:require [clojure.test :refer :all]
            [lab2.core :refer :all]
            [net.cgrand.enlive-html :as html]))

(defn load-html [filepath]
  (html/html-snippet (slurp filepath)))


(deftest parse-html
  (testing "Parse html page correctly."
    (let [content (load-html "test/lab2/devby.html")
          uris (get-valid-urls "http://dev.by" content)]
      (is (= (count uris) 156)))))


(deftest page-not-found
  (testing "Error 404"
    (let [server-respone (fetch-url "http://www.google.ru/somebadlink")]
      (is (= (:status server-respone) 404)))))
