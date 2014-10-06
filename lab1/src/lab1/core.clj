(ns lab1.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [cli]])
  (:require [clojure.java.io   :as io])
  (:require [clojure.string    :as string])
  (:require [clojure.math.numeric-tower :as math])
  (import java.lang.Math))


(defn str-to-vector [string]
  ; convert scheme:
  ; string -> split on parts -> delete last element (cause its cluster number/name) -> convert to double -> to vector
  (into [] (map read-string (drop-last (string/split string #",")))))


(defn Euclid-distance [point_1 point_2]
  ; point_1, point_2 - vectors, which contains coords
  (let [sub-of-squares
       (fn [p1 p2]
           (Math/pow (- p1 p2) 2))]
  (Math/sqrt(reduce + 0 (map sub-of-squares point_1 point_2)))))


(defn Hamming-distance [point_1 point_2]
  ; point_1, point_2 - vectors, which contains coords
  (let [equals?
       (fn [p1 p2]
           (if (= p1 p2) 0 1))]
  (reduce + 0 (map equals? point_1 point_2))))


(defn potentials [data alpha fn-distance]
  ; Calculating potential for one point
  (let [point-potential 
       (fn [x]
         (let [len (reduce + 0 (map #(Math/exp (- (* alpha (fn-distance % x)))) data))]
         (list len x)))]
  (map point-potential data)))


(defn clusterize [data fn-distance]
  (let [threshold-upper 0.5                        ; max threshold
        threshold-lower 0.15                       ; min threshold
        Ra 3                                       ; Ra constant
        Rb (* Ra 1.5)                              ; Rb constant
        alpha (/ 4 (math/expt Ra 2))               
        beta  (/ 4 (math/expt Rb 2))               
        p-points (potentials data alpha fn-distance)] ; calculated potentials
    (loop [fdata fdata
           clusters ()]
      (let [p1  (or (first (first clusters)) 0)          
            pxk (apply max-key first p-points)        
            pk  (first pxk)
            xk  (last pxk)
            p-points-n (map #(cons (- (first %)
                          (* pk (Math/exp (- (* beta (fn-distance (last %) xk)))))) (rest %))
                          p-points)]
        (cond
         (> pk (* threshold-upper p1)) (recur p-points-n (conj clusters pxk))   ; pxk as a cluster center and continue...
         (< pk (* threshold-lower p1)) clusters                              ; reject last cluster and end process...
         :else 
           (let [dmin (apply min (map #(fn-distance xk (last %)) clusters))] ; dmin - its shortest distance between xk and cluster center
                (if (>= (+ (/ dmin Ra) (/ pk p1)) 1)
                   ; xk - its new cluster center and continue...
                   (recur p-points-n (conj clusters pxk))                  
                 ; else reject xk and potential of xk = 0; take max xk and after that re-test clusters 
                 (recur (map #(if (= xk (last %)) (list 0 xk) %) p-points) clusters))))))))


(defn processing-data [file fn-distance]
  ; Read common separated file line-by-line
  (def data (atom []))
  (with-open [rdr (io/reader file)]
    (doseq [line (line-seq rdr)]
       (if (not= line "")
           (swap! data conj (str-to-vector line)))))
  ; lets clusterize data and output
  (println (string/join "\n" (map str (clusterize @data fn-distance)))))


(defn -main [& args]
  ; Main function for project
  ; Using: lein run -- --file=src/lab1/data1.data --hamming=true
  ;        lein run -- --file=src/lab1/data2.txt --euclid=true
  (let [[opts args banner]
        (cli args
          ["-h" "--help" "Print this help" :default false :flag true]
          ["-f" "--file" "REQUIRED: File with data"]
          ["-g" "--hamming" "Use Hamming algorithm"]
          ["-e" "--euclid" "Use Euclid algorithm"]
          )]
    ; Print help, when no typed args
    (when (:help opts)
      (println banner)
      (System/exit 0))
    ; Or process args and start work
    (if (and (:file opts) (or (:hamming opts) (:euclid opts)))
      (do
        ; Use Hamming algorithm
        (if (:hamming opts)
          (processing-data (:file opts) Hamming-distance)
        ; Use Euclid algorithm
        (processing-data (:file opts) Euclid-distance)))
      (println "Please, type path for file and algorithm!"))))
