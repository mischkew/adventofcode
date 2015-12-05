;; compute area of wrapping paper
(require '[clojure.string :as str])
(use 'clojure.java.io)
(def filename "/Users/sven/Documents/workspace/adventofcode/day2-input.txt")

(defn area-of-present [l w h]
  (let [area (+ (* 2 l w) (* 2 w h) (* 2 h l))
        extra (/ (* l w h) (max l w h))]
    (+ area extra)))

(defn ribbon-of-present [l w h]
  (let [ribbon (- (+ l l w w h h) (* 2 (max l w h)))
        bow (* l w h)]
    (+ ribbon bow)))

(defn present-string-to-vector [present-string]
  (map read-string (str/split present-string #"x")))

(defn area-from-string [present-string]
  (apply area-of-present (present-string-to-vector present-string)))

(defn ribbon-from-string [string]
  (apply ribbon-of-present (present-string-to-vector string)))

(defn compute-from-input [filename cb]
  (with-open [rdr (reader filename)]
    (cb (line-seq rdr))))

(defn compute-area []
  (compute-from-input
   filename
   (fn [lines]
     (reduce
      (fn [acc string] (+ acc (area-from-string string)))
      0
      lines))))

(defn compute-ribbon []
  (compute-from-input
   filename
   (fn [lines]
     (reduce
      (fn [acc string] (+ acc (ribbon-from-string string)))
      0
      lines))))
  
;; (compute-area)
;; --> 1586300
