;; compute area of wrapping paper
(require '[clojure.string :as str])
(use 'clojure.java.io)

(defn area-of-present [l w h]
  (let [area (+ (* 2 l w) (* 2 w h) (* 2 h l))
        extra (/ (* l w h) (max l w h))]
    (+ area extra)))

(defn present-string-to-vector [present-string]
  (map read-string (str/split present-string #"x")))

(defn area-from-string [present-string]
  (apply area-of-present (present-string-to-vector present-string)))
                                     
(defn compute-from-input [filename]
  (with-open [rdr (reader filename)]
    (loop [count 0 lines (line-seq rdr)]
      (if (empty? lines) count
          (recur
           (+ count (area-from-string (first lines)))
           (rest lines))))))

;; (compute-from-input "./day2-input.txt")
;; --> 1586300
