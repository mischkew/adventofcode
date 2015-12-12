;; learn something about 2dimensional datastructures in clojure...

;; == IMPORTS ==

(require '[clojure.string :as str])
(use 'clojure.java.io)


;; == HELPERS ==

(def filename "/Users/sven/Documents/workspace/adventofcode/day6-input.txt")

(defn compute-from-input [filename cb]
  (with-open [rdr (reader filename)]
    (cb (line-seq rdr))))

;; == DAY 6 HELPERS ==

(defn build-grid [sizeX sizeY value]
  (loop [grid (transient []) y 0]
    (if (= y sizeY)
      (persistent! grid)
      (recur (conj! grid (vec (map (fn [_] value) (range sizeX)))) (inc y)))))
       
(defn operation [string]
  (cond (.contains string "turn on") :on
        (.contains string "turn off") :off
        (.contains string "toggle") :toggle))
              
(defn region [string]
  (let [v (vec (map read-string (re-seq #"\d+" string)))]
    {:min {:x (get v 0), :y (get v 1)}
     :max {:x (get v 2), :y (get v 3)}}))

(defn set-region [grid region fnvalue]
  (defn in-range [value orientation]
    (and (<= value (-> region :max orientation))
         (>= value (-> region :min orientation))))
  (defn set-y-region [grid-y]
    (vec (for [y (range (count grid-y))
          :let [op (get grid-y y)]]
      (if (in-range y :y)
        (fnvalue op)
        op))))
  (defn set-x-region []
    (vec (for [x (range (count grid))
          :let [grid-y (get grid x)]]
      (if (in-range x :x)
        (set-y-region grid-y)
        grid-y))))
  (set-x-region))


;; == CHALLENGE 1 ==

(defn update-operation [operation-key current]
  (if (= operation-key :toggle) (if (= current :on) :off :on)
      operation-key))

(defn apply-instruction [grid instruction-string]
  (set-region grid
              (region instruction-string)
              (partial update-operation (operation instruction-string))))

(defn count-lit-helper [grid]
  (count (for [x (flatten grid)
               :when (= x :on)] x)))

(defn count-lit []
  (compute-from-input
   filename
   (fn [lines]
     (loop [instructions lines
            grid (build-grid 1000 1000 :off)]
       (if (empty? instructions)
         (count-lit-helper grid)
         (recur (rest instructions) (apply-instruction grid (first instructions))))))))

;; (count-lit)
;; --> 377891


;; == CHALLENGE 2 ==

(defn update-count [operation current]
  (cond (= :on operation) (inc current)
        (= :off operation) (max (dec current) 0)
        (= :toggle operation) (+ current 2)))

(defn sum-brightness [grid]
  (reduce (fn [brightness value]
            (+ brightness value))
          0
          (flatten grid)))

(defn count-instruction [grid instruction-string]
  (set-region grid
              (region instruction-string)
              (partial update-count (operation instruction-string))))

(defn count-brightness []
  (compute-from-input
   filename
   (fn [lines]
     (loop [instructions lines
            grid (build-grid 1000 1000 0)]
       (if (empty? instructions)
         (sum-brightness grid)
         (recur (rest instructions) (count-instruction grid (first instructions))))))))

;; (count-brightness)
;; --> 14110788
