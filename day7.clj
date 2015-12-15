;; emulating circuits :)
(use 'clojure.java.io)
(def filename "/Users/sven/Documents/workspace/adventofcode/day7-input.txt")

(defn compute-from-input [filename cb]
  (with-open [rdr (reader filename)]
    (cb (line-seq rdr))))

(def example-input '("123 -> xy"
                     "xy AND y -> d"
                     "y RSHIFT 2 -> g"
                     "456 -> y"                     
                     "xy OR y -> e"
                     "xy LSHIFT 2 -> f"
                     "NOT xy -> h"
                     "NOT y -> i"
                     "1 AND y -> k"))


(def operations
  {"NOT" bit-not
   "AND" bit-and
   "OR" bit-or
   "LSHIFT" bit-shift-left
   "RSHIFT" bit-shift-right})

(defn operation [op-string]
  (operations op-string))

;; direct operation

(defn direct [string]
  (rest (re-find #"^(\d+|[a-z]+)\s->\s([a-z]+)$" string)))

(defn direct-input [op-direct]
  (first op-direct))

(defn direct-output [op-direct]
  (last op-direct))

;; unary operation

(defn unary [string]
  (rest (re-find #"(NOT)\s(\d+|[a-z]+)\s->\s([a-z]+)" string)))

(defn unary-input [op-unary]
  (nth op-unary 1))

(defn unary-output [op-unary]
  (last op-unary))

(defn unary-op [op-unary]
  (operation (first op-unary)))

;; binary operation

(defn binary [string]
  (rest (re-find #"(\d+|[a-z]+)\s(AND|OR|LSHIFT|RSHIFT)\s(\d+|[a-z]+)\s->\s([a-z]+)" string)))

(defn binary-input [op-binary]
  (list (first op-binary) (nth op-binary 2)))

(defn binary-output [op-binary]
  (last op-binary))

(defn binary-op [op-binary]
  (operation (nth op-binary 1)))

;; hashmap

(defn store [hashmap _key value]
  (assoc hashmap _key value))

;; queuing

(defn signal? [string]
  (some? (re-find #"\d+" string)))

(defn string->signal [string]
  (read-string string))

(defn build-queue [op-tuples]
  (apply (partial conj (clojure.lang.PersistentQueue/EMPTY)) op-tuples))

(defn perform-direct [hashmap op]
  (cond (signal? (direct-input op)) (store hashmap
                                (direct-output op)
                                (-> op
                                    direct-input
                                    string->signal))
        (contains? hashmap (direct-input op)) (store hashmap
                                                     (direct-output op)
                                                     (hashmap (direct-input op)))))

(defn perform-unary [hashmap op]
  (if (contains? hashmap (unary-input op)) (store hashmap
                                                  (unary-output op)
                                                  ((unary-op op) (hashmap (unary-input op))))))



(defn perform-binary [hashmap op]
  (defn _store [i1 i2]
    (store hashmap  (binary-output op) ((binary-op op) i1 i2)))
  (let [first-input (first (binary-input op))
        last-input (last (binary-input op))
        first-signal (signal? first-input)
        first-stored (contains? hashmap first-input)
        last-signal (signal? last-input)
        last-stored (contains? hashmap last-input)]
    (cond (and first-signal last-signal) (_store (string->signal first-input)
                                                 (string->signal last-input))
          (and first-signal last-stored) (_store (string->signal first-input)
                                                 (hashmap last-input))
          (and first-stored last-signal) (_store (hashmap first-input)
                                                 (string->signal last-input))
          (and first-stored last-stored) (_store (hashmap first-input)
                                                 (hashmap last-input)))))
                           

(defn perform-instruction [hashmap string]
  (let [_direct (direct string)
        _unary (unary string)
        _binary (binary string)]
    (cond (not (empty? _direct)) (perform-direct hashmap _direct)
          (not (empty? _unary)) (perform-unary hashmap _unary)
          (not (empty? _binary)) (perform-binary hashmap _binary))))

(defn intrange [value]
  (if (some? value)
    (if (> 0 value)
      (+ 65536 value)
      value)))
    
(defn perform-or-enqueue [hashmap queue]
  (let [result (perform-instruction hashmap (peek queue))]
    (if (some? result)
      (list result (pop queue))
      (list hashmap (pop (conj queue (peek queue)))))))

(defn run-raw [hashmap queue]
  (loop [h hashmap q queue]
    (if (empty? q)
      h
      (let [[_h _q] (perform-or-enqueue h q)]
        (recur _h _q)))))

(defn run [hashmap queue]
  (into {} (map #(vector (first %) (intrange (second %)))
                (run-raw hashmap queue))))

(defn wires []
  (compute-from-input
   filename
   (fn [lines]
     (run {} (build-queue lines)))))

;; challenge 1
;; (wires)
;; 16076

;; challenge 2
;; (wires)
;; 2797
