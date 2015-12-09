;; regex magic
(use 'clojure.java.io)
(def filename "/Users/sven/Documents/workspace/adventofcode/day5-input.txt")

(defn three-vowels? [string]
  (<= 3 (count (re-seq #"[aeiou]" string))))

(defn double-character? [string]
  (some? (re-find #"([a-z])\1" string)))

(defn valid-pairs? [string]
  (nil? (re-find #"ab|cd|pq|xy" string)))

(defn nice-string? [string]
  (and (three-vowels? string)
       (double-character? string)
       (valid-pairs? string)))

(defn compute-from-input [filename cb]
  (with-open [rdr (reader filename)]
    (cb (line-seq rdr))))

(defn count-nice-strings []
  (compute-from-input
   filename
   (fn [lines]
     (-> (filter nice-string? lines)
         count))))

(defn encaps-character? [string]
  (some? (re-find #"([a-z]).\1" string)))

(defn double-pair? [string]
  (some? (re-find #"([a-z]{2}).*\1" string)))

(defn nice-string2? [string]
  (and (encaps-character? string)
       (double-pair? string)))

(defn count-nice-strings2 []
  (compute-from-input
   filename
   (fn [lines]
     (-> (filter nice-string2? lines)
         count))))
     
    
;; (count-nice-strings)
;; --> 236

;; (count-nice-strings2)
;; --> 51
