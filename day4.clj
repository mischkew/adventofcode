;; md5 helper
(declare md5)

(defn has-leading-zeros [string amount]
  (.startsWith string (apply str (repeat amount "0"))))

(defn brute-force [secret amount]
  (loop [count 0]
    (if (has-leading-zeros (md5 (str secret count)) amount)
      count
      (recur (inc count)))))

;; (brute-force "bgvyzdsv" 5)
;; --> 254575

;; (brute-force "bgvyzdsv" 6)
;; --> 1038736

(defn md5 [string]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes string)))))))


