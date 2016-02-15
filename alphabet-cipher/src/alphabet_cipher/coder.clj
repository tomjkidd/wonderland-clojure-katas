(ns alphabet-cipher.coder)

(def alphabet
  "A list of all the characters in the the alphabet, from a to z"
  (map #(-> %
            (char)) (range (int \a) (inc (int \z)))))

(defn char-index
  [ch]
  (-> ch
      (str)      
      (clojure.string/lower-case)
      (first)
      (int)
      (- 97)))

(defn rotate
  "Rotate a sequence by n. Negative numbers to rotate right, positive to rotate left."
  [n coll]
  (let [num-items (count coll)
        to-drop (if (< n 0)
                  (mod (+ num-items n) num-items)
                  (mod n num-items))
        [dropped remaining] (split-at to-drop coll)]
    (concat remaining dropped)))

(defn table
  "The alphabet based table for encode/decode"
  []
  (loop [acc {} remaining alphabet]
    (let [f (first remaining)
          val (if (nil? f) nil (rotate (char-index f) alphabet))
          r (rest remaining)]
      (if (nil? f)
        acc
        (recur (assoc acc f val) r)))))

(defn lookup
  [row-char col-char]
  (nth ((table) row-char) (char-index col-char)))

(defn match-length
  [n message]
  (take n (cycle message)))

(defn encode [keyword message]
  (let [to-take (max (count keyword) (count message))
        k (match-length to-take keyword)
        m (match-length to-take message)
        zipper (map vector k m)]
    (->> (map #(lookup (first %) (first (rest %))) zipper)
        (reduce str ""))))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

