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
  (loop [acc [] remaining alphabet]
    (let [f (first remaining)
          val (if (nil? f) nil (rotate (char-index f) alphabet))
          r (rest remaining)]
      (if (nil? f)
        acc
        (recur (conj acc val) r)))))

(defn column
  "Get a column from the alphabet table"
  [n]
  (->> (table)
       (map #(nth %  n))))

(defn encode-letter
  [row-char col-char]
  (-> (table)
       (nth (char-index row-char))
       (nth (char-index col-char))))

(defn match-length
  [n message]
  (take n (cycle message)))

(defn decode-letter
  [key-char enc-char]
  (let [decoded-index (->> (column (char-index key-char))
                           (map-indexed (fn [idx itm] [idx itm]))
                           (filter #(= (first (rest %)) enc-char))
                           (first)
                           (first))
        decoded-char (nth (nth (table) 0) decoded-index)]
    decoded-char))

(defn encode [keyword message]
  (let [to-take (max (count keyword) (count message))
        k (match-length to-take keyword)
        m (match-length to-take message)
        zipper (map vector k m)]
    (->> (map #(encode-letter (first %) (first (rest %))) zipper)
         (reduce str ""))))

(defn decode [keyword encoded]
  (let [to-take (max (count keyword) (count encoded))
        k (match-length to-take keyword)
        e (match-length to-take encoded)
        zipper (map vector k e)]
    (->> (map #(decode-letter (first %) (first (rest %))) zipper)
         (reduce str ""))))

(defn decipher [cipher message]
  "decypherme")

