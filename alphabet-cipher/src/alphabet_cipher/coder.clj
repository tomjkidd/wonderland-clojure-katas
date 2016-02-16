(ns alphabet-cipher.coder)

(def alphabet
  "A list of all the characters in the the alphabet, from a to z"
  (map #(-> %
            (char)) (range (int \a) (inc (int \z)))))

(defn char-index
  "Get the 0 based index of a char in the alphabet"
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
  "The alphabet based table of permutations for encode/decode"
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
  "Encode a single letter"
  [row-char col-char]
  (-> (table)
       (nth (char-index row-char))
       (nth (char-index col-char))))

(defn match-length
  "Cycle over a message to create a message of a certain length"
  [n message]
  (take n (cycle message)))

(defn decode-letter
  "Decode a single letter"
  [key-char enc-char]
  (let [decoded-index (->> (column (char-index key-char))
                           (map-indexed (fn [idx itm] [idx itm]))
                           (filter #(= (first (rest %)) enc-char))
                           (first)
                           (first))
        decoded-char (nth (nth (table) 0) decoded-index)]
    decoded-char))

(defn match-length-then-apply
  "Make sure the length of the keyword and message are the same, and then perform an operation mapped over each letter."
  [keyword other-word map-letter-fn]
  (let [to-take (max (count keyword) (count other-word))
        k (match-length to-take keyword)
        o (match-length to-take other-word)
        zipper (map vector k o)]
    (->> (map #(map-letter-fn (first %) (first (rest %))) zipper)
         (reduce str ""))))

(defn encode [keyword message]
  "Encode a message using a given keyword"
  (match-length-then-apply keyword message encode-letter))

(defn decode [keyword encoded]
  "Decode a message using a given keyword"
  (match-length-then-apply keyword encoded decode-letter))

(defn decipher [cipher message]
  "decypherme")

