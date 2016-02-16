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

(defn decipher-letter
  "Deciphers a single letter"
  [msg-char enc-char]
  (let [keyword-index (->> (nth (table) (char-index msg-char))
                           (map-indexed (fn [idx itm] [idx itm]))
                           (filter #(= (first (rest %)) enc-char))
                           (first)
                           (first))
        keyword-char (nth (nth (table) 0) keyword-index)]
    keyword-char))

(defn determine-cipher
  "Determines the cipher from a value that might repeat it multiple times."
  [possibly-repeating-cipher]
  (let [len (count possibly-repeating-cipher)]
    (loop [cur-idx 1]
      (let [cipher-candidate (subs possibly-repeating-cipher 0 cur-idx)
            expanded-cipher (apply str (take len (cycle cipher-candidate)))]
        (if (or (= cur-idx len)
                (= possibly-repeating-cipher expanded-cipher))
          cipher-candidate
          (recur (inc cur-idx)))))))

(defn match-length-then-apply
  "Make sure the length of the keyword and message are the same, and then perform an operation mapped over each letter."
  [word1 word2 map-letter-fn]
  (let [to-take (max (count word1) (count word2))
        w1 (match-length to-take word1)
        w2 (match-length to-take word2)
        zipper (map vector w1 w2)]
    (->> (map #(map-letter-fn (first %) (first (rest %))) zipper)
         (reduce str ""))))

(defn encode [keyword message]
  "Encode a message using a given keyword"
  (match-length-then-apply keyword message encode-letter))

(defn decode [keyword encoded]
  "Decode a message using a given keyword"
  (match-length-then-apply keyword encoded decode-letter))

(defn decipher [cipher message]
  "Determines the keyword given an encoded message and the decoded message"
  (-> (match-length-then-apply message cipher decipher-letter)
      (determine-cipher)))

