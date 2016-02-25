(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn magic-square [values]
  (let [sum (reduce + values)
        cell-avg (/ sum (count values))]
    [[3.5 1.0 4.5]
     [4.0 3.0 2.0]
     [1.5 5.0 2.5]]))

(defn magic-square-sums
  "Sums all of the relevant positions that need to be equal for a magic square."
  [[[a b c]
    [d e f]
    [g h i]]]
  (list (+ a b c)
     (+ d e f)
     (+ g h i)
     (+ a d g)
     (+ b e h)
     (+ c f i)
     (+ a e i)
     (+ c e g)))

(defn magic-square?
  "Determine if the proposed matrix is a magic square."
  [matrix]
  (let [sums (magic-square-sums matrix)]
    (every? #(= (first sums) %) sums)))
