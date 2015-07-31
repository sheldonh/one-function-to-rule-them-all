(ns one-function-to-rule-them-all)

(defn concat-elements
  [a-seq]
  (reduce concat () a-seq))

(defn str-cat
  [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose
  [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count
  [a-seq]
  (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse
  [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element
  [a-seq]
  (let [initial (repeat 2 (first a-seq))
        min-max (fn [[x-min x-max] n] [(min n x-min) (max n x-max)])]
    (reduce min-max initial (rest a-seq))))

(defn insert
  [sorted-seq n]
  (if (empty? sorted-seq)
    (cons n sorted-seq)
    (let [[fst snd] (split-with #(> n %) sorted-seq)]
      (concat fst (cons n snd)))))

(defn insertion-sort
  [a-seq]
  (reduce insert '() a-seq))

(defn parity
  [a-seq]
  (let [toggle (fn [a-set e] (if (contains? a-set e) (disj a-set e) (conj a-set e)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x]   (- x))
  ([x y] (- x y)))

(defn count-params
  [& args]
  (count args))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & args] (reduce * x args)))

(defn pred-and [& ps]
  (fn [e] (every? #(% e) ps)))

(defn my-map [f & seqs]
  (lazy-seq
    (loop [acc []
           xs   seqs]
      (if (some empty? xs)
        acc
        (let [firsts (reduce #(conj %1 (first %2)) [] xs)
              rests  (reduce #(conj %1 (rest %2)) [] xs)]
          (recur (conj acc (apply f firsts)) rests))))))
