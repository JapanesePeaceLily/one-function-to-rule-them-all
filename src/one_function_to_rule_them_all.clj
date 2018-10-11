(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat () a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (do (def ready "")
        (reduce (fn [ready word] (str ready " " word)) a-seq))))

(defn my-interpose [x a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [insert (fn [seq e]
                   (conj seq x e))]
      (reduce insert [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [counter (fn [num e]
                  (inc num))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [minmax e]
                  (if (or (nil? (nth minmax 0 nil)) (<= e (minmax 0)))
                    (assoc minmax 0 e)
                    (if (or (nil? (nth minmax 1 nil)) (>= e (minmax 1)))
                      (assoc minmax 1 e)
                      minmax)))]
    (reduce min-max [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (conj sorted-seq n)
    (loop [seq1 sorted-seq
           seq2 []
           e n]
      (cond
        (empty? seq1) (conj seq2 e)
        (< e (first seq1)) (concat seq2 (vector e) seq1)
        :else (recur
               (rest seq1)
               (conj seq2 (first seq1))
               e)))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [odds (fn [odd-set e]
               (if (contains? odd-set e)
                 (disj odd-set e)
                 (conj odd-set e)))]
    (reduce odds #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] (count x))
  ([x y] (conj x y))
  ([x y & more]
   (reduce count-params (count-params [] x) more)))

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
