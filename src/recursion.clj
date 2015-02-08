(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn my-last [coll]
  (cond (singleton? coll) (first coll)
        (empty? coll) nil
        :else (my-last (rest coll))))


(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max
               (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
      true
   :else
    (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))
   :else
     []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      '()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond (= k 1) n
        (= k 0) 1
        :else (* n (power n (dec k)))))

(defn fib [n]
  (if (or (= n 0) (= n 1))
    n
    (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (cond (<= how-many-times 0) []
        (= how-many-times 1) [what-to-repeat]
        :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    ['()]
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotate [a-seq]
  (seq (concat (rest a-seq) (list (first a-seq)))))

(defn rotations-w-num [a-seq n]
  (if (== n (count a-seq))
    (list a-seq)
    (cons a-seq (rotations-w-num (rotate a-seq) (inc n)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-w-num a-seq 1)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (get freqs (first a-seq))
      (my-frequencies-helper (conj freqs {(first a-seq) (+ 1 (get freqs (first a-seq)))}) (rest a-seq))
      (my-frequencies-helper (conj freqs {(first a-seq) 1}) (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (flatten (map (fn [x] (repeat (get a-map x) x)) (keys a-map))))


(concat (repeat 5 :a) (repeat 2 :b))


(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

