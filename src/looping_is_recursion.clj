(ns looping-is-recursion)

(defn power-reg [n k]
  (if (zero? k)
    1
    (* n (power-reg n (dec k)))))

(defn power-helper [base exp acc]
  (if (zero? exp)
    base
    (power-helper (* base acc) (dec exp) acc)))

(defn power [x n]
  (letfn [(helper [acc n]
            (if (zero? n)
              acc
              (recur (* x acc) (dec n))))]
    (helper 1 n)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [pred pred a-seq a-seq index 0]
    (cond 
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else
      (recur pred (rest a-seq) (inc index)))))
      

(defn avg [a-seq]
  (loop [a-seq a-seq sum 0 number 0]
    (cond
      (and (empty? a-seq) (zero? number)) 0 
      (empty? a-seq) (/ sum number)
      :else
      (recur (rest a-seq) (+ sum (first a-seq)) (inc number)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-seq a-seq result #{}]
    (cond
      (empty? a-seq) result
      :else
      (recur (rest a-seq) (toggle result (first a-seq))))))

(defn fibonacci [n]
  (cond
    (zero? n)
    0
    (== 1 n)
    1
    :else
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(defn fast-fibo [n]
  (loop [fx 0 fx-1 1 fx-2 0 i 0]
    (cond
      (== i n)
      fx
      :else
      (recur (+ fx fx-1) fx fx-1 (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq check #{} result []]
    (cond
      (or (empty? a-seq) (contains? check (first a-seq)))
      result
      :else
      (recur (rest a-seq) (conj check (first a-seq)) (conj result (first a-seq))))))

