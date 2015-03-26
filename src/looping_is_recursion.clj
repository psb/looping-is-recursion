(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [h-base h-exp]
                 (cond
                   (= 1 h-exp) h-base
                   (zero? h-exp) 1
                   :else (recur (* h-base base) (dec h-exp))))]
    (helper base exp)))

(defn last-element [a-seq]
  (if (nil? (next a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (or (empty? seq1) (empty? seq2))
      false
    (= (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         the-seq a-seq]
    (cond
      (empty? the-seq) nil
      (pred (first the-seq)) idx
      :else (recur (inc idx) (rest the-seq)))))

(defn avg [a-seq]
  (loop [idx 0
         total 0
         the-seq a-seq]
    (if (empty? the-seq)
      (/ total idx)
      (recur (inc idx)
             (+ total (first the-seq))
             (rest the-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [the-set #{}
           the-seq a-seq]
      (if (empty? the-seq)
        the-set
        (recur
          (toggle the-set (first the-seq))
          (rest the-seq))))))

(defn fast-fibo [n]
  (loop [acc 0
         last-num 1
         itr 0]
    (if (= itr n)
      acc
      (recur (+ acc last-num)
             acc
             (inc itr)))))

(defn cut-at-repetition [a-seq]
  (loop [the-vec []
         the-seq a-seq]
    (if (or (= (first the-vec)
               (first the-seq))
            (empty? the-seq))
      the-vec
      (recur (conj the-vec (first the-seq))
             (rest the-seq)))))

