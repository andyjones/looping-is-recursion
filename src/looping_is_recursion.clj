(ns looping-is-recursion)

;; run the tests
;; (do (use 'midje.repl) (use 'looping-is-recursion :reload-all) (load-facts 'looping-is-recursion 'looping-is-recursion-test) (check-facts :all))

(defn power [base exp]
  (if (zero? exp)
    1
    (let [helper (fn [acc n k]
                   (if (zero? k)
                     acc
                     (recur (* acc n) n (dec k))))]
      (helper base base (dec exp)))))

(defn power' [base exp]
  (if (zero? exp)
    1
    (loop [acc base
           n   base
           k   (dec exp)]
      (if (zero? k)
        acc
        (recur (* acc n) n (dec k))))))

(defn last-element [a-seq]
  (let [tail (rest a-seq)]
    (if (empty? tail)
      (first a-seq)
      (recur tail))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2))  false
    (not= (first seq1) (first seq2))  false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) idx
      :else (recur (inc idx) (rest a-seq)))))

(comment
  ; find-first-index using lazy sequences instead of recursion
  (defn filter-with-index [pred a-seq]
    (let [indices (iterate inc 0) ; build a lazy list of indices
          ; create a new seq containing [[elem1, 1], [elem2, 2] ... [elemN, N]]
          seq-with-idx (map vector a-seq indices)]
      (filter #(pred (first %)) seq-with-idx)))

  (defn find-first-index' [pred a-seq]
    (second (first (filter-with-index pred a-seq)))))

(comment
  ; find-first-index using clojure's built in keep-indexed instead of recursion
  (defn find-first-index'' [pred a-seq]
    (first (keep-indexed #(when (pred %2) %1) a-seq))))

(defn avg [a-seq]
  (loop [sum   0
         cnt   0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum cnt)
      (recur (+ sum (first a-seq)) (inc cnt) (rest a-seq)))))

(defn- toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [odds  #{}
         a-seq a-seq]
    (if (empty? a-seq)
      odds
      (recur (toggle odds (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (if (<= n 1)
    n
    (loop [fibn   1
           fibn-1 0
           cnt    2]
      (if (= cnt n)
        (+ fibn fibn-1)
        (recur (+ fibn fibn-1) fibn (inc cnt))))))

;; take a sequence and return elements from the sequence
;; up to the first repetition
(defn cut-at-repetition [a-seq]
  (loop [remaining a-seq
         acc-seq  []
         seen     #{}]
    (if (or (empty? remaining) (contains? seen (first remaining)))
      acc-seq
      (recur (rest remaining)
             (conj acc-seq (first remaining))
             (conj seen (first remaining))))))

(comment
  ;; build a sequence until we see the first element again
  ;; feels like cheating because it doesn't stop after any repetition
  (defn cut-at-repetition [a-seq]
    (loop [remaining a-seq
           new-seq   []]
      (cond
        (empty? remaining) new-seq
        (= (first remaining) (first new-seq)) new-seq
        :else (recur (rest remaining) (conj new-seq (first remaining)))))))

