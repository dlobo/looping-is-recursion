(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [lst a-seq]
                 (if (empty? a-seq)
                   lst
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         l-seq a-seq]
    (cond
     (empty? l-seq) nil
     (pred (first l-seq)) index
     :else (recur (inc index) (rest l-seq)))))

(defn avg [a-seq]
  (loop [total 0
         numE 0
         l-seq a-seq]
    (if (empty? l-seq)
      (if (= numE 0) 0 (/ total numE))
      (recur (+ total (first l-seq)) (inc numE) (rest l-seq)))))

(defn parity [a-seq]
  (let [freqs (frequencies a-seq)]
    (keys (filter (fn [x] (= (mod (second x) 2) 1)) freqs))))

(defn fast-fibo [n]
    (loop [fib1 0
           fib2 1
           fibo 1]
    (cond
     (<= n 1) n
     (== fibo n) fib2
     :else (recur fib2 (+ fib1 fib2 ) (inc fibo)))))


(defn cut-at-repetition [a-seq]
  (loop [memory #{}
         curr '()
         l-seq a-seq]
    (if (or (contains? memory (first l-seq)) (empty? l-seq))
      (reverse curr)
      (recur (conj memory (first l-seq)) (conj curr (first l-seq)) (rest l-seq)))))
