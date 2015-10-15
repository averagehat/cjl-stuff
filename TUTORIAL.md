











;; x & xs is equivalent to [x:xs]; giving xs as a result
;; note we need the inner [] for the list to be caught
;; [[fst snd]] will also work

;(map + [1 2 3] [0 1 2])
;
;
;(defn third [s] (nth s 2))
;
;(third [1 2 3])
;
;
;((fn [[_ _ x]] x) [1 2 3 5])
;
;
;((fn [[_ _ x]] x) '(1 2 3 ))
;((fn [[_ _ x]] x) (map inc '(1 2 3 ))) ;even works with lazy sequences
;;; this leads directly into pattern-matching
;;; fibonacci with pattern matching
;;; fizzbuzz with pattern matching
;;; examples from leetcode?
;;map takes a function followed by an arbitrary number of arguments. i.e.
;
;(map + [1 2 3] [0 1 2])
;
;(require '[clojure.core.match :refer [match]])
;

;lisp has minimal syntax. In clojure, the first argument is always a function; sometimes this function is a "macro", a special
;kind of function which manipulates the code which follows it. For example. `defn` is a macro:
;
;(defn third [s] (nth s 2 "not-found"))
;(nth (map inc '(1 2 3 4 5)) 2)
;
;
;`nth` is a function which takes a seqable and 
;
;(t/cf nth)
;(t/All [x y] (t/IFn [(t/U (t/I Sequential (Seqable x)) (Indexed x)) t/AnyInteger -> x] [(t/U nil (t/I Sequential (Seqable x)) (Indexed x)) t/AnyInteger y -> (t/U y x)] [(t/U nil (t/I Sequential (Seqable x)) (Indexed x)) t/AnyInteger -> (t/U x nil)]))
;
;(use '[defun :only [defun]])
;(use '[defun :only [fun]])
;(use '[defun :only [defun]])
;(use ['defun
;(defn five? [x] (= 5 x))
;(defun five? 
;  ([5] true)
;  ([_] false))
;
;(defun fib
;  ([0] 0)
;  ([1] 1)
;  ([n] (+ (fib (dec n)) (fib (- n 2 )))))
;
;(filter identity (map fib (range 12)))
;
;(defun fib
;  ([] 0)
;  ([
;(defun fib-seq 
;  ([] 0)
;  ([_] 2)
;
;   [nil _ _] 1
;  
;
;
;(fib 10)
;      (let [fives (map #(* 5 %) (range))
;           threes (map #(* 3 %) (range))]
;(for [i threes j fives]
;  :while (
;(range 0 10 3)
;(let [sub-range (fn [x] (take-while #(> 1000 %) (map #(* x %) (range))))
;      threes (sub-range 3)
;      fives (sub-range 5)]
;  (reduce + (concat threes fives)))
;
;
;(require '[clojure.core.match :refer [match]])
;(let [n 10]
;(for [ [i j] [(range 0 n 3) (range 0 n 5)]]
;  (apply + (distinct [i j] ))))
;
;  (let [x 1 y 1]
;(list
;;can't pattern match for identical values :(
; (match [x y] 
;        [x x] x 
;        [x y] (+ x y)))
;(let [n 1000
;      threes (range 0 n 3)
;      fives (range 0 n 5)]
;  [threes fives])
;  (reduce + (concat threes fives)))
;
;;; the next result is equal to the current value
;;; concatenated with the sum of the last two values
;          ;;
;(defn fib-seq [x y & xs]
;  (cons (+ x y) xs))
;
;;((fn [[x y & _ :as all ]]
;((fn [[x y :as all ]]
;  (cons (+ x y) all)) [2 1 0 ])
;
;((fun [[x y :as all ]]
;  (cons (+ x y) all)) [2 1 0 ])
;
;((fn [[x y :as all ] n]
;   (when (> n (count all ))
;     (cons (+ x y) all))) [2 1 ] 4)
;
;((fn [[x y :as all ] n]
;   (when (> n (count all ))
;     (recur (cons (+ x y) all) n))) [2 1 ] 4)
;
;((fn [[x y :as all ] n]
;   (let [res (cons (+ x y) all)]
;   (if (> n (count res ))
;     (recur res n)
;     res))) [2 1 ] 10)
;;; can only recur from tail position
;((fn [[x y :as all ] n]
;   (let [res (cons (+ x y) all)]
;   (when (> n (count res ))
;     (recur res n))
;     res)) [2 1 ] 10)
;;; this is bad it counts every tmie 
;          ;;
;          ;;
;          ;;
;          ;;
;; iterate returns a lazy sequence of f(f(f(...f(x))))
;(t/cf
;  (t/ann fib-seq [t/AnyInteger -> (t/Seqable t/AnyInteger)]))
;
;(t/cf
;(defn fib-seq [n]
;  (->
;    (iterate 
;      (fn [[x y :as all ] ]
;        (cons (+ x y) all))
;      [2 1])
;    nth n)))
;
;(fib-seq 10)          
;
;(defn fib-seq [n]
;  (->
;    (iterate 
;      (fn [[x y :as all ] ]
;        (cons (+ x y) all))
;      [1 0])
;    (nth n)))
;
;(second 
;(iterate 
;(fn [[x y :as all ] ]
;    (cons (+ x y) all)) [3 2 1])))
;;If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;
;;Find the sum of all the multiples of 3 or 5 below 1000.
;; generate all multiples of 3 or 5.
;; using filter or range + distinct filter
;; sum them 
;; note that nums is lazy. 
;(let [n 10
;      nums (concat (range 0 n 3) (range 0 n 5)) ]
;       (reduce + (distinct nums)))
;(let [n 1000
;      threes (range 0 n 3)
;      fives (range 0 n 5)]
;  (->> (concat threes fives)
;        distinct 
;       (reduce + )))
;
;;; show it without distinct 
;(let [n 10]
;  (->> (concat (range 0 n 3) (range 0 n 5))
;        distinct 
;       (reduce + )))
;
;(let [n 1000
;      mul? (fn [x] #(= (mod % x) 0))]
;  (->> (range 0 n)
;       (filter (some-fn (mul? 5) (mul? 3)))
;       (reduce +)))
;
;  (reduce + (concat threes fives)))
;(is (= (five? 5) true)
;    (= (five? 3) false))
;
;(mod 
; ; ; ;            (require '[clojure.core.typed :as t]) ;
