(ns sicp-in-clojure.ch02
  ;;(:refer-clojure)
  (:require [sicp-in-clojure.ch01 :as ch01])
  (:gen-class))

(defn make-rat
  [x y]
  (list x y))

(defn numer
  [x]
  (nth x 0))

(defn denom
  [y]
  (nth y 1))

(defn add-rat
  [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat
  [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat
  [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat
  [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat?
  [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn print-rat
  [x]
  (println (str (numer x) "/" (denom x))))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))

(print-rat one-half)
;; => 1/2

(print-rat (add-rat one-half one-third))
;; => 5/6

(print-rat (mul-rat one-half one-third))
;; => 1/6

(print-rat (add-rat one-third one-third))
;; => 6/9

(defn make-rat
  [n d]
  (let [g (ch01/gcd n d)]
    (list (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))
;; => 2/3

(defn make-segment
  [p1 p2]
  (list p1 p2))

(defn start-segment
  [segment]
  (nth segment 0))

(defn end-segment
  [segment]
  (nth segment 1))

(defn make-point
  [x y]
  (list x y))

(defn x-point
  [point]
  (nth point 0))

(defn y-point
  [point]
  (nth point 1))

(defn print-point
  [point]
  (println (str "("
                (x-point point)
                ","
                (y-point point)
                ")")))

(defn make-interval
  [x y]
  (list x y))

(defn lower-bound
  [x]
  (apply min x))

(defn upper-bound
  [x]
  (apply max x))
(defn add-interval
  [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval
  [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval
  [x y]
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(def one-through-four (list 1 2 3 4))

(defn list-ref
  [items n]
  (if (= n 0)
    (first items)
    (list-ref (rest items) (- n 1))))

(def squares (list 1 4 9 16 25))

(list-ref squares 3)
;; => 16

(defn length
  [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))

(def odds (list 1 3 5 7))

(length odds)
;; => 4

(defn length'
  [items]
  (letfn [(length-iter [a count]
            (if (empty? a)
              count
              (length-iter (rest a) (+ 1 count))))]
    (length-iter items 0)))

(length' squares)
;; => 5

(defn append
  [list1 list2]
  (if (empty? list1)
    list2
    (cons (first list1) (append (rest list1) list2))))

(append squares odds)
;; => (1 4 9 16 25 1 3 5 7)


(append odds squares)
;; => (1 3 5 7 1 4 9 16 25)

(defn last-pair
  [items]
  (if (empty? (rest items))
    (first items)
    (last-pair (rest items))))

(last-pair (list 23 72 149 34))
;; => 34

(defn reverse'
  [items]
  (if (empty? items)
    items
    (into '() items)))

(reverse' squares)
;; => (25 16 9 4 1)

(defn scale-list
  [items factor]
  (if (empty? items)
    nil
    (cons (* (first items) factor)
          (scale-list (rest items) factor))))

(scale-list (list 1 2 3 4 5) 10)
;; => (10 20 30 40 50)

(defn map'
  [proc items]
  (if (empty? items)
    nil
    (cons (proc (first items))
          (map' proc (rest items)))))

(map' ch01/abs (list -10 2.5 -11.6 17))
;; => (10 2.5 11.6 17) 

(map' (fn [x] (* x x)) (list 1 2 3 4))
;; => (1 4 9 16)

(defn scale-list'
  [items factor]
  (map' (fn [x] (* x factor)) items))

(def x (conj (list 3 4) (list 1 2)))

;;;;;;;;;;;;;;;;;;;;;;;; DONT USE THIS ;;;;;;;;;;;;;;;;;;;
(defn pair?
  [x]
  (if (and (not (int? x)) (= 2 (length x)) (list? x))
    true
    false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-leaves
  [x]
  (cond
    (nil? x) 0
    (not (pair? x)) 1
    :else (+ (count-leaves (first x))
             (count-leaves (rest x)))))

;; modified version
(defn count-leaves'
  [x]
  (if (seq? x)
    (apply + (map count-leaves' x))
    1))

(length x)
;; => 3

(count-leaves' x)
;; => 4

(length (list x x))
;; => 2

(count-leaves' (list x x))
;; => 8

(defn scale-tree
  [tree factor]
  (cond
    (not (seq? tree)) (* tree factor)
    (nil? tree) 1
    :else (cons (scale-tree (first tree) factor)
                (scale-tree (rest tree) factor))))

(defn filter'
  [predicate coll]
  (cond
    (empty? coll) nil
    (predicate (first coll)) (cons (first coll)
                                 (filter' predicate (rest coll)))
    :else (filter' predicate (rest coll))))
;; => #'sicp-in-clojure.ch02/filter'

(filter' odd? (list 1 2 3 4 5))
;; => (1 3 5)

(defn accumulate
  [op initial coll]
  (if (empty? coll)
    initial
    (op (first coll)
        (accumulate op initial (rest coll)))))

(accumulate + 0 (list 1 2 3 4 5))
;; => 15

(accumulate * 1 (list 1 2 3 4 5))
;; => 120

(accumulate cons nil (list 1 2 3 4 5))
;; => (1 2 3 4 5)

(defn enumerate-interval
  [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)
;; => (2 3 4 5 6 7)

(defn list-fib-squares
  [n]
  (accumulate
   cons
   nil
   (map ch01/square (map ch01/fib (enumerate-interval 0 n)))))

(list-fib-squares 10)
;; => (0 1 1 4 9 25 64 169 441 1156 3025)

(defn product-of-squares-of-odd-elements
  [coll]
  (accumulate * 1 (map ch01/square (filter' odd? coll))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))
;; => 225

(defn flatmap
  [proc coll]
  (accumulate append nil (map proc coll)))

(defn remove'
  [item coll]
  (filter' (fn [x] (not (= x item)))
           coll))

(defn permutations
  [s]
  (if (empty? s)
    (list nil)
    (flatmap (fn [x]
               (map (fn [p] (cons x p))
                    (permutations (remove' x s))))
             s)))

(permutations '(1 2 3))
;; => ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

(defn memq
  [item x]
  (cond
    (empty? x) false
    (= item (first x)) x
    :else (memq item (rest x))))

(defn variable?
  [x]
  (symbol? x))

(defn same-variable?
  [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum
  [a1 a2]
  (list '+ a1 a2))

(defn make-product
  [m1 m2]
  (list '* m1 m2))

(defn sum?
  [x]
  (and (pair? x) (= (first x) '+)))

(defn addend [s] (first (rest s)))

(defn augend [s] (first (rest (rest s))))

(defn product? [x] (and (pair? x) (= (first x) '*)))

(defn multiplier [p] (first (rest p)))

(defn multiplicand [p] (first (rest (rest p))))

(defn deriv
  [exp var]
  (cond
    (number? exp) 0
    (variable? exp) (if (same-variable? exp var) 1 0)
    (sum? exp) (make-sum (deriv (addend exp) var)
                         (deriv (augend exp) var))
    (product? exp) (make-sum
                    (make-product (multiplier exp)
                                  (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                  (multiplicand exp)))
    :else (throw (Exception. (str "unknown expression type: DERIV" exp)) )))


(defn element-of-set?
  [x coll]
  (cond
    (empty? coll) false
    (= x (first coll)) true
    :else (element-of-set? x (rest coll))))

(element-of-set? 1 '(3 4 1 2))
;; => true

(defn adjoin-set
  [x coll]
  (if (element-of-set? x coll)
    coll
    (cons x coll)))

(defn intersection-set
  [set1 set2]
  (cond
    (or (empty? set1) (empty? set2)) '()
    (element-of-set? (first set1) set2) (cons (first set1) (intersection-set (rest set1) set2))
    :else (intersection-set (rest set1) set2)))

(defn element-of-ord-set?
  [x coll]
  (cond
    (empty? coll) false
    (= x (first coll)) true
    (< x (first coll)) false
    :else (element-of-ord-set? x (rest coll))))

(defn intersection-ord-set
  [set1 set2]
  (if (or (empty? set1) (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond
        (= x1 x2) (cons x1 (intersection-ord-set (rest set1) (rest set2)))
        (< x1 x2) (intersection-ord-set (rest set1) set2)
        (< x2 x1) (intersection-ord-set set1 (rest set2))))))

