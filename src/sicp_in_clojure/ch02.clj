(ns sicp-in-clojure.ch02
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
