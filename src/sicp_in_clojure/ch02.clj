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
