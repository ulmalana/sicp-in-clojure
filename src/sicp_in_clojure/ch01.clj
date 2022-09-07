(ns sicp-in-clojure.ch01
  (:gen-class))

486
;; => 486

(+ 137 349)
;; => 486

(- 1000 334)
;; => 666

(* 5 99)
;; => 495

(/ 10 5)
;; => 2

(+ 2.7 10)
;; => 12.7

(+ 21 35 12 7)
;; => 75

(* 25 4 12)
;; => 1200

(+ (* 3 5) (- 10 6))
;; => 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;; => 57

(def size 2)
;; => #'sicp-in-clojure.ch01/size

size
;; => 2

(* 5 size)
;; => 10

(def pi 3.14159)
;; => #'sicp-in-clojure.ch01/pi
(def radius 10)
;; => #'sicp-in-clojure.ch01/radius

(* pi (* radius radius))
;; => 314.159

(def circumference (* 2 pi radius))
;; => #'sicp-in-clojure.ch01/circumference

circumference
;; => 62.8318

(* (+ 2 (* 4 6))
   (+ 3 5 7))
;; => 390

(defn square
  [x]
  (* x x))
;; => #'sicp-in-clojure.ch01/square

(square 21)
;; => 441

(square (+ 2 5))
;; => 49

(square (square 3))
;; => 81

(defn sum-of-squares
  [x y]
  (+ (square x) (square y)))
;; => #'sicp-in-clojure.ch01/sum-of-squares

(sum-of-squares 3 4)
;; => 25

(defn f
  [a]
  (sum-of-squares (+ a 1) (* a 2)))
;; => #'sicp-in-clojure.ch01/f

(f 5)
;; => 136

(defn abs
  [x]
  (cond
    (> x 0) x
    (= x 0) 0
    (< x 0) (- x)))

(defn abs'
  [x]
  (cond
    (< x 0) (- x)
    :else x))

(defn abs''
  [x]
  (if (< x 0)
    (- x)
    x))

(defn geq
  "greater or equal >="
  [x y]
  (or (> x y) (= x y)))

(defn geq'
  "greater or equal >="
  [x y]
  (not (< x y)))

;;;;;; exercise ;;;;;;;;;;;;;

(+ 5 3 4)
;; => 12

(- 9 1)
;; => 8

(/ 6 2)
;; => 3

(+ (* 2 4) (- 4 6))
;; => 6

(def a 3)
;; => #'sicp-in-clojure.ch01/a

(def b (+ a 1))
;; => #'sicp-in-clojure.ch01/b

(+ a b (* a b))
;; => 19

(= a b)
;; => false

(if (and (> b a) (< b (* a b)))
  b
  a)
;; => 4

(cond
  (= a 4) 6
  (= b 4) (+ 6 7 a)
  :else 25)
;; => 16

(+ 2 (if (> b a) b a))
;; => 6

(* (cond
     (> a b) a
     (< a b) b
     :else -1)
   (+ a 1))
;; => 16

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;; => -37/150

(defn sum-of-squares-larger
  [x y z]
  (- (apply + (map square (list x y z)))
     (square (min x y z))))

(sum-of-squares-larger 8 5 6)
;; => 100

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn good-enough?
  [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn average
  [x y]
  (/ (+ x y) 2))

(defn improve
  [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt
  [x]
  (sqrt-iter 1.0 x))

(sqrt 9)
;; => 3.00009155413138

(sqrt (+ 100 37))
;; => 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
;; => 1.7739279023207892

(square (sqrt 1000))
;; => 1000.000369924366
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cube
  [x]
  (* x x x))

(defn cbrt-iter
  [guess x]
  (letfn [(good-enough? [guess x]
            (< (abs (- (cube guess) x)) 0.001))
          (improve [guess x]
            (/ (+ (/ x (square guess)) (* 2 guess)) 3))]
    (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x))))

(defn cbrt
  [x]
  (cbrt-iter 1.0 x))

(cbrt 64)
;; => 4.000017449510739
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn double'
  [x]
  (+ x x))

(defn factorial
  [n]
  (if (= n 1)
    1
    (* n (factorial (dec n)))))

(defn factorial'
  [n]
  (letfn [(iter [product counter]
            (if (> counter n)
              product
              (iter (* counter product) (inc counter))))]
    (iter 1N 1)))

(defn fib
  [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn fib-iter
  [a b cnt]
  (if (= cnt 0)
    b
    (fib-iter (+ a b) a (dec cnt))))

(defn fib'
  [n]
  (fib-iter 1 0 n))
