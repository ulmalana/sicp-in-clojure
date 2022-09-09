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

(defn first-denomination
  [kinds-of-coins]
  (cond
    (= kinds-of-coins 1) 1
    (= kinds-of-coins 2) 5
    (= kinds-of-coins 3) 10
    (= kinds-of-coins 4) 25
    (= kinds-of-coins 5) 50))

(defn cc
  [amount kinds-of-coins]
  (cond
    (= amount 0) 1
    (or (< amount 0) (= kinds-of-coins 0)) 0
    :else (+ (cc amount
                 (- kinds-of-coins 1))
             (cc (- amount
                    (first-denomination kinds-of-coins))
                 kinds-of-coins))))
(defn count-change
  [amount]
  (cc amount 5))

(count-change 100)
;; => 292

(defn p
  [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine
  [angle]
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)
;; => -0.39980345741334

(sine 0.785) ;; sin 45
;; => 0.707531343720907

(defn expt
  [b n]
  (if (= n 0)
    1
    (* b (expt b (dec n)))))

(defn expt-iter
  [b counter product]
  (if (= counter 0)
    product
    (expt-iter b
               (dec counter)
               (* b product))))

(defn expt'
  [b n]
  (expt-iter b n 1))

(defn fast-expt
  [b n]
  (cond
    (= n 0) 1
    (even? n) (square (fast-expt b (/ n 2)))
    :else (* b (fast-expt b (dec n)))))

(defn gcd
  [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

(gcd 20 16)
;; => 4

(defn divides?
  [a b]
  (= (rem b a) 0))

(defn find-divisor
  [n test-divisor]
  (cond
    (> (square test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor
  [n]
  (find-divisor n 2))

(defn prime?
  [n]
  (= n (smallest-divisor n)))

(defn expmod
  [base exp m]
  (cond
    (= exp 0) 1
    (even? exp) (rem (square (expmod base (/ exp 2) m)) m)
    :else (rem (* base (expmod base (- exp 1) m)) m)))

(defn fermat-test
  [n]
  (letfn [(try-it [a]
            (= (expmod a n n) a))]
    (try-it (+ 1 (rand-int (dec n))))))

(defn fast-prime?
  [n times]
  (cond
    (= times 0) true
    (fermat-test n) (fast-prime? n (- times 1))
    :else false))

(defn sum-integers
  [a b]
  (if (> a b)
    0
    (+ a (sum-integers (inc a) b))))

(defn sum-cubes
  [a b]
  (if (> a b)
    0
    (+ (cube a)
       (sum-cubes (inc a) b))))

(defn pi-sum
  [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

(defn sum
  [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn sum-cubes'
  [a b]
  (sum cube a inc b))

(sum-cubes' 1 10)
;; => 3025

(defn sum-integers'
  [a b]
  (sum identity a inc b))

(sum-integers' 1 10)
;; => 55

(defn pi-sum'
  [a b]
  (letfn [(pi-term [x]
            (/ 1.0 (* x (+ x 2))))
          (pi-next [x]
            (+ x 4))]
    (sum pi-term a pi-next b)))

(* 8 (pi-sum 1 1000))
;; => 3.139592655589783

(* 8 (pi-sum 1 10000))
;; => 3.141392653591793

(defn integral
  [f a b dx]
  (letfn [(add-dx [x]
            (+ x dx))]
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)))

(integral cube 0 1 0.01)
;; => 0.24998750000000042

(integral cube 0 1 0.001)
;; => 0.249999875000001

(defn pi-sum''
  [a b]
  (sum (fn [x] (/ 1.0 (* x (+ x 2))))
       a
       (fn [x] (+ x 4))
       b))

(defn integral'
  [f a b dx]
  (* (sum f
          (+ a (/ dx 2.0))
          (fn [x] (+ x dx))
          b)
     dx))

(def plus4
  (fn [x]
    (+ x 4)))

((fn [x y z] (+ x y (square z))) 1 2 3)
;; => 12

(defn f
  [x y]
  (let [a (+ 1 (* x y))
        b (- 1 y)]
    (+ (* x (square a))
       (* y b)
       (* a b))))

(defn close-enough?
  [x y]
  (< (abs (- x y)) 0.001))

(defn search
  [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond
          (pos? test-value) (search f neg-point midpoint)
          (neg? test-value) (search f midpoint pos-point)
          :else midpoint)))))

(defn half-interval-method
  [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond
      (and (neg? a-value) (pos? b-value)) (search f a b)
      (and (neg? b-value) (pos? a-value)) (search f b a)
      :else (throw (Exception. (str "Values are not of opposite sign: " a b))))))

(defn sin [n]
  (Math/sin n))

(defn cos [n]
  (Math/cos n))

(half-interval-method sin 2.0 4.0)
;; => 3.14111328125 

(half-interval-method (fn [x]
                        (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)
;; => 1.89306640625

(def tolerance 0.00001)

(defn fixed-point
  [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (abs (- v1 v2)) tolerance))
          (try-it [guess]
               (let [next (f guess)]
                 (if (close-enough? guess next)
                   next
                   (try-it next))))]
    (try-it first-guess)))

(fixed-point cos 1.0)
;; => 0.7390822985224024

(fixed-point (fn [y] (+ (sin y) (cos y))) 1.0)
;; => 1.2587315962971173

(defn average-damp
  [f]
  (fn [x] (average x (f x))))

((average-damp square) 10)
;; => 55

(defn sqrt'
  [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(defn cube-root
  [x]
  (fixed-point (average-damp (fn [y] (/ x (square y)))) 1.0))

(def dx 0.00001)

(defn deriv
  [g]
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))

((deriv cube) 5) 
;; => 75.00014999664018

(defn newton-transform
  [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method
  [g guess]
  (fixed-point (newton-transform g) guess))

(defn sqrt''
  [x]
  (newtons-method
   (fn [y] (- (square y) x)) 1.0))

(defn fixed-point-of-transform
  [g transform guess]
  (fixed-point (transform g) guess))
