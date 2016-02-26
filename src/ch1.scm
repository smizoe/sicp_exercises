(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

(define (abs x)
  (cond
    ((x < 0) - x)
    (else x)))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

;;(define (sqrt-iter guess x)
;;  (if (good-enough? guess x)
;;    guess
;;    (sqrt-iter (improve guess x) x)))
;;
;;(define (improve guess x)
;;  (average guess (/ x guess)))
;;
(define (average x y)
  (/ (+ x y) 2))

;;(define (good-enough? guess x)
;;  (< (abs (- (square guess) x)) 0.001))
;;
;;(define (sqrt x)
;;  (sqrt-iter 1.0 x))

;;(define (sqrt x)
;;  (define (good-enough? guess x)
;;    (< (abs (- (square guess) x)) 0.001))
;;  (define (improve guess x) (average guess (/ x guess)))
;;  (define (sqrt-iter guess x)
;;    (if (good-enough? guess x)
;;      guess
;;      (sqrt-iter (improve guess x) x)))
;;  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;(define (factorial n)
;;  (if (= n 1)
;;    1
;;    (* n (factorial (- n 1)))))
;;
;;(define (factorial n) (fact-iter 1 1 n))
;;
;;(define (fact-iter product counter max-count)
;;  (if (> counter max-count)
;;    product
;;    (fact-iter (* counter product)
;;               (+ counter 1)
;;               max-count)))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1)
  )

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1))
             (fib (- n 2))))))
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount
                 (- kinds-of-coins 1))
             (cc (- amount (first-denomination kinds-of-coins))
                 kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond
    ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))
