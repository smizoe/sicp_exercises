(define true #t)
(define false #f)

(use srfi-27)

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

(define nil '())

