(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    ((= n 2) 2)
    (else (f-iter 0 1 2 (- n 2)))))
(define (f-iter a b c count)
  (if (= count 0)
    c
    (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
