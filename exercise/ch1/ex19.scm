;; note that T_{p, q} in a matrix form is
;; T_{p, q} = ( p + q, q)
;;            ( q , p   )
;; thus
;; T_{p, q} ** 2 = ( (p+q) ** 2 + q ** 2, (p+q)q + pq)
;;                 ( (p+q)q + pq, 2pq)
;;               = ( p**2 + 2 q**2 + 2pq,  2pq + q ** 2)
;;                 ( 2pq + q**2, q ** 2 + p ** 2 )
;; thus p' = p ** 2 + q ** 2, q' = 2pq + q**2

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond
    ((= count 0) b)
    ((even? count)
     (fib-iter a
               b
               (+ (square p) (square q))
               (+ (* 2 p q) (square q))
               (/ count 2)))
     (else (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))
