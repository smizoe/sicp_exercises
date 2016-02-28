(define (halve n) (/ n 2))
(define (double n) (* n 2))

(define (mul a b)
  (cond
    ((or (= a 0) (= b 0)) 0)
    ((> a b) (mul-iter a b 0))
    (else (mul-iter b a 0))))

;; b: even
;; a . b = a x 2 x b = (double a) x b'
;; b: odd
;; a . b = a x (2 x b' + 1) = (double a) x b' + a

;; here we assume a > b
(define (mul-iter a b rem)
  (cond
    ((= b 1) (+ a rem))
    ((even? b) (mul-iter (double a) (halve b) rem))
    (else (mul-iter (double a) (halve (- b 1)) (+ rem  a)))))
