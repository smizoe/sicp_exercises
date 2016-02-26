;; pascal-triangle returns value of col-th column in row-th row
;; (pascal-triangle 0 0) = 1
;; (pascal-triangle 1 0) = 1
;; (pascal-triangle 1 1) = 1
;; (pascal-triangle 2 0) = 1
;; (pascal-triangle 2 1) = 2
;; (pascal-triangle 4 2) = 6

(define (pascal-triangle row col) (permutation (- row col) col))
(define (permutation n m)
  (cond
    ((= (* n m) 0) 1)
    (else (+ (permutation (- n 1) m) (permutation n (- m 1))))))
