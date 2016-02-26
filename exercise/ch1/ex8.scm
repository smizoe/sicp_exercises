(define (cubrt-iter guess x)
  (if (cub-good-enough? guess x)
    guess
    (cubrt-iter (cub-improve guess x) x)))

(define (cub-good-enough? guess x)

  (< (abs (/ (- (* guess guess guess) x) guess)) 0.001))
(define (cub-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cubrt x) (cubrt-iter 1.0 x))
