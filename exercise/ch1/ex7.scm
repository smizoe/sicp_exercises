(define (good-enough? guess x)
  (< (abs (/ (- guess x) guess)) 0.0001))
