(define (sos-larger-2 x y z)
  (cond
    ((and (> x z) (> y z)) (sum-of-squares x y))
    ((and (> y x) (> z x)) (sum-of-squares y z))
     (else (sum-of-squares x z))
     )
  )
