(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-time (- (runtime) start-time))
    ))

(define (report-time elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end required-num)
  (cond
    ((or (> start end) (= required-num 0)) ())
    ((even? start) (search-for-primes (+ start 1) end required-num))
    (else (timed-prime-test start)
           (if (prime? start)
            (cons start (search-for-primes (+ start 2) end (- required-num 1)))
            (search-for-primes (+ start 2) end required-num)))))


