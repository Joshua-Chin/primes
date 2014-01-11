#lang racket

(require racket/unsafe/ops)

(define (nth-prime n)
  (cond
    ((= n 1) 2)
    ((= n 2) 3)
    ((= n 3) 5)
    ((= n 4) 7)
    ((= n 5) 11)
    (else (_nth-prime n))))

(define (_nth-prime n)
  (define bounds (inexact->exact
                  (round (* n (+ (log n) (log (log n)))))))
  (define is-prime (sieve-unsafe bounds))
  (let helper ([prime-count 0] [index 0])
    (if (unsafe-vector-ref is-prime index)
        (if (= prime-count (- n 2))
            (+ (* 2 index) 1)
            (helper (+ prime-count 1) (+ index 1)))
        (helper prime-count (+ index 1)))))

(define (sieve-unsafe n)
  (define prime-count 2)
  (define is-prime (make-vector (round (/ n 2)) #t))
  (unsafe-vector-set! is-prime 0 #f)
  (for ([i (in-range (+ (round (sqrt (/ n 2))) 1) )] 
        #:when(unsafe-vector-ref is-prime i)
        [j (in-range
            (* 2 i (+ i 1))
            (round (/ n 2))
            (+ (* 2 i) 1))])
    (unsafe-vector-set! is-prime j #f))
  is-prime)

(void (time (nth-prime 1234567)))
