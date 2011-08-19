#lang racket/base
;; from http://programmingpraxis.com/2010/02/19/sieve-of-atkin-improved/2/

(require racket/cmdline)

#| TODO:
  * write results to file
  * don't keep a huge list in memory
  * use unsafe operations
|#

(define (atkin limit)
  (define (exact x) (inexact->exact (floor x)))
  (let ((sieve (make-vector (+ (quotient limit 2) (modulo limit 2)) #f))
        (primes (list 3 2)))
    (define (flip! m) (vector-set! sieve m (not (vector-ref sieve m))))

    (let ((x-max (exact (sqrt (/ (- limit 1) 4)))) (x2 0))
      (do ((xd 4 (+ xd 8))) ((<= (+ (* x-max 8) 2) xd))
        (set! x2 (+ x2 xd))
        (let* ((y-max (exact (sqrt (- limit x2))))
               (n (+ x2 (* y-max y-max)))
               (n-diff (+ y-max y-max -1)))
          (when (even? n) (set! n (- n n-diff)) (set! n-diff (- n-diff 2)))
          (do ((d (* (- n-diff 1) 2) (- d 8))) ((<= d -1))
            (when (member (modulo n 12) (list 1 5)) (flip! (quotient n 2)))
            (set! n (- n d))))))

    (let ((x-max (exact (sqrt (/ (- limit 1) 3)))) (x2 0))
      (do ((xd 3 (+ xd 6))) ((<= (+ (* x-max 6) 2) xd))
        (set! x2 (+ x2 xd))
        (let* ((y-max (exact (sqrt (- limit x2))))
               (n (+ x2 (* y-max y-max)))
               (n-diff (+ y-max y-max -1)))
          (when (even? n) (set! n (- n n-diff)) (set! n-diff (- n-diff 2)))
          (do ((d (* (- n-diff 1) 2) (- d 8))) ((<= d -1))
            (when (= (modulo n 12) 7) (flip! (quotient n 2)))
            (set! n (- n d))))))

    (let ((x-max (exact (/ (+ (sqrt (- 4 (* (- 1 limit) 8))) 2) 4)))
          (y-min -1) (x2 0) (xd 3))
      (do ((x 1 (+ x 1))) ((<= (+ x-max 1) x))
        (set! x2 (+ x2 xd)) (set! xd (+ xd 6))
        (when (<= limit x2)
          (set! y-min (* (- (* (- (inexact->exact (ceiling (sqrt (- x2 limit)))) 1) 2) 2) 2)))
        (let ((n (- (* (+ (* x x) x) 2) 1))
              (n-diff (* (- (* (- x 1) 2) 2) 2)))
          (do ((d n-diff (- d 8))) ((<= d y-min))
            (when (= (modulo n 12) 11) (flip! (quotient n 2)))
            (set! n (+ n d))))))

    (do ((n 2 (+ n 1))) ((<= (quotient (+ (exact (sqrt limit)) 1) 2) n))
      (when (vector-ref sieve n)
        (let* ((p (+ n n 1)) (p2 (* p p)))
          (set! primes (cons p primes))
          (do ((k p2 (+ k (+ p2 p2)))) ((<= limit k))
            (vector-set! sieve (quotient k 2) #f)))))

    (do ((p (+ (exact (sqrt limit)) 1) (+ p 2))) ((<= limit p))
      (when (vector-ref sieve (quotient p 2))
        (set! primes (cons p primes))))

    (reverse primes)))


(command-line
  #:args (limit)
  (atkin (string->number limit)))
