#lang racket

;; Second stage: solve challenge for 11 and 13 digits
#|
This time it is impractical to list primes of 11 and 13 digits,
so I shall approach the problem differently.
Rather than filtering 11-digit palindromic primes, I can scan for
11-digit palindromes in pi, and check whether they are prime numbers.
I can do that, since I have 7-digit primes already.
|#

(define (main)
  (printf "First palindromic prime of 11 digits found in π:\n")
  (printf "~a\n" (first-palindromic-prime/11))
  (printf "First palindromic prime of 13 digits found in π:\n")
  (printf "~a\n" (first-palindromic-prime/13)))

; -> byte-string
(define (first-palindromic-prime/11)
  (call-with-input-file "pi100M.txt" ; pi as a string of 100,000,000 digits
    (λ (pi/100M)
      (findf (compose1 is-prime? string->number bytes->string/utf-8)
             (regexp-match* #px"(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)\\5\\4\\3\\2\\1" pi/100M)))))

; -> byte-string
(define (first-palindromic-prime/13)
  (call-with-input-file "pi100M.txt" ; pi as a string of 100,000,000 digits
    (λ (pi/100M)
      (findf (compose1 is-prime? string->number bytes->string/utf-8)
             (regexp-match* #px"(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)\\6\\5\\4\\3\\2\\1" pi/100M)))))

; list of the first million primes
; (ListOf number)
(define primes/1M
  (call-with-input-file "primes1.txt"
    (λ (in)
      (map (compose1 string->number bytes->string/utf-8) (regexp-match* #px"\\d+" in)))))

(define divisible? 
  (compose1 (curry = 0) remainder))

; number -> boolean
(define (is-prime? x)
  (not
   (for/or ([prime (in-list primes/1M)]
            #:when (> x prime))
     (divisible? x prime))))

(main)
