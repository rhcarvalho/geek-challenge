#lang racket

;; Second stage: solve challenge for 11 and 13 digits
#|
This time it is impractical to list primes of 11 and 13 digits,
so I shall approach the problem differently.
Rather than filtering 11-digit palindromic primes, I can scan for
11-digit palindromes in pi, and check whether they are prime numbers.
I can do that, since I have 7-digit primes already.
|#

; number->boolean
(define (is-prime? x)
  #f)

(call-with-input-file "pi100M.txt" ; pi as a string of 100,000,000 digits
  (λ (pi/100M)
    (findf is-prime?
            (regexp-match* #px"(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)\\5\\4\\3\\2\\1" pi/100M))))
