#lang racket/base
(require racket/port)

;; I saw this challenge on a wall at UFRJ
;; in the afternoon of 18/Aug/2011.

; tests to see if the byte string is a palindrome
; byte-string->boolean
(define (palindrome? bstring)
  (let ([blist (bytes->list bstring)])
    (equal? blist (reverse blist))))

; all palindromic prime numbers of 7 digits
; out of a list of the first million primes
; from http://primes.utm.edu/lists/small/millions/primes1.zip
(define prime/len7/palindrome
  (filter palindrome?
          (call-with-input-file "primes1.txt"
            (λ (in)
              (regexp-match* #px"\\d{7}" in)))))

; pi as a string of 100,000 digits
; from http://gc3.net84.net/pi/pi_e5_0.htm
(define pi
  (call-with-input-file "pi.txt"
    (λ (in)
      (regexp-replace* #px"[^\\d]" (port->string in) ""))))

; all palindromic prime numbers of 7 digits found in pi
(define p7p/indexed
  (filter car
          (for/list ([x (in-list prime/len7/palindrome)])
            (list (regexp-match-positions x pi) x))))

(printf "First palindromic prime to appear in pi:\n~a\n"
        (cadar (sort p7p/indexed < #:key caaar)))


;; Second stage: solve for 11 and 13 digits
#|
This time it is impractical to list primes of 11 and 13 digits,
so I shall approach the problem differently.
Rather than filtering 11-digit palindromic primes, I can scan for
11-digit palindromes in pi, and check whether they are prime numbers.
I can do that, since I have 7-digit primes already.
|#
