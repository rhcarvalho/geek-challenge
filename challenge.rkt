#lang racket/base
(require racket/port racket/list)

;; I saw this challenge on a wall at UFRJ
;; in the afternoon of 18/Aug/2011.

(define (main)
  (printf "First palindromic prime of 7 digits found in π:\n")
  (printf "~a\n" (first-palindromic-prime/7)))

; -> byte-string
(define (first-palindromic-prime/7)
  (argmin pi-index (palindromic-primes/7)))

; all palindromic prime numbers of 7 digits
; out of a list of the first million primes
; from http://primes.utm.edu/lists/small/millions/primes1.zip
; -> (ListOf byte-string)
(define (palindromic-primes/7)
  (call-with-input-file "primes1.txt"
    (λ (in)
      (regexp-match* #px"(\\d)(\\d)(\\d)(\\d)\\3\\2\\1" in))))

; pi as a string of 100,000 digits
(define pi
  (call-with-input-file "pi100k.txt"
    (λ (in)
      (port->string in))))

; string -> number
(define (pi-index str)
  (cond
    [(regexp-match-positions str pi) => caar]
    [else +inf.0]))

(main)
