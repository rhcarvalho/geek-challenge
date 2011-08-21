#lang racket/base
(require racket/function
         racket/port
         tests/eli-tester)

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
    (λ (pi)
      (for/first ([palindrome/11 (in-producer next-odd-palindrome/11 #f pi)]
                  #:when (is-prime? palindrome/11))
        palindrome/11))))

; port -> (or number #f)
(define (next-odd-palindrome/11 port)
  ; rewind port if necessary, so that
  ; overlapping palidromes are considered
  (let ([pos (file-position port)])
    (unless (= 0 pos)
      (file-position port (- pos 6))))
  (let ([match->number (compose1 string->number bytes->string/utf-8 car)])
    (cond [(regexp-match #px"([13579])(\\d)(\\d)(\\d)(\\d)(\\d)\\5\\4\\3\\2\\1" port) => match->number]
          [else #f])))

(test
 (call-with-input-string
  "123457543212345734450605443"
  (λ (s)
    (for/list ([i (in-producer next-odd-palindrome/11 #f s)])
      i)))
 =>
 '(12345754321
   75432123457
   34450605443))

; -> byte-string
(define (first-palindromic-prime/13)
  (call-with-input-file "pi100M.txt" ; pi as a string of 100,000,000 digits
    (λ (pi)
      (for/first ([palindrome/13 (in-producer next-odd-palindrome/13 #f pi)]
                  #:when (is-prime? palindrome/13))
        palindrome/13))))

; port -> (or number #f)
(define (next-odd-palindrome/13 port)
  ; rewind port if necessary, so that
  ; overlapping palidromes are considered
  (let ([pos (file-position port)])
    (unless (= 0 pos)
      (file-position port (- pos 7))))
  (let ([match->number (compose1 string->number bytes->string/utf-8 car)])
    (cond [(regexp-match #px"([13579])(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)\\6\\5\\4\\3\\2\\1" port) => match->number]
          [else #f])))

; list of the first million primes
; (ListOf number)
(define primes/1M
  (call-with-input-file "primes1.txt"
    (λ (in)
      (for/list ([n (in-producer read eof in)])
        n))))

; number number -> boolean
(define divisible?
  (compose1 (curry = 0) remainder))

; number -> boolean
(define (is-prime? n)
  (not
   (for/or ([prime (stop-before (in-list primes/1M) (curry <= (sqrt n)))])
     (divisible? n prime))))

(main)
