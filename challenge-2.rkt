#lang racket/base
(require racket/function
         racket/list
         racket/port
         racket/string
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
  (printf "~a\n" (first-palindromic-prime 11))
  (printf "First palindromic prime of 13 digits found in π:\n")
  (printf "~a\n" (first-palindromic-prime 13)))

; number -> number
(define (first-palindromic-prime digits)
  (call-with-input-file "pi100M.txt" ; pi as a string of 100,000,000 digits
    (λ (pi)
      (for/first ([palindrome (in-producer (next-odd-palindrome/gen digits) #f pi)]
                  #:when (is-prime? palindrome))
        palindrome))))

; number -> procedure
(define (next-odd-palindrome/gen digits)
  ; port -> (or number #f)
  (λ (port)
    ; rewind port if necessary, so that
    ; overlapping palidromes are considered
    (let ([pos (file-position port)])
      (unless (= 0 pos)
        (file-position port (- pos (/ (add1 digits) 2)))))
    (let ([odd-palindrome (pregexp (odd-palindrome-pattern digits))]
          [match->number (compose1 string->number bytes->string/utf-8 car)])
      (cond [(regexp-match odd-palindrome port) => match->number]
            [else #f]))))

; build a pattern to find odd palindromes.
; number -> string
(define (odd-palindrome-pattern digits)
  (let* ([k (/ (sub1 digits) 2)]
         [k- (floor k)]
         [k+ (ceiling k)])
    (string-append* "([13579])"
                    (string-repeat k- "(\\d)")
                    (for/list ([i (in-range k+ 0 -1)])
                      (format "\\~a" i)))))

; number -> boolean
(define (is-prime? n)
  (not
   (for/or ([prime (stop-before (in-list primes/1M) (curry <= (sqrt n)))])
     (divisible? n prime))))

; number number -> boolean
(define divisible?
  (compose1 (curry = 0) remainder))

; number string -> string
(define (string-repeat n str)
  (apply string-append (make-list n str)))

; list of the first million primes
; (ListOf number)
(define primes/1M
  (call-with-input-file "primes1.txt"
    (λ (in)
      (for/list ([n (in-producer read eof in)])
        n))))

; tests
(test
 (call-with-input-string
  "123457543212345734450605443"
  (λ (s)
    (for/list ([i (in-producer (next-odd-palindrome/gen 11) #f s)])
      i)))
 =>
 '(12345754321
   75432123457
   34450605443)
 
 (odd-palindrome-pattern 1)
 =>
 "([13579])"
 
 (odd-palindrome-pattern 2)
 =>
 "([13579])\\1"
 
 (odd-palindrome-pattern 3)
 =>
 "([13579])(\\d)\\1"
 
 (odd-palindrome-pattern 11)
 =>
 "([13579])(\\d)(\\d)(\\d)(\\d)(\\d)\\5\\4\\3\\2\\1")

(main)
