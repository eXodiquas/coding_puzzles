#lang racket

;;; DEFINITIONS

(define (digits n)
  (string-length (format "~a" n)))

(define (fib digs)
  (define (aux n a b)
    (if (= (digits a) digs)
        n
        (aux (add1 n) b (+ a b))))
  (aux 1 1 1))

;;; SOLUTION

(define (run-timed input)
  (time (fib input)))

(define (run input)
  (fib input))

(printf "~a" (run 1000))
