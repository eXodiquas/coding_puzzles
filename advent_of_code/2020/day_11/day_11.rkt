#lang racket

(define (read-input path)
  (string-split (file->string path) "\r\n"))

(define (input->hash path)
  (let ((inp (read-input path))
        (ht (make-hash)))
    (for ([i inp]
          [height (range 0 (length inp))])
      (for ([c i]
            [width (range 0 (string-length (car inp)))])
        (hash-set! ht (list width height) c)))
    ht))

(define (seats-in-sight ht pos)
  (for*/first ([x (range (car pos) 10)] ; look right
               [y (cadr pos)]
               #:unless (equal? (hash-ref ht (list x y) #\.) #\.))
    (hash-ref ht (list x y))))
