#lang racket
;;; INPUT

(define input #hash((1 . 1)
                    (0 . 2)
                    (18 . 3)
                    (10 . 4)
                    (19 . 5)
                    (6 . 6)))

;;; DAY 15 - PART 1

(define (generate-upto initial n)
  (define (aux ht n c recent)
    (if (zero? n)
        (filter (λ (x) (equal? (cdr x) (sub1 c))) (hash->list ht))
        (if (equal? (hash-ref ht recent 'nope) 'nope)
            (aux (hash-set ht recent c) (sub1 n) (add1 c) 0)
            (aux (hash-set ht recent c) (sub1 n) (add1 c) (- c (hash-ref ht recent))))))
  (aux initial (- n (hash-count initial)) (add1 (hash-count initial)) 0))

(generate-upto input 2020)

;;; DAY 15 - PART 2 

(generate-upto input 30000000)
