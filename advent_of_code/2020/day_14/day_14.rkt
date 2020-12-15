#lang racket
;;; INPUT TRANSFORMATION

(define (read-input path)
  (string-split
   (file->string path) "\r\n" #:trim? #t))

;;; DAY 14 - PART 1


;;; DAY 14 - PART 2 

