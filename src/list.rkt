#lang racket

(provide take-until)

;; Function to take elements from a list until predicate is true
(define (take-until predicate list)
  (cond [(empty? list) empty]
        [(predicate (car list)) empty]
        [else (cons (car list) (take-until predicate (cdr list)))]))
