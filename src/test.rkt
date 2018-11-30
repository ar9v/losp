#lang racket
(require racket/tcp)

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define-values (in out) (tcp-accept listener))
  (define input (read-string 19 in))
  (display input out)
  (flush-output out)
  (display (read-string 18 in) out)
  (close-input-port in)
  (close-output-port out)
  (tcp-close listener))
