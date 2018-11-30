#lang racket
(require racket/tcp)

;; Server code (defines listener and returns a function to stop the server)
(define (serve port-no)
  (define listener (tcp-listen port-no))
  (define t (thread (lambda ()
                      (accept-handle listener))))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

;; Defines the input and output ports by accepting a TCP connection.
;; Calls the message handler
(define (accept-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-output-port out)
  (close-input-port in))

;; Message handler.
;; Receives input and output ports and reads them into a buffer.
;; Processes the client's commands and sends answer from server to client
(define (handle in out)
  (let* ([buffer (make-bytes 256)]
         [length (read-bytes-avail! buffer in)])
    (when (not (eof-object? length))
      (define msg (bytes->string/utf-8 (subbytes buffer 0 length)))
      (printf "Server recibió: ~a~n" msg)
      (displayln (process msg) out)
      (flush-output out)
      (flush-output)
      (handle in out))))

(define (process msg)
  msg)

;; Changes the message sent by the client into a list of strings
(define (tokenizer msg)
  (define string-list (string-split msg))
  (display string-list))

;; Function to remove comments from the tokenized message
(define (strip-comments string-listn)
  (take-until (lambda (x) (string-contains? x "/"))))

;; Function to take elements from a list until predicate is true
(define (take-until predicate list)
  (if (predicate (car list))
      '()
      (cons (car list) (take-until predicate (cdr list)))))
