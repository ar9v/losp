#lang racket
(require racket/tcp)
(require racket/pretty)

(require "command.rkt")

;; Server code (defines listener and returns a function to stop the server)
(define (serve port-no)
  (define listener (tcp-listen port-no))
  (define (loop)
    (accept-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

;; Defines the input and output ports by accepting a TCP connection.
;; Calls the message handler
(define (accept-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (displayln "Closing Connection...")
  (close-output-port out)
  (close-input-port in))

;; Consumes all the information available from port in, and returns it as a
;; string
(define (consume in)
  (let* ([buffer (make-bytes 256)]
         [length (read-bytes-avail! buffer in)])
    (if (eof-object? length)
        empty
        (bytes->string/utf-8 (subbytes buffer 0 length)))))

;; Message handler.
;; Receives input and output ports and reads them into a buffer.
;; Processes the client's commands and sends answer from server to client
(define (handle in out)
  ;; Define state (all of the information to report: queues, memory, addresses, etc.)
  ;; Also define the parameters (memory, swap and page size)
  (define state (make-hash `(("command" . ,empty)
                             ("timestamp" . ,empty)
                             ("address" . ,empty)
                             ("ready-queue" . ,empty)
                             ("cpu" . ,empty)
                             ("memory" . ,empty)
                             ("swap" . ,empty)
                             ("finished-queue" . ,empty)
                             ("current-pid" . 0))))
  (define params (make-hash `(("start" . ,(current-inexact-milliseconds)))))
  
  ;; Consume the first messages of the protocol
  (consume in)
  (write "ACK" out)
  (flush-output out)
  (consume in)
  (write "ACK" out)
  (flush-output out)

  ;; Listen and handle the rest of the messages in the connection
  (displayln "handling connection")
  (define (loop log)
    (define msg (consume in))
    (cons (hash-copy state)

          (if (not (empty? msg))
              (begin
                (printf "Server recibió: ~a~n" msg)
                (displayln msg out)
                (displayln (command msg params state) out)
                ;;                (displayln state)
                (pretty-print state)
                (flush-output out)
                (flush-output)
                (loop (cons (hash-copy state) log)))
              empty)))
  (define complete-log (loop empty))
  complete-log
  ;;(pretty-print complete-log)
  )

;; Serve from port 10000 and return a function to stop the connection
(define stop (serve 10000))
