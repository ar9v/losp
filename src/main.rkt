#lang racket
(require racket/tcp)

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
  (define state (make-hash '(("command" . empty)
                             ("timestamp" . empty)
                             ("address" . empty)
                             ("ready-queue" . empty)
                             ("cpu" . empty)
                             ("memory" . empty)
                             ("swap" . empty)
                             ("finished-queue" . empty))))
  (define params (make-hash))

  ;; Consume the first messages of the protocol
  (consume in)
  (write "ACK" out)
  (flush-output out)
  (consume in)
  (write "ACK" out)
  (flush-output out)

  ;; Listen and handle the rest of the messages in the connection
  (displayln "handling connection")
  (define (loop)
    (define msg (consume in))
    (when (not (empty? msg))
      (printf "Server recibió: ~a~n" msg)
      (displayln msg out)
      (process msg params state)
      (displayln state)
;;    (displayln (process msg) out)
      (flush-output out)
      (flush-output)
      (loop)))
  (loop))

(define (name->function name)
  (case name
    [("CreateP") create]
    [("Address") address]
    [("Fin")  fin]
    [("End") end]
    [("RealMemory") real-mem]
    [("SwapMemory") swap-mem]
    [("PageSize") page-size]
    [else no-command]))

(define (process msg params state)
  (define string-list (tokenizer msg))
  (define f (name->function (car string-list)))
  (apply f params state (cdr string-list)))

(define (create params state s n)
  (hash-set! state "command" "Create")
  (format "s:~a n:~a" s n))

(define (address params state pid v)
  (hash-set! state "command" "Address")
  (format "pid:~a v:~a" pid v))

(define (fin params state pid)
  (format "pid:~a" pid))

(define (end params state)
  (format "The End."))

(define (real-mem params state m)
  (hash-set! params "memory" m)
  (format "real memory:~a" m))

(define (swap-mem params state m)
  (hash-set! params "swap" m)
  (format "swap memory:~a" m))

(define (page-size params state p)
  (hash-set! params "page" p)
  (format "page size:~a" p))

(define (no-command params state . sink)
  (displayln "This Command name does not exist"))

;; Changes the message sent by the client into a list of strings
(define (tokenizer msg)
  (define string-list (string-split msg))
  (strip-comments string-list))

;; Function to remove strip-comments from the tokenized message
(define (strip-comments string-list)
  (take-until (lambda (x) (string-contains? x "/")) string-list))

;; Function to take elements from a list until predicate is true
(define (take-until predicate list)
  (cond [(empty? list) empty]
        [(predicate (car list)) empty]
        [else (cons (car list) (take-until predicate (cdr list)))]))
