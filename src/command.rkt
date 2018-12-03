#lang racket

(provide command)

(require "list.rkt")

(define (command msg params state)
  (define string-list (tokenizer msg))
  (hash-set! state "command" (car string-list))

  (define (set-timestamp)
    (define start (hash-ref params "start"))
    (hash-set! state "timestamp" (time-diff (time-now) start)))
  
  (set-timestamp)

  (define (add-to-ready proc)
    (hash-update! state "ready-queue" (lambda (old) (append old proc))))
  (define (add-to-cpu proc)
    (hash-set! state "cpu" proc))
  
  (define (create s n)
    (define new-pid (hash-ref state "current-pid"))
    (hash-update! state "current-pid" inc)
    (define (make-process)
      (list new-pid s n))
    (define (pid process)
      (car process))
    (define (size process)
      (cadr process))
    (define (priority process)
      (caddr process))
    (define new-process (make-process))
    
    ;; three cases,
    ;; there is no process in cpu
    ;; the process in the cpu has greater priority
    ;; the process in the cpu has lower or equal priority
    (define proc-in-cpu (hash-ref state "cpu"))
    (displayln (empty? proc-in-cpu))
    (displayln proc-in-cpu)
    (cond
      [(empty? proc-in-cpu) (add-to-cpu new-process)]
      [(< (string->number (priority proc-in-cpu)) (string->number (priority new-process)))
       (add-to-ready proc-in-cpu)
       (add-to-cpu new-process)]
      [else (add-to-ready new-process)])
    (format "s:~a n:~a" s n))

  (define (address pid v)
    (format "pid:~a v:~a" pid v))

  (define (fin pid)
    (format "pid:~a" pid))

  (define (end)
    (format "The End."))

  (define (real-mem m)
    (hash-set! params "memory" m)
    (format "real memory:~a" m))

  (define (swap-mem m)
    (hash-set! params "swap" m)
    (format "swap memory:~a" m))

  (define (page-size p)
    (hash-set! params "page" p)
    (format "page size:~a" p))

  (define (no-command . sink)
    (displayln "This Command name does not exist"))
  
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

  (define f (name->function (car string-list)))
  (apply f params state (cdr string-list)))


;; Changes the message sent by the client into a list of strings
(define (tokenizer msg)
  (define string-list (string-split msg))
  (strip-comments string-list))

;; Function to remove strip-comments from the tokenized message
(define (strip-comments string-list)
  (take-until (lambda (x) (string-contains? x "/")) string-list))

(define (time-now)
  (current-inexact-milliseconds))

(define (time-diff t1 t2)
  (real->decimal-string (ms->s (- t1 t2))) 3)

(define (ms->s sec)
  (/ sec 1000))

(define (inc n)
  (+ n 1))
