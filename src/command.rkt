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
    (hash-update! state "ready-queue" (lambda (old) (cons proc old))))
  
  (define (add-to-cpu proc)
    (hash-set! state "cpu" proc))
  
  (define (add-to-finished proc)
    (hash-update! state "finished-queue" (lambda (old) (cons proc old))))
 
  (define (get-cpu)
    (hash-ref state "cpu"))

  ;; process list accesors
  (define (pid process)
    (car process))
  (define (size process)
    (cadr process))
  (define (priority process)
    (caddr process))

  (define (process<=? x y)
    (< (priority x) (priority y)))

  (define (remove-from-ready proc)
    (hash-set! state "ready-queue" proc))

  (define (next-proc)
    ;; head for now, it needs to be the highest priority
    (car (hash-ref state "ready-queue")))

  (define (create s n)
    (define new-pid (hash-ref state "current-pid"))
    (hash-update! state "current-pid" inc)
    (define (make-process)
      (list new-pid s n))
    (define new-process (make-process))
    (define proc-in-cpu (get-cpu))
    
    ;; three cases,
    ;; there is no process in cpu
    ;; the process in the cpu has greater priority
    ;; the process in the cpu has lower or equal priority
    (cond
      [(empty? proc-in-cpu) (add-to-cpu new-process)]
      [(< (string->number (priority proc-in-cpu)) (string->number (priority new-process)))
       (add-to-ready proc-in-cpu)
       (add-to-cpu new-process)]
      [else (add-to-ready new-process)])
    (format "s:~a n:~a" s n))

  (define (address pid v)
    (cond [(not (= (string->number pid) (car (hash-ref state "cpu"))))
           (format "<~a> ~a no se está ejecutando" (hash-ref state "timestamp") pid)]
          [else (alloc-mem pid v)]))

  (define (alloc-mem pid v)
    (format "ALLOCATING MEM~n")
    (define virt (string->number v))
    (define psize (* (hash-ref params "page") 1024))
    (define msize (* (hash-ref params "memory") 1024))
    (define num-pages (/ msize psize))
    (define (calculate-address-pair)
      (cons (truncate (/ virt psize)) (modulo virt psize)))
    (define address-pair (calculate-address-pair))
    (define pid-page (cons pid (car address-pair)))

    (define page-counter 0)
    (define (get-page pid-page ls)
      (cond [(eq? pid-page (car ls))
             page-counter]
            [else (add1 page-counter) (get-page pid-page (cdr ls))]))
             
    ;; Check if page is in memory and if there's space to load
    (cond [(member pid-page (hash-ref state "memory"))
           (define frame (get-page pid-page (hash-ref state "memory")))
           (displayln "FRAME A)"]
          [(member empty (hash-ref state "memory"))
           (define frame (get-page empty (hash-ref state "memory")))
           (displayln "FRAME B")]
          [else
           (lru pid-page)]))

  (define (lru pid-page)
    (displayln "lru aplicado"))

  (define (fin procid)
    ;; two cases
    ;; the process to end is in the cpu,
    ;; the process to end is in the ready queue
    (format "pid:~a" procid))

  (define (end)
    (format "The End."))

  (define (real-mem m)
    (hash-set! params "memory" (string->number m))
    (format "real memory:~a" m))

  (define (swap-mem m)
    (hash-set! params "swap" (string->number m))
    (format "swap memory:~a" m))

  (define (page-size p)
    (hash-set! params "page" (string->number p))
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
  (apply f (cdr string-list)))


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
