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

  (define (get-ready)
    (hash-ref state "ready-queue"))

  (define (get-memory)
    (hash-ref state "memory"))

  ;; a process accesors
  ;;(define (pid process)
  ;;  (car process))
  (define (size process)
    (cadr process))
  (define (priority process)
    (caddr process))

  (define (process<=? x y)
    (< (priority x) (priority y)))

  (define (process=pid processid process)
      (= (car process) processid))

  ;; removes from ready-queue, and returns it
  (define (remove-from-ready proc)
      (define found 
              (findf (lambda (p) (process=pid proc p)) (get-ready)))
      (hash-update! state
                    "ready-queue"
                    (lambda (old)
                     (remove proc old process=pid)))
      found)

  (define (next-proc)
    ;; head for now, it needs to be the highest priority
    (define ready (get-ready))
    (cond 
        [(empty? ready) empty]
        [else (remove-from-ready (caar ready)) (car ready)]))

  (define (create s n)
    (define (make-process)
      (define new-pid (hash-ref state "current-pid"))
      (hash-update! state "current-pid" inc)
      (list new-pid (string->number s) (string->number n)))
    (define new-process (make-process))
    (define proc-in-cpu (get-cpu))

    ;; three cases,
    ;; there is no process in cpu
    ;; the process in the cpu has greater priority
    ;; the process in the cpu has lower or equal priority
    (cond
      [(empty? proc-in-cpu) (add-to-cpu new-process)]
      [(< (priority proc-in-cpu) (priority new-process))
       (add-to-ready proc-in-cpu)
       (add-to-cpu new-process)]
      [else (add-to-ready new-process)])
    (format "s:~a n:~a" s n))

  (define (address pid v)
    (define proc-in-cpu (get-cpu))
    (cond [(or (empty? proc-in-cpu) (not (= (string->number pid) (car proc-in-cpu))))
           (format "<~a> ~a no se está ejecutando" (hash-ref state "timestamp") pid)]
          [else (alloc-mem pid v)]))

  (define (alloc-mem pid v)
    (define virt (string->number v))
    (define psize (hash-ref params "page"))
    (define (calculate-address-pair)
      (cons (truncate (/ virt psize)) (modulo virt psize)))

    (displayln "antes del let")
    (let* ([address-pair (calculate-address-pair)]
           [pid-page (cons pid (car address-pair))]
           [memory (get-memory)]
           [pos-pid-page (vector-member pid-page memory)]
           [pos-empty-page (vector-member 0 memory)])
      (displayln "Ricardo dijo que imprimiera")
      (displayln pos-empty-page)
      (displayln pos-pid-page)
      (cond [pos-pid-page
             (hash-set! state "address" (+ (* pos-pid-page psize) (cdr address-pair)))]
            [pos-empty-page
             (hash-set! state "address" (+ (* pos-pid-page psize) (cdr address-pair)))
             (vector-set! memory pos-empty-page pid-page)]
            [else
             (lru pid-page)])))

    (define (lru pid-page)
      (displayln "lru aplicado"))

  (define (fin procid)
    (define nprocid (string->number procid))
    (define proc-in-cpu (get-cpu))
    
    ;; two cases
    ;; the process to end is in the cpu,
    ;; the process to end is in the ready queue
    (cond
      [(= (car proc-in-cpu) nprocid)
        (add-to-finished proc-in-cpu)
        (add-to-cpu (next-proc))]
      [else (add-to-finished (remove-from-ready nprocid))])
    (format "pid:~a" procid))

  (define (end)
    (format "The End."))

  (define (real-mem m)
    (hash-set! params "memory" (* 1024 (string->number m)))
    (format "real memory:~a" m))

  (define (swap-mem m)
    (hash-set! params "swap" (* 1024 (string->number m)))
    (format "swap memory:~a" m))

  (define (page-size p)
    (define psize (* 1024 (string->number p)))
    (hash-set! params "page" psize)
    (let* ([ ssize (hash-ref params "swap")]
           [ rsize (hash-ref params "memory")]
           [ num-pages-swap (quotient ssize psize)]
           [ num-pages-real (quotient rsize psize)])
      (hash-set! state "memory" (make-vector num-pages-real))
      (hash-set! state "swap" (make-vector num-pages-swap)))
  (format "page size:~a" psize))

  (define (no-command . sink)
    (displayln "This Command name does not exist"))
  
  (define (name->function name)
    (case name
      [("CreateP") create]
      [("Address") address]
      [("Fin") fin]
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
  (real->decimal-string (ms->s (- t1 t2)) 3))

(define (ms->s sec)
  (/ sec 1000))

(define (inc n)
  (+ n 1))
