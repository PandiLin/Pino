#lang racket
(define reactive-sequence
  (lambda ()
    (letrec ((observers '()))
      (define (notify observer v)
        (observer v))
      (define (notify-observers v)
        (for-each (lambda (observer) (notify observer v)) observers))
      (define (add-observer observer)
        (set! observers (cons observer observers)))
      (define (add-observers new-observers)
        (for-each add-observer new-observers))
      (define (pull-value v)
        (notify-observers v))
      (define (summarize)
        (length observers))
      (lambda (msg . args)
        (case msg
          [(update) (apply pull-value args)]
          [(add-observer) (apply add-observer args)]
          [(add-observers) (apply add-observers args)]
          [(summarize) (summarize)]
          [else #f])))))

(define (update r v)
  (r 'update v))

(define (add-observer r op)
  (r 'add-observer op))

(define (add-observers r ops)
  (r 'add-observers ops))

(define f-list '())  

(define (add-func-association func seq)
  (set! f-list (cons (cons func seq) f-list)))

(define construct-reactive-func
    (letrec ((inner (reactive-sequence))
          (proc (lambda (v) (update inner v))))
    (add-func-association proc inner)
    (lambda ()
      proc))) 

(define (get-func-association r-func)
  (assoc r-func f-list))

(define (observe-func r-func op)
  (let ((assoc-val (get-func-association r-func)))
    (if assoc-val
        (let ((seq (cdr assoc-val)))
          (op seq))
        (begin (println "no association found") #f))))

(define construct-reactive-op
  (lambda (f)
    (lambda (prev)
      (let* ((new (reactive-sequence)))
          (add-observer prev (lambda (v)  (f v new)))
      new))))

(define r:log
  (construct-reactive-op
   (lambda (v new)
    (println v))))

(define r:filter
  (lambda (pred)
    (construct-reactive-op
     (lambda (v new)
       (if (pred v)
          (new 'update v)
          'empty)))))                        

(define r:map
  (lambda (handle)
    (construct-reactive-op
     (lambda (v new)
       (update new (handle v))))))

(define r:subscribe
  (lambda (op)
    (lambda (observer)
      (observer 'add-observer (lambda (v) (op v))))))

(define (single? lst)
  (= (length lst) 1))

(define (pipe v . ops)
  (letrec ((loop
              (lambda (last-v rest-ops)
                (if (single? rest-ops)
                    ((car rest-ops) last-v)
                    (loop ((car rest-ops) last-v) (cdr rest-ops))))))
      (loop v ops)))

(define (compose . ops)
  (lambda (v)
    (letrec ((loop
              (lambda (last-v rest-ops)
                (if (single? rest-ops)
                    ((car rest-ops) last-v)
                    (loop ((car rest-ops) last-v) (cdr rest-ops))))))
      (loop v ops))))

(define (r:merge . rs)
  (lambda (observers)
    (let ((inner (reactive-sequence)))
      (inner 'add-observers observers)
      (for-each (lambda (seq)
                  (seq 'add-observer
                       (lambda (v)
                         (inner 'update v))))
                rs))))

(define (r:combine-latest . rs)
  (lambda (observers)
    (let ((latest-values (make-vector (length rs) #f))
          (inner (reactive-sequence)))
      (define (update-latest idx v)
        (vector-set! latest-values idx v)
        (when (not (member #f (vector->list latest-values)))
          (inner 'update (vector->list latest-values))))
      (inner 'add-observers observers)
      (for-each (lambda (seq idx)
                  (seq 'add-observer
                       (lambda (v)
                         (update-latest idx v))))
                rs
                (build-list (length rs) values)))))

(define test (reactive-sequence))
(pipe test (r:map (lambda (x) (+ x 1))) (r:filter (lambda (x) (> x 10))) r:log)

(test 'update 11)