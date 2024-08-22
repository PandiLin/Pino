#lang racket
(define reactive-sequence
  (lambda ()
    (let ((observers '()))
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
      (lambda (msg . args)
        (case msg
          [(update) (apply pull-value args)]
          [(add-observer) (apply add-observer args)]
          [(add-observers) (apply add-observers args)]
          [else #f])))))

(define (update r v)
  (r 'update v))

(define (add-observer r op)
  (r 'add-observer op))

(define (add-observers r ops)
  (r 'add-observers ops))

(define construct-reactive-func
    (let ((inner (reactive-sequence)))
        (lambda (v)
          (update inner v))))

(define construct-reactive-op
  (let ((inner (reactive-sequence)))
      (lambda (f)
        (lambda (op)
          (add-observer op (lambda (v) (f v inner)))
          inner))))

(define r:log
  (construct-reactive-op
   (lambda (v new)
    (println v))))


(define r:filter
  (lambda (pred)
    (construct-reactive-op
     (lambda (v new)
       (if (pred v)
          (update new v)
          new)))))                        

(define r:map
  (lambda (op)
    (construct-reactive-op
     (lambda (v new)
       (update new (op v))))))

(define r:subscribe
  (lambda (op)
    (lambda (v)
      (op v)
      'subscribed)))

(define (single? lst)
  (= (length lst) 1))

(define (pipe v . ops)
  (letrec ((loop
              (lambda (last-v rest-ops)
                (if (single? rest-ops)
                    ((car rest-ops) last-v)
                    (loop ((car rest-ops) last-v) (cdr rest-ops))))))
      (loop v ops)))

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

(pipe test  (r:filter (lambda (v) (> v 10))) r:log)
