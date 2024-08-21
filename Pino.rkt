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
    (define (add-observers observers)
      (for-each (lambda (observer) (add-observer observer)) observers))
    (define (pull-value v)
      (notify-observers v))
      (lambda (msg . args)
        (cond
          ((equal? msg 'update) (apply pull-value args))
          ((equal? msg 'add-observer) (apply add-observer args))
          ((equal? msg 'add-observers) (apply add-observers args))
          (else #f)))
      )))

(define log
  (lambda (x)
    (println x)))


(define filter
  (lambda (pred)
    (lambda (v)
      (if (pred v)
          v
          empty))))

(define map
  (lambda (op)
    (lambda (v)
      (op v))))

(define subscribe
  (lambda (op)
    (lambda (v)
      (op v)
      'subscribed)))

(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))

(define (single? lst)
  (equal? (length lst) 1))

(define (pipe . ops)
    (lambda (v)
      (letrec ((loop
                (lambda (last-v rest-ops)
                   (if (single? rest-ops)
                       ((car rest-ops) last-v)
                       (loop ((car rest-ops) last-v) (cdr rest-ops))))))
        (loop v ops))))


(define (merge . rs)
  (lambda (observers)
    (letrec ((inner
              (reactive-sequence)))
      (inner 'add-observers observers)
      (for-each (lambda (seq) (seq 'add-observer
                                   (lambda (v)
                                     (inner 'update v))))
                rs))))


(define (combine-latest . rs)
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
      

