(define *promises* '())

(define (make-promise value state then handle thunk)
  (list '&promise value state then handle thunk))

(define (promise? p)
  (and (list? p)
       (equal? (car p) '&promise)))

(define (promise-value p)
  (cadr p))

(define (promise-state p)
  (caddr p))

(define (promise-then p)
  (cadddr p))

(define (promise-handle p)
  (car (cddddr p)))

(define (promise-thunk p)
  (cadr (cddddr p)))

(define (promise fun)
  (let* ((val '())
         (state '())
         (on-resolve (lambda (x) x))
         (on-reject (lambda (x) x))
         (resolve (lambda (v)
                    (set! val v)
                    (set! state 'resolved)
                    (on-resolve val)))
         (reject (lambda (e)
                   (set! val e)
                   (set! state 'rejected)
                   (on-reject val)))
         (then (lambda (t)
                 (set! on-resolve t)
                 (when (equal? state 'resolved)
                   (on-resolve val))))
         (handle (lambda (h)
                   (set! on-reject h)
                   (when (equal? state 'rejected)
                     (on-reject val))))
         (promise (make-promise (lambda () val)
                                (lambda () state)
                                then
                                handle
                                (lambda () (fun resolve reject)))))
    (set! *promises* (cons promise *promises*))
    promise))

(define (run! p)
  ((promise-thunk p)))

(define (run-all!)
  (let ((ps *promises*))
    (set! *promises* '())
    (map run! ps)
    (unless (equal? *promises* '())
      (run-all!))))

(define (on-resolve promise fun)
  ((promise-then promise) fun))

(define (on-reject promise fun)
  ((promise-handle promise) fun))

(define (value promise)
  ((promise-value promise)))

(define (state promise)
  ((promise-state promise)))

(define (>>= p0 fun)
  (promise (lambda (resolve reject)
             (on-reject p0 reject)
             (on-resolve p0
                         (lambda (val)
                           (let ((p1 (fun val)))
                             (on-reject p1 reject)
                             (on-resolve p1 resolve)))))))

(define (then p fun)
  (promise (lambda (resolve reject)
             (on-reject p reject)
             (on-resolve p
                         (lambda (val)
                           (resolve (fun val)))))))

(define (handle p handler)
  (promise (lambda (resolve _)
             (on-reject p
                        (lambda (error)
                          (resolve (handler error)))))))

;;;

(define-syntax &if
  (syntax-rules ()
    ((&if c t e)
     (>>= c (lambda (r)
              (if r t e))))))

(define (primop op)
  (lambda (a b)
    (>>= a (lambda (a)
             (then b (lambda (b)
                       (op a b)))))))

(define &equal? (primop equal?))
(define &* (primop *))
(define &- (primop -))
(define &+ (primop +))
(define &<= (primop <=))

(define (& value)
  (promise (lambda (resolve _)
             (resolve value))))

;;;

(define (on-success result)
  (display "result: ")
  (display result)
  (newline)
  result)

(define (on-failure error)
  (display "error: ")
  (display error)
  (newline)
  error)

(define (fact n)
  (&if (&equal? n (& 0))
       (& 1)
       (&* n (fact (&- n (& 1))))))

(then (fact (& 20)) on-success)

(run-all!)

;;; Error handling:

(define (&/ a b)
  (>>= a (lambda (a)
           (>>= b (lambda (b)
                    (promise (lambda (resolve reject)
                               (if (equal? b 0)
                                   (reject "Can't divide by 0!")
                                   (resolve (/ a b))))))))))

(handle (then (&/ (& 10) (& 0))
              on-success)
        on-failure)

(run-all!)

;; Simulated asynchronity.

(then (&/ (& 3.14) (&* (& 8) (& 9))) on-success)

(set! *promises* (shuffle *promises*))
(run-all!)

;;; Benchmark:

(define (on-success result)
  result)

(define (on-failure error)
  error)

(define n 23)

(define (benchmark times thunk)
  (collect-garbage)
  (let-values (((_ cpu real gc) (time-apply (lambda ()
                                              (let loop ((n times))
                                                (unless (= n 0)
                                                  (thunk)
                                                  (loop (- n 1)))))
                                            '())))
    (collect-garbage)
    (list (cons 'cpu-time (/ cpu 1.0 times))
          (cons 'real-time (/ real 1.0 times))
          (cons 'gc-time (/ gc 1.0 times)))))

;; Synchronous:
(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(benchmark 1000
           (lambda ()
             (on-success (fib n))))

;; Promise:
(define (fib-promise n)
  (&if (&<= n (& 1))
       (& 1)
       (&+ (fib-promise (&- n (& 1)))
           (fib-promise (&- n (& 2))))))

(benchmark 100
           (lambda ()
             (then (fib-promise (& n)) on-success)
             (run-all!)))

;; CPS:
(define (cps-primop op)
  (lambda (a b cont handler)
    (cont (op a b))))

(define %<= (cps-primop <=))
(define %- (cps-primop -))
(define %+ (cps-primop +))

(define (fib-cps n cont handler)
  (%<= n
       1
       (lambda (comp)
         (if comp
             (cont 1)
             (%- n
                 1
                 (lambda (sub1)
                   (fib-cps sub1
                            (lambda (fib1)
                              (%- n
                                  2
                                  (lambda (sub2)
                                    (fib-cps sub2
                                             (lambda (fib2)
                                               (%+ fib1
                                                   fib2
                                                   cont
                                                   handler))
                                             handler))
                                  handler))
                            handler))
                 handler)))
       handler))

(benchmark 1000
           (lambda ()
             (fib-cps n on-success on-failure)))

;; CPS with yield:
(define *continuation* '())

(define (%yield val cont handler)
  (set! *continuation* (list val cont handler)))

(define (step! cont)
  ((cadr cont) (car cont)))

(define (step-all!)
  (unless (equal? *continuation* '())
    (let ((c *continuation*))
      (set! *continuation* '())
      (step! c)
      (step-all!))))

(define (schedule thunk)
  (%yield 23
          (lambda (_)
            (thunk))
          (lambda (e) e)))

(define (cps-primop op)
  (lambda (a b cont handler)
    (%yield (op a b) cont handler)))

(define %<= (cps-primop <=))
(define %- (cps-primop -))
(define %+ (cps-primop +))

(define (fib-cps2 n cont handler)
  (%<= n
       1
       (lambda (comp)
         (if comp
             (%yield 1 cont handler)
             (%- n
                 1
                 (lambda (sub1)
                   (fib-cps2 sub1
                             (lambda (fib1)
                               (%- n
                                   2
                                   (lambda (sub2)
                                     (fib-cps2 sub2
                                               (lambda (fib2)
                                                 (%+ fib1
                                                     fib2
                                                     cont
                                                     handler))
                                               handler))
                                   handler))
                             handler))
                 handler)))
       handler))

(benchmark 1000
           (lambda ()
             (schedule (lambda ()
                         (fib-cps2 n
                                   on-success
                                   on-failure)))
             (step-all!)))

;;; CPS error handling:

(define (%/ a b cont handler)
  (if (equal? b 0)
      (handler "Can't divide by 0!")
      (%yield (/ a b) cont handler)))

(schedule (lambda ()
            (%/ 10 0 on-success on-failure)))

(step-all!)
