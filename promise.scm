(define *runnables* '())

(define (promise fun)
  (let ((val '())
        (state '())
        (on-resolve (lambda (x) x))
        (on-reject (lambda (x) x)))
    (let ((resolve (lambda (v)
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
          (get-value (lambda () val))
          (get-state (lambda () state)))
      (let ((promise (list get-value get-state then handle)))
        (set! *runnables* (cons (list fun resolve reject) *runnables*))
        promise))))

(define (run! runnable)
  ((car runnable) (cadr runnable) (caddr runnable)))

(define (run-all!)
  (let ((r *runnables*))
    (set! *runnables* '())
    (map run! r)
    (unless (equal? *runnables* '())
      (run-all!))))

(define (on-resolve promise fun)
  ((caddr promise) fun))

(define (on-reject promise fun)
  ((cadddr promise) fun))

(define (value promise)
  ((car promise)))

(define (state promise)
  ((cadr promise)))

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
             ((cadddr p)
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

(set! *runnables* (shuffle *runnables*))
(run-all!)

;;;

(define n 30)

;; Synchronous:
(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(time (on-success (fib n)))

;; result is: 1346269
;; cpu time: 34 real time: 32 gc time: 0

;; Promise:
(define (fib-promise n)
  (&if (&<= n (& 1))
       (& 1)
       (&+ (fib-promise (&- n (& 1)))
           (fib-promise (&- n (& 2))))))

(then (fib-promise (& n)) on-success)

(time (run-all!))

;; result is: 1346269
;; cpu time: 79264 real time: 79233 gc time: 63396

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

(time (fib-cps n on-success on-failure))

;; result is: 1346269
;; cpu time: 280 real time: 281 gc time: 0

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

(schedule (lambda ()
            (fib-cps2 n
                      (lambda (result)
                        (display "result is: ")
                        (display result)
                        (newline))
                      (lambda (e) e))))

(time (step-all!))

;; result is: 1346269
;; cpu time: 533 real time: 534 gc time: 7

;;; CPS error handling:

(define (%/ a b cont handler)
  (if (equal? b 0)
      (handler "Can't divide by 0!")
      (%yield (/ a b) cont handler)))

(schedule (lambda ()
            (%/ 10 0 on-success on-failure)))

(step-all!)
