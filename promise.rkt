#lang racket

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

(define (id x) x)

(define (promise fun)
  (let* ((val '())
         (state 'pending)
         (on-resolve id)
         (on-reject id)
         (resolve (lambda (v)
                    (set! val v)
                    (set! state 'resolved)
                    (on-resolve val)))
         (reject (lambda (e)
                   (set! val e)
                   (set! state 'rejected)
                   (on-reject val)))
         (then (lambda (t)
                 (if (equal? state 'resolved)
                     (t val)
                     (set! on-resolve t))))
         (handle (lambda (h)
                   (if (equal? state 'rejected)
                       (h val)
                       (set! on-reject h))))
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
  (make-promise (lambda () value)
                (lambda () 'resolved)
                (lambda (t)
                  (t value))
                (lambda (h) value)
                (lambda () value)))

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

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (time-exec times fun arg)
  (let-values (((_ cpu real gc) (time-apply (lambda ()
                                              (let loop ((i times))
                                                (unless (= i 0)
                                                  (fun arg)
                                                  (loop (- i 1)))))
                                            '())))
    (list real cpu gc)))

(define (benchmark cores times bench-name arg)
  (collect-garbage)
  (let ((ps (map (lambda (i)
                   (let ((c (place ch
                                   (let* ((bench-name (place-channel-get ch))
                                          (fun (eval bench-name ns))
                                          (core (place-channel-get ch))
                                          (times (place-channel-get ch))
                                          (n (place-channel-get ch)))
                                     (display bench-name)
                                     (display " running ")
                                     (display times)
                                     (display " times on core ")
                                     (display core)
                                     (newline)
                                     (place-channel-put ch (time-exec times fun n))))))
                     (place-channel-put c bench-name)
                     (place-channel-put c i)
                     (place-channel-put c (quotient times cores))
                     (place-channel-put c arg)
                     c))
                 (range cores))))
    (let ((results (map (lambda (r)
                          (/ r 1.0 times))
                        (foldl (lambda (a b)
                                 (map + a b))
                               '(0 0 0)
                               (map place-channel-get ps)))))
      (display bench-name)
      (display " results: ")
      (display (map cons '(real-time cpu-time gc-time) results))
      (newline))))

(define (run-benchmarks times cores n)
  (benchmark cores times 'baseline-bench n)
  (benchmark cores times 'promise-bench n)
  (benchmark cores times 'cps-bench n)
  (benchmark cores times 'cps-bench-2 n))

(provide main)
(define (main)
  (run-benchmarks 1000 (processor-count) 23))

;; Synchronous:
(define (baseline-bench n)
  (define (fib n)
    (if (<= n 1)
        1
        (+ (fib (- n 1))
           (fib (- n 2)))))
  (on-success (fib n)))

;; Promise:
(define (promise-bench n)
  (define (fib-promise n)
    (&if (&<= n (& 1))
         (& 1)
         (&+ (fib-promise (&- n (& 1)))
             (fib-promise (&- n (& 2))))))
  (then (fib-promise (& n)) on-success)
  (run-all!))

;; CPS:
(define (cps-primop op)
  (lambda (a b cont handler)
    (cont (op a b))))

(define ^<= (cps-primop <=))
(define ^- (cps-primop -))
(define ^+ (cps-primop +))

(define (cps-bench n)
  (define (fib-cps n cont handler)
    (^<= n
         1
         (lambda (comp)
           (if comp
               (cont 1)
               (^- n
                   1
                   (lambda (sub1)
                     (fib-cps sub1
                              (lambda (fib1)
                                (^- n
                                    2
                                    (lambda (sub2)
                                      (fib-cps sub2
                                               (lambda (fib2)
                                                 (^+ fib1
                                                     fib2
                                                     cont
                                                     handler))
                                               handler))
                                    handler))
                              handler))
                   handler)))
         handler))
  (fib-cps n on-success on-failure))

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

(define (cps-primop-2 op)
  (lambda (a b cont handler)
    (%yield (op a b) cont handler)))

(define %<= (cps-primop-2 <=))
(define %- (cps-primop-2 -))
(define %+ (cps-primop-2 +))

(define (cps-bench-2 n)
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
                        on-success
                        on-failure)))
  (step-all!))

;;; CPS error handling:

(define (%/ a b cont handler)
  (if (equal? b 0)
      (handler "Can't divide by 0!")
      (%yield (/ a b) cont handler)))

(schedule (lambda ()
            (%/ 10 0 on-success on-failure)))

(step-all!)
