#lang racket

;;; Promise-based version.

(define *promises* '())

(struct promise
  (value state then handle thunk))

(define (id x) x)

(define (make-promise fun)
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
         (p (promise (lambda () val)
                     (lambda () state)
                     then
                     handle
                     (lambda () (fun resolve reject)))))
    (set! *promises* (cons p *promises*))
    p))

(define (run! p)
  ((promise-thunk p)))

(define (run-all!)
  (let ((ps *promises*))
    (set! *promises* '())
    (map run! ps)
    (unless (equal? *promises* '())
      (run-all!))))

(provide run-all!)

(define (on-resolve promise fun)
  ((promise-then promise) fun))

(define (on-reject promise fun)
  ((promise-handle promise) fun))

(define (value promise)
  ((promise-value promise)))

(define (state promise)
  ((promise-state promise)))

(define (>>= p0 fun)
  (make-promise (lambda (resolve reject)
                  (on-reject p0 reject)
                  (on-resolve p0
                              (lambda (val)
                                (let ((p1 (fun val)))
                                  (on-reject p1 reject)
                                  (on-resolve p1 resolve)))))))

(define (>>=-handle p0 handler)
  (make-promise (lambda (resolve reject)
                  (on-reject p0
                             (lambda (error)
                               (let ((p1 (handler error)))
                                 (on-reject p1 reject)
                                 (on-resolve p1 resolve)))))))

(define (then p fun)
  (make-promise (lambda (resolve reject)
                  (on-reject p reject)
                  (on-resolve p
                              (lambda (val)
                                (resolve (fun val)))))))

(provide then)

(define (handle p handler)
  (make-promise (lambda (resolve _)
                  (on-reject p
                             (lambda (error)
                               (resolve (handler error)))))))

(provide handle)

;; Builtins:

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
  (promise (lambda () value)
           (lambda () 'resolved)
           (lambda (t)
             (t value))
           (lambda (h) value)
           (lambda () value)))

(provide &)

;; Factorial:

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

;; Error handling:

(define (&/ a b)
  (>>= a (lambda (a)
           (>>= b (lambda (b)
                    (make-promise (lambda (resolve reject)
                                    (if (equal? b 0)
                                        (reject "Can't divide by 0!")
                                        (resolve (/ a b))))))))))

(define (&catch value handler)
  (>>= handler
       (lambda (h)
         (>>=-handle value
                     (lambda (error)
                       (h (& error)))))))

(handle (then (&catch (&/ (& 10) (& 0))
                      (& (lambda (error)
                           (&/ (& 10) (& 0.001))))) ;; Close enough!
              on-success)
        on-failure)

(run-all!)

;; Simulated asynchronity.

(then (&/ (& 3.14) (&* (& 8) (& 9))) on-success)

(set! *promises* (shuffle *promises*))
(run-all!)

;; Fibonacci:

(define (fib-promise n)
  (&if (&<= n (& 1))
       (& 1)
       (&+ (fib-promise (&- n (& 1)))
           (fib-promise (&- n (& 2))))))

(provide fib-promise)
