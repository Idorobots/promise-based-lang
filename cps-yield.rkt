#lang racket

;;; CPS version with yield.

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

(provide step-all!)

(define (schedule thunk)
  (%yield 23
          (lambda (_)
            (thunk))
          (lambda (e) e)))

(provide schedule)

(define (cps-yield-primop op)
  (lambda (a b cont handler)
    (%yield (op a b) cont handler)))

(define %equal? (cps-yield-primop equal?))
(define %* (cps-yield-primop *))
(define %- (cps-yield-primop -))
(define %+ (cps-yield-primop +))
(define %<= (cps-yield-primop <=))

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

(define (fact-cps-yield n cont handler)
  (%equal? n 0
           (lambda (comp)
             (if comp
                 (%yield 1 cont handler)
                 (%- n
                     1
                     (lambda (sub)
                       (fact-cps-yield sub
                                       (lambda (fact)
                                         (%* n
                                             fact
                                             cont
                                             handler))
                                       handler))
                     handler)))
           handler))

(schedule (lambda ()
            (fact-cps-yield 20 on-success on-failure)))

(step-all!)

;; CPS error handling:

(define (%/ a b cont handler)
  (if (equal? b 0)
      (handler "Can't divide by 0!")
      (%yield (/ a b) cont handler)))

(schedule (lambda ()
            (%/ 10 0 on-success on-failure)))

(step-all!)


;; Fibonacci:

(define (fib-cps-yield n cont handler)
  (%<= n
       1
       (lambda (comp)
         (if comp
             (%yield 1 cont handler)
             (%- n
                 1
                 (lambda (sub1)
                   (fib-cps-yield sub1
                                  (lambda (fib1)
                                    (%- n
                                        2
                                        (lambda (sub2)
                                          (fib-cps-yield sub2
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

(provide fib-cps-yield)
