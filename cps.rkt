#lang racket

;;; Basic CPS version.

(define (cps-primop op)
  (lambda (a b cont handler)
    (cont (op a b))))

(define ^equal? (cps-primop equal?))
(define ^* (cps-primop *))
(define ^- (cps-primop -))
(define ^+ (cps-primop +))
(define ^<= (cps-primop <=))

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

(define (fact-cps n cont handler)
  (^equal? n 0
           (lambda (comp)
             (if comp
                 (cont 1)
                 (^- n
                     1
                     (lambda (sub)
                       (fact-cps sub
                                 (lambda (fact)
                                   (^* n
                                       fact
                                       cont
                                       handler))
                                 handler))
                     handler)))
           handler))

(fact-cps 20 on-success on-failure)

;; CPS error handling:

(define (^/ a b cont handler)
  (if (equal? b 0)
      (handler "Can't divide by 0!")
      (cont (/ a b))))

(^/ 10 0 on-success on-failure)

;; Fibonacci:

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

(provide fib-cps)
