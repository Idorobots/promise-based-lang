#lang racket

(define (on-success result)
  result)

(define (on-failure error)
  error)

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
  (benchmark cores times 'cps-yield-bench n))

(provide main)
(define (main)
  (run-benchmarks 1000 (processor-count) 23))

;; Synchronous:
(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (baseline-bench n)
  (on-success (fib n)))

;; Promise:
(require "promise.rkt")

(define (promise-bench n)
  (then (fib-promise (& n)) on-success)
  (run-all!))

;; CPS:
(require "cps.rkt")

(define (cps-bench n)
  (fib-cps n on-success on-failure))

;; CPS with yield:
(require "cps-yield.rkt")

(define (cps-yield-bench n)
  (schedule (lambda ()
              (fib-cps-yield n
                             on-success
                             on-failure)))
  (step-all!))
