#lang racket

(define-syntax while
  (syntax-rules ()
    [(_ condition body ...)
     (begin
       (define [looper]
         (when condition
           body ...
           (looper)))
       (looper))]))
    
(define [while-loop condition body]
  (when (condition)
    (body)
    (while-loop condition body)))
