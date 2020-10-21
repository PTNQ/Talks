#lang racket

(define-syntax rec
  (syntax-rules ()
    [(_ (name . args) body ...)
     (rec name (lambda args body ...))]
    [(_ name value)
     (let ()
       (define name value)
       name)]))
       
(define-syntax mlet
  (syntax-rules ()
    [(_ tag ((id value) ...) body ...)
     ((rec (tag id ...)
        body ...) 
      value ...)]))
