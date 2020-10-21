#lang racket

(define-syntax mlet
  (syntax-rules ()
    [(_ ((id value) ...) body ...)
     ((lambda (id ...) body ...)
      value ...)]))
