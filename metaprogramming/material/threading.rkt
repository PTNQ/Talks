#lang racket

(define-syntax ->>
  (syntax-rules ()
    [(_ x) x]
    [(_ x (f ...))
     (f ... x)]
    [(_ x f)
     (f x)]
    [(_ x f g ...)
     (-> (-> x f) g ...)]))

(define-syntax ->
  (syntax-rules ()
    [(_ x) x]
    [(_ x (f xs ...))
     (f x xs ...)]
    [(_ x f)
     (f x)]
    [(_ x f g ...)
     (-> (-> x f) g ...)]))
