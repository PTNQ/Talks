#lang racket

(require racket/stxparam)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Normal let example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax mlet
  (syntax-rules ()
    [(_ ((id value) ...) body ...)
     ((lambda (id ...) body ...)
      value ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Named let example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax rec
  (syntax-rules ()
    [(_ (name . args) body ...)
     (rec name (lambda args body ...))]
    [(_ name value)
     (let ()
       (define name value)
       name)]))
       
(define-syntax nlet
  (syntax-rules ()
    [(_ tag ((id value) ...) body ...)
     ((rec (tag id ...)
        body ...) 
      value ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Threading macros examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Boolean Operators examples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax or
  (syntax-rules ()
    [(_) #true]
    [(_ x) x]
    [(_ x xs ...)
     (if x x (or xs ...))]))

(define-syntax and
  (syntax-rules ()
    [(_) #true]
    [(_ x) x]
    [(_ x xs ...)
     (if x (and xs ...) x)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; While loop example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-parameter break
  (lambda (stx) 
    (raise-syntax-error #f "Used outside the context of a while body" stx)))

(define-syntax-parameter continue
  (lambda (stx) 
    (raise-syntax-error #f "Used outside the context of a while body" stx)))

(define-for-syntax [force-use-with-parens b]
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       (with-syntax ([b b])
         (syntax/loc stx
           (b)))]
      [(kw arg arg-rest ...)
       (raise-syntax-error
        #f
        (format "Must be directly used without arguments [e.g: (~a)]"
                (syntax->datum #'kw))
        stx
        #'arg)]
      [_
       (identifier? stx)
       (raise-syntax-error
        #f
        (format "Must be directly used [e.g: (~a)]"
                (syntax->datum stx))
        stx)])))

(define-syntax [while stx]
  (syntax-case stx ()
    [(_)
     (raise-syntax-error #f "missing test and body" stx)]
    [(_ cond)
     (raise-syntax-error #f "missing body" stx)]
    [(_ cond body ...)
     (syntax/loc stx
       (let/ec fresh-break 
         (let loop ()
           (when cond
             (let/ec fresh-continue
               (syntax-parameterize 
                   ([break (force-use-with-parens #'fresh-break)]
                    [continue (force-use-with-parens #'fresh-continue)])
                 (begin body ...)))
             (loop)))))]))
