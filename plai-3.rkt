#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [uminusS (e : ArithS)])

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
  [numC (n) n]
  <idC-interp-case>
  <appC-interp-case>
  [plusC (l r) (+ (interp l fds) (interp r fds))]
  [multC (l r) (* (interp l fds) (interp r fds))]))

;; http://cs.brown.edu/courses/cs173/2012/book/first-desugar.html
(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC  n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]
    [uminusS (e) (plusC (numC 0) (multC (numC -1) (desugar e)))]))

(define-type ExprC
  [numC (n : number)]
  [idC  (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusEC (l : ExprC) (r : ExprC)]
  [multEC (l : ExprC) (r : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

; a1
(define a1 (plusC (numC 4) (numC 11)))
; bool?
(define (bool? a)
  (cond
    [ (= (interp a) 0) #t]
    [else #f]))

(define (double x) (+ x x))
(define (quadruple x) (double (double x)))
(define (const5 _) 5)
(fdC 'double 'x (plusEC (idC 'x) (idC 'x)))
(fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
(fdC 'const5 '_ (numEC 5))
; tests
(test (interp a1) 15)
(test (bool? a1) #f)
(test (bool? (plusC (numC 4) (numC 11))) #f)
(test (bool? (multC (numC 0) (plusC (numC 4) (numC 11)))) #t)
