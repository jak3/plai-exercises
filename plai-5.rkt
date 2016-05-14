#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC  (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
  [numC (n) n]
  [idC (_) (error 'interp "shouldn't get here")]
  [appC (f a) (local ([define fd (get-fundef f fds)])
              (interp (substH (interp a fds)
                             (fdC-arg fd)
                             (fdC-body fd))
                      fds))]
  [plusC (l r) (+ (interp l fds) (interp r fds))]
  [multC (l r) (* (interp l fds) (interp r fds))]))

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

; get-fundef : symbol * (listof FunDefC) -> FunDefC
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

; subst : ExprC * symbol * ExprC -> ExprC
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond
             [(symbol=? s for) what]
             [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l) (subst what for r))]
    [multC (l r) (multC (subst what for l) (subst what for r))]))
; substH : number * symbol * ExprC -> ExprC
(define (substH [what : number] [for : symbol] [in : ExprC]) : ExprC
  (subst (numC what) for in))

; [ definitions ] --------------------------------------------
(define (double x) (+ x x))
(define (quadruple x) (double (double x)))
(define (const5 _) 5)
(fdC 'double 'x (plusC (idC 'x) (idC 'x)))
(fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
(fdC 'const5 '_ (numC 5))
; [ tests ] --------------------------------------------------

