#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC  (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : number)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (interp [e : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
  [numC (n) n]
  [idC (n) (lookup n env)]
  [appC (f a) (local ([define fd (get-fundef f fds)])
              (interp (fdC-body fd)
                      (extend-env (bind (fdC-arg fd)
                                        (interp a env fds))
                                  env)
                      fds))]
  [plusC (l r) (+ (interp l env fds) (interp r env fds))]
  [multC (l r) (* (interp l env fds) (interp r env fds))]))

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

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

(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

; change line 25 to mt-env and we have truly reproduced
; the behavior of the substitution interpreter.
(interp (appC 'f1 (numC 3))
                  mt-env
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))