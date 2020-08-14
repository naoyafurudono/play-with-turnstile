#lang turnstile/quicklang
(require racket/control)

(provide (all-defined-out) quote)

(define-base-types Int Bool)
(define-type-constructor → #:arity > 1)

(begin-for-syntax
  (define (stx-truth? a)
    (and a (not (and (syntax? a) (false? (syntax-e a))))))
  
  (define (stx-or a b)
    (cond [(stx-truth? a) a]
          [else b])))

;;;;;;;;;;; STLC ;;;;;;;;;;;

(define-typed-syntax (λ ([x:id (~datum :) τ:type] ...) e) ≫
  [[x ≫ x- : τ] ... ⊢ e ≫ e-
                           (⇒ : τ-out)
                           (⇒ exn body-exn)]
  ------------
  [⊢ (λ- (x- ...) e-)
     (⇒ : (→ τ ... τ-out) (⇒ exn body-exn))])

(define-typed-syntax (#%app e-fn e-arg ...) ≫
  [⊢ e-fn ≫ e-fn-
     (⇒ : (~→ τ-in ... τ-out) (⇒ exn ty-exn))
     (⇒ exn fn-exn)]
  #:fail-unless (stx-length=? #'[τ-in ...] #'[e-arg ...])
                (num-args-fail-msg #'e-fn #'[τ-in ...] #'[e-arg ...])
  [⊢ e-arg ≫ e-arg-
     (⇐ τ-in)
     (⇒ exn arg-exn)] ...
  ------------
  [⊢ (#%app- e-fn- e-arg- ...)
     (⇒ : τ-out)
     (⇒ exn (stx-or #'ty-exn #'fn-exn (#'arg-exn ...)))])

;;;;;;;;;;; Numbers and Booleans ;;;;;;;;;;;

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
   ------------
   [⊢ (#%datum- . n) ⇒ Int]]
  [(_ . b:boolean) ≫
   ------------
   [⊢ (#%datum- . b) ⇒ Bool]]
  [(_ . x) ≫
   ------------
   [#:error (type-error #:src #'x #:msg "Unsupported literal ~v" #'x)]])

(define-typed-syntax (+ e1 e2) ≫
  [⊢ e1 ≫ e1-
     (⇐ : Int)
     (⇒ exn exn1)]
  [⊢ e2 ≫ e2-
     (⇐ : Int)
     (⇒ exn exn2)]
  ------------
  [⊢ (#%app- +- e1- e2-)
     (⇒ : Int)
     (⇒ exn (stx-or #'exn1 #'exn2))])

(define-typed-syntax (< e1 e2) ≫
  [⊢ e1 ≫ e1-
     (⇐ : Int)
     (⇒ exn exn1)]
  [⊢ e2 ≫ e2-
     (⇐ : Int)
     (⇒ exn exn2)]
  ------------
  [⊢ (#%app- <- e1- e2-)
     (⇒ : Bool)
     (⇒ exn (stx-or #'exn1 #'exn2))])

(define-typed-syntax (if exp1 exp2 exp3) ≫
  [⊢ exp1 ≫ exp1-
     (⇐ Bool)
     (⇒ exn exn1)]
  [⊢ exp2 ≫ exp2-
     (⇒ τ2)
     (⇒ exn exn2)]
  [⊢ exp3 ≫ exp3-
     (⇐ τ2)
     (⇒ exn exn3)]
  --------------------
  [⊢ (if- exp1- exp2- exp3-)
     (⇒ τ2)
     (⇒ exn (stx-or exn1 exn2 exn3))])

;;;;;;;;;;; Exceptions ;;;;;;;;;;;

(define-typed-syntax (raise e (~datum :) τ:type) ≫
  [⊢ e ≫ e-
     (⇐ : Int)
     (⇐ exn #f)]
  ------------
  [⊢ (#%app- fcontrol e-)
     (⇒ τ)
     (⇒ exn #t)])

(define-typed-syntax (try-handle e1 x:id e2) ≫
  [⊢ e1 ≫ e1-
     (⇒ : τ)
     (⇒ exn exn1)]
  [[x ≫ x- : Int] ⊢ e2 ≫ e2-
     (⇐ : τ)
     (⇒ exn exn2)]
  --------------------
  [⊢ (% e1- (λ- (x- k-) e2-))
     (⇒ : τ)
     (⇒ exn exn2)])

;;;;;;;;;;; Examples ;;;;;;;;;;;

; (try-handle (+ 12 30) x 0)

; (try-handle (+ 12 (raise 30 : Int)) x (+ x 8))
