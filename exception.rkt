#lang turnstile/quicklang

;; λ項 + 足し算 + 例外
(provide (all-defined-out))

(define-base-type Int)
(define-type-constructor → #:arity > 1)
(define-primop + : (→ Int Int Int))

(define-typed-syntax #%datum
  [(_ . n:integer) ≫
   ------------
   [⊢ (#%datum- . n) ⇒ Int]]
  [(_ . x) ≫
   ------------
   [#:error (type-error #:src #'x #:msg "Unsupported literal ~v" #'x)]])

(define-typed-syntax (λ ([x:id (~datum :) τ:type] ...) e) ≫
  [[x ≫ x- : τ] ... ⊢ e ≫ e- ⇒ τ-out]
  ------------
  [⊢ (λ- (x- ...) e-) ⇒ (→ τ ... τ-out)])

(define-typed-syntax (#%app e-fn e-arg ...) ≫
  [⊢ e-fn ≫ e-fn- ⇒ (~→ τ-in ... τ-out)]
  #:fail-unless (stx-length=? #'[τ-in ...] #'[e-arg ...]) (num-args-fail-msg #'e-fn #'[τ-in ...] #'[e-arg ...])
  [⊢ e-arg ≫ e-arg- ⇐ τ-in] ...
  ------------
  [⊢ (#%app- e-fn- e-arg- ...) ⇒ τ-out])

