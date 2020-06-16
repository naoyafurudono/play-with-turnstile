#lang turnstile/quicklang

(require racket/control)
(provide (all-defined-out))

(define-base-types Int Bool)
(define-type-constructor → #:arity > 1)

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

;;;;;;;;;;;;;;;;;;;; exception ;;;;;;;;;;;;;;;;;;;;

;; We assume exception is represented as Int value.

;; denote the type of this expression by programmer
(define-typed-syntax (raise (~datum :) τ) ≫
  ------------
  [⊢ (#%app- fcontrol #f)
     (⇒  τ)
     (⇒ exn #t)])

(define-typed-syntax (with-handler  handler e) ≫
  [⊢ e ≫ e-
     (⇒ : τ-e)]
     ;; e-exnは不要
  [⊢ handler ≫ handler-
     (⇐ : τ-e)
     (⇒ exn handler-exn)]
  --------------------
  [⊢ (% e- (λ- (x- k-) handler-))
     (⇒ : τ-e)
     (⇒ exn handler-exn)])

;; (begin-for-syntax
;;   (define remove-e
;;     (lambda (exn-list)
;;       (if (null? exn-list)
;;           '()
;;           ((if (eval #'(p (car exn-list)))
;;               (lambda (x) x)
;;               (cons (car exn-list)))
;;            (e (cdr exn-list) p))))))
;; ;; (define-typed-syntax (with-handlers ([p handler]) e) ≫
;;   [⊢ e ≫ e-
;;      (⇒ : τ-e
;;         (⇒ exn (~locs e-exn ...)))]
;;   [⊢ p ≫ p-
;;      (⇒ : (~→ ~Int ~Bool)
;;         (⇒ exn (~locs p-exn ...)))]
;;   [⊢ handler ≫ handler-
;;      (⇒ : (~→ ~Int τ-handler-out)
;;         (⇒ exn (~locs handler-exn ...)))]
;;   [τ-handler-out τ= τ-e]
;;   --------------------
;;   [⊢ (with-handlers- ([p- handler-]) e-)
;;      (⇒ : τ-e)
;;      (⇒ exn (e-exn ... p-exn ... handler-exn ...))])
;; ;;;;;;;;;;;;;;;;;;;;;;; STLC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-typed-syntax (λ ([x:id (~datum :) τ:type] ...) e) ≫
  [[x ≫ x- : τ] ... ⊢ e ≫ e-
                (⇒ : τ-out)
                (⇒ exn body-exn)
                ]
  ------------
  [⊢ (λ- (x- ...) e-)
     (⇒ : (→ τ ... τ-out)
        (⇒ exn body-exn)
        )])

(define-typed-syntax (#%app e-fn e-arg ...) ≫
  [⊢ e-fn ≫ e-fn-
     (⇒ : (~→ τ-in ... τ-out)
        (⇒ exn ty-exn ))
     (⇒ exn fn-exn)]
  #:fail-unless (stx-length=? #'[τ-in ...] #'[e-arg ...])
                (num-args-fail-msg #'e-fn #'[τ-in ...] #'[e-arg ...])
  [⊢ e-arg ≫ e-arg-
     (⇐ τ-in)
     (⇒ exn arg-exn)] ...
  #:with exn-whole (or #'ty-exn #'fn-exn #'(arg-exn ...))
  ------------
  [⊢ (#%app- e-fn- e-arg- ...)
     (⇒ : τ-out)
     (⇒ exn exn-whole)
     ])

;;;;;;;;;;;;;;;;;;;; arithmetic ;;;;;;;;;;;;
(define-typed-syntax (+ e1 e2) ≫
  [⊢ e1 ≫ e1-
     (⇐ : Int)
     (⇒ exn exn1)]
  [⊢ e2 ≫ e2-
     (⇐ : Int)
     (⇒ exn exn2)]
  #:with whole-exn (or #'exn1 #'exn2)
  ------------
  [⊢ (#%app- +- e1- e2-)
     (⇒ : Int)
     (⇒ exn whole-exn)]
  )

(define-typed-syntax (< e1 e2) ≫
  [⊢ e1 ≫ e1-
     (⇐ : Int)
     (⇒ exn exn1)]
  [⊢ e2 ≫ e2-
     (⇐ : Int)
     (⇒ exn exn2)]
  #:with whole-exn (or #'exn1 #'exn2)
  ------------
  [⊢ (#%app- <- e1- e2-)
     (⇒ : Bool)
     (⇒ exn whole-exn)]
  )

