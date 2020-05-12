#lang turnstile/quicklang
(provide (all-defined-out))

;;;;;;;;;;;; basic ;;;;;;;;;;;;
(define-base-types Int Unit Bool String)

(define-type-constructor → #:arity > 0)
(define-type-constructor product #:arity = 2)

(define-primop + : (→ Int Int Int))
(define-primop - : (→ Int Int Int))
(define-primop * : (→ Int Int Int))
(define-primop quotient : (→ Int Int Int))
(define-primop modulo : (→ Int Int Int))
(define-primop zero? : (→ Int Bool))
(define-primop < : (→ Int Int Bool))
(define-primop > : (→ Int Int Bool))
;; (define-primop print : (→ String Unit))
(define-primop number->string : (→ Int String))


;;;;;;;;;;;; primitive ;;;;;;;;;;;;;;;;
(define-typed-syntax #%datum
  [(_ . n:integer) ≫
   -----
   [⊢ (#%datum- . n) ⇒ Int]]

  [(_ . b:boolean) ≫
   -----
   [⊢ (#%datum- . b) ⇒ Bool]]

  [(_ . s:string) ≫
   ------
   [⊢ (#%datum- . s) ⇒ String]]

  [(_ . x) ≫
   ------
   [#:error (type-error #:src #'x #:msg "Unsupported literal: ~v" #'x)]])

;; 例えば、printは'unitを返さない
;; TaPL 11.2 unit
(define-typed-syntax (unit) ≫
  -----
  [⊢ 'unit ⇒ Unit])

(define-typed-syntax (print e) ≫
  [⊢ e ≫ e- ⇒ _]
  -----
  [⊢ (begin- (#%app- print- e-) (unit)) ⇒ Unit])
;;;;;;;;;;;; stlc ;;;;;;;;;;;;;;;;;
(define-typed-syntax (#%app e-fn e-arg ...) ≫
  [⊢ e-fn ≫ e-fn- ⇒ (~→ τ-in ... τ-out)]
  ;; v1(1.2.3) add syntax-parse option
  #:fail-unless (stx-length=? #'[τ-in ...] #'[e-arg ...])
  (format "arity mismatch, expected ~a args, given ~a"
          (stx-length #'[τ-in ...]) #'[e-arg ...])

  [⊢ e-arg ≫ e-arg- ⇐ τ-in] ...
  -----
  [⊢ (#%app- e-fn- e-arg- ...) ⇒ τ-out])

(define-typed-syntax λ #:datum-literals (:)
  [(_ ([x:id : τ-in:type] ...) e) ≫
   [[x ≫ x- : τ-in.norm] ... ⊢ e ≫ e- ⇒ τ-out]
   -----
   [⊢ (λ- (x- ...) e-) ⇒ (→ τ-in.norm ... τ-out)]]

  ;; synthesis rule
  ;; フォーマッタが謎のインデントをしてしまう... ⇐が苦手？
  ;; 日本語がまとも
  [(_ (x:id ...) e) ⇐ (~→ τ-in ... τ-out) ≫
                    #:fail-unless (stx-length=? #'[τ-in ...] #'[x ...])
                    (format "arity mismatch, expected ~a args, given ~a"
                            (stx-length #'[τ-in ...]) #'[x ...])
                    [[x ≫ x- : τ-in] ... ⊢ e ≫ e- ⇐ τ-out]
                    -----
                    [⊢ (λ- (x- ...) e-)]])

(define-typed-syntax (ann e (~datum :) τ:type) ≫
  [⊢ e ≫ e- ⇐ τ.norm]
  -----
  [⊢ e- ⇒ τ.norm])

;;;;;;;;;;;; condition ;;;;;;;;;;;;
(define-typed-syntax (if c-exp e1 e2) ≫
  [⊢ c-exp ≫ c-exp- ⇐ Bool]
  [⊢ e1 ≫ e1- ⇒ τ1]
  [⊢ e2 ≫ e2- ⇒ τ2]
  #:fail-unless (type=? #'τ1 #'τ2) (format "type-unmatched")
  ------
  [⊢ (if- c-exp- e1- e2-) ⇒ τ1])



;;;;;;;;;;;; TaPL 11.3 : sequencing ;;;;;;;;;;;;
(define-typed-syntax (begin e ... e-fin) ≫
  [⊢ e ≫ e- ⇐ Unit] ...
  [⊢ e-fin ≫ e-fin- ⇒ τ-fin]
  -----
  [⊢ e-fin- ⇒ τ-fin])

;; derived form. beginとseqは同じ意味。エラーメッセージは異なる。
(define-typed-syntax seq
  [(_ e1 e2) ≫
   -------
   [≻ ((λ ([x : Unit]) e2) e1)]]

  [(_ e0 e1 ... e-fin) ≫
   ------
   [≻ ((λ ([x : Unit]) (seq e1 ... e-fin)) e0)]])

;;;;;;;;;;;; TaPL 11.4 ascription ;;;;;;;;;;;;
(define-typed-syntax (as τ:type e) ≫
  [⊢ e ≫ e- ⇐ τ]
  ------
  [⊢ e- ⇒ τ])
;; derived form
;; (define-typed-syntax (as τ:type e) ≫
;;   ------
;;   [≻ ((λ ([x : τ]) x) e)])

;;;;;;;;;;;; TaPL 11.5 let, named let, letrec ;;;;;;;;;;;;
(define-typed-syntax let
  [(_ ([x e] ...) body) ≫
   [⊢ e ≫ e- ⇒ τ-bind] ...
   [[x ≫ x- : τ-bind] ... ⊢ body ≫ body- ⇒ τ-body]
   -----
   [⊢ (let- ([x- e-] ...) body-) ⇒ τ-body]]

  ;; named let
  ;; 推論するのは、レベルが高そうなので今回はパス. τ-outとして、プログラマがアノテーションをつける。型検査ではそれが正しいことをチェックする。
  [(_ f:identifier ([x:identifier e] ...) (~datum :) τ-out:type body) ≫
   [⊢ e ≫ e- ⇒ τ-bind] ...
   [[x ≫ x- : τ-bind] ... [f ≫ f- : (→ τ-bind ... τ-out)] ⊢ body ≫ body- ⇐ τ-out]
   -------
   [⊢ (let- f- ([x- e-] ...) body-) ⇒ τ-out]]
  )

;; だいぶ不安
;; derived form with let and named-let
(define-typed-syntax (letrec f ([var (~datum :) τ-arg:type] ...) (~datum :) τ-out:type body (~datum in) rest) ≫
  -----
  [≻ (let ([f (λ ([var : τ-arg] ...)  ;; normal let
                (let f ([var var] ...) : τ-out body))]) rest)])  ;; named let

;;;;;;;;;;;; TaPL 11.6 pairs ;;;;;;;;;;;;
(define-typed-syntax (pair e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ1]
  [⊢ e2 ≫ e2- ⇒ τ2]
  -----
  [⊢ (#%app- cons- e1- e2-) ⇒ (product τ1 τ2)])

(define-typed-syntax (fst e) ≫
  [⊢ e ≫ e- ⇒ (~product τ1 _)]
  -------
  [⊢ (#%app- car- e-) ⇒ τ1])

(define-typed-syntax (snd e) ≫
  [⊢ e ≫ e- ⇒ (~product _ τ2)]
  ------
  [⊢ (#%app- cdr- e-) ⇒ τ2])

;;;;;;;;;;;; define ;;;;;;;;;;;;

;; nameに型がつかない。defineした変数を呼び出すときに型エラーになる
;; racketのenvironmentの実装を調べる？envのvarがsyntax onjectなら、syntax propertyとして型を付ければいい。
;; (define-typed-syntax (define name e) ≫
;;   -------
;;   [⊢ (define- name e) ⇒ Unit])

