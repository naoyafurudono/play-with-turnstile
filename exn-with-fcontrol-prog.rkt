#lang s-exp "exception-with-fcontrol.rkt"
(require rackunit/turnstile)

;; single exception
(try-handle
 (raise : Int 'exn)
 'exn
 (λ ([x : Int]) 12)) ;; expected: 12, actual: 12

;; forwarded exception (doesn't work)
(try-handle
 (try-handle
  (raise : Int 'exn2)
  'exn
  (λ ([x : Int]) 3))
 'exn2
 (λ ([x : Int]) 111)) ;; expected: 111, actual: 3

;; no exception
(try-handle
 9
 'exn
 (λ ([x : Int]) 0))  ;; expected: 9, actual: 9

;; 静的解析がうまくいっているかチェックする
;; (print-type
;;   (raise : Int 'exn)
;; #:tag exn)


;; (print-type
;;  (+ (raise : Int 'foo)
;;     (raise : Int 'boo))
;;  #:tag exn)

;; two exceptions
(try-handle
 (+ (raise : Int 'foo)
    (raise : Int 'boo))
 'foo
 (λ ([x : Int]) 3)) ;; expected: 3, actual: 3

;; (print-type
;;  (try-handle
;;   (raise : Int 'exn)
;;   'exn 11)
;;  #:tag exn)

;; (print-type
;;  (try-handle
;;   (raise : Int 'uncaught)
;;   'exn 11)
;;  #:tag exn)


