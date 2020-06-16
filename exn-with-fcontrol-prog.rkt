#lang s-exp "exception-with-fcontrol.rkt"
(require rackunit/turnstile)

;; catch
(try-handle
 (raise : Int 'exn)
 'exn
 12) ;; 12

;; through and catch
;; (try-handle
;;  (try-handle
;;   (raise : Int 'exn2)
;;   'exn 3)
;;  'exn2 111)  ;; 111

;; do not catch
(try-handle
 9
 'exn
 0)  ;; 9

;; 静的解析がうまくいっているかチェックする
;; (print-type
;;   (raise : Int 'exn)
;; #:tag exn
;;  )


;; (print-type
;;  (+ (raise : Int 'foo)
;;     (raise : Int 'boo))
;;  #:tag exn)

(try-handle
 (+ (raise : Int 'foo)
    (raise : Int 'boo))
 'foo 3
)
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


