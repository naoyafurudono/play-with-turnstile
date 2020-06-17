#lang s-exp "exn-with-fcontrol.rkt"
(require rackunit/turnstile)

;; single exception
(try-handle
 (raise : Int 'exn)
 'exn
 12) ;; expected: 12, actual: 12

;; forwarded exception
(try-handle
 (try-handle
  (raise : Int 'exn2)
  'exn
   3)
 'exn2
  111) ;; expected: 111, actual: 111

;; ;; no exception
(try-handle
 9
 'exn
 0)  ;; expected: 9, actual: 9

;; 静的解析がうまくいっているかチェックする
(print-type
  (raise : Int 'exn)
#:tag exn)


(print-type
 (+ (raise : Int 'foo)
    (raise : Int 'boo))
 #:tag exn)

(print-type
 (try-handle
  (+ (raise : Int 'bacon)
     (raise : Int 'ham))
  'bacon
  2)
 #:tag exn)  ;; expected: ('ham), actual: ('ham)

;; ;; two exceptions
(try-handle
 (+ (raise : Int 'foo)
    (raise : Int 'boo))
 'foo
 3) ;; expected: 3, actual: 3

(print-type
 (try-handle
  (raise : Int 'exn)
  'exn
  11)
 #:tag exn) ;; expected: (), actual: ()

;; (print-type
;;  (try-handle
;;   (raise : Int 'uncaught)
;;   'exn 11)
;;  #:tag exn)

(print-type
 (try-handle
  (+
   (raise : Int 'exn)
   (raise : Int 'exn))
  'exn
  0)
 #:tag exn)  ;; expect: (), actual: ('exn)

(print-type
 (try-handle
  0
  'exn
  (raise : Int 'exn))
 #:tag exn)  ;; expect ('exn), actual: ()
