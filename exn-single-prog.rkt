#lang s-exp "exn-single.rkt"
(require rackunit/turnstile)

(+ 1 2)

(with-handler 12 (raise : Int))

(print-type
 (+ 1 2)
 #:tag exn)

(print-type
 (raise : Int)
 #:tag exn)
