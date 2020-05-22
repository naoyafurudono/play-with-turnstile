#lang s-exp "exception.rkt"

;; effect : 12, 32
;; (with-handlers
;;   ([(λ ([x : Int]) #t) (λ ([y : Int]) 3)])
;;   ((λ ([n : Int] [m : Int])
;;      (+ (raise 12) m))
;;    (raise 23) 1))

;; effect : nil
;; (with-handlers
;;   ([(λ ([x : Int]) #t) (λ ([y : Int]) 3)])
;;   (λ ([n : Int]) (raise 12)))

;; 問題なく動作する
((λ ([x : Int]) (+ 12 x)) 3)
