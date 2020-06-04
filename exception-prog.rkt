#lang s-exp "exception.rkt"

(with-handlers
  ([(λ ([x : Int]) #t) (λ ([y : Int]) 22)])
  (raise 1))

(with-handlers
  ([(λ ([x : Int]) (< 0 x)) (λ ([y : Int]) 1)])
  ((λ ([n : Int]) (+ n (raise 12))) 3))

((λ ([x : Int]) (+ 12 x)) 3)
