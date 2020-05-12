#lang s-exp "my-lang.rkt"

(unit)
;; p1(mat-pow(1 1     (n1
;;            1 0, n)  n0) )
(letrec fib-gen ([n : Int] [n0 : Int] [n1 : Int]) : Int
        (if (zero? n)
            n1
            (fib-gen (- n 1) n1 (+ n0 n1))) in
        (fib-gen 50 0 1))

(letrec fact ([n : Int]) : Int
        (if (zero? n)
            1
            (* n (fact (- n 1)))) in
        (fact 4))
