#lang s-exp "my-lang.rkt"


(let ([modn? (λ ([m : Int] [n : Int]) (zero? (modulo m n)))])  ;; let
;;   (let fizz-buzz ([n 1] [limit 100]) : Unit  ;; named let
;;        (if (> n limit)
;;            (unit)
;;            (seq (if (modn? n 15) (print "fizzbuzz")
;;                 (if (modn? n 3) (print "fizz")
;;                     (if (modn? n 5) (print "buzz")
;;                         (print (number->string n)))))
;;                 (fizz-buzz (+ n 1) limit)))))

(letrec fizz-buzz ([n : Int] [limit : Int]) : Unit
        (if (> n limit)
            (unit)
            (seq (if (modn? n 15) (print "fizzbuzz")
                     (if (modn? n 3) (print "fizz")
                         (if (modn? n 5) (print "buzz")
                             (print (number->string n)))))
                 (fizz-buzz (+ n 1) limit)))
        in (fizz-buzz 1 100)))

