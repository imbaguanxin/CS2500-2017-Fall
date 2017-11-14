;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |problem set 2a|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Finger Exercise

;Exercise 1
;minus 2 function
(define (minus_2 num)
  (- num 2)
  )

;Exercise 2
;calculate distance function
(define (distance t)
  (+ 1.5 (* 1.5 t))
  )

;Exercise 3
;turn graph to function
(define (y-value x)
  (+ 10 (* -1 x))
  )

;Exercise 4
(define (Print-string x)
  (cond
    [(= x 1) "QWERT"]
    [(= x 2) "QWER"]
    [(= x 3) "QWE"]
    [(= x 4) "QW"]
    [(= x 5) "Q"]
    [(= x 6) ""]
    [else "Please enter 1,2,3,4 or 5"]
    )
  )

;Exercise 5
(define (qwerty x)
  (cond
    [(not (= (- x (floor x)) 0)) "please enter a natural number"]
    [(= x 0) ""]
    [(= x 1) "q"]
    [(= x 2) "qw"]
    [(= x 3) "qwe"]
    [(= x 4) "qwer"]
    [(= x 5) "qwert"]
    [(>= x 6) "qwerty"]
    [else "please enter a natural number"]
    )
  )