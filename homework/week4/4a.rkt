;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Exercise 5
;; Purpose of Programe
;; Design a Finite State Machine to exam user's input
;; Its state changes according to user's input
;; Change of state:
;; START: haven’t seen any part of "good" yet
;; G: have seen the intial "g"
;; O1: have seen at least one "o"
;; O2: have seen at least two "o"s
;; D: have seen the final "d"

;; Finite-State-Machine is the main function
;; it changes the world color according to user input
;; The initial value of the world is "white"
;; A world is a string that stands for the color of rectangles
;; String -> String
(define (Finite-State-Machine s)
  (big-bang "white"
            (on-key exam)        ; String key -> String
            (to-draw render)))   ; String -> image

;; exam is the function that changes the string
;; that stands for color on user's key action
;; String key -> String
(check-expect (exam "white" "g")"pale green")
(check-expect (exam "pale green" "o")"spring green")
(check-expect (exam "spring green" "o")"lime green")
(check-expect (exam "lime green" "d")"dark green")
(check-expect (exam "lime green" "o")"lime green")
(check-expect (exam "lime green" "x")"white")
(check-expect (exam "pale green" "y")"white")
(check-expect (exam "spring green" "z")"white")
(check-expect (exam "lime green" "3")"white")
(check-expect (exam "dark green" "g")"dark green")
(check-expect (exam "dark green" "o")"dark green")
(check-expect (exam "dark green" "w")"dark green")


(define (exam s ke)
  (cond
    [(string=? s "dark green") "dark green"]
    [(key=? ke "g") "pale green"]
    [(and (key=? ke "o") (string=? "pale green" s))
     "spring green"]
    [(and (key=? ke "o") (string=? "spring green" s))
     "lime green"]
    [(and (key=? ke "o") (string=? "lime green" s))
     "lime green"]
    [(and (key=? ke "d") (string=? "lime green" s))
     "dark green"]
    [else "white"]))

;; render function draws rectangle with
;; different color according to world
(check-expect (render "lime green")
              (rectangle 300 500 "solid" "lime green"))
(check-expect (render "dark green")
              (rectangle 300 500 "solid" "dark green"))
(define (render s)
  (rectangle 300 500 "solid" s))

;; Exercise 6
;; Consumes a String and produces a (make-posn ...)
;; whose x-component is the first character of the given string, and
;; whose y-component is the remainder of the given string.
;; An empty input string should error with "decode: bad input string"
;; String -> Posn / String
(check-expect (Posn-exam "") "decode: bad input string")
(check-expect (Posn-exam "E=MC^2") (make-posn "E" "=MC^2"))
(define (Posn-exam p)
  (if (string=? p "")
      "decode: bad input string"
      (make-posn
       (substring p 0 1)
       (substring p 1 (string-length p)))))