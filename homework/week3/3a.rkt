;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 3a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;Exercise 3
;;posn-up-x outputs an new position based on given new-x value and a position
;; posn number -> posn
;; template:
#;(define (posn-up-x a-posn new-x)
    (make-posn ... new-x
               ... (posn-y a-posn)))

(define (posn-up-x a-posn new-x)
  (make-posn new-x
             (posn-y a-posn)))
(check-expect (posn-up-x (make-posn 3 4) 5) (make-posn 5 4))
(check-expect (posn-up-x (make-posn "haha" 3) 5) (make-posn 5 3))
(check-expect (posn-up-x (make-posn 3 "interesting") 5) (make-posn 5 "interesting"))

;;Exercise 4
;;move-liner uses the given x- or y-coordinate of something, velocity and an amount of time
;;to produce the final
;;x- or y-coordinate of that thing after moving at the given velocity for given amount of time
;; num num num -> num
;;template:
#; (define (move-linear p v dt)
     (... p... v... dt...))
(define (move-linear p v dt)
  (+ p (* v dt)))

(check-expect (move-linear 10 5 10) 60)
(check-expect (move-linear 10 -5 10) -40)

;;Exercise 5
;;moves a ball in both y and x coordinates

;;Ball-2d is (make-Ball-2d position velocity)
(define-struct Ball-2d [posn vel])

;; Ball is a solid black circle whose radius is 20
(define BALL (circle 20 "solid" "black"))

;; The main function of moving a ball
;; A world is a Ball-2d
(define (move-ball ball)
  (big-bang ball
            [on-tick next]
            [to-draw draw-ball]))

;; next function generates the next ball
;; Ball-2d -> Ball-2d
#;(define (next ball)
    (... (posn-x (Ball-2d-posn ball))...
         (posn-y (Ball-2d-posn ball))...
         (Ball-2d-vel ball)...))

(define (next ball)
  (make-Ball-2d (make-posn (+ (posn-x (Ball-2d-posn ball)) (Ball-2d-vel ball))
                           (+ (posn-y (Ball-2d-posn ball)) (Ball-2d-vel ball)))
                (Ball-2d-vel ball)))

(check-expect (next (make-Ball-2d (make-posn 10 10) 10)) (make-Ball-2d (make-posn 20 20) 10))
(check-expect (next (make-Ball-2d (make-posn 10 10) -5)) (make-Ball-2d (make-posn 5 5) -5))

;; draw-ball function places the ball
;; at a certain location of canvas
;; Ball-2d -> Image
#;(define (draw-ball ball)
    (...(posn-x (Ball-2d-posn ball))...
        (posn-y (Ball-2d-posn ball))...))

(define (draw-ball ball)
  (place-image BALL
               (posn-x (Ball-2d-posn ball))
               (posn-y (Ball-2d-posn ball))
               (empty-scene 300 300)))

(check-expect (draw-ball (make-Ball-2d (make-posn 10 5) 15))
              (place-image BALL 10 5 (empty-scene 300 300)))
(check-expect (draw-ball (make-Ball-2d (make-posn 10 5) -15))
              (place-image BALL 10 5 (empty-scene 300 300)))
(check-expect (draw-ball (make-Ball-2d (make-posn -10 5) 15))
              (place-image BALL -10 5 (empty-scene 300 300)))
                
;;Exercise 6
;; Purpose: simulating a Forum
;; that looks like Piazza andthis is the client program.
;; It can reveive other people's
;; answers to question from the server, 
;; create new answer and new question

;; When the client reveives entries of strings, it uses them as
;; the state and display them on canvas

;; When the client sends a answer
;; The sever appends the string of the answer under the question

;; simple-forum is String
;; interpretation the state of the client are words
;; Changes every time when a message arrives from the server

;; String -> String
#;(define (simple-forum str)
    (big-bang str
              [name ...]
              [register ...]
              [port ...]
              [on-tick send]
              [to-draw render]
              [on-reveive reveive]))

;; forum -> Package (of String and String)
;; send the answer "I don't know" to the server
(check-expect (send "I don't know the answer")
              (make-package "answer: " "I don't know the answer"))
(check-expect (send "Ask TA!") (make-package "answer: " "Ask TA!"))
(define (send str)
  (make-package "answer: " str))

;; forum -> Image
;; render the state of the forum as a text image with a 20 font in black
(check-expect (render "how to design the forum client?")
              (text "how to design the forum client?" 20 "black"))
(define (render str)
  (text str 20 "black"))

; forum String -> Client 
; append the received word to the current state of the forum 
(check-expect (receive "1" "23") "1 \\ 23")
(define (receive str m)
  (string-append str " \\ " m))