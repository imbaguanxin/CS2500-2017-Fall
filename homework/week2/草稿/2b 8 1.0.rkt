;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2b 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (main x)
  (big-bang x
          (on-tick next)        ; World -> World
          (to-draw draw-world)  ; World -> Image
          (stop-when stop?)))   ; World -> Boolean

; World -> Image
; Draw the current world

(define T "qwerty")               ;input a text
(define LENGTH (string-length T)) ;calculate the length of the text that was inputed

(define (instant-length world)
  (- LENGTH (floor(/ world 28)))) ;calculate the length of the text when as worlds generated

(define (cut-text TEXT world)
  (substring TEXT 0 (instant-length world))) ;present the TEXT according to time

(define (draw-world world) ;draw the world according to time
  (overlay
   (text (cut-text T world) 30 "olive")
   (empty-scene 500 300)))

; World -> World
; Gives the next world
(define (next world)
  (cond [(= (instant-length world) 0) 0]
        [else (+ world 1)]))

; World -> Boolean
; Check if this is the last world
(define (stop? world)
  (= (instant-length world) 0))

(main 1)