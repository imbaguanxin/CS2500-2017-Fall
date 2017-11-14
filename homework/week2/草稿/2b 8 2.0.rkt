;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2b 8 2.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (main x)
  (big-bang x
          (on-tick next)        ; World -> World
          (to-draw draw-world)  ; World -> Image
          (stop-when stop?)))   ; World -> Boolean

; World -> Image
; Draw the current world

;Write down the text that was given
(define T "Welcome to CS2500")
;calculate the length of the text that was inputed
(define LENGTH (string-length T))

(define (TXTlength world)
  (- LENGTH (floor world))) ;calculate the length of the text when as worlds generated

(define (cut-text TEXT world)
  (substring TEXT 0 (TXTlength world))) ;present the TEXT according to time

(define (draw-world world) ;draw the world according to time
  (overlay
   (text (cut-text T world) 30 "olive")
   (empty-scene 500 300)))

; World -> World
; Gives the next world
(define (next world)
  (cond [(<= (TXTlength world)  (-(/ 27 28))) 0]
        [else (+ world (/ 1 28))]))

; World -> Boolean
; Check if this is the last world
(define (stop? world)
  (<= (TXTlength world) (-(/ 27 28))))

(main 0)