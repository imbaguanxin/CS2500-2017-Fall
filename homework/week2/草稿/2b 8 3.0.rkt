;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2b 8 3.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (main txt)
  (big-bang (string-append  txt " ")
            (to-draw draw-world)     ; World -> Image
            (on-tick next 1 )        ; World -> World
            (stop-when stop? lastworld)))      ; World -> Boolean

; World -> Image
; Draw the current world

(define (draw-world txt) ;draw the world according to time
  (overlay
   (text txt 30 "olive")
   (empty-scene 500 300)))

(check-expect (draw-world "qwert")
              (overlay
               (text "qwert" 30 "olive")
               (empty-scene 500 300)))

; World -> World
; Gives the next world
(define (next txt)
  (substring txt 0 (- (string-length txt) 1))
  )

(check-expect (next "qwerty") "qwert")
; World -> Boolean
; Check if this is the last world
(define (stop? txt)
  (= (string-length txt) 0))

(check-expect (stop? "") #true)

(define (lastworld txt)
   (empty-scene 500 300))
(main "123")