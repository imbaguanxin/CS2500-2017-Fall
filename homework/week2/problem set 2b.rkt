;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |problem set 2b|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Exercise 1
;number->number
;use given x to produce x^2+12
(define (fx x)
  (+ 12 (* x x))
  )

;Exercise 2
(define x 1)
(define (math-is-boring x)
  (+ (* -4 (expt x 3)) (* 8 (expt x 1)) (* 9 (expt x 0))))

;Exercise 3
(define (hello x)
  (cond
    [(string=? "Ben" x) (string-append "Dear " x ", Esquire:")]
    [(string=? "Nada" x) (string-append "Dear " x ", Esquirette:")]
    [else (string-append "Greetings, " x ", ")]))

;Exercise 4
;string->image
;use given string to produce a picture where the text lays on a white rectangel
(define (render-string t)
  (overlay
   (text (substring "qwerty" 0 t) 22 "blue")
   (rectangle 200 100 "solid" "white")
   )
  )
;Exercise 5
;string1 string2 image ->image
;add a new image that writes the string1 "said" string2 below image
(define (add-message str1 str2 image)
  (above
   (text (string-append str1 " said " str2) 22 "black")
   image)
  )

;Exercise 7
(define (ticket s l)
  (cond
    [(< s l) "fine"]
    [(<= s (+ l 5)) "danger"]
    [else (string-append "ticket" "you drove " (number->string s) " mph")]
    )
  )

;;Exercise 8
;;simulating deleting text at the speed of one character per second
;;cuts the given text

(define (main txt)
  (big-bang (string-append txt " ")
          (on-tick next 1)      ; text -> text
          (to-draw draw-text)  ; text -> Image
          (stop-when stop?)))   ; text -> Boolean

;;text -> Image
;;Draw the current text on a blank scene
#;(define (draw-text txt)
    ...)
(define (draw-text txt) ;draw the world according to time
  (overlay
   (text txt 30 "olive")
   (empty-scene 500 300)))

(check-expect (draw-text "qwert")
              (overlay
               (text "qwert" 30 "olive")
               (empty-scene 500 300)))

;;text -> text
;;Gives the next text
#;(define (next txt)
     substring txt...)
(define (next txt)
  (substring txt 0 (- (string-length txt) 1))
  )

(check-expect (next "qwerty") "qwert")

;;text -> Boolean
;;Check if the text was wiped out if it is then stop
#;(define (stop? txt)
    ... stringlength txt)
(define (stop? txt)
  (= (string-length txt) -1))

(check-expect (stop? " ") #true)

(main "rt")