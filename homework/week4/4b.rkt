;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 4b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Exercise 5
;; purpose: fram all of the given images that appear in a list
;; list of image -> list of image
;; Firstly define some list of images for examples
(define A
  (cons (circle 30 "solid" "red")
        (cons (circle 30 "solid" "red")
              (cons (circle 30 "solid" "red")
                    (cons (circle 30 "solid" "red") '())))))
(define B
  (cons (square 60 "solid" "blue")
        (cons (circle 30 "solid" "green")
              (cons (right-triangle 60 60 "solid" "red") '()))))
;; then add frames for defined lists of images
(define A-f
  (cons (frame (circle 30 "solid" "red"))
        (cons (frame (circle 30 "solid" "red"))
              (cons (frame (circle 30 "solid" "red"))
                    (cons (frame (circle 30 "solid" "red")) '())))))
(define B-f
  (cons (frame (square 60 "solid" "blue"))
        (cons (frame (circle 30 "solid" "green"))
              (cons (frame (right-triangle 60 60 "solid" "red")) '()))))

;; picture-frames:
;; list of images -> list of images
;; template:
#;(define (loi-temp loi)
    (cond
      [(empty? loi) ...'() ]
      [(cons? loi) (cons
                    ... (first loi)...
                    ... (loi-temp (rest loi)...))]))

(define (picture-frames loi)
  (cond
    [(empty? loi) '()]
    [(cons? loi) (cons (frame (first loi))
                       (picture-frames (rest loi)))]))

;; examples: A becomes A-f and B becomes B-f
(check-expect (picture-frames A) A-f)
(check-expect (picture-frames B) B-f)

;----------------------------------------------------------------------------
;; Exercise 6
;; Purpose:
;; computing the total area of a list of images
;; Note: the area of a image is its size but not what it contants.
;; Therefore the area is the length of the image times the width of the image

;; total-area:
;; times the width and height of each image and add them up
;; list of images -> number
;; template:
#;(define (loi-temp loi)
    (cond
      [(empty? loi) ...]
      [(cons? loi)
       ... (first loi)...
       ... (loi-temp (rest loi))...]))

(define (total-area loi)
  (cond
    [(empty? loi) 0]
    [(cons? loi) (+ (* (image-width (first loi))(image-height (first loi)))
                    (total-area (rest loi)))]))

;; examples:
;; the total area of A is 60*60*4=14400
;; the total area of B is 60*60*3=10800
(check-expect (total-area A) 14400)
(check-expect (total-area B) 10800)

;-----------------------------------------------------------------------------
;; Exercise 7
;; Purpose:
;; firtly shrink the images in a list at a certain rate
;; then place the images in a list
(define SHRINKRATE 0.7)
;; define the shunk A and B (A-S and B-S)
;; and the image of Shunk A and B (A-S-I and B-S-I) for examples
(define A-S
  (cons (circle 30 "solid" "red")
        (cons (circle (* 30 SHRINKRATE) "solid" "red")
              (cons (circle (* 30 SHRINKRATE SHRINKRATE) "solid" "red")
                    (cons (circle (* 30 SHRINKRATE SHRINKRATE SHRINKRATE) "solid" "red") '())))))

(define B-S
  (cons (square 60 "solid" "blue")
        (cons (scale SHRINKRATE (circle 30 "solid" "green"))
              (cons (scale (* SHRINKRATE SHRINKRATE) (right-triangle 60 60 "solid" "red")) '()))))

(define A-S-I
  (beside/align "bottom"
                (circle 30 "solid" "red")
                (circle (* 30 SHRINKRATE) "solid" "red")
                (circle (* 30 SHRINKRATE SHRINKRATE) "solid" "red")
                (circle (* 30 SHRINKRATE SHRINKRATE SHRINKRATE) "solid" "red")))

(define B-S-I
  (beside/align "bottom"
                (square 60 "solid" "blue")
                (scale SHRINKRATE (circle 30 "solid" "green"))
                (scale (* SHRINKRATE SHRINKRATE) (right-triangle 60 60 "solid" "red"))))
;; trailing-off:
;; list of images -> a image
;; firstly I wish I have a function that shrink the image in the list
;; and output them in a new list
;; lets call it shrink
;; then I wish I have a function that place image align at their bottoms
;; lets call it place
; trailing-off:
(define (trailing-off loi)
  (place (shrink loi (length loi))))
; examples for trailing-off:
(check-expect (trailing-off A) A-S-I)
(check-expect (trailing-off B) B-S-I)

;; Helper function1 -- shrink:
;; shrink the images in the given list and put them in a new list
;; the shrink rate is related to the # of the image
;; the # of image (starts with 0) relates to the shrink rate
;; which is SHRINKRATE^(# of image)
;; the # of image is the total length of the original taken-in loi (a constant)
;; minus the # of images in the current loi
;; list of images, Number -> list of images
;; template:
#;(define (loi-temp loi t)
    (cond
      [(empty? loi) ... '()]
      [(cons? loi)
       ... (first loi)...
       ... (loi-temp (rest loi) t)...]))

(define (shrink loi t)
  (cond
    [(empty? loi) '()]
    [(cons? loi)
     (cons (scale (expt SHRINKRATE (- t (length loi))) (first loi))
           (shrink (rest loi) t))]))

;; examples:
(check-expect (shrink A (length A)) A-S)
(check-expect (shrink B (length B)) B-S)

;; helper function2 -- place:
;; place the images in the given list beside each other and align at bottoms
;; list of images -> a image
;; template:
#;(define (loi-temp loi)
    (cond
      [(empty? loi) ... ]
      [(cons? loi)
       ... (first loi)...
       ... (loi-temp (rest loi))...]))
      
(define (place loi)
  (cond
    [(empty? loi) empty-image]
    [(cons? loi) (beside/align "bottom" (first loi) (place (rest loi)))]))

;; examples:
(check-expect (place A-S) A-S-I)
(check-expect (place B-S) B-S-I)

  


