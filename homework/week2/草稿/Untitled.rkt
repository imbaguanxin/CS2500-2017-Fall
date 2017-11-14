;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (insert-word w)
  (big-bang (string-length w)
   [to-draw typewrite]
   [on-tick sub1 .5]
   [stop-when zero?]
   [close-on-stop #t]))
  
(define (typewrite S)
  (place-image (text (substring "12345" 0 S) 50 "black")
               200 200
               (rectangle 400 400 "solid" "white")))
