;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(define V 100)
(define UFO
  (circle 20 "solid" "blue"))

(define MIS
  (triangle 10 "solid" "black"))

(define TANK
  (place-image
   (rectangle 5 30 "solid" "black")
   20 10
   (above
    (rectangle 40 10 "solid" "white")
    (beside
     (rectangle 10 40 "solid" "black")
     (square 20 "solid" "dark green")
     (rectangle 10 40 "solid" "black")))))

(define WIDTH 1600)
(define HEIGHT 900)

(define (change-tank a-world dx)
  (make-ufogame
   dx
   (ufogame-ufo a-world)
   (ufogame-mis a-world)))

(define (read-tank a-world)
  (make-ufogame
   (ufogame-tank a-world)
   (ufogame-ufo a-world)
   (make-missile (ufogame-tank a-world) (missile-y (ufogame-mis a-world)))))

(define-struct ufogame [tank ufo mis])
(define-struct missile [x y])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main x-of-tank UFO-posn missile)
  (big-bang (make-ufogame x-of-tank UFO-posn missile)
            (on-tick next 0.5)
            (on-key position)
            (to-draw game)))

(define (position world key)
  (cond
    [(key=? key "left") (change-tank world (- (ufogame-tank world) 50))]
    [(key=? key "right") (change-tank world (+ (ufogame-tank world) 50))]
    [(key=? key " ") (read-tank world)]
    [else world]))

(define (next world)
  (cond
    [(= 0 (missile-x (ufogame-mis world)))
     (make-ufogame
      (ufogame-tank world)
      (+ 1 (ufogame-ufo world))
      (ufogame-mis world))]
    [else
     (make-ufogame
      (ufogame-tank world)
      (+ 1 (ufogame-ufo world))
      (make-missile (missile-x (ufogame-mis world)) (+ 1 (missile-y (ufogame-mis world)))))]))

(define (x-UFO world)
  (* V (remainder (ufogame-ufo world) (/ WIDTH V))))

(define (y-UFO world)
  (+ 50 (* 100 (floor (/ (ufogame-ufo world) (/ WIDTH V) )))))

(define (x-mis world)
  (missile-x (ufogame-mis world)))

(define (y-mis world)
  (- HEIGHT 100 (* V (missile-y (ufogame-mis world)))))

(define (game world)
  (place-image
   MIS
   (x-mis world)
   (y-mis world)
   (place-image
    UFO
    (x-UFO world)
    (y-UFO world)
    (place-image
     TANK
     (ufogame-tank world) 850
     (empty-scene WIDTH HEIGHT)))))

(main 800 0 (make-missile 0 0))