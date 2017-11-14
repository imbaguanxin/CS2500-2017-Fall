;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(define V-ufo 10)
(define V-tank 30)
(define V-mis -30)
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

(define CONG (text "win!" 50 "yellow"))

(define WIDTH 800)
(define HEIGHT 500)
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define-struct game [ufo tank miss])
(define-struct ufo [x y v])
(define-struct tank [x])
(define-struct mis [x y])

(define INITINAL (make-game (make-ufo 20 20 V-ufo) (make-tank (/ WIDTH 2)) (cons (make-mis 200 200)'())))
(define (main d)
  (big-bang INITINAL
            [on-tick timepass]
            [to-draw render]
            [on-key attack]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (timepass game)
  (if (game? game)
      (if (hit? game)
          #t
          (renew (make-game
                  (game-ufo game)
                  (game-tank game)
                  (remove-miss (game-miss game)))))
      game))
;------------------------------
;------------------------------
(define (hit? game)
  (cond
    [(empty? (game-miss game)) #f]
    [(cons? (game-miss game))
     (if (< (distance game) 30)
         #t
         (hit? (make-game (game-ufo game)
                          (game-tank game)
                          (rest (game-miss game)))))]))
;------------------------------
(define (distance game)
  (sqrt (+ (expt (- (ufo-x (game-ufo game)) (mis-x(first (game-miss game)))) 2)
           (expt (- (ufo-y (game-ufo game)) (mis-y(first (game-miss game)))) 2))))
;------------------------------
(define (remove-miss miss)
  (cond
    [(empty? miss) '()]
    [(cons? miss)
     (if (< (mis-y(first miss)) 0)
         (remove-miss (rest miss))
         (cons (first miss) (remove-miss (rest miss))))]))
;---------------------------------------
(define (renew game)
  (make-game (time-ufo (game-ufo game)) (game-tank game) (time-miss(game-miss game))))

;;-----------------------------
(define (time-ufo ufo)
  (cond
    [(> (ufo-x ufo) WIDTH)
     (make-ufo (- (ufo-x ufo) V-ufo V-ufo)
               (+ 20 (ufo-y ufo))
               (- 0 (ufo-v ufo)))]
    [(< (ufo-x ufo) 0)
     (make-ufo (+ (ufo-x ufo) V-ufo V-ufo)
               (+ 20 (ufo-y ufo))
               (- 0 (ufo-v ufo)))]
    [else (make-ufo (+ (ufo-v ufo) (ufo-x ufo))
                    (ufo-y ufo)
                    (ufo-v ufo))]))
;;-----------------------------
(define (time-miss miss)
  (cond
    [(empty? miss) '()]
    [else (cons (make-mis (mis-x (first miss)) (+ V-mis (mis-y (first miss))))
                (time-miss (rest miss)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (render game)
  (if (game? game)
      (place-image
       UFO
       (ufo-x (game-ufo game))
       (ufo-y (game-ufo game))
       (place-image
        TANK
        (tank-x (game-tank game))
        (- HEIGHT 100)
        (draw-miss (game-miss game))))
      (overlay CONG BACKGROUND)))
;;-----------------------------
(define (draw-miss miss)
  (cond
    [(empty? miss) BACKGROUND]
    [(cons? miss)
     (place-image
      MIS
      (mis-x (first miss))
      (mis-y (first miss))
      (draw-miss (rest miss)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (attack game ke)
  (if (game? game)
      (cond
        [(string=? ke "left")
         (left-tank game)]
        [(string=? ke "right")
         (right-tank game)]
        [(string=? ke " ")
         (shoot game)]
        [else game])
      game))
;---------------------------------
(define (left-tank game)
  (if
   (and (> (tank-x (game-tank game)) 0)
        (< (tank-x (game-tank game)) WIDTH))
   (make-game
    (game-ufo game)
    (make-tank (- (tank-x (game-tank game)) V-tank))
    (game-miss game))
   game))
;----------------------------------
(define (right-tank game)
  (if
   (and (> (tank-x (game-tank game)) 0)
        (< (tank-x (game-tank game)) WIDTH))
   (make-game
    (game-ufo game)
    (make-tank (+ (tank-x (game-tank game)) V-tank))
    (game-miss game))
   game))
;-----------------------------------
(define (shoot game)
  (make-game
   (game-ufo game)
   (game-tank game)
   (cons (make-mis (tank-x (game-tank game)) (- HEIGHT 100)) (game-miss game))))

(main 3)