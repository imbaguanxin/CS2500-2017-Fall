;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 11a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise1
; A Road is one of:
; - 'dead-end
; - (make-straightaway String PositiveNumber Road)
; - Intersection
 
(define-struct straightaway [name distance more])
; INTERPRETATION: A road with some name and some amount of distance
; until the next portion of road
 
; An Intersection is a [List-of Road]

; examples:
(define r0 'dead-end)
(define r1 (make-straightaway "1" 10 r0))
(define r2 (make-straightaway "2" 15 r1))
(define r3 (make-straightaway "3" 21 r2))
(define r4 (list r1 r2 r3))

; total-road-length: road -> number
; calculates the road length
(define (total-road-length r)
  (local
    [(define (calculate-straightaway sw)
       (+ (straightaway-distance sw) (total-road-length (straightaway-more sw))))
     (define (calculate-intersection lor)
       (local
         [(define (add-total-road-length r num)
            (+ num (total-road-length r)))]
         (foldr add-total-road-length 0 lor)))]
    (cond
      [(symbol? r) 0]
      [(straightaway? r) (calculate-straightaway r)]
      [(list? r) (calculate-intersection r)])))

; examples:
(check-expect (total-road-length r0) 0)
(check-expect (total-road-length r1) 10)
(check-expect (total-road-length r2) 25)
(check-expect (total-road-length r3) 46)
(check-expect (total-road-length r4) 81)

; Exercise 2
(define (road-names r)
  (local
    [(define (name-straightaway sw)
       (append (list (straightaway-name sw))
               (road-names (straightaway-more sw))))
     (define (name-intersection lor)
       (local
         [(define (append-road-names r l)
            (local
              [(define (pick-road-names n)
                 (local
                   [(define (=n? e)
                      (string=? e n))]
                   (not(ormap =n? l))))]
              (append (filter pick-road-names (road-names r)) l)))]
         (foldr append-road-names '() lor)))]
    (cond
      [(symbol? r) '()]
      [(straightaway? r) (name-straightaway r)]
      [(list? r) (name-intersection r)])))
(check-expect (road-names r0) '())
(check-expect (road-names r1) (list "1"))
(check-expect (road-names r2) (list "2" "1"))
(check-expect (road-names r3) (list "3" "2" "1"))
(check-expect (road-names r4) (list "3" "2" "1"))