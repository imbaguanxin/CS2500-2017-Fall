;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 5b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 4
;; list of numbers, a number -> list of numbers
;; purpose:
;; add a number onto the end of a list of numbers

;; define some constants for test:
(define LON1 (list 1 2 3 4 5 6 7))
(define LON2 (list 3 4 5 6))

;; examples:
(check-expect (snoc 5 LON1) (list 1 2 3 4 5 6 7 5))
(check-expect (snoc 3 LON2) (list 3 4 5 6 3))
(check-expect (snoc 3 '()) (list 3))

;; template:
#; (define (snoc-temp num lon)
     (cond
       [(empty? lon) (cons...num...)]
       [(cons? lon) (cons...(first lon)...(snoc num (rest lon)))]))

(define (snoc num lon)
  (cond
    [(empty? lon) (cons num '())]
    [(cons? lon) (cons (first lon) (snoc num (rest lon)))]))

;-----------------------------------
;; Exercise 5:
;; string, list of strings -> list of strings
;; purpose:
;; takes a string and a list of strings, and returns a list containing
;; all the strings in the given list that contain the given string as a substring

;; define some constants for test:
(define LOS1 (list "a" "bcd" "imba" "pdd" "bbc"))
(define LOS2 (list "nce" "air" "kawai" "miku" "hatsune"))

;; examples:
(check-expect (search-for-string "a" LOS1)
              (list "a" "imba"))
(check-expect (search-for-string "at" LOS2)
              (list "hatsune"))
(check-expect (search-for-string "a" '())
              '())
;; main function:
;; template:
#;(define (search-fro-string-temp str los)
    (cond
      [(empty? los) '()]
      [(... str (first los)) (cons ...(first los)...(rest los))]
      [else (search-for-string-temp str (rest los))]))

(define (search-for-string str los)
  (cond
    [(empty? los) '()]
    [(string-contains? str (first los))
     (cons (first los)
           (search-for-string str (rest los)))]
    [else (search-for-string str (rest los))]))