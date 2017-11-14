;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |deleted helpers|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;-------------------------------------------------------------
; editview helper:

; ev-edit-l-wordp is a helper function that take out
; the "l" part (the part that is left to the cursor)
; editview -> string
(check-expect (ev-edit-l-wordp edit0) "")
(check-expect (ev-edit-l-wordp edit1) "editing")
(define (ev-edit-l-wordp s)
  (wordp-l(editview-edit s)))

; edit-r-wordp is a helper function that take out
; the "r" part (the part that is right to the cursor)
; editview -> string
(check-expect (ev-edit-r-wordp edit0) "")
(check-expect (ev-edit-r-wordp edit1) "")
(define (ev-edit-r-wordp s)
  (wordp-r(editview-edit s)))

; search helper:

; s-edit-l-wordp is a helper function that take out
; the "l" part (the part that is left to the cursor)
; searchp -> string
(check-expect (s-search-l-wordp search0) "")
(check-expect (s-search-l-wordp search2) "a")
(define (s-search-l-wordp s)
  (wordp-l(searchp-search s)))

; edit-r-wordp is a helper function that take out
; the "r" part (the part that is right to the cursor)
; searchp -> string
(check-expect (s-search-r-wordp edit0) "")
(check-expect (s-search-r-wordp edit1) "")
(define (s-search-r-wordp s)
  (wordp-r(searchp-search s)))
;--------------------------------------------------------------