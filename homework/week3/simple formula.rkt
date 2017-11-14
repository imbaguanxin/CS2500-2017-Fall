;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |simple formula|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Purpose of Programe
;; Simulate a word processor
;; User can send their post to already
;; existed posts by presing enter
;; User can move cursor on the last line
;; and add or delete character at the cursor
;; using left and right arrow

;; a wordp (word processor) is (make-wordp string string)
;; it is the structure of the unposted entry
;; it devide the post into two parts
;; and I am going to insert an cursor between the two part
(define-struct wordp [l r])
;; template:
#;(define (wordp-tem a-wordp)
    (...(wordp-l a-wordp)...
        (wordp-r a-wordp)...))

;; a wholetext is (make-wholetext string wordp)
;; It is the structure of the existing post and unposted entry
(define-struct wholetext [pre wordp])
;; template:
#;(define (wholetext-tem a-wholetext)
    (...(wholetext-pre a-wholetext)...
        (wholetext-wordp a-wholetext)...))

;; a small rectangle stands for a curosr on the last line
(define CURSOR (rectangle 5 20 "solid" "red"))
;; a background is a white rectangle
(define BACKGROUND (rectangle 800 500 "solid" "white"))
;; a previous text window is a empty scene
(define PRETEXT (empty-scene 800 350))
;; a editing text window is a smaller empty scene
(define EDITEXT (empty-scene 800 150))

;; create two wholetext for check-expects
(define WT1 (make-wholetext "previous text" (make-wordp "123" "456")))
(define WT2 (make-wholetext "no text" (make-wordp "Here is" " text")))

;; l-wordp is a helper function that take out
;; the "l" part (the part that is left to the cursor)
;; wholetext -> string
(check-expect (l-wordp WT1) "123")
(check-expect (l-wordp WT2) "Here is")
(define (l-wordp s)
  (wordp-l(wholetext-wordp s)))

;; r-wordp is a helper function that take out
;; the "r" part (the part that is right to the cursor)
;; wholetext -> string
(check-expect (r-wordp WT1) "456")
(check-expect (r-wordp WT2) " text")
(define (r-wordp s)
  (wordp-r (wholetext-wordp s)))

;; Here is the main function
;; wholetext -> wholetext
(define (simple-forum str)
  (big-bang str
            [on-key move]       ;; wholetext -> wholetext
            [to-draw render]))  ;; wholetext -> image

;; render function:
;; draws how the forum shoud look like
;; it display previous post on the upper window
;; and the unposted one on the lowwer window
;; wholetext -> image

(check-expect (render WT1)
              (above
               (overlay (text "previous text" 20 "black") PRETEXT)
               (lowerw WT1)))
(check-expect (render WT2)
              (above
               (overlay (text "no text" 20 "black") PRETEXT)
               (lowerw WT2)))

(define (render str)
  (above (overlay
          (text (wholetext-pre str) 20 "black")
          PRETEXT)
         (lowerw str)))

;; since the unposted entry is editable, a cursor is inserted.
;; lowerw funtion:
;; a helper function that display the inserted cursor on the lowerwindow.
;; wholetext -> image

(check-expect (lowerw WT1)
              (overlay (beside (text "123" 20 "green") CURSOR (text "456" 20 "green")) EDITEXT))
(check-expect (lowerw WT2)
              (overlay (beside (text "Here is" 20 "green") CURSOR (text " text" 20 "green")) EDITEXT))

(define (lowerw str)
  (overlay (beside (text (l-wordp str) 20 "green")
                   CURSOR
                   (text (r-wordp str) 20 "green"))
           EDITEXT))

;; move function : wholetext key -> wholetext
;; move function take in user's key and a wholetext
;; to preduce wanted wholetext according to the key

(check-expect (move WT1 "\b") (make-wholetext "previous text" (make-wordp "12" "456")))
(check-expect (move WT2 "\b") (make-wholetext "no text" (make-wordp "Here i" " text")))
(check-expect (move WT1 "\r") (make-wholetext "previous text 123456" (make-wordp "" "")))
(check-expect (move WT2 "\r") (make-wholetext "no text Here is text" (make-wordp "" "")))
(check-expect (move WT1 "left") (make-wholetext "previous text" (make-wordp "12" "3456")))
(check-expect (move WT2 "left") (make-wholetext "no text" (make-wordp "Here i" "s text")))
(check-expect (move WT1 "right") (make-wholetext "previous text" (make-wordp "1234" "56")))
(check-expect (move WT2 "right") (make-wholetext "no text" (make-wordp "Here is " "text")))
(check-expect (move WT1 "g") (make-wholetext "previous text" (make-wordp "123g" "456")))
(check-expect (move WT2 "k") (make-wholetext "no text" (make-wordp "Here isk" " text")))

(define (move str key)
  (cond
    ;; pressing backspace:
    ;; Delete the text that is infront of the cursor if there is text there
    ;; Do nothing if there is not text before the cursor
    ;; Deleting the (l) part of wordp by one character
    [(key=? key "\b")
     (if (>= (- (string-length (l-wordp str)) 1)0)
         (make-wholetext
          (wholetext-pre str)
          (make-wordp (substring (l-wordp str) 0 (- (string-length (l-wordp str)) 1))
                      (r-wordp str)))
         str)]
    
    ;; pressing enter:
    ;; add the unposted entry to posted ones
    ;; and create a new entry for user to enter
    ;; append both (l) and (r) part of wordp to (pre) part of wholetext
    ;; make a empty whorp
    
    [(key=? key "\r") 
     (make-wholetext 
      (string-append (wholetext-pre str) " " (l-wordp str)(r-wordp str))
      (make-wordp "" ""))]

    ;; pressing leftarrow:
    ;; move cursor to left if there is text before the cursor
    ;; substract (l) by one character and
    ;; append the last character of (l) to the begining of (r)
    
    [(key=? key "left")
     (if (>= (- (string-length (l-wordp str)) 1) 0)
         (make-wholetext
          (wholetext-pre str)
          (make-wordp (substring (l-wordp str) 0 (- (string-length (l-wordp str)) 1))
                      (string-append 
                       (substring (l-wordp str)
                                  (- (string-length (l-wordp str)) 1)
                                  (string-length (l-wordp str)))
                       (r-wordp str))))
         str)]
    
    ;; pressing rightarrow:
    ;; move cursor to right if there is text after the cursor
    ;; cut the first character of (r) and paste to (l)
    
    [(key=? key "right")
     (if (>= (string-length (r-wordp str)) 1)
         (make-wholetext
          (wholetext-pre str)
          (make-wordp (string-append 
                       (l-wordp str)
                       (substring (r-wordp str)0 1))
                      (substring (r-wordp str) 1 (string-length (r-wordp str)))))
         str)]

    ;; pressing other key to insert a character
    
    [else (make-wholetext
           (wholetext-pre str)
           (make-wordp (string-append (l-wordp str)key)
                       (r-wordp str)))]))
;; a sample run of the main function (simple-forum):
(simple-forum (make-wholetext "previous text" (make-wordp "[]~(￣▽￣)~*" " cheers!!")))