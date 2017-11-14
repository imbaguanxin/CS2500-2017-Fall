;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5a problem set|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 5
;; Purpose:
;; list of numbers, number -> list of numbers
;; takes a List of Numbers lon and a Number n,
;; and constructs a new List of Numbers with
;; all the same values as lon except for any numbers equal to n

;; testing examples:
(define T1 (list 5 4 3 5 2))
(define T2 (list 3 4 5 6 3 3))
(define T3 (list 3 5 4 3 2 4 5 1 3 2))
(define T4 (list 9 8 7 8 7 6 7 6 5 4 3 4 3 2 2 3 1))
(check-expect (eliminate T1 5) (list 4 3 2))
(check-expect (eliminate T2 3) (list 4 5 6))
(check-expect (eliminate T3 4) (list 3 5 3 2 5 1 3 2))
(check-expect (eliminate T4 6) (list 9 8 7 8 7 7 5 4 3 4 3 2 2 3 1))

;; template:
#;(define (lon-temp lon n)
    (cond
      [(empty? lon) ...]
      [(... n ... (first lon)...)
       (lon-temp ...(rest lon)...n)]
      [else (cons (first lon) (lon-temp (rest lon)n))]))

(define (eliminate lon n)
  (cond
    [(empty? lon)'()]
    [(= n (first lon)) (eliminate (rest lon) n)]
    [else (cons (first lon) (eliminate (rest lon) n))]))

;---------------------------------------------------------
;; Exercise 5 challenge
;; takes a List of Numbers, and constructs a new list
;; with any duplicate values removed (and keeping just
;; a single one of the copies)

;; testing examples:
(check-expect (no-dups T3) (list 3 5 4 2 1))
(check-expect (no-dups T4) (list 9 8 7 6 5 4 3 2 1))

;; the main function: no-dups
;; list of number -> list of number
(define (no-dups lon)
  (real-no-dups lon '()))

;; I need two arguments in the "real-no-dups"
;; a list that contents numbers that has not been determined 
;; whether they should be a part of the new list yet
;; another list that contents already existing numbers
;; in order to help compare with rest of the numbers
;; and this list is originally a empty list

;; testing examples:
(check-expect (real-no-dups T3 '()) (list 3 5 4 2 1))
(check-expect (real-no-dups T4 '()) (list 9 8 7 6 5 4 3 2 1))

;; list of number, list of number -> list of number
;; template:
#;(define (lon-temp lon1 lon2)
    (cond
      [(empty? lon1) '()]
      [(... (first lon1)... lon2)(...(first lon1)...(rest lon1)...lon2...)]
      [else (...(first lon1)...(rest lon1)...lon2...)]))

(define (real-no-dups lon test)
  (cond
    [(empty? lon) '()]
    ;; I wish I have a helper function (duplicate? n lon) to
    ;; determin whether the (first lon) equals one of the numbers in the test
    [(duplicate? (first lon) test)(real-no-dups (rest lon) test)]
    [else (cons (first lon) (real-no-dups (rest lon) (cons (first lon) test)))]))

;; helper function: duplicate? --- determins whether a given
;; number equals one of the number in the given list of numbers
;; examples:
(check-expect (duplicate? 5 T1) #t)
(check-expect (duplicate? 0 T2) #f)

;; number, list of number -> boolean
;; template:
#;(define (lon-temp num lon)
    (cond
      [(empty? lon) "a boolean"]
      [(...(first lon)...n...) "a boolean"]
      [else (lon-temp num (rest lon))]))

(define (duplicate? n test)
  (cond
    [(empty? test) #f]
    [(= (first test) n) #t]
    [else (duplicate? n (rest test))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exercise 6
(require 2htdp/image)
(require 2htdp/universe)

;; Purpose of Program
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

;; a wholetext is (make-wholetext image wordp)
;; It is the structure of the existing post and unposted entry
(define-struct wholetext [history wordp])
;; template:
#;(define (wholetext-tem a-wholetext)
    (...(wholetext-history a-wholetext)...
        (wholetext-wordp a-wholetext)...))

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
;-------------------------------------------------------
;; test examples and constants:

;; Constants:
;; Sever:
(define the-server "dictionary.ccs.neu.edu")
;; the port
(define the-port   10001)
;; a small rectangle stands for a curosr on the last line
(define CURSOR (rectangle 5 20 "solid" "red"))
;; a background is a white rectangle
(define BACKGROUND (rectangle 800 500 "solid" "white"))
;; a previous text window is a empty scene
(define HISTORY (empty-scene 800 800))
;; a editing text window is a smaller empty scene
(define EDITEXT (empty-scene 800 100))

;; create two wholetext for check-expects
(define WT1 (make-wholetext (text "previous text" 20 "black") (make-wordp "123" "456")))
(define WT2 (make-wholetext (text "no text" 20 "black") (make-wordp "Here is" " text")))
;-------------------------------------------------------

;; Here is the main function
;; wholetext -> wholetext
(define (simple-forum str)
  (big-bang str
            [name       "guan.xin:1968"]
            [register   the-server]
            [port       the-port]
            [on-key     move]            ;; wholetext -> wholetext
            [to-draw    render]          ;; wholetext -> image
            [on-receive receive-word]))  ;; wholetext -> image

;------------------------------------------------------
;; render function:
;; draws how the forum shoud look like
;; it display previous post on the history window
;; and the unposted one on the wordprocessor window
;; wholetext -> image

(define (render str)
  (above/align "left"
               ;; I wish I have two helper function
               ;; print-history to draw the previous posts
               ;; print-wordp to draw currently processing posts
               (print-history str)
               (print-wordp str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper function print-history:
;; wholetext -> image
;; draw the previous posts
;; examples:
(check-expect (print-history WT1)
              (overlay/align "left" "top"
                             (text "previous text" 20 "black")
                             HISTORY))
(check-expect (print-history WT2)
              (overlay/align "left" "top"
                             (text "no text" 20 "black")
                             HISTORY))

(define (print-history w)
  (overlay/align "left" "top"
                 (wholetext-history w)
                 HISTORY))

;; helper function print-wordp:
;; wholetext -> image
;; since the unposted entry is editable, a cursor is inserted
;; examples:
(check-expect (print-wordp WT1)
              (overlay (beside (text "123" 20 "green")
                               CURSOR
                               (text "456" 20 "green"))
                       EDITEXT))
(check-expect (print-wordp WT2)
              (overlay (beside (text "Here is" 20 "green")
                               CURSOR
                               (text " text" 20 "green"))
                       EDITEXT))

(define (print-wordp w)
  (overlay (beside (text (l-wordp w) 20 "green")
                   CURSOR
                   (text (r-wordp w) 20 "green"))
           EDITEXT))
   
;-----------------------------------------------------------------------
;; move function
;; wholetext key -> wholetext/package
;; move function take in user's key and a wholetext
;; to preduce wanted wholetext according to the key or send a package
;; examples:
;; 1) backspace:
(check-expect (move WT1 "\b")
              (make-wholetext (text "previous text" 20 "black") (make-wordp "12" "456")))
(check-expect (move WT2 "\b")
              (make-wholetext (text "no text" 20 "black") (make-wordp "Here i" " text")))
;; 2) enter:
(check-expect (move WT1 "\r") (make-package (make-wholetext (text "previous text" 20 "black")
                                                            (make-wordp "" ""))
                                            "123456"))
(check-expect (move WT2 "\r") (make-package (make-wholetext (text "no text" 20 "black")
                                                            (make-wordp "" ""))
                                            "Here is text"))
;; 3) left and right arrow:
(check-expect (move WT1 "left")
              (make-wholetext (text "previous text" 20 "black") (make-wordp "12" "3456")))
(check-expect (move WT2 "left")
              (make-wholetext (text "no text" 20 "black") (make-wordp "Here i" "s text")))
(check-expect (move WT1 "right")
              (make-wholetext (text "previous text" 20 "black") (make-wordp "1234" "56")))
(check-expect (move WT2 "right")
              (make-wholetext (text "no text" 20 "black") (make-wordp "Here is " "text")))
;; 4) do-nothing keys:
(check-expect (move WT1 "menu")
              (make-wholetext (text "previous text" 20 "black") (make-wordp "123" "456")))
(check-expect (move WT2 "shift")
              (make-wholetext (text "no text" 20 "black") (make-wordp "Here is" " text")))
;; 5) other keys:
(check-expect (move WT1 "a")
              (make-wholetext (text "previous text" 20 "black") (make-wordp "123a" "456")))
(check-expect (move WT2 "d")
              (make-wholetext (text "no text" 20 "black") (make-wordp "Here isd" " text")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (move str key)
  (cond
    ;; pressing backspace:
    ;; Delete the text that is infront of the cursor if there is text there
    ;; Do nothing if there is not text before the cursor
    ;; Deleting the (l) part of wordp by one character
    [(key=? key "\b")
     (if (> (string-length (l-wordp str)) 0)
         (make-wholetext
          (wholetext-history str)
          (make-wordp (substring (l-wordp str) 0 (- (string-length (l-wordp str)) 1))
                      (r-wordp str)))
         str)]
    
    ;; pressing enter:
    ;; purpose:
    ;; add the unposted entry to posted ones
    ;; and create a new entry for user to enter
    ;; thus, it send a package to the server
    ;; contaning the currently editing post
    ;; which is the combination of l-wordp and r-wordp
    ;; and make a new wholetext that contains
    ;; an unchanged history and an empty whorp
    
    [(key=? key "\r") 
     (make-package
      (make-wholetext
       (wholetext-history str)
       (make-wordp "" ""))
      (string-append (l-wordp str) (r-wordp str)))]

    ;; pressing leftarrow:
    ;; move cursor to left if there is text before the cursor
    ;; substract (l) by one character and
    ;; append the last character of (l) to the begining of (r)
    
    [(key=? key "left")
     (if (> (string-length (l-wordp str)) 0)
         (make-wholetext
          (wholetext-history str)
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
          (wholetext-history str)
          (make-wordp (string-append 
                       (l-wordp str)
                       (substring (r-wordp str)0 1))
                      (substring (r-wordp str) 1 (string-length (r-wordp str)))))
         str)]
    ;; pressing specific key and do nothing
    [(or (key=? key "shift")
         (key=? key "rshift")
         (key=? key "rcontrol")
         (key=? key "control")
         (key=? key "menu")
         (key=? key "escape")
         (key=? key "f1")
         (key=? key "f2")
         (key=? key "f3")
         (key=? key "f4")
         (key=? key "f5")
         (key=? key "f6")
         (key=? key "f7")
         (key=? key "f8")
         (key=? key "f9")
         (key=? key "f10")
         (key=? key "f11")
         (key=? key "f12")
         (key=? key "prior")
         (key=? key "next")
         (key=? key "home")
         (key=? key "end")
         (key=? key "insert")
         (key=? key "scroll")
         (key=? key "pause")
         (key=? key "numlock"))
     str]

    ;; pressing other key to insert a character before the cursor
    
    [else (make-wholetext
           (wholetext-history str)
           (make-wordp (string-append (l-wordp str)key)
                       (r-wordp str)))]))
;------------------------------------------------------------------------------
;; receive-word function:
;; wholetext string -> wholetext
;; it receives the message from the server and renew the world state
;; examples:
(check-expect (receive-word WT1 "hahaha")
              (make-wholetext
               (above/align "left"
                            (text "previous text" 20 "black")
                            (text "hahaha" 20 "black"))
               (make-wordp "123" "456")))
(check-expect (receive-word WT2 "Today is Chinese National Day")
              (make-wholetext
               (above/align "left"
                            (text "no text" 20 "black")
                            (text "Today is Chinese National Day" 20 "black"))
               (make-wordp "Here is" " text")))

(define (receive-word world message)
  (make-wholetext
   (above/align "left"
                (wholetext-history world)
                (text message 20 "black"))
   (make-wordp (l-wordp world) (r-wordp world))))

;; a sample run of the main function (simple-forum):
(simple-forum (make-wholetext (text "" 20 "black") (make-wordp "" "")))