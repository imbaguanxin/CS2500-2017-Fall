;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |6a samples|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

; A World is one of
; - EditviewPosts
; - SearchPosts
; INTERPRETATION: Represents two different "views" in your program.

(define-struct editview [edit history search])
; A EditviewPosts is a (make-editview Edit History Search)
; INTERPRETATION: Means the user is viewing all posts and
; potentially typing in a new one.
; examples:
(define edit0 (make-editview "" '() ""))
(define edit1 (make-editview "editing" '() ""))
(define edit2 (make-editview "editing" (list "1:a:a" "2:b:b" "3:c:c") "a"))
(define edit3 (make-editview "editing" '() "a"))
; template:
#;(define (editview-temp ev)
    (...(editview-edit)...
        (history-temp (editview-history))...
        (editview-search)....))

(define-struct searchp [edit history search])
; A SearchPosts   is a (make-search Edit History Search)
; INTERPRETATION: Means the user is trying to view only a subset
; of the existing messages.
; examples:
(define search0 (make-searchp "" '() ""))
(define search1 (make-searchp "editing" '() ""))
(define search2 (make-searchp "editing" (list "1:a:a" "2:b:b" "3:c:c") "a"))
(define search3 (make-searchp "editing" '() "a"))
; template:
#;(define (searchp-temp ev)
    (...(searchp-edit)...
        (history-temp (searchp-history))...
        (searchp-search)....))

; A Edit is a String
; INTERPRETATION: the contents of the post the user is currently
; editing.
; exampls:
(define Edit0 "")
(define Edit1 "111111")
(define Edit2 "222222")
(define Edit3 "333333")
; template:
(define (edit-temp e)
  ...)

; A History is a List of Strings
; INTERPRETATION: the prior posts received from the server.
; examples:
(define h0 '())
(define h1 (list "0:a:a"))
(define h2 (list "0:a:a" "1:b:b" "3:c:c"))
; template:
#;(define (history-temp h)
    (cond
      [(empty? h) ...]
      [(cons? h) ...(first h)...
                 ...(history-temp (rest h))...]))

; A Search is a string 
; INTERPRETATION: the current search term the user is looking for
; examples:
(define s0 "")
(define s1 "a")
(define s2 "bc")
; template:
#;(define (search-temp s)
    ...)
  
;-------------------------------------------------------------
; some constants:
; Sever:
(define the-server "dictionary.ccs.neu.edu")
; the port
(define the-port   10001)
; a previous text window is a empty scene
(define HISTORY (empty-scene 800 800))
; a editing text window is a smaller empty scene
(define TEXT (empty-scene 800 100))
; define a initinal-world that contain nothing
(define initinal-world (make-editview "" '() ""))
;-------------------------------------------------------------
(define (simple-net-forum str)
  (big-bang initinal-world
            [name            "NAME:NUID"]
            [register        the-server]
            [port            the-port]
            [on-key          keyaction]               ;editview/searchp key -> editview/searchp
            [to-draw         render]             ;editview/searchp -> image
            [on-receive      receive-message]))          ;editview/searchp -> editview     

;------------------------------------------------------
;; I need to split the edit and search motion.
;; when on-key, to-draw, on-receive is called
;; I have to go to different brench.
;; editview/searchp key -> editview/searchp
;; examples are in sub-brenches
(define (keyaction w)
  (cond
    [(editview? w) (editmode w ke)]
    [(searchp? w) (searchmode w ke)]))

; editview/searchp -> image
(define (render w)
  (cond
    [(editview? w) (render-edit w)]
    [(searchp? w) (render-searchp w)]))

; editview/searchp string -> editview/searchp
(define (receive-message w m)
  (cond
    [(editview? w) (receive-edit w m)]
    [(searchp? w) (receive-search w m)]))
;-------------------------------------------------------
;; editview branch:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render-edit
;; draws how the forum shoud look like
;; it display previous post on the history window
;; and the unposted one on the edit window
;; editview -> image
;; exampls:
(check-expect (render-edit edit0)
              (above HISTORY TEXT))
(check-expect (render-edit edit1)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (text "editing" 20 "black")
                                    TEXT)))
(check-expect (render-edit edit2)
              (above (overlay/align "left" "top"
                                    (above/align "left"
                                                 (text "a:a" 20 "black")
                                                 (text "b:b" 20 "black")
                                                 (text "c:c" 20 "black"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (text "editing" 20 "black")
                                    TEXT)))
(check-expect (render-edit edit3)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (text "editing" 20 "black")
                                    TEXT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editmode:
;; editview -> editview / searchp / package
;; it takes in an editview and react according to user's key action
;; examples:

;; 1. enter:
;; it send out editing posts when the post is not empty
;; and clear the currently editing post
(check-expect (editmode edit0 "\r")
              edit0)
(check-expect (editmode edit1 "\r")
              (make-package (make-editview "" '() "")
                            "editing"))
(check-expect (editmode edit2 "\r")
              (make-package (make-editview "" (list "1:a:a" "2:b:b" "3:c:c") "a")
                            "editing"))
(check-expect (editmode edit3 "\r")
              (make-package (make-editview "" '() "a")
                            "editing"))

;; 2. backspace:
;; delete the last character in the edit part of editview
;; it do nothing when the edit part is empty
(check-expect (editmode edit0 "\b")
              edit0)
(check-expect (editmode edit1 "\b")
              (make-editview "editin" '() ""))
(check-expect (editmode edit2 "\b")
              (make-editview "editin" (list "1:a:a" "2:b:b" "3:c:c") "a"))
(check-expect (editmode edit3 "\b")
              (make-editview "editin" '() "a"))

;; 3. f2
;; switch to search mode
(check-expect (editmode edit0 "f2")
              (make-searchp "" '() ""))
(check-expect (editmode edit1 "f2")
              (make-searchp "editing" '() ""))
(check-expect (editmode edit2 "f2")
              (make-searchp "editing" (list "1:a:a" "2:b:b" "3:c:c") "a"))
(check-expect (editmode edit3 "f2")
              (make-searchp "editing" '() "a"))

;; 4. other keys
;; add a character to the end of the edit part of editview
(check-expect (editmode edit0 "f")
              (make-editview "f" '() ""))
(check-expect (editmode edit1 "2")
              (make-editview "editing2" '() ""))            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive-edit
;; editview, string -> editview
;; it renew the editview when receive a message from the server
(check-expect (receive-edit edit0 "4:d:d")
              (make-editview "" (list "4:d:d") ""))
(check-expect (receive-edit edit2 "4:d:d")
              (make-editview "editing" (list "1:a:a" "2:b:b" "3:c:c" "4:d:d") "a"))

;-------------------------------------------------------------------
;; searchp brench

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render-search
;; searchp -> image
;; it shows the search result of given word
(check-expect (render-search edit0)
              (above HISTORY TEXT))
(check-expect (render-search edit1)
              (above HISTORY TEXT))
(check-expect (render-search edit2)
              (above (overlay/align "left top"
                                    (text "a:a" 20 "black")
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (text "a" 20 "black")
                                    TEXT)))
(check-expect (render-search edit3)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (text "a" 20 "black")
                                    TEXT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search-mode:
;; search-mode -> editview / searchp
;; it takes in a searchp and react according to user's key action
;; examples:

;; 1. backspace:
;; delete the last character in the search part of searchp
;; it do nothing when the search part is empty
(check-expect (searchmode search0 "\b")
              search0)
(check-expect (searchmode search0 "\b")
              search1)
(check-expect (searchmode edit2 "\b")
              (make-searchp "editing" (list "1:a:a" "2:b:b" "3:c:c") ""))
(check-expect (searchmode edit3 "\b")
              (make-searchp "editing" '() ""))

;; 2. f1
;; switch to edit mode
(check-expect (searchmode search0 "f1")
              (make-editview "" '() ""))
(check-expect (searchmode search1 "f1")
              (make-editview "editing" '() ""))
(check-expect (searchmode search2 "f1")
              (make-editview "editing" (list "1:a:a" "2:b:b" "3:c:c") "a"))
(check-expect (searchmode search3 "f1")
              (make-editview "editing" '() "a"))

;; 3. other keys
;; add a character to the end of the edit part of editview
(check-expect (searchmode search0 "f")
              (make-searchp "" '() "f"))
(check-expect (searchmode search1 "2")
              (make-searchp "editing" '() "2"))            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive-search
;; reveive the message from the server and add it to history
(check-expect (receive-search search0 "4:d:d")
              (make-searchp "" (list "4:d:d") ""))
(check-expect (receive-search search2 "4:d:d")
              (make-searchp "editing" (list "1:a:a" "2:b:b" "3:c:c" "4:d:d") "a"))