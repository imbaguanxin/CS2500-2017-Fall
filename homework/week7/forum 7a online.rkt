;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |forum 7a online|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;-------------------------------------------------------------------
;; PURPOSE:
;; simiulate pizza
;; function 1 : add post to the server
;; function 2 : receive posts that received from server and display only
;;              the id and posts, numbers are not displayed but not deleted
;; function 3 : search the history, and will highlight characters that are
;;              the same to user's input in green
;; function 4 : when editing, press left or right arrow to move the cursor
;-------------------------------------------------------------------
;; A wordp (word processor) is (make-wordp string string)
;; it is the structure of the word processor
;; it devide the post and search into two parts
;; and I am going to insert an cursor between the two part
(define-struct wordp [l r])
; template:
#;(define (wordp-temp a-wordp)
    (...(wordp-l a-wordp)...
        (wordp-r a-wordp)...))

;; A World is one of
;; - EditviewPosts
;; - SearchPosts
;; INTERPRETATION: Represents two different "views" in your program.

(define-struct editview [edit history search])
;; A EditviewPosts is a (make-editview wordp History wordp)
;; INTERPRETATION: Means the user is viewing all posts and
;; potentially typing in a new one.
;; examples:
(define edit0 (make-editview (make-wordp "" "")'() (make-wordp "" "")))
(define edit1 (make-editview (make-wordp "editing" "") '() (make-wordp "" "")))
(define edit2
  (make-editview (make-wordp "editing" "") (list "1:a:a" "2:b:b" "3:c:c") (make-wordp "a" "")))
(define edit3
  (make-editview (make-wordp "editing" "") '() (make-wordp "a" "")))
;; template:
#;(define (editview-temp ev)
    (...(editview-edit)...
        (history-temp (editview-history))...
        (editview-search)....))

(define-struct searchp [edit history search])
;; A SearchPosts   is a (make-search Edit History Search)
;; INTERPRETATION: Means the user is trying to view only a subset
;; of the existing messages.
;; examples:
(define search0 (make-searchp (make-wordp "" "") '() (make-wordp "" "")))
(define search1 (make-searchp (make-wordp "editing" "") '() (make-wordp "" "")))
(define search2
  (make-searchp (make-wordp "editing" "") (list "1:a:a" "2:b:b" "3:c:c") (make-wordp "a" "")))
(define search3
  (make-searchp (make-wordp "editing" "") '() (make-wordp "a" "")))
;; template:
#;(define (searchp-temp ev)
    (...(searchp-edit)...
        (history-temp (searchp-history))...
        (searchp-search)....))

;; A History is a List of Strings
;; INTERPRETATION: the prior posts received from the server.
;; examples:
(define h0 '())
(define h1 (list "0:a:a"))
(define h2 (list "0:a:a" "1:b:b" "3:c:c"))
;; template:
#;(define (history-temp h)
    (cond
      [(empty? h) ...]
      [(cons? h) ...(first h)...
                 ...(history-temp (rest h))...]))
  
;-------------------------------------------------------------
;; some constants:
;; Sever:
(define the-server "dictionary.ccs.neu.edu")
;; the port
(define the-port   10001)
;; a previous text window is a empty scene
(define HISTORY (empty-scene 800 800))
;; a editing text window is a smaller empty scene
(define TEXT (empty-scene 800 100))
;; a small rectangle stands for a curosr on the last line
(define CURSOR (rectangle 5 20 "solid" "red"))
;-------------------------------------------------------------
; main function:
(define (simple-net-forum dont-care)
  (big-bang edit0
            [name            "guan.xin:1968"]
            [register        the-server]
            [port            the-port]
            [on-key          keyaction]               ;world key -> editview/searchp/package
            [to-draw         render]                  ;world -> image
            [on-receive      receive-message] ))      ;world -> editview/searchp   
;------------------------------------------------------

;; I need to split the edit and search motion.
;; when on-key, to-draw, on-receive is called
;; I have to go to different brench.
;; examples are in sub-brenches

;; keyaction:
;; PURPOSE: respond to different key
;; editview/searchp key -> editview/searchp
(define (keyaction w ke)
  (cond [(searchp? w) (searchmode w ke)]
        [(editview? w) (editmode w ke)]))

;; render:
;; PURPOSE: draw the UI according to world
;; editview/searchp -> image
(define (render w)
  (cond [(editview? w) (render-edit w)]
        [(searchp? w) (render-search w)]))

;; receive-message
;; PURPOSE: receive message from the server and add it to history
;; editview/searchp string -> editview/searchp
(define (receive-message w m)
  (cond [(editview? w) (receive-edit w m)]
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
              (above HISTORY (overlay/align "left" "top"
                                            (beside (text "" 20 "black") CURSOR)
                                            TEXT)))
(check-expect (render-edit edit1)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "editing" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (render-edit edit2)
              (above (overlay/align "left" "top"
                                    (above/align "left"
                                                 (text "a:a" 20 "black")
                                                 (text "b:b" 20 "black")
                                                 (text "c:c" 20 "black"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "editing" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (render-edit edit3)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "editing" 20 "black") CURSOR)
                                    TEXT)))

(define (render-edit ev)
  (above
   ; I wish I have a function to render the history entries
   (render-edit-history ev)
   ; I wish I have a function to render the editing post
   (render-input (editview-edit ev))))

;; render-edit-history
;; First, I have to process the message: cut the number and ":" and detect errors.
;; Then, I draw each entry and put them in a list.
;; Lastly, I overlay the images in the list.
;; editview -> image
;; examples:
(check-expect (render-edit-history edit0) HISTORY)
(check-expect (render-edit-history edit2)
              (overlay/align "left" "top"
                             (above/align "left"
                                          (text "a:a" 20 "black")
                                          (text "b:b" 20 "black")
                                          (text "c:c" 20 "black"))
                             HISTORY))
(define (render-edit-history ev)
  (overlay/align "left" "top"
                 (arrange-history(draw-edit-message(process-message(editview-history ev))))
                 HISTORY))

;; helper function:
;; process-message:
;; detect whether it is an error or a normal post then delete the id# and ":"
;; list of string -> string
;; examples:
(check-expect (process-message (list "ERROR")) (list "There is an error connecting to the server"))
(check-expect (process-message (list "222:Euler:F" "333:asdf:a")) (list "Euler:F" "asdf:a"))
(define (process-message los)
  (local
    ;; map: [X Y][X -> Y]  [List-of X] -> [List-of Y]
    ;; X = string
    ;; Y = string
    ;; Helper: process-m
    ;; delete the id# and detect errors
    ;; string -> string
    ;; example:
    ;; (check-expect(process-m "ERROR") "There is an error connecting to the server")
    ;; (check-expect (process-m "1:A:b") "A:b")
    ((define (process-m m)
       (if (and (>= (string-length m)5)(string=? (substring m 0 5) "ERROR"))
           "There is an error connecting to the server"
           (remove-number m))))
    (map process-m los)))

;; helper function: remove-number
;; remove the # at the begining of the strings
;; and remove ":" if there is one
;; string -> string
;; examples
(check-expect (remove-number "333:asd:asdf") "asd:asdf")
(define (remove-number m)
  (cond
    [(number? (string->number(substring m 0 1)))
     (remove-number (substring m 1 (string-length m)))]
    [(string=? ":" (substring m 0 1))
     (remove-number (substring m 1 (string-length m)))]
    [else m]))

;; helper function: draw-edit-message
;; turn entries to a image
;; list of string -> image
(define (draw-edit-message los)
  (local
    ;; map: [X Y][X -> Y]  [List-of X] -> [List-of Y]
    ;; X = string
    ;; Y = image
    ;; Helper : text!
    ;; string -> image
    ;; draw a single entry to an image
    ;; example:
    ;; (check-expect (text! "Tu:!") (text "Tu:!" 20 "black")
    ((define (text! m) (text m 20 "black")))
    (map text! los)))

;; helper function: above!
;; overlay two pictures to the left up corner
;; image, image -> image
(define (arrange-history loi)
  (local
    ;; foldr: [X Y] [X Y -> Y] Y [list of X -> Y]
    ;; X = image
    ;; Y = image
    ;; Helper: above!
    ;; place image2 below image1 to the left
    ;; image image -> image
    ;; example:
    ;; (check-expect (above! (circle 3 "solid" "black")(circle 3 "solid" "black"))
    ;;                       (above/align "left"
    ;;                                    (circle 3 "solid" "black")(circle 3 "solid" "black")))
    ((define (above! i1 i2)
       (above/align "left" i1 i2)))
    (foldr above! empty-image loi)))

;; helper function: render-input
;; wordp -> image
(define (render-input wp)
  (overlay/align "left" "top"
                 (beside
                  (text (wordp-l wp) 20 "black")
                  CURSOR
                  (text (wordp-r wp) 20 "black"))
                 TEXT))
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
              (make-package (make-editview (make-wordp "" "") '() (make-wordp "" ""))
                            "editing"))
(check-expect (editmode edit2 "\r")
              (make-package (make-editview
                             (make-wordp "" "")
                             (list "1:a:a" "2:b:b" "3:c:c")
                             (make-wordp "a" ""))
                            "editing"))
(check-expect (editmode edit3 "\r")
              (make-package (make-editview (make-wordp "" "") '() (make-wordp "a" ""))
                            "editing"))

;; 2. backspace:
;; delete the last character in the left part of the edit part in editview
;; it do nothing when it part is empty
(check-expect (editmode edit0 "\b")
              edit0)
(check-expect (editmode edit1 "\b")
              (make-editview (make-wordp "editin" "") '() (make-wordp "" "")))
(check-expect (editmode edit2 "\b")
              (make-editview (make-wordp "editin" "")
                             (list "1:a:a" "2:b:b" "3:c:c")
                             (make-wordp "a" "")))
(check-expect (editmode edit3 "\b")
              (make-editview (make-wordp "editin" "") '() (make-wordp "a" "")))

;; 3. f2
;; switch to search mode
(check-expect (editmode edit0 "f2")
              (make-searchp (make-wordp "" "")'() (make-wordp "" "")))
(check-expect (editmode edit1 "f2")
              (make-searchp (make-wordp "editing" "") '() (make-wordp "" "")))
(check-expect (editmode edit2 "f2")
              (make-searchp (make-wordp "editing" "")
                            (list "1:a:a" "2:b:b" "3:c:c")
                            (make-wordp "a" "")))
(check-expect (editmode edit3 "f2")
              (make-searchp (make-wordp "editing" "") '() (make-wordp "a" "")))

;; 4. left and right arrow
;; moves the cursor

;; 5. other keys (excluding some of the keys such as menu or shift)
;; add a character to the end of the edit part of editview
(check-expect (editmode edit0 "f")
              (make-editview (make-wordp "f" "")'() (make-wordp "" "")))
(check-expect (editmode edit1 "2")
              (make-editview (make-wordp "editing2" "")'() (make-wordp "" "")))
(check-expect (editmode edit1 "shift")
              (make-editview (make-wordp "editing" "")'() (make-wordp "" "")))

(define (editmode ev ke)
  (cond
    [(string=? ke "\r")
     (if (and (string=? (wordp-l(editview-edit ev)) "") (string=? (wordp-r(editview-edit ev)) ""))
         ev
         (make-package
          (make-editview (make-wordp "" "")
                         (editview-history ev)
                         (editview-search ev))
          (string-append (wordp-l(editview-edit ev))(wordp-r(editview-edit ev)))))]
    
    [(string=? ke "\b")
     (make-editview (<=back (editview-edit ev))
                    ; I wish I have a helper function to process the backspace
                    (editview-history ev)
                    (editview-search ev))]
    
    [(string=? ke "f2")
     (make-searchp (editview-edit ev)
                   (editview-history ev)
                   (editview-search ev))]
    
    [(string=? ke "left")
     (make-editview (<-move (editview-edit ev))
                    ; i wish I have a helper function to move the cursor to
                    ; the left
                    (editview-history ev)
                    (editview-search ev))]

    [(string=? ke "right")
     (make-editview (->move (editview-edit ev))
                    ; i wish I have a helper function to move the cursor to
                    ; the right
                    (editview-history ev)
                    (editview-search ev))]

    [(> (string-length ke) 1) ev]

    [else (make-editview (addch (editview-edit ev) ke)
                         ; I wish I have a helper function to add characters to wordp
                         (editview-history ev)
                         (editview-search ev))]))
;=============================================================
;; helper function: <=back
;; wordp -> wordp
;; delete the last character in the left part of wordp
;; examples:
(check-expect (<=back (make-wordp "" "a"))(make-wordp "" "a"))
(check-expect (<=back (make-wordp "kk" "ii")) (make-wordp "k" "ii"))
(define (<=back wp)
  (if (string=? (wordp-l wp) "")
      wp
      (make-wordp
       (substring (wordp-l wp) 0 (- (string-length (wordp-l wp)) 1))
       (wordp-r wp))))

;; helper function: addch
;; wordp string -> wordp
;; add a character to last character in the left part of wrodp
(define (addch wp str)
  (make-wordp
   (string-append (wordp-l wp) str)
   (wordp-r wp)))

;; helper function: <-move
;; wordp -> wordp
;; move the cursor to the left
(define (<-move wp)
  (if (string=? (wordp-l wp) "")
      wp
      (make-wordp (substring (wordp-l wp) 0 (- (string-length (wordp-l wp)) 1))
                  (string-append 
                   (substring (wordp-l wp)
                              (- (string-length (wordp-l wp)) 1)
                              (string-length (wordp-l wp)))
                   (wordp-r wp)))))

;; helper function: ->move
;; wordp -> wordp
;; move the cursor to the right
(define (->move wp)
  (if (string=? (wordp-r wp) "")
      wp
      (make-wordp (string-append (wordp-l wp)(substring (wordp-r wp)0 1))
                  (substring (wordp-r wp) 1 (string-length (wordp-r wp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive-edit
;; editview, string -> editview
;; it renew the editview when receive a message from the server
(check-expect (receive-edit edit0 "4:d:d")
              (make-editview (make-wordp "" "") (list "4:d:d") (make-wordp "" "")))
(check-expect (receive-edit edit2 "4:d:d")
              (make-editview (make-wordp "editing" "")
                             (list "1:a:a" "2:b:b" "3:c:c" "4:d:d")
                             (make-wordp "a" "")))

(define (receive-edit ev m)
  (make-editview (editview-edit ev)
                 (addback m (editview-history ev))
                 ;I wish I have a helper function to add string at the end of a list
                 (editview-search ev)))

;; helper function: addback
;; list of strings, string -> list of strings
;; add a string to the end of the given list of strings
(define (addback x l)
  (cond
    [(empty? l) (cons x '())]
    [(cons? l) (cons (first l) (addback x (rest l)))]))
;-------------------------------------------------------------------
;; searchp brench

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; render-search
;; searchp -> image
;; it shows the search result of given word
;; the results are entries that contain user's input
;; and the same parts in the entries are highlighted in green
(check-expect (render-search search0)
              (above HISTORY (overlay/align "left" "top"
                                            (beside (text "" 20 "black") CURSOR)
                                            TEXT)))
(check-expect (render-search search1)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (render-search search2)
              (above (overlay/align "left" "top"
                                    (beside (text "a" 20 "green")
                                            (text ":" 20 "black")
                                            (text "a" 20 "green"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "a" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (render-search search3)
              (above HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "a" 20 "black") CURSOR)
                                    TEXT)))

(define (render-search sp)
  (above
   (render-search-history sp)
   ; i first pick all the entries that should be appeared on screen
   ; then using process-message to wipe the numbers and ":" at the front
   ; then highlight each entry and store them as image in a list
   ; finally I render each entry
   (render-input (searchp-search sp))))

;; render-search-history
;; searchp -> image
;; i first pick all the entries that should be appeared on screen
;; then using process-message to wipe the numbers and ":" at the front
;; then highlight each entry and store them as image in a list
;; finally I render each entry
;; examples:
(check-expect (render-search-history search0) HISTORY)
(check-expect (render-search-history search2)
              (overlay/align "left" "top"
                             (beside (text "a" 20 "green")
                                     (text ":" 20 "black")
                                     (text "a" 20 "green"))
                             HISTORY))
              
(define (render-search-history sp)
  (overlay/align "left" "top"
                 (arrange-history
                  (highlight-entries
                   (process-message
                    (pick-entries sp))
                   (string-append (wordp-l(searchp-search sp))
                                  (wordp-r(searchp-search sp)))))
                 HISTORY))
;; helper function: pick-entries
;; pick all the entries that contain strings that user inputs
;; I need an argument to contain the string
;; str, list of strings -> list of strings
;; examples:
(check-expect (pick-entries search2) '("1:a:a"))
(define (pick-entries sp)
  (local
    ;; helper function: real-pick-entries
    ;; I need two arguments: one is user's input the other is remaining histories
    ;; string [list of strings] -> [list of strings]
    ((define (real-pick-entries str l)
       (cond
         [(empty? l) '()]
         [(string-contains? str (remove-number(first l)))
          (cons (first l)
                (real-pick-entries str (rest l)))]
         [else (real-pick-entries str (rest l))])))
    (real-pick-entries (string-append (wordp-l(searchp-search sp))
                                      (wordp-r(searchp-search sp)))
                       (searchp-history sp))))

;; helper function: highlight-entries
;; highlights characters that are the same as user's input
;; str, list of strings -> list of images
(define (highlight-entries l s)
  (cond
    [(empty? l) '()]
    [(cons? l) (cons (highlight (first l) s)
                     ; I need a helper function to highlight the characters
                     ; that is given by users
                     (highlight-entries (rest l) s))]))
;; helper function: highlight
;; str, str -> image
;; it highlights characters in a single entries
(define (highlight inlist str)
  (cond
    [(string=? str "") empty-image]
    [(>= (string-length inlist) (string-length str))
     (if (string=? (substring inlist 0 (string-length str)) str)
         (beside (text str 20 "green")
                 (highlight (substring inlist (string-length str) (string-length inlist)) str))
         (beside (text (substring inlist 0 1) 20 "black")
                 (highlight (substring inlist 1 (string-length inlist)) str)))]
    [else (text inlist 20 "black")]))

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
(check-expect (searchmode search1 "\b")
              search1)
(check-expect (searchmode search2 "\b")
              (make-searchp
               (make-wordp "editing" "")
               (list "1:a:a" "2:b:b" "3:c:c")
               (make-wordp "" "")))
(check-expect (searchmode search3 "\b")
              (make-searchp (make-wordp"editing" "")
                            '()
                            (make-wordp "" "")))

;; 2. f1
;; switch to search mode
(check-expect (searchmode search0 "f1")
              (make-editview (make-wordp "" "") '() (make-wordp "" "")))
(check-expect (searchmode search1 "f1")
              (make-editview (make-wordp "editing" "")
                             '()
                             (make-wordp "" "")))
(check-expect (searchmode search2 "f1")
              (make-editview (make-wordp "editing" "")
                             (list "1:a:a" "2:b:b" "3:c:c")
                             (make-wordp "a" "")))
(check-expect (searchmode search3 "f1")
              (make-editview (make-wordp "editing" "")
                             '()
                             (make-wordp "a""")))

;; 3. other keys
;; add a character to the end of the edit part of editview
(check-expect (searchmode search0 "f")
              (make-searchp (make-wordp"" "") '() (make-wordp "f" "")))
(check-expect (searchmode search1 "2")
              (make-searchp (make-wordp "editing" "") '() (make-wordp "2" "")))
(define (searchmode sp ke)
  (cond
    [(string=? ke "\b")
     (make-searchp (searchp-edit sp)
                   (searchp-history sp)
                   (<=back(searchp-search sp)))]
    
    [(string=? ke "f1")
     (make-editview (searchp-edit sp)
                    (searchp-history sp)
                    (searchp-search sp))]
    
    [(string=? ke "left")
     (make-searchp (searchp-edit sp)
                   (editview-history sp)
                   (<-move (editview-search sp)))]

    [(string=? ke "right")
     (make-searchp (editview-edit sp)
                   (editview-history sp)
                   (->move (editview-search sp)))]

    [(> (string-length ke) 1) sp]

    [else (make-searchp (searchp-edit sp)
                        (searchp-history sp)
                        (addch (searchp-search sp) ke))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive-search
;; reveive the message from the server and add it to history
(check-expect (receive-search search0 "4:d:d")
              (make-searchp (make-wordp "" "") (list "4:d:d") (make-wordp "" "")))
(check-expect (receive-search search2 "4:d:d")
              (make-searchp (make-wordp "editing" "")
                            (list "1:a:a" "2:b:b" "3:c:c" "4:d:d")
                            (make-wordp "a" "")))

(define (receive-search ev m)
  (make-searchp (searchp-edit ev)
                (addback m (searchp-history ev))
                (searchp-search ev)))