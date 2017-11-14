;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 8b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;-------------------------------------------------------------------
;; PURPOSE:
;; simiulate pizza
;; function 1 : add post to the server
;; function 2 : receive posts that received from server and display only
;;              the number, id and posts
;; function 3 : when editing, press left or right arrow to move the cursor
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

; A Reply is a (make-reply number string string)
(define-struct reply [author content])
; INTERPRETATION: author and content
; examples:
(define reply1 (make-reply "Kate" "Good morning!"))
(define reply2 (make-reply "Kyle" "Good!"))
(define reply3 (make-reply "Murphy" "cheer!"))

; A Post is a (list Number (make-post number string string [list of replys]))
(define-struct post [id author content reply])
; INTERPRETATION: The ID# of a post, the author,
;                 teh content of a post and list of replys of the post
; examples:
(define post1 (make-post 1 "Alex" "Good morning!" (list reply1 reply2)))
(define post2 (make-post 2 "Benn" "What's up!" (list reply3)))
(define post3 (make-post 3 "Cathrine" "deadline delayed" '()))
; template:
(define (post-temp p)
  (...(post-id p)...(post-author p)...(post-content p)...(reply-temp(post-reply p))...))


; template:
(define (reply-temp p)
  (...(reply-author p)...(reply-content p)))

;; A History is a List of posts or string
;; INTERPRETATION: the history of top-level posts seen so far.
;; examples:
(define h0 '())
(define h1 (list post1 post2))
(define h2 (list post1 post2 post3))
;; template:
#;(define (history-temp h)
    (cond
      [(empty? h) ...]
      [(cons? h)
       (cond
         [(post? (first h)) ...(post-temp (first h))...]
         [(string? (first h)) ...])
       ...(history-temp (rest h))...]))

;; A World is one of
;; - EditviewPosts
;; - SearchPosts
;; INTERPRETATION: Represents two different "views" in your program.

(define-struct editview [edit history search])
;; A EditviewPosts is a (make-editview wordp [list of posts/replys] wordp)
;; INTERPRETATION: Means the user is viewing all posts and
;; potentially typing in a new one.
;; examples:
(define edit0 (make-editview (make-wordp "" "")'() (make-wordp "" "")))
(define edit1 (make-editview (make-wordp "editing" "") (list post1 post2) (make-wordp "" "")))
(define edit2
  (make-editview (make-wordp "editing" "") (list post3) (make-wordp "a" "")))
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
(define search1
  (make-searchp (make-wordp "editing" "") (list post1 post2) (make-wordp "a" "")))
(define search2
  (make-searchp (make-wordp "editing" "") (list post3) (make-wordp "a" "")))
;; template:
#;(define (searchp-temp ev)
    (...(searchp-edit)...
        (history-temp (searchp-history))...
        (searchp-search)....))


  
;-------------------------------------------------------------
;; some constants:
;; Sever:
(define the-server "dictionary.ccs.neu.edu")
;; the port
(define the-port   10009)
;; a previous text window is a empty scene
(define HISTORY (empty-scene 800 800))
;; a editing text window is a smaller empty scene
(define TEXT (empty-scene 800 100))
;; a small rectangle stands for a curosr on the last line
(define CURSOR (rectangle 3 18 "solid" "red"))
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
              (above (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post1-author:Alex- Good morning!" 20 "black")
                                     (text "Reply-author:Kate- Good morning!" 20 "blue")
                                     (text "Reply-author:Kyle- Good!" 20 "blue")
                                     (text "Post2-author:Benn- What's up!" 20 "black")
                                     (text "Reply-author:Murphy- cheer!" 20 "blue"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "editing" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (render-edit edit2)
              (above (overlay/align "left" "top"
                                    (text "Post3-author:Cathrine- deadline delayed" 20 "black")
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "editing" 20 "black") CURSOR)
                                    TEXT)))

(define (render-edit ev)
  (above
   (overlay/align "left" "top"
                  (arrange-history (editview-history ev))
                  HISTORY)
   ; I wish I have a function to render the editing post
   (render-input (editview-edit ev))))

; Helper: above-left
; INTERPRETATION: place an image on another to the left
; example
(check-expect (above-left (circle 10 "solid" "black")
                          (circle 20 "solid" "green"))
              (above/align "left"
                           (circle 10 "solid" "black")
                           (circle 20 "solid" "green")))
; image image -> image
(define (above-left i1 i2)
  (above/align "left" i1 i2))

; Helper: arrage-history
; turn a history to a picture
; history -> image
; examples:
(check-expect (arrange-history '()) empty-image)
(check-expect (arrange-history h1)
              (above/align
               "left"
               (text "Post1-author:Alex- Good morning!" 20 "black")
               (text "Reply-author:Kate- Good morning!" 20 "blue")
               (text "Reply-author:Kyle- Good!" 20 "blue")
               (text "Post2-author:Benn- What's up!" 20 "black")
               (text "Reply-author:Murphy- cheer!" 20 "blue")))

(define (arrange-history history)
  (local
    ; map:
    ; [x y] [x -> y] [list of x] -> [list of y]\
    ; x = history
    ; y = image
    ; foldr:
    ; [x y] [x y -> y] y [list of x] -> y
    ; x = image y = image
    
    ; NEED helper function:
    ; draw-entries:
    ; INTERPRETATION: turn a post to a picture
    #; (check-expect (draw-entries post1)
                     (text "Post1-author:Alex- Good morning!" 20 "black")
                     (text "Reply-author:Kate- Good morning!" 20 "blue")
                     (text "Reply-author:Kyle- Good!" 20 "blue")
                     (text "Post2-author:Benn- What's up!" 20 "black")
                     (text "Reply-author:Murphy- cheer!" 20 "blue"))
    #; (check-expect (draw-entries post2)
                     (text "Post2-author:Benn- What's up!" 20 "black"))
    ; reply / post -> image

    ((define (draw-entries e)
       (above/align
        "left"
        (text
         (string-append "Post"
                        (number->string (post-id e))
                        "-author:"
                        (post-author e)
                        "- "
                        (post-content e))
         20 "black")
        (draw-replies (post-reply e))))
     ; HELPER: draw-replies
     ; arrage replies to one picture
     ; [list of replies] -> image
     ; it just draw all the relies one by one above eachother to the left
     #; (check-expect (draw-replies (list reply1 reply2))
                      (above/align "left"
                                   (text "Reply-author:Kate- Good morning!" 20 "blue")
                                   (text "Reply-author:Kyle- Good!" 20 "blue")))
     (define (draw-replies lop)
       (local
         ; map:
         ; [x y] [x -> y] [list of x] -> [list of y]\
         ; x = reply
         ; y = image
         ; foldr:
         ; [x y] [x y -> y] y [list of x] -> y
         ; x = image y = image

         ; NEED helper:
         ; draw-replies:
         ; INTERPRETATION: turn replies to a picture
         #; (check-expect (draw-replies reply1) (text "Reply-author:Kate- Good morning!" 20 "blue"))
         ((define (draw-replies reply)
            (text (string-append "Reply-author:" (reply-author reply) "- " (reply-content reply))
                  20 "blue")))
         (foldr above-left empty-image (map draw-replies lop)))))
    (foldr above-left empty-image (map draw-entries history))))

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
              (make-package (make-editview (make-wordp "" "") (list post1 post2) (make-wordp "" ""))
                            (list "POST" "editing")))
(check-expect (editmode edit2 "\r")
              (make-package (make-editview
                             (make-wordp "" "")
                             (list post3)
                             (make-wordp "a" ""))
                            (list "POST" "editing")))

;; 2. backspace:
;; delete the last character in the left part of the edit part in editview
;; it do nothing when it part is empty
(check-expect (editmode edit0 "\b")
              edit0)
(check-expect (editmode edit1 "\b")
              (make-editview (make-wordp "editin" "") (list post1 post2) (make-wordp "" "")))
(check-expect (editmode edit2 "\b")
              (make-editview (make-wordp "editin" "")
                             (list post3)
                             (make-wordp "a" "")))

;; 3. f2
;; switch to search mode
(check-expect (editmode edit0 "f2")
              (make-searchp (make-wordp "" "")'() (make-wordp "" "")))
(check-expect (editmode edit1 "f2")
              (make-searchp (make-wordp "editing" "") (list post1 post2) (make-wordp "" "")))
(check-expect (editmode edit2 "f2")
              (make-searchp (make-wordp "editing" "")
                            (list post3)
                            (make-wordp "a" "")))

;; 4. left and right arrow
;; moves the cursor

;; 5. other keys (excluding some of the keys such as menu or shift)
;; add a character to the end of the edit part of editview
(check-expect (editmode edit0 "f")
              (make-editview (make-wordp "f" "")'() (make-wordp "" "")))
(check-expect (editmode edit1 "2")
              (make-editview (make-wordp "editing2" "")(list post1 post2) (make-wordp "" "")))
(check-expect (editmode edit1 "shift")
              (make-editview (make-wordp "editing" "")(list post1 post2) (make-wordp "" "")))

(define (editmode ev ke)
  (cond
    [(string=? ke "\r")
     (cond
       [(string=? "" (append-wordp ev)) ev]
       [else
        (make-package
         (make-editview (make-wordp "" "")
                        (editview-history ev)
                        (editview-search ev))
         (list "POST" (append-wordp ev)))])]
    
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
    [(= 1 (string-length ke))
     (make-editview (addch (editview-edit ev) ke)
                    ; I wish I have a helper function to add characters to wordp
                    (editview-history ev)
                    (editview-search ev))]
    
    [else ev]))


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
;; editview, ServerMsg -> editview
;; it renew the editview when receive a message from the server
(check-expect (receive-edit edit0 (list "POST" 4 "David" "Test") )
              (make-editview (make-wordp "" "")
                             (list (make-post 4 "David" "Test" '()))
                             (make-wordp "" "")))
(check-expect (receive-edit edit2 (list "ERROR" "invalid input"))
              (make-editview (make-wordp "editing" "")
                             (list post3 "ERROR:invalid input")
                             (make-wordp "a" "")))

(define (receive-edit ev m)
  (cond
    [(string=? "POST"(first m))
     (make-editview (editview-edit ev)
                    (addback (make-post
                              (second m)
                              (third m)
                              (fourth m) '())
                             (editview-history ev))
                    ;I wish I have a helper function to add a post or an error at the end of a list
                    (editview-search ev))]
    [(string=? "ERROR" (first m))
     (make-editview (editview-edit ev)
                    (addback (string-append "ERROR:" (second m)) (editview-history ev))
                    (editview-search ev))]))

;; helper function: addback
;; list of post, Post/error -> list of post
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
              (above (overlay/align
                      "left" "top"
                      (above/align
                       "left"
                       (text "Post2-author:Benn- What's up!" 20 "black")
                       (text "Reply-author:Murphy- cheer!" 20 "blue"))
                      HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "a" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (render-search search2)
              (above (overlay/align
                      "left" "top"
                      (text "Post3-author:Cathrine- deadline delayed" 20 "black")
                      HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "a" 20 "black") CURSOR)
                                    TEXT)))

(define (render-search sp)
  (above
   (overlay/align "left" "top"
                  (arrange-search-results sp)
                  ; I need a helper to draw the search-results
                  HISTORY)
   (render-input (searchp-search sp))))

; helper: arrange-search-results
; searchp -> image
; find out the entries that contain user's search and print them out
(check-expect (arrange-search-results search0) empty-image)
(check-expect (arrange-search-results search1)
              (above/align
               "left"
               (text "Post2-author:Benn- What's up!" 20 "black")
               (text "Reply-author:Murphy- cheer!" 20 "blue")))
(check-expect (arrange-search-results search2)
              (text "Post3-author:Cathrine- deadline delayed" 20 "black"))

(define (arrange-search-results sp)
  (cond
    [(string=? (append-wordp sp) "") empty-image]
    [else
     (local
       ; filter: [x] [x -> boolean] [list of x] -> [list of x]
       ; x = post
       ; HELPER:
       ; find-entries
       ; post -> boolean
       ; it only detect whether there is a user's searching word in the post but not in relies
       ; (check-expect (find-entries post2) #t)
       ((define (find-entries post)
          (or (string-contains? (append-wordp sp)(number->string(post-id post)))
              (string-contains? (append-wordp sp)(post-author post))
              (string-contains? (append-wordp sp)(post-content post)))))
       (arrange-history (filter find-entries (searchp-history sp))))]))

; Helper: append-wordp
; editview / searchp -> string
; INTERPRETATION: takes in a editview or searchp and give out whats in the user input
(check-expect (append-wordp edit1) "editing")
(check-expect (append-wordp search1) "a")
(define (append-wordp x)
  (cond
    [(editview? x) (string-append (wordp-l (editview-edit x)) (wordp-r (editview-edit x)))]
    [(searchp? x) (string-append (wordp-l (searchp-search x)) (wordp-r (searchp-search x)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search-mode:
;; searchp -> editview / searchp
;; it takes in a searchp and react according to user's key action
;; examples:

;; 1. backspace:
;; delete the last character in the search part of searchp
;; it do nothing when the search part is empty
(check-expect (searchmode search0 "\b")
              search0)
(check-expect (searchmode search1 "\b")
              (make-searchp
               (make-wordp "editing" "")
               (list post1 post2)
               (make-wordp "" "")))
(check-expect (searchmode search2 "\b")
              (make-searchp
               (make-wordp "editing" "")
               (list post3)
               (make-wordp "" "")))

;; 2. f1
;; switch to search mode
(check-expect (searchmode search0 "f1")
              (make-editview (make-wordp "" "") '() (make-wordp "" "")))
(check-expect (searchmode search1 "f1")
              (make-editview (make-wordp "editing" "")
                             (list post1 post2)
                             (make-wordp "a" "")))
(check-expect (searchmode search2 "f1")
              (make-editview (make-wordp "editing" "")
                             (list post3)
                             (make-wordp "a" "")))

;; 3. other keys
;; add a character to the end of the edit part of editview
(check-expect (searchmode search0 "f")
              (make-searchp (make-wordp"" "") '() (make-wordp "f" "")))
(check-expect (searchmode search1 "2")
              (make-searchp (make-wordp "editing" "") (list post1 post2) (make-wordp "a2" "")))
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

    [(= (string-length ke) 1)
     (make-searchp (searchp-edit sp)
                   (searchp-history sp)
                   (addch (searchp-search sp) ke))]

    [else (make-searchp (searchp-edit sp)
                        (searchp-history sp)
                        (addch (searchp-search sp) ke))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; receive-search
;; reveive the message from the server and add it to history
;; searchp, servermsg -> searchp
(check-expect (receive-search search0 (list "POST" 4 "VK" "homework"))
              (make-searchp (make-wordp "" "")
                            (list (make-post 4 "VK" "homework" '()))
                            (make-wordp "" "")))
(check-expect (receive-search search1 (list "POST" 5 "Kevin" "Assignment"))
              (make-searchp (make-wordp "editing" "")
                            (list post1
                                  post2
                                  (make-post 5 "Kevin" "Assignment" '()))
                            (make-wordp "a" "")))

(define (receive-search sp m)
  (cond
    [(string=? (first m) "POST")
     (make-searchp (searchp-edit sp)
                   (addback (make-post
                             (second m)
                             (third m)
                             (fourth m) '())
                            (searchp-history sp))
                   (searchp-search sp))]
    [(string=? (first m) "ERROR")
     (make-searchp (searchp-edit sp)
                   (addback (string-append "ERROR" (second m)) (searchp-history sp))
                   (searchp-search sp))]))
(simple-net-forum 3)
