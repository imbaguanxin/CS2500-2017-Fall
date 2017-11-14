;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 9b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)
;;-------------------------------------------------------------------
;; PURPOSE:
;; simiulate pizza

;---------------------------------------------------------------------------
; A wordp (word processor) is (make-wordp string string)
; it is the structure of the word processor
; it devide the post and search into two parts
; and I am going to insert an cursor between the two part
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
(define reply4 (make-reply "Max" "Interesting"))
; template:
(define (reply-temp p)
  (...(reply-author p)...(reply-content p)))

; A Post is a (list Number (make-post number string string [list of replys]))
(define-struct post [id author content reply])
; INTERPRETATION: The ID# of a post, the author,
;                 teh content of a post and list of replys of the post
; examples:
(define post1 (make-post 1 "Alex" "Good morning!" (list reply1 reply2)))
(define post1.1 (make-post 1 "Alex" "Good morning!" (list reply1 reply2 reply4)))
(define post2 (make-post 2 "Benn" "What's up!" (list reply3)))
(define post3 (make-post 3 "Cathrine" "deadline delayed" '()))
; template:
(define (post-temp p)
  (...(post-id p)...(post-author p)...(post-content p)...(reply-temp(post-reply p))...))

;; A History is one of
;; - List of posts
;; - string
;; INTERPRETATION: the history of top-level posts seen so far. the string represents error message.
;; examples:
(define h0 '())
(define h1 (list post1 post2))
(define h2 (list post1 post2 post3))
(define h1.1 (list post1.1 post2))
(define h2.2 (list post1.1 post2 post3))
;; template:
#;(define (history-temp h)
    (cond
      [(empty? h) ...]
      [(cons? h)
       (cond
         [(post? (first h)) ...(post-temp (first h))...]
         [(string? (first h)) ...])
       ...(history-temp (rest h))...]))

;-----------------------------------------------------------------
; A World is one of
; - Viewall
; - Threadview
; - Newitem
; - Search
; INTERPRETATION: Represents four different "views" in your program.
 
; A Viewall is a (make-viewall Wordp History)
; INTERPRETATION: The user is viewing all posts (but not replies), 
;   and possibly typing a Command.
(define-struct viewall [command history])
; examples:
(define viewall0 (make-viewall (make-wordp """") h0))
(define viewall1 (make-viewall (make-wordp """") h1))
(define viewall2 (make-viewall (make-wordp "new""") h2))
(define viewall3 (make-viewall (make-wordp "view 2" "") h2))
(define viewall4 (make-viewall (make-wordp "reply ""1") h2))
(define viewall5 (make-viewall (make-wordp "CATCHUP""")h0))

; A Threadview is a (make-threadview Post History)
; INTERPRETATION: The user is viewing a specific Post and its replies.
(define-struct threadview [viewing history])
; examples:
(define threadview0 (make-threadview '() h0))
(define threadview1 (make-threadview '() h2))
(define threadview2 (make-threadview post2 h2))

; A Newitem is a (make-newitem [Maybe Natural] Wordp History)
; INTERPRETATION: The user is entering a new post (if the maybe is #false),
;   or a new reply (if the maybe is a number).
(define-struct newitem [id content history])
; examples:
(define newitemP0 (make-newitem #f (make-wordp """") h0))
(define newitemP1 (make-newitem #f (make-wordp "Homework""") h1))
(define newitemR0 (make-newitem 1 (make-wordp """") h1))
(define newitemR1 (make-newitem 1 (make-wordp "Yes""") h1))

; A Search is a (make-search History Wordp)
; INTERPRETATION: The user is trying to view only a subset
;   of the existing messages.
(define-struct search [history search])
(define search0.0 (make-search h0 (make-wordp """")))
(define search0 (make-search h2 (make-wordp """")))
(define search1 (make-search h2 (make-wordp "Benn""")))
(define search2 (make-search h2 (make-wordp "3""")))
(define search3 (make-search h2 (make-wordp "cheer!""")))
(define search4 (make-search h2 (make-wordp "xxx""")))
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
(define CURSOR (rectangle 2 18 "solid" "red"))
;-------------------------------------------------------------
; main function:
; world -> world (viewall/newitem/threadview/search)/package
(define (simple-net-forum dont-care)
  (big-bang viewall0
            [name            "guan.xin:1968"]
            [register        the-server]
            [port            the-port]
            [on-key          keyhandler]      ;world key -> viewall/newitem/threadview/search/package
            [to-draw         render]          ;world -> image
            [on-receive      receive-message]))  ;world -> viewall/newitem/threadview/search 

;------------------------------------------------------

;; I need to split the function according to world.
;; when on-key, to-draw, on-receive is called
;; I have to go to different brench.
;; examples are in sub-brenches

;; keyhandler:
;; PURPOSE: respond to different key
;; world -> world
(define (keyhandler w ke)
  (cond [(viewall? w) (viewall-key w ke)]
        [(threadview? w) (threadview-key w ke)]
        [(newitem? w) (newitem-key w ke)]
        [(search? w) (search-key w ke)]))

;; render:
;; PURPOSE: draw the UI according to world
;; viewall/threadview/newitem/search -> image
(define (render w)
  (cond [(viewall? w) (viewall-render w)]
        [(threadview? w) (threadview-render w)]
        [(newitem? w) (newitem-render w)]
        [(search? w) (search-render w)]))

;; receive-message
;; PURPOSE: receive message from the server and add it to history
;; viewall/searchp string -> viewall/searchp
(define (receive-message w m)
  (cond [(viewall? w) (viewall-receive w m)]
        [(threadview? w) (threadview-receive w m)]
        [(newitem? w) (newitem-receive w m)]
        [(search? w) (search-receive w m)]))

;; append-wordp
;; PURPOSE: append wordps in all worlds
;; viewall/newitem/search -> string
(define (append-wordp x)
  (cond
    [(viewall? x) (string-append (wordp-l (viewall-command x)) (wordp-r (viewall-command x)))]
    [(newitem? x) (string-append (wordp-l (newitem-content x)) (wordp-r (newitem-content x)))]
    [(search? x) (string-append (wordp-l (search-search x)) (wordp-r (search-search x)))]))
;---------------------------------------------------
; keyhandler section:
; 1.VIEWALL HANDLER
; PURPOSE:
; 1. handle command: press enter and the keyhandler detects the command
(check-expect (viewall-key viewall0 "\r") viewall0)
(check-expect (viewall-key viewall1 "\r") viewall1)
(check-expect (viewall-key viewall2 "\r")
              (make-newitem #f (make-wordp """") (viewall-history viewall2)))
(check-expect (viewall-key viewall3 "\r")
              (make-threadview post2 (viewall-history viewall3)))
(check-expect (viewall-key viewall4 "\r")
              (make-newitem 1 (make-wordp """") (viewall-history viewall4)))
(check-expect (viewall-key viewall5 "\r")
              (make-package (make-viewall (make-wordp """") (viewall-history viewall5)) "CATCHUP"))
; 2. backspace, moving cursor: press backspace to delete one character before the cursor
;                              press left or right error to move the cursor
(check-expect (viewall-key viewall0 "\b") viewall0)
(check-expect (viewall-key viewall2 "\b") (make-viewall (make-wordp "ne""") h2))
(check-expect (viewall-key viewall0 "left") viewall0)
(check-expect (viewall-key viewall4 "left") (make-viewall (make-wordp "reply"" 1") h2))
(check-expect (viewall-key viewall0 "right") viewall0)
(check-expect (viewall-key viewall4 "right") (make-viewall (make-wordp "reply 1""") h2))
; 3. f2: go to search mode
(check-expect (viewall-key viewall3 "f2") (make-search h2 (make-wordp "" "")))
; 4. add a character
(check-expect (viewall-key viewall0 "a") (make-viewall (make-wordp "a" "") h0))
(check-expect (viewall-key viewall3 "3") (make-viewall (make-wordp "view 23" "")h2))
; Viewall, String -> world/package
(define (viewall-key va ke)
  (cond
    
    [(string=? ke "\r")
     (cond
       ; I wish I had a helper to detect whether the command is "new"
       [(command-new? (append-wordp va))
        (make-newitem #f (make-wordp "" "") (viewall-history va))]
       
       ; I wish I had a helper to detect whether the command is "view ..."
       [(command-view? (append-wordp va))
        ; I wish I had a hepler to change viewall to threadview
        (search-viewall-history va)]
       
       ; I wish I had a helper to detect whether the command is "reply ..."
       [(command-reply? (append-wordp va))
        ; I wish I had a hepler to change viewall to newitem
        (write-reply va)]
       
       ; I wish I had a helper to detect whether the command is "CATCHUP"
       [(command-catchup? (append-wordp va))
        (make-package (make-viewall (make-wordp "" "") (viewall-history va))
                      "CATCHUP")]
       [else va])]
    
    [(string=? ke "\b")
     (make-viewall (<=back (viewall-command va))
                   ; I wish I have a helper function to process the backspace
                   (viewall-history va))]
    
    [(string=? ke "f2")
     (make-search (viewall-history va)(make-wordp "" ""))]
    
    [(string=? ke "left")
     (make-viewall (<-move (viewall-command va))
                   ; i wish I have a helper function to move the cursor to
                   ; the left
                   (viewall-history va))]

    [(string=? ke "right")
     (make-viewall (->move (viewall-command va))
                   ; i wish I have a helper function to move the cursor to
                   ; the right
                   (viewall-history va))]
    
    [(= 1 (string-length ke))
     (make-viewall (addch (viewall-command va) ke)
                   ; I wish I have a helper function to add characters to wordp
                   (viewall-history va))]
    
    [else va]))


;=============================================================
;; helper function: command-new?
;; string -> boolean
;; detect whether a command means to create a new post
;; examples:
(check-expect (command-new? "new") #t)
(check-expect (command-new? "new?") #f)
(define (command-new? str)
  (and (= (string-length str) 3 )
       (string=? "new" (first(string-split str)))))

;; helper function: command-view?
;; string -> boolean
;; detect whether a command means to view a specific post
;; examples:
(check-expect (command-view? "view 3") #t)
(check-expect (command-view? "view d") #f)
(define (command-view? str)
  (and (= (length (string-split str)) 2)
       (string=? "view" (first(string-split str)))
       (number? (string->number (second(string-split str))))))

;; helper function: search-viewall-history
;; create a threadview according to user's input
;; if there is no such post in history, the post position remain empty
;; examples:
(check-expect (search-viewall-history viewall3) threadview2)
(check-expect (search-viewall-history (make-viewall (make-wordp "view 4" "") h2))
              threadview1)
(define (search-viewall-history va)
  (local
    ; Need helper: search-post
    ; viewall -> post / '()
    ; find a post according to user's input
    ; if can't find, return '()
    ((define (search-post va)
       (local
         ; filter: [X] [X -> boolean] [List of X]->[List of X]
         ; X = post
         ; Helper: id=?
         ; post -> boolean
         ; compare the idnumber of a post and given number
         ((define (id=? post)
            (if (string? post)
                #f
                (= (post-id post)
                   (string->number (second(string-split (append-wordp va))))))))
         (cond
           [(empty? (filter id=? (viewall-history va))) '()]
           [else (first (filter id=? (viewall-history va)))]))))
    (make-threadview (search-post va)
                     (viewall-history va))))

;; helper function: command-reply?
;; string -> boolean
;; detect whether a command means to create a new reply
;; examples:
(check-expect (command-reply? "reply") #f)
(check-expect (command-reply? "reply ds")#f)
(check-expect (command-reply? "reply 3") #t)
(define (command-reply? str)
  (and (= (length (string-split str)) 2)
       (string=? "reply" (first(string-split str)))
       (number? (string->number (second(string-split str))))))

;; helper function: write-reply
;; viewall -> newitem
;; make a newitem according to user's input
;; examples:
(check-expect (write-reply viewall4) (make-newitem 1 (make-wordp """") h2))
(define (write-reply va)
  (local
    ((define number
       (string->number (second (string-split (append-wordp va))))))
    (make-newitem number
                  (make-wordp "" "")
                  (viewall-history va))))

;; helper function: command-catchup?
;; string -> boolean
;; detect whether a command means to catchup
;; examples:
(check-expect (command-catchup? "catchup") #f)
(check-expect (command-catchup? "CATCHUP") #t)
(define (command-catchup? str)
  (and (= (length (string-split str)) 1)
       (string=? (first(string-split str)) "CATCHUP")))

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

;-------------------------------------------------------------------------------
; 2 threadview HANDLER
; PURPOSE: help user get back to viewall state, press f1 to go back, f2 to search mode
(check-expect (threadview-key threadview0 "f1") viewall0)
(check-expect (threadview-key threadview1 "f1") (make-viewall (make-wordp """") h2))
(define (threadview-key tv ke)
  (cond
    [(string=? ke "f1") (make-viewall (make-wordp """") (threadview-history tv))]
    [(string=? ke "f1") (make-search (threadview-history tv)(make-wordp "" ""))]
    [else tv]))
;---------------------------------------------------------------------------------
; 3.newitem HANDLER
; PURPOSE:
; 1. send a ServerMSG to server (make a package)
(check-expect (newitem-key newitemP0 "\r") newitemP0)
(check-expect (newitem-key newitemP1 "\r") (make-package (make-viewall (make-wordp """") h1)
                                                         (list "POST" "Homework")))
(check-expect (newitem-key newitemR0 "\r") newitemR0)
(check-expect (newitem-key newitemR1 "\r") (make-package (make-newitem 1 (make-wordp """") h1)
                                                         (list "REPLY" 1 "Yes")))
; 2. backspace, moving cursor: press backspace to delete one character before the cursor
;                              press left or right error to move the cursor
; 3. f1,f2: press f1 to return to a empty viewall world (history will remain)
;           press f2 to go to search mode
(check-expect (newitem-key newitemP0 "f1") viewall0)
(check-expect (newitem-key newitemP1 "f1") viewall1)
(check-expect (newitem-key newitemR0 "f2") (make-search h1 (make-wordp "" "")))
(check-expect (newitem-key newitemR1 "f2") (make-search h1 (make-wordp "" "")))
; 4. add a character
(define (newitem-key n ke)
  (cond
    
    [(string=? ke "\r")
     ; I wish I had a helper to send SeverMSG
     (send-message n)]
    
    [(string=? ke "\b")
     (make-newitem (newitem-id n)
                   (<=back (newitem-content n))
                   (newitem-history n))]
    
    [(string=? ke "f1")
     (make-viewall (make-wordp "" "")(newitem-history n))]
    
    [(string=? ke "f2")
     (make-search (newitem-history n)(make-wordp "" ""))]
    
    [(string=? ke "left")
     (make-newitem (newitem-id n)
                   (<-move (newitem-content n))
                   (newitem-history n))]

    [(string=? ke "right")
     (make-newitem (newitem-id n)
                   (->move (newitem-content n))
                   (newitem-history n))]
    
    [(= 1 (string-length ke))
     (make-newitem (newitem-id n)
                   (addch (newitem-content n) ke)
                   (newitem-history n))]
    
    [else n]))
;============================================================================
; helper function: send-message
; takes in a newitem and send a package, contaning different ServerMSG
; if content is empty, it returns what it take
; if it is sending a post, it return to viewall
; if it is sending a reply, it remains in a empty newitem 
; newitem -> package/newitem
(check-expect (send-message newitemP0) newitemP0)
(check-expect (send-message newitemP1) (make-package (make-viewall (make-wordp """") h1)
                                                     (list "POST" "Homework")))
(check-expect (send-message newitemR0) newitemR0)
(check-expect (send-message newitemR1) (make-package (make-newitem 1 (make-wordp """") h1)
                                                     (list "REPLY" 1 "Yes")))
(define (send-message n)
  (if (string=? "" (append-wordp n))
      n
      (if (number? (newitem-id n))
          (make-package (make-newitem (newitem-id n)
                                      (make-wordp """")
                                      (newitem-history n))
                        (list "REPLY"
                              (newitem-id n)
                              (append-wordp n)))
          (make-package (make-viewall (make-wordp "" "")
                                      (newitem-history n))
                        (list "POST" (append-wordp n))))))
;-------------------------------------------------------------------------------
; 4. search HANDLER
; PURPOSE: enter characters or go back to viewall
; 1. backspace, moving cursor: press backspace to delete one character before the cursor
;                              press left or right error to move the cursor
; 2. f1: press f1 to go to viewall mode
(check-expect (search-key search3 "f1") (make-viewall (make-wordp "" "")h2)) 
; 3. add a character
(define (search-key s ke)
  (cond
    
    [(string=? ke "\b")
     (make-search  (search-history s)
                   (<=back (search-search s)))]
    
    [(string=? ke "f1")
     (make-viewall (make-wordp "" "")(search-history s))]
    
    [(string=? ke "left")
     (make-search (search-history s)
                  (<-move (search-search s)))]

    [(string=? ke "right")
     (make-search (search-history s)
                  (->move (search-search s)))]
    
    [(= 1 (string-length ke))
     (make-search (search-history s)
                  (addch (search-search s) ke))] 
    
    [else s]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; render section:
; 1. viewall-render
; PURPOSE:
; draws how the forum shoud look like
; it display previous posts on the history window
; and command on the edit window
; editview -> image
; exampls:
(check-expect (viewall-render viewall0)
              (above (text "ViewAll Mode" 25 "sky blue")
                     HISTORY (overlay/align "left" "top"
                                            (beside (text "" 20 "black") CURSOR)
                                            TEXT)))
(check-expect (viewall-render viewall2)
              (above (text "ViewAll Mode" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post1-author:Alex- Good morning!" 20 "black")
                                     (text "Post2-author:Benn- What's up!" 20 "black")
                                     (text "Post3-author:Cathrine- deadline delayed" 20 "black"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "new" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (viewall-render viewall4)
              (above (text "ViewAll Mode" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post1-author:Alex- Good morning!" 20 "black")
                                     (text "Post2-author:Benn- What's up!" 20 "black")
                                     (text "Post3-author:Cathrine- deadline delayed" 20 "black"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "reply " 20 "black") CURSOR (text "1" 20 "black"))
                                    TEXT)))

(define (viewall-render va)
  (above (text "ViewAll Mode" 25 "sky blue")
         (overlay/align "left" "top"
                        (render-post (viewall-history va))
                        HISTORY)
         (render-input (viewall-command va))))

;; helper function: render post
;; turn a history to a picture
;; history -> image
(check-expect (render-post '()) empty-image)
(check-expect (render-post h1)
              (above/align
               "left"
               (text "Post1-author:Alex- Good morning!" 20 "black")
               (text "Post2-author:Benn- What's up!" 20 "black")))
(define (render-post history)
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
    #;(check-expect (draw-entries post1)
                    (above/align "left"
                                 (text "Post1-author:Alex- Good morning!" 20 "black")))
    #;(check-expect (draw-entries post2)
                    (above/align "left"
                                 (text "Post2-author:Benn- What's up!" 20 "black")))
    #;(check-expect (draw-entries "ERROR: SeverMSG")
                    (text "ERROR: SeverMSG" 20 "red"))
    ; reply / post -> image

    ((define (draw-entries e)
       (if (post? e)
           (text
            (string-append "Post"
                           (number->string (post-id e))
                           "-author:"
                           (post-author e)
                           "- "
                           (post-content e))
            20 "black")
           (text e 20 "red"))))
    (foldr above-left empty-image (map draw-entries history))))

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

;; helper function: render-input
;; wordp -> image
(define (render-input wp)
  (overlay/align "left" "top"
                 (beside
                  (text (wordp-l wp) 20 "black")
                  CURSOR
                  (text (wordp-r wp) 20 "black"))
                 TEXT))
;---------------------------------------------------------
; 2. threadview-render
; PURPOSE: render a given post with its replies
; threadview -> image
(check-expect (threadview-render threadview0)
              (above (text "ThreadView Mode" 25 "sky blue")
                     HISTORY
                     TEXT))
(check-expect (threadview-render threadview2)
              (above (text "ThreadView Mode" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post2-author:Benn- What's up!" 20 "black")
                                     (text "Reply-author:Murphy- cheer!" 15 "blue"))
                                    HISTORY)
                     TEXT))
(define (threadview-render tv)
  (above (text "ThreadView Mode" 25 "sky blue")
         (overlay/align "left" "top"
                        (render-post-replies (threadview-viewing tv))
                        HISTORY)
         TEXT))
; Helper: render-post-replies
; takes in a post/error and render post and its replies
; post -> image
(check-expect (render-post-replies post1)
              (above/align "left"
                           (text "Post1-author:Alex- Good morning!" 20 "black")
                           (text "Reply-author:Kate- Good morning!" 15 "blue")
                           (text "Reply-author:Kyle- Good!" 15 "blue")))
(check-expect (render-post-replies post2)
              (above/align "left"
                           (text "Post2-author:Benn- What's up!" 20 "black")
                           (text "Reply-author:Murphy- cheer!" 15 "blue")))
(define (render-post-replies p)
  (if (empty? p)
      empty-image
      (if (post? p)
          (above/align
           "left"
           (text
            (string-append "Post"
                           (number->string (post-id p))
                           "-author:"
                           (post-author p)
                           "- "
                           (post-content p))
            20 "black")
           (draw-replies (post-reply p)))
          (text p 20 "red"))))

; Helper: draw-replies
; takes in a list of replies and draw them out
; listof replies -> image
(check-expect (draw-replies (list reply1 reply2 reply3))
              (above/align "left"
                           (text "Reply-author:Kate- Good morning!" 15 "blue")
                           (text "Reply-author:Kyle- Good!" 15 "blue")
                           (text "Reply-author:Murphy- cheer!" 15 "blue")))
(define (draw-replies lor)
  (local
    ; map:
    ; [x y] [x -> y] [list of x] -> [list of y]
    ; x = reply
    ; y = image
    ; foldr:
    ; [x y] [x y -> y] y [list of x] -> y
    ; x = image y = image

    ; NEED helper:
    ; draw-replies:
    ; INTERPRETATION: turn replies to a picture
    ((define (draw-replies reply)
       (text (string-append "Reply-author:" (reply-author reply) "- " (reply-content reply))
             15 "blue")))
    (foldr above-left empty-image (map draw-replies lor))))
;-------------------------------------------------
; 3. newitem-render
; PURPOSE: render the creating post or entry
;          when creating a new post, the title shows that it is creating a new post
;          and the history window shows all posts in history
;          when replying to a post, the title shows that it is replying to a specific post
;          and the history window shows the post and replies
; newitem -> image
; examples:
(check-expect (newitem-render newitemP0)
              (above (text "NewItem mode:creating a new post" 25 "sky blue")
                     HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (newitem-render newitemP1)
              (above (text "NewItem mode:creating a new post" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post1-author:Alex- Good morning!" 20 "black")
                                     (text "Post2-author:Benn- What's up!" 20 "black"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "Homework" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (newitem-render newitemR0)
              (above (text "NewItem mode:replying to Post #1" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post1-author:Alex- Good morning!" 20 "black")
                                     (text "Reply-author:Kate- Good morning!" 15 "blue")
                                     (text "Reply-author:Kyle- Good!" 15 "blue"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (newitem-render newitemR1)
              (above (text "NewItem mode:replying to Post #1" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (above/align
                                     "left"
                                     (text "Post1-author:Alex- Good morning!" 20 "black")
                                     (text "Reply-author:Kate- Good morning!" 15 "blue")
                                     (text "Reply-author:Kyle- Good!" 15 "blue"))
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "Yes" 20 "black") CURSOR)
                                    TEXT)))
              
              
(define (newitem-render n)
  (above (title n)
         (overlay/align "left" "top"
                        (render-newitem n)
                        HISTORY)
         (render-input (newitem-content n))))

; helper: title
; PURPOSE: build the title according to the id of a n
; newitem -> image
(check-expect (title newitemP0) (text "NewItem mode:creating a new post" 25 "sky blue"))
(check-expect (title newitemR0) (text "NewItem mode:replying to Post #1" 25 "sky blue"))
(define (title n)
  (if (number? (newitem-id n))
      (text (string-append "NewItem mode:replying to Post #"
                           (number->string(newitem-id n)))
            25 "sky blue")
      (text "NewItem mode:creating a new post" 25 "sky blue")))

; helper: render-newitem
; PURPOSE: render the history window's content according to the id of a n
; newitem -> image
(check-expect (render-newitem newitemP0) empty-image)
(check-expect (render-newitem newitemR0) (above/align
                                          "left"
                                          (text "Post1-author:Alex- Good morning!" 20 "black")
                                          (text "Reply-author:Kate- Good morning!" 15 "blue")
                                          (text "Reply-author:Kyle- Good!" 15 "blue")))
(define (render-newitem n)
  (if (number? (newitem-id n))
      (render-post-replies (search-post-n n))
      (render-post (newitem-history n))))

; Need helper: search-post-n
; newitem -> post / '()
; find a post according to user's input
; if can't find, return '()
(check-expect (search-post-n newitemR0) post1)
(check-expect (search-post-n newitemR1) post1)
(define (search-post-n n)
  (local
    ; filter: [X] [X -> boolean] [List of X]->[List of X]
    ; X = post
    ; Helper: id=?
    ; post -> boolean
    ; compare the idnumber of a post and given number
    ((define (id=? post)
       (if (string? post)
           #f
           (= (post-id post)
              (newitem-id n)))))
    (cond
      [(empty? (filter id=? (newitem-history n))) '()]
      [else (first (filter id=? (newitem-history n)))])))

;---------------------------------------------------------
; 4. search-render
; search -> image
; find the post and render its post and replies
(check-expect (search-render search0)
              (above (text "Search Mode" 25 "sky blue")
                     HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (search-render search1)
              (above (text "Search Mode" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (render-post-replies post2)
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "Benn" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (search-render search2)
              (above (text "Search Mode" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (render-post-replies post3)
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "3" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (search-render search3)
              (above (text "Search Mode" 25 "sky blue")
                     (overlay/align "left" "top"
                                    (render-post-replies post2)
                                    HISTORY)
                     (overlay/align "left" "top"
                                    (beside (text "cheer!" 20 "black") CURSOR)
                                    TEXT)))
(check-expect (search-render search4)
              (above (text "Search Mode" 25 "sky blue")
                     HISTORY
                     (overlay/align "left" "top"
                                    (beside (text "xxx" 20 "black") CURSOR)
                                    TEXT)))

(define (search-render s)
  (above
   (text "Search Mode" 25 "sky blue")
   (overlay/align "left" "top"
                  (render-search-result s)
                  HISTORY)
   (render-input (search-search s))))

(check-expect (render-search-result search0) empty-image)
(check-expect (render-search-result search1) (render-post-replies post2))
(check-expect (render-search-result search2) (render-post-replies post3))
(check-expect (render-search-result search3) (render-post-replies post2))
(check-expect (render-search-result search4) empty-image)

(define (render-search-result s)
  (cond
    [(string=? (append-wordp s) "") empty-image]
    [else
     (local
       ; filter: [x] [x -> boolean] [listof x] -> [listof x]
       ; x = post/error
       ; map: [x y] [x -> y] [list of x] -> [list of y]
       ; x = post/error y = image
       ; foldr: [x y] [x -> y] y [list of X] -> y
       ; x = image y = image
       ; HELPER: find-entries
       ; post/error -> boolean
       ((define (find-entries post)
          (if (string? post)
              (string-contains?  post (append-wordp s))
              (or (string-contains? (number->string(post-id post))(append-wordp s))
                  (string-contains? (post-author post)(append-wordp s))
                  (string-contains? (post-content post)(append-wordp s))
                  (scan-replies (post-reply post)))))
        ; helper: scan-replies
        ; list of replies -> boolean
        (define (scan-replies lor)
          (local
            ; ormap: [X] [X -> boolean] [listof x] -> boolean
            ; helper: good-reply?
            ; reply -> boolean
            ((define (good-reply? r)
               (or (string-contains? (reply-author r)(append-wordp s))
                   (string-contains? (reply-content r)(append-wordp s)))))
            (ormap good-reply? lor))))
       (foldr above-left
              empty-image
              (map render-post-replies (filter find-entries (search-history s)))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Receive section
; graded helper

; replace-post
; searches for a Post in the given History that has the same
; ID# as the given Post, and replaces it with the given Post.
; history post -> post
(check-expect (replace-post h1 (make-post 2 "Max" "Game" '()))
              (list post1 (make-post 2 "Max" "Game" '())))
(define (replace-post history post)
  (local
    ; map: [x y] [x -> y] {list of x -> list of y}
    ; x = post y = post
    ; HELPER:
    ; replace-old:
    ; if the examing post id equals to that of the given post
    ; -> given post, else return the examed post
    ; post -> post
    ; (check-expect (replace-old post2) (make-post 2 "Max" "Game" '())) 
    ((define (replace-old p)
       (cond
         [(string? p) p]
         [(= (post-id p) (post-id post)) post]
         [else p])))
    (map replace-old history)))

; add-reply:
; takes a current History and a "REPLY" ServerMsg,
; and adds a Reply to the relevant Post in the History
; history servermsg -> history
(check-expect (add-reply h1 (list "REPLY" 2 "name" "content"))
              (list
               (make-post 1 "Alex" "Good morning!"
                          (list (make-reply "Kate" "Good morning!")(make-reply "Kyle" "Good!")))
               (make-post 2 "Benn" "What's up!"
                          (list (make-reply "Murphy" "cheer!")(make-reply "name" "content")))))
(check-expect (add-reply h1 (list "REPLY" 4 "name" "content")) h1)
                                                                                              
(define (add-reply history replymsg)
  (local
    ; filter: [x] [x -> boolean] [list of x -> list of x]
    ; x = post
    ; HELPER:
    ; find-history
    ; post -> boolean
    ; if the examing post shares the same id with the replymsg it is true
    ; (check-expect (find-history post2) #t) (replymsg is in the overall check-expect)
    ((define (find-history post)
       (if (string? post)
           #f
           (= (second replymsg) (post-id post))))
     ; Helper:
     ; insert-reply
     ; adds an reply at the end of the reply list
     ; post -> post
     ; (insert-reply post2) -> adds the given reply at the end of replies
     (define (insert-reply post)
       (make-post
        (post-id post)
        (post-author post)
        (post-content post)
        (append (post-reply post) (list(make-reply (third replymsg) (fourth replymsg)))))))
    (if (empty? (filter find-history history))
        history
        (replace-post history (insert-reply(first(filter find-history history)))))))

; receive-ServerMSG:
; takes in a History and a ServerMSG
; adds the post/reply/error to the history
(check-expect (receive-ServerMSG h2 (list "ERROR" "content")) (append h2 (list "ERROR:content")))
(check-expect (receive-ServerMSG h2 (list "POST" 5 "name" "content"))
              (append h2 (list (make-post 5 "name" "content" '()))))
(define (receive-ServerMSG history SMSG)
  (cond
    [(string=? "POST"(first SMSG))
     (append history (list (make-post (second SMSG) (third SMSG) (fourth SMSG) '())))]
    [(string=? "REPLY"(first SMSG))
     (add-reply history SMSG)]
    [(string=? "ERROR"(first SMSG))
     (append history (list (string-append "ERROR:" (second SMSG))))]))
;---------------------------------------------------------------------------------------------
; viewall-receive:
; takes in a viewall and a ServerMSG and add entries to history
; viewall ServerMSG -> history
(check-expect (viewall-receive viewall0 (list "ERROR" "content"))
              (make-viewall (make-wordp """") (list "ERROR:content")))
(check-expect (viewall-receive viewall0 (list "POST" 5 "name" "content"))
              (make-viewall (make-wordp """") (list (make-post 5 "name" "content" '()))))
(check-expect (viewall-receive viewall1 (list "REPLY" 1 "Max" "Interesting"))
              (make-viewall (make-wordp """") h1.1))
(define (viewall-receive va m)
  (make-viewall (viewall-command va) (receive-ServerMSG (viewall-history va) m)))

; threadview-receive:
; takes in a threadview and a ServerMSG and add entries to history
; threadview ServerMSG -> threadview
(check-expect (threadview-receive threadview0 (list "ERROR" "content"))
              (make-threadview '() (list "ERROR:content")))
(check-expect (threadview-receive threadview0 (list "POST" 5 "name" "content"))
              (make-threadview '() (list (make-post 5 "name" "content" '()))))
(check-expect (threadview-receive threadview1 (list "REPLY" 1 "Max" "Interesting"))
              (make-threadview '() h2.2))
(define (threadview-receive tv m)
  (make-threadview (threadview-viewing tv) (receive-ServerMSG (threadview-history tv) m)))

; newitem-receive
; takes in n newitem and a ServerMSG and add entries to history
; newitem ServerMSG -> newitem
(check-expect (newitem-receive newitemP0 (list "ERROR" "content"))
              (make-newitem #f (make-wordp """") (list "ERROR:content")))
(check-expect (newitem-receive newitemP0 (list "POST" 5 "name" "content"))
              (make-newitem #f (make-wordp """") (list (make-post 5 "name" "content" '()))))
(check-expect (newitem-receive newitemP1 (list "REPLY" 1 "Max" "Interesting"))
              (make-newitem #F (make-wordp "Homework""") h1.1))
(define (newitem-receive n m)
  (make-newitem (newitem-id n) (newitem-content n) (receive-ServerMSG (newitem-history n) m)))

; search-receive
; takes in n search and a ServerMSG and add entries to history
; search ServerMSG -> search
(check-expect (search-receive search0.0 (list "ERROR" "content"))
              (make-search (list "ERROR:content") (make-wordp """")))
(check-expect (search-receive search0.0 (list "POST" 5 "name" "content"))
              (make-search (list (make-post 5 "name" "content" '())) (make-wordp """")))
(check-expect (search-receive search1 (list "REPLY" 1 "Max" "Interesting"))
              (make-search h2.2 (make-wordp "Benn""")))
(define (search-receive s m)
  (make-search (receive-ServerMSG (search-history s) m) (search-search s)))
