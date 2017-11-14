;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |9b kian|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/string)

; Server with which program will communicate
(define the-server "dictionary.ccs.neu.edu") 
; Access port within the server
(define the-port 10006)
; User credentials
(define the-name "mehrabani.k:9016")

; A ClientMsg is one of
; - "CATCHUP"                     <<== ask server to send old posts
; - (list "POST" String)          <<== create new post with given text
; - (list "REPLY" Natural String) <<== reply to post with this id, and given text

; A ServerMsg is one of:
; - (list "POST" Natural String String)  <<== new post (id, author, content)
; - (list "REPLY" Natural String String) <<== new reply (post id, author, content)
; - (list "ERROR" String)                <<== something went wrong

; A Buffer is a (make-buffer String String)
(define-struct buffer [left right])
; Interpretation: Message or search query being typed stored in sections
; to the left and right of the cursor
(define empty-buffer (make-buffer "" ""))

; A Reply is a (make-reply Number String Buffer)
(define-struct reply [author contents])
(define reply1 (make-reply "A'" "abc"))
(define reply2 (make-reply "Kian" "hhh"))
; - author is the name of the author
; - and contents is the message

; A Post is one of
; - (list Number (make-post String String [List-of Reply]))
; - where the Number is the Post ID
(define-struct post [author contents replies])
(define post1 (list 1 (make-post "A" "aaa" (list reply1))))
(define post2 (list 2 (make-post "B" "bbb" '())))
(define post2.1 (list 2 (make-post "B" "bbb" (list reply2))))
(define post3 (list 3 (make-post "Amy" "Hello" '())))
; - author is the name of the author
; - contents is the message
; - replies is a list of replies to the post

; A History is a [List-of Post]
; INTERPRETATION: the history of top-level posts seen so far.
(define h1 (list post1 post2))
(define h1.1 (list post1 post2.1))
(define h2 (list post1 post2 post3))

; A Post-Error is a (make-post-error String History)
(define-struct post-error [contents log])
; - where contents is the description of the error
(define error1 (make-post-error "Warning" h1))

; A Client is one of
; - Viewall
; - Threadview
; - Newitem
; - Search
; - (make-package Viewall ClientMsg)
; - PostError
; INTERPRETATION: Represents four different "views" in your program.

; A Viewall is a (make-viewall Buffer History)
; INTERPRETATION: The user is viewing all posts (but not replies), 
;   and possibly typing a Command.
; examples:

; A Threadview is a (make-threadview Post History)
; INTERPRETATION: The user is viewing a specific Post and its replies.

; A Newitem is a (make-newitem [Maybe Natural] Buffer History)
; INTERPRETATION: The user is entering a new post (if the maybe is #false),
;   or a new reply (if the maybe is a number).  

; A Search is a (make-search History Buffer)
; INTERPRETATION: The user is trying to view only a subset
;   of the existing messages.

(define-struct viewall [cmd log])
(define view0 (make-viewall empty-buffer '()))
(define view1 (make-viewall (make-buffer "catchup""") '()))
(define view2 (make-viewall (make-buffer "new""") h1))
(define view3 (make-viewall (make-buffer "reply 1""") h1))
(define view4 (make-viewall (make-buffer "view 1""") h1))

(define-struct threadview [post log])
(define thread1 (make-threadview post1 h1))

(define-struct newitem [mid msg log])
(define newP0 (make-newitem #f empty-buffer h1))
(define newP1 (make-newitem #f (make-buffer "!" "") h1))
(define newR0 (make-newitem 1 empty-buffer h1))
(define newR1 (make-newitem 1 (make-buffer "abc?" "") h1))

(define-struct search [log query])
(define search0 (make-search h1 empty-buffer))
(define search1 (make-search h1 (make-buffer "a""")))



; forum : Client -> Client
; A messaging program that provides viewing, editing, sending, and receiving of messages
(define (forum username server port-number)
  (big-bang (make-viewall empty-buffer '())
            [name username]
            [register server]
            [port port-number]
            [on-key key-handler]
            [to-draw draw 600 500]
            [on-receive receive-handler]))

; key-handler : Client -> Client
; Updates the given Client based on the key input given
; f1:
(check-expect (key-handler thread1 "f1")(make-viewall empty-buffer h1))
(check-expect (key-handler newP0 "f1")(make-viewall empty-buffer h1))
(check-expect (key-handler newP1 "f1")(make-viewall (make-buffer "!" "") h1))
(check-expect (key-handler newR0 "f1")(make-viewall empty-buffer h1))
(check-expect (key-handler newR1 "f1")(make-viewall (make-buffer "abc?" "") h1))
(check-expect (key-handler search0 "f1")(make-viewall empty-buffer h1))
(check-expect (key-handler search1 "f1")(make-viewall (make-buffer "a""") h1))
; f2:
(check-expect (key-handler thread1 "f2")(make-search h1 empty-buffer))
(check-expect (key-handler newP0 "f2")(make-search h1 empty-buffer))
(check-expect (key-handler newP1 "f2")(make-search h1 (make-buffer "!" "")))
(check-expect (key-handler newR0 "f2")(make-search h1 empty-buffer))
(check-expect (key-handler newR1 "f2")(make-search h1 (make-buffer "abc?" "")))
(check-expect (key-handler view0 "f2")(make-search '() empty-buffer))
(check-expect (key-handler view1 "f2")(make-search '() (make-buffer "catchup""")))
(check-expect (key-handler view2 "f2")(make-search h1 (make-buffer "new""")))
(check-expect (key-handler view3 "f2")(make-search h1 (make-buffer "reply 1""")))
(check-expect (key-handler view4 "f2")(make-search h1 (make-buffer "view 1""")))
; "\r"
(check-expect (key-handler view1 "\r")(make-package (make-viewall empty-buffer '()) "CATCHUP"))
(check-expect (key-handler view2 "\r")(make-newitem #f empty-buffer h1))
(check-expect (key-handler view3 "\r")(make-newitem 1 empty-buffer h1))
(check-expect (key-handler view4 "\r") thread1)
(check-expect (key-handler error1 "\r")(make-viewall empty-buffer h1))
(check-expect (key-handler newP0 "\r") newP0)
(check-expect (key-handler newP1 "\r")
              (make-package (make-viewall empty-buffer h1)(list "POST" "!")))
(check-expect (key-handler newR0 "\r") newR0)
(check-expect (key-handler newR1 "\r")
              (make-package (make-viewall empty-buffer h1)(list "REPLY" 1 "abc?")))
; \b
(check-expect (key-handler newR1 "\b") (make-newitem 1 (make-buffer "abc""") h1))
(check-expect (key-handler view3 "\b") (make-viewall (make-buffer "reply """) h1))
(check-expect (key-handler search1 "\b") search0)
(check-expect (key-handler newP1 "\b") newP0)
; \u007F
(check-expect (key-handler (make-newitem 1 (make-buffer "abc""333") h1) "\u007F")
              (make-newitem 1 (make-buffer "abc""33") h1))
(check-expect (key-handler (make-viewall (make-buffer "reply 1""2") h1) "\u007F")view3)
(check-expect (key-handler search1 "\u007F") search1)
(check-expect (key-handler newP1 "\u007F") newP1)
; left
(check-expect (key-handler newR1 "left") (make-newitem 1 (make-buffer "abc""?") h1))
(check-expect (key-handler view3 "left") (make-viewall (make-buffer "reply ""1") h1))
(check-expect (key-handler search1 "left") (make-search h1 (make-buffer """a")))
(check-expect (key-handler newP1 "left") (make-newitem #f (make-buffer "" "!") h1))
; right
(check-expect (key-handler search1 "right") search1)
(check-expect (key-handler newP1 "right") newP1)
(check-expect (key-handler (make-newitem 1 (make-buffer "abc""333") h1) "right")
              (make-newitem 1 (make-buffer "abc3""33") h1))
(check-expect (key-handler (make-viewall (make-buffer "reply 1""2") h1) "right")
              (make-viewall (make-buffer "reply 12""") h1))
; random 1-length character
(check-expect (key-handler newR1 "a") (make-newitem 1 (make-buffer "abc?a""") h1))
(check-expect (key-handler view3 "b") (make-viewall (make-buffer "reply 1b""") h1))
(check-expect (key-handler search1 "c") (make-search h1 (make-buffer "ac""")))
(check-expect (key-handler newP1 "d") (make-newitem #f (make-buffer "!d" "") h1))

(define (key-handler c k)
  (cond
    [(string=? k "f1") (make-viewall (client-active c) (client-log c))]
    [(string=? k "f2") (make-search (client-log c) (client-active c))]
    [(and (string=? k "\r") (viewall? c)) (process-command c)]
    [(and (string=? k "\r") (newitem? c)) (send-handler c)]
    [(and (post-error? c) (string=? k "\r")) (make-viewall empty-buffer (post-error-log c))]
    [(string=? k "\b") 
     (update-client c
                    (make-buffer (string-init (buffer-left (client-active c)))
                                 (buffer-right (client-active c)))
                    (client-log c))]
    [(string=? k "\u007F")
     (update-client c
                    (make-buffer (buffer-left (client-active c))
                                 (string-tail (buffer-right (client-active c))))
                    (client-log c))]
    [(string=? k "left")
     (update-client c
                    (make-buffer
                     (string-init (buffer-left (client-active c)))
                     (string-append (string-last (buffer-left (client-active c)))
                                    (buffer-right (client-active c))))
                    (client-log c))]
    [(string=? k "right")
     (update-client c
                    (make-buffer (string-append (buffer-left (client-active c))
                                                (string-head (buffer-right (client-active c))))
                                 (string-tail (buffer-right (client-active c))))
                    (client-log c))]
    [(= (string-length k) 1)
     (update-client c
                    (make-buffer (string-append (buffer-left (client-active c)) k)
                                 (buffer-right (client-active c)))
                    (client-log c))]
    [else c]))

; process-command : Viewall -> Client
; Executes commands in Viewall mode
(define (process-command v)
  (local
    [(define split (string-split (buffer->string (viewall-cmd v))))]
    (cond
      [(string=? (first split) "clear")
       (make-viewall empty-buffer '())]
      [(string=? (first split) "catchup")
       (make-package (make-viewall empty-buffer '()) "CATCHUP")]
      [(string=? (first split) "new")
       (make-newitem #f empty-buffer (viewall-log v))]
      [(string=? (first split) "reply")
       (make-newitem (string->number (second split)) empty-buffer (viewall-log v))]
      [(string=? (first split) "view")
       (make-threadview (assoc (string->number (second split)) (viewall-log v)) (viewall-log v))]
      [else (make-post-error "invalid command" (viewall-log v))])))

; send-handler : Newitem -> Client
; Executes commands in the client buffer and rejects invalid messages before sending to the server
(check-expect (send-handler newP0) newP0)
(check-expect (send-handler newP1)
              (make-package (make-viewall empty-buffer h1)(list "POST" "!")))
(check-expect (send-handler newR0) newR0)
(check-expect (send-handler newR1)
              (make-package (make-viewall empty-buffer h1)(list "REPLY" 1 "abc?")))

(define (send-handler n)
  (cond 
    [(string=? (buffer->string (newitem-msg n)) "") n]
    [(number? (newitem-mid n)) 
     (make-package (make-viewall empty-buffer (newitem-log n)) 
                   (list "REPLY" (newitem-mid n) (buffer->string (newitem-msg n))))]
    [else 
     (make-package (make-viewall empty-buffer (newitem-log n))
                   (list "POST" (buffer->string (newitem-msg n))))]))

; receive-handler : Client ServerMsg -> Client
; Adds the message sent by the server to the client log
; receive an error:
(check-expect (receive-handler view0 (list "ERROR" "error content"))
              (make-post-error "error content" '()))
(check-expect (receive-handler search0 (list "ERROR" "error content"))
              (make-post-error "error content" h1))
(check-expect (receive-handler thread1 (list "ERROR" "error content"))
              (make-post-error "error content" h1))
(check-expect (receive-handler newP0 (list "ERROR" "error content"))
              (make-post-error "error content" h1))
; receive a post
(check-expect (receive-handler view0 (list "POST" 3 "Amy" "Hello"))
              (make-viewall empty-buffer (list post3)))
(check-expect (receive-handler search0 (list "POST" 3 "Amy" "Hello"))
              (make-search h2 empty-buffer))
(check-expect (receive-handler thread1 (list "POST" 3 "Amy" "Hello"))
              (make-threadview post1 h2))
(check-expect (receive-handler newP1 (list "POST" 3 "Amy" "Hello"))
              (make-newitem #f (make-buffer "!" "") h2))
; receive a reply
(check-expect (receive-handler view2 (list "REPLY" 2 "Kian" "hhh"))
              (make-viewall (make-buffer "new""") h1.1))
(check-expect (receive-handler search0 (list "REPLY" 2 "Kian" "hhh"))
              (make-search h1.1 empty-buffer))
(check-expect (receive-handler thread1 (list "REPLY" 2 "Kian" "hhh"))
              (make-threadview post1 h1.1))
(check-expect (receive-handler newP1 (list "REPLY" 2 "Kian" "hhh"))
              (make-newitem #f (make-buffer "!" "") h1.1))

(define (receive-handler c m)
  (cond 
    [(string=? "ERROR" (first m))
     (make-post-error (second m) (client-log c))]
    [(string=? "POST" (first m)) 
     (update-client c
                    (client-active c)
                    (append (client-log c) 
                            (list (list (second m) (make-post (third m) (fourth m) '())))))]
    [(string=? "REPLY" (first m))
     (update-client c
                    (client-active c)
                    (add-reply (client-log c) m))]))

; draw : Client -> Image
(check-expect (draw view0)
              (above/align "left"
                           (line 600 0 "purple")
                           (beside (text "command: " 15 "dark green") 
                                   (rectangle 1 16 "solid" "dark green"))))
(check-expect (draw view1)
              (above/align "left"
                           (line 600 0 "purple")
                           (beside (text "command: catchup" 15 "dark green") 
                                   (rectangle 1 16 "solid" "dark green"))))
(check-expect (draw view2)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (beside (text "command: new" 15 "dark green") 
                                   (rectangle 1 16 "solid" "dark green"))))
(check-expect (draw view3)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (beside (text "command: reply 1" 15 "dark green") 
                                   (rectangle 1 16 "solid" "dark green"))))
(check-expect (draw view4)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (beside (text "command: view 1" 15 "dark green") 
                                   (rectangle 1 16 "solid" "dark green"))))
(check-expect (draw search1)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (line 600 0 "purple")
                           (beside (text "search: a" 15 "teal") 
                                   (rectangle 1 16 "solid" "teal"))))
(check-expect (draw search0)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (beside (text "search: " 15 "teal") 
                                   (rectangle 1 16 "solid" "teal"))))
(check-expect (draw thread1)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (line 600 0 "purple")
                           (text "viewing: 1" 15 "orange")))
(check-expect (draw newP1)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (beside (text "mehrabani.k:9016: !" 15 "blue") 
                                   (rectangle 1 16 "solid" "blue"))))
(check-expect (draw newR1)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (beside (text "mehrabani.k:9016: abc?" 15 "blue") 
                                   (rectangle 1 16 "solid" "blue"))))
(check-expect (draw error1)
              (above/align "left"
                           (text "1 | A: aaa" 15 "maroon")
                           (text "   -> A': abc" 15 "brown")
                           (text "2 | B: bbb" 15 "maroon")
                           (line 600 0 "purple")
                           (text "Warning; press ENTER to continue" 15 "red")))

; Draws the log and buffer from the latest Client state
(define (draw c)
  (local
    ; foldr [X Y] [X Y -> Y] Y [Listof X] -> Y
    ; X= image Y = image
    [; Helper: Contains-query?
     ; Post -> Boolean
     (define (contains-query? p) (string-contains? (post-contents (second p))
                                                   (buffer->string (client-active c))))
     ; Helper: above/align-left
     ; image image -> image
     (define (above/align-left x y) (above/align "left" x y))
     ; Helper constant Processed-image
     (define processed-images
       (cond
         ; if it is a search
         ; search -> list of images
         [(search? c)
          ; filter [X] [X -> Boolean] [Listof X] -> [Listof X]; X = post
          ; map [X Y] [X -> Y] [Listof X] -> [Listof Y] X = post Y = image
          (map draw-post-and-replies (filter contains-query? (client-log c)))]
         
         [(threadview? c) (list (draw-post-and-replies (threadview-post c)))]
         
         [else
          ; map [X Y] [X -> Y] [Listof X] -> [Listof Y] X = post Y = image
          (map draw-post-and-replies (client-log c))]))
     (define acc
       (cond
         [(post-error? c) 
          (above/align "left"
                       (line 600 0 "purple")
                       (text (string-append (post-error-contents c) "; press ENTER to continue") 
                             15 "red"))]
         [(threadview? c) 
          (above/align "left"
                       (line 600 0 "purple")
                       (text (string-append "viewing: " 
                                            (number->string (first (threadview-post c)))) 
                             15 "orange"))]
         [(search? c)
          (above/align "left"
                       (line 600 0 "purple")
                       (beside (text (string-append "search: " (buffer-left (client-active c))) 
                                     15 
                                     "teal") 
                               (rectangle 1 16 "solid" "teal")
                               (text (buffer-right (client-active c)) 15 "teal")))]
         [(viewall? c)
          (above/align "left"
                       (line 600 0 "purple")
                       (beside (text (string-append "command: " (buffer-left (client-active c))) 
                                     15 
                                     "dark green") 
                               (rectangle 1 16 "solid" "dark green")
                               (text (buffer-right (client-active c)) 15 "dark green")))]
         [(newitem? c)
          (above/align "left"
                       (line 600 0 "purple")
                       (beside (text (string-append the-name ": " (buffer-left (client-active c))) 
                                     15 
                                     "blue") 
                               (rectangle 1 16 "solid" "blue")
                               (text (buffer-right (client-active c)) 15 "blue")))]))]
    (foldr above/align-left acc processed-images)))



;-----------------------------------------------------------------------------------------------------
; Helper Functions

; update-client : Client Buffer History -> Client
; Makes a Client that has the type of the given Client and with the applicable given fields
(check-expect (update-client view0 (make-buffer "chan" "ge") h2)
              (make-viewall (make-buffer "chan" "ge") h2))
(check-expect (update-client newP0 (make-buffer "chan" "ge") h2)
              (make-newitem #f (make-buffer "chan" "ge") h2))
(check-expect (update-client search1 (make-buffer "chan" "ge") h2)
              (make-search h2 (make-buffer "chan" "ge")))
(check-expect (update-client thread1 (make-buffer "chan" "ge") h2)
              (make-threadview post1 h2))
(check-expect (update-client error1 (make-buffer "chan" "ge") h2)
              (make-post-error "Warning" h2))

(define (update-client c b h)
  (cond
    [(viewall? c)    (make-viewall b h)]
    [(newitem? c)    (make-newitem (newitem-mid c) b h)]
    [(threadview? c) (make-threadview (threadview-post c) h)]
    [(search? c)     (make-search h b)]
    [(post-error? c) (make-post-error (post-error-contents c) h)]))

; client-active : Client -> Buffer
; Extracts the first Buffer from the given Client, regardless of what kind of Client it is
(check-expect (client-active view0) empty-buffer)
(check-expect (client-active newP0) empty-buffer)
(check-expect (client-active thread1) empty-buffer)
(check-expect (client-active search1) (make-buffer "a"""))
(check-expect (client-active error1) "Warning")
(define (client-active c)
  (cond
    [(viewall? c)    (viewall-cmd c)]
    [(threadview? c) empty-buffer]
    [(newitem? c)    (newitem-msg c)]
    [(search? c)     (search-query c)]
    [(post-error? c) (post-error-contents c)]))

; client-log : Client -> [List-of String]
; Extracts the log from the given Client, regardless of what kind of Client it is
(check-expect (client-log view0) '())
(check-expect (client-log newP0) h1)
(check-expect (client-log thread1) h1)
(check-expect (client-log search1) h1)
(check-expect (client-log error1) h1)
(define (client-log c)
  (cond
    [(viewall? c)    (viewall-log c)]
    [(threadview? c) (threadview-log c)]
    [(newitem? c)    (newitem-log c)]
    [(search? c)     (search-log c)]
    [(post-error? c) (post-error-log c)]))

; client-saved : Client -> Buffer
; Extracts the second Buffer from the given Client, regardless of what kind of Client it is
; (define (client-saved c)
;   (cond
;     [(editview? c) (editview-search c)]
;     [(searchposts? c) (searchposts-buffer c)]))

; buffer->string : Buffer -> String
; Collapses a Buffer into a String
(define (buffer->string b)
  (string-append (buffer-left b) (buffer-right b)))

(check-expect (buffer->string empty-buffer) "")
(check-expect (buffer->string (make-buffer "a" "")) "a")
(check-expect (buffer->string (make-buffer "" "b")) "b")
(check-expect (buffer->string (make-buffer "a" "b")) "ab")

; add-reply : History ServerMsg -> History
; Adds the reply provided by the server to the relevant post in the History
(define (add-reply h r)
  (local
    [(define mp (assoc (second r) h))]
    (cond
      [(false? mp) h]
      [else (replace-post h (list (first mp)
                                  (make-post (post-author (second mp))
                                             (post-contents (second mp))
                                             (append (post-replies (second mp)) 
                                                     (list (make-reply (third r) (fourth r)))))))])))
(define history (list (list 1 (make-post "pete" "a" '())) 
                      (list 2 (make-post "nick" "b" '())) 
                      (list 3 (make-post "dan" "c" '()))))
(check-expect (add-reply history (list "REPLY" 1 "kian" "d"))
              (list (list 1 (make-post "pete" "a" (list (make-reply "kian" "d")))) 
                    (list 2 (make-post "nick" "b" '())) 
                    (list 3 (make-post "dan" "c" '()))))
(check-expect (add-reply '() (list "REPLY" 1 "kian" "d")) '())

; replace-post : History Post -> History
; Replaces the given post in the given History
(define (replace-post h p)
  (local
    [(define (replace-on-id ph) (cond 
                                  [(= (first ph) (first p)) p]
                                  [else ph]))]
    (map replace-on-id h)))

(check-expect (replace-post history (list 1 (make-post "kian" "d" '())))
              (list (list 1 (make-post "kian" "d" '())) 
                    (list 2 (make-post "nick" "b" '())) 
                    (list 3 (make-post "dan" "c" '()))))
(check-expect (replace-post '() (list 1 (make-post "kian" "d" '()))) '())

; draw-post-and-replies : Post -> Image
(define (draw-post-and-replies p)
  (local
    [(define (acc-reply r a) (above/align "left" a (text (string-append "   -> " 
                                                                        (reply-author r) 
                                                                        ": " 
                                                                        (reply-contents r)) 
                                                         15 
                                                         "brown")))]
    (above/align "left"
                 (text (string-append (number->string (first p))
                                      " | " 
                                      (post-author (second p)) 
                                      ": " 
                                      (post-contents (second p)))
                       15 "maroon")
                 (foldr acc-reply empty-image (post-replies (second p))))))

(check-expect (draw-post-and-replies (list 1 (make-post "pete" "a" (list (make-reply "kian" "d")))))
              (above/align "left"
                           (text "1 | pete: a" 15 "maroon")
                           (text "   -> kian: d" 15 "brown")))

;-----------------------------------------------------------------------------------------------------
; A suite of String manipulation functions for changing the buffer

; string-init : String -> String
; string-init returns every part of the string except the last character
(define (string-init s)
  (substring s 0 (max 0 (sub1 (string-length s)))))

(check-expect (string-init "test") "tes")
(check-expect (string-init "t") "")
(check-expect (string-init "") "")

; string-last : String -> String
; string-last returns the last character of the string
(define (string-last s)
  (substring s (max 0 (sub1 (string-length s)))))

(check-expect (string-last "tested") "d")
(check-expect (string-last "t") "t")
(check-expect (string-last "") "")

; string-head : String -> String
; Returns the first character in a string
(define (string-head s)
  (substring s 0 (min 1 (string-length s))))

(check-expect (string-head "tested") "t")
(check-expect (string-head "t") "t")
(check-expect (string-head "") "")

; string-tail : String -> String
; Returns everything except the head of the string
(define (string-tail s)
  (substring s (min 1 (string-length s))))

(check-expect (string-tail "test") "est")
(check-expect (string-tail "t") "")
(check-expect (string-tail "") "")