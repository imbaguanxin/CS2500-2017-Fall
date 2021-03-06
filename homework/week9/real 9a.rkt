;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |real 9a|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;-------------------------------------------------------------------
;; DATA STRUCTURE:

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

;; A History is one of
;; - List of posts
;; - string
;; INTERPRETATION: the history of top-level posts seen so far. the string represents error messages.
;; examples:
(define h0 '())
(define h1 (list post1 post2))
(define h1.1 (list post1 post2 "ERROR"))
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
;; - SearchPPosts
;; INTERPRETATION: Represents two different "views" in program.

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
;; A SearchPosts is a (make-search Edit History Search)
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
;---------------------------------------------------------------------

; graded helper

; replace-post
; searches for a Post in the given History that has the same
; ID# as the given Post, and replaces it with the given Post.
; if it does not find a post having the same number
; it generate a post for history
; history post -> post
(check-expect (replace-post h1 (make-post 2 "Max" "Game" '()))
              (list post1 (make-post 2 "Max" "Game" '())))
(check-expect (replace-post '() (make-post 2 "Max" "Game" '()))
              (list (make-post 2 "Max" "Game" '())))
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
         [(= (post-id p) (post-id post)) post]
         [else p])))
    (if (empty? history)
        (list post)
        (map replace-old history))))

; add-reply:
; takes a current History and a "REPLY" ServerMsg,
; and adds a Reply to the relevant Post in the History
; if cannot find a POST that match the given REPLY
; it creates a error message (string)
; history servermsg -> history
(check-expect (add-reply h1 (list "REPLY" 2 "name" "content"))
              (list
               (make-post 1 "Alex" "Good morning!"
                          (list (make-reply "Kate" "Good morning!")(make-reply "Kyle" "Good!")))
               (make-post 2 "Benn" "What's up!"
                          (list (make-reply "Murphy" "cheer!")(make-reply "name" "content")))))
(check-expect (add-reply '() (list "REPLY" 2 "name" "content"))
              (list "ERROR: CANNOT FIND A POST THAT MATCH THE RECEIVING REPLY"))
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
       (= (second replymsg) (post-id post)))
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
        (append (post-reply post)
                (list (make-reply (third replymsg) (fourth replymsg)))))))
    (if (empty? (filter find-history history))
        (append history (list "ERROR: CANNOT FIND A POST THAT MATCH THE RECEIVING REPLY"))
        (replace-post history (insert-reply(first(filter find-history history)))))))

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

; draw-post-and-replies:
; INTERPRETATION: turn a post to a picture
; post -> image
; examples:
(check-expect (draw-post-and-replies post1)
              (above/align "left"
                           (text "Post1-author:Alex- Good morning!" 20 "black")
                           (text "Reply-author:Kate- Good morning!" 15 "blue")
                           (text "Reply-author:Kyle- Good!" 15 "blue")))
(check-expect (draw-post-and-replies post2)
              (above/align "left"
                           (text "Post2-author:Benn- What's up!" 20 "black")
                           (text "Reply-author:Murphy- cheer!" 15 "blue")))
  

(define (draw-post-and-replies post)
  (local
    ; HELPER: draw-replies
    ; arrage replies to one picture
    ; [list of replies] -> image
    ((define (draw-replies lor)
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
         #; (check-expect (draw-replies reply1)
                          (text "Reply-author:Kate- Good morning!" 15 "blue"))
         ((define (draw-replies reply)
            (text (string-append "Reply-author:" (reply-author reply) "- " (reply-content reply))
                  15 "blue")))
         (foldr above-left empty-image (map draw-replies lor)))))
    
    (above/align
     "left"
     (text
      (string-append "Post"
                     (number->string (post-id post))
                     "-author:"
                     (post-author post)
                     "- "
                     (post-content post))
      20 "black")
     (draw-replies (post-reply post)))))