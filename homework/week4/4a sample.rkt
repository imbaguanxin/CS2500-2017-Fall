;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4a sample|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Purpose: simulating a Forum
;; that looks like Piazza andthis is the client program.
;; It can reveive other people's
;; answers to question from the server, 
;; create new answer and new question

;; When the client reveives entries of strings, it uses them as
;; the state and display them on canvas

;; When the client sends a answer
;; The sever appends the string of the answer under the question

;; a local is a (make-local string string string)
;; it contains the NO. username and content of a single entry
(define-struct local [id name content])
;; template:
#; (define (local-temp a-local)
     (...(local-id a-local)...
         (local-name a-local)...
         (local-content a-local)...))

;; a Server is a (make-Server boolean string)
;; it illustrate weather it is an error and then display message from the server
(define-struct Server [Er content])
;; template:
#; (define (Server-temp a-Server)
     (...(Server-er a-Server)...
         (server-content a-Server)...))

;; a forum is a (make-forum local Server)
;; it conbines the structure of local and server so the big-bang can
;; regard the structure as world
(define-struct forum [local Server])
;; template:
#; (define (forum-temp a-forum)
     (...(local-temp(forum-local a-forum))...
         (Server-temp(forum-Server a-forum))...))

;; In order to simplify the check expects
;; two forums are defined here
(define test1
  (make-forum (make-local "1" "Max" "how are you?")
              (make-Server #f "good")))
(define test2
  (make-forum (make-local "2" "Misaka" "Are you OK?")
              (make-Server #t "Connection Interrupted")))

;; a world is a forum
;; forum -> forum
#;(define (simple-forum str)
    (big-bang str
              [name ...]
              [register ...]
              [port ...]
              [on-key type]                  ;; forum key -> forum
              [on-tick send]                 ;; forum -> package
              [to-draw render]               ;; forum -> image
              [on-reveive reveive]))         ;; forum string -> forum

;; type function:
;; forum key -> forum
;; take in what is typed and add it to "local"
(check-expect (type test1 "g")
              (make-forum (make-local "1" "Max" "how are you?g")
                          (make-Server #f "good")))
(check-expect (type test2 "k")
              (make-forum (make-local "2" "Misaka" "Are you OK?k")
                          (make-Server  #t "Connection Interrupted")))

;; send function:
;; forum -> Package (of String and String)
;; send the answer that contains in "local" to the server
(check-expect (send test1)
              (make-package "answer: " "1:Max:how are you?"))
(check-expect (send test2)
              (make-package "answer: " "2:Misaka:Are you OK?"))

;; render function:
;; forum -> Image
;; render the state of the forum as a text image
;; text from local client appears in black
;; text retrived from server appears in blue
(check-expect (render test1)
              (overlay
               (overlay (text "1:Max:how are you" 20 "black")
                        (text "good" 20 "blue"))
               (empty-scene 500 500)))
(check-expect (render test2)
              (overlay
               (overlay (text "2:Misak:Are you OK?" 20 "black")
                        (text "Error: Connection Interrupted" 20 "blue"))
               (empty-scene 500 500)))

;; receive function:
;; String -> forum
;; receive message from the server, both error and content
(check-expect (receive "hi from server")
              (make-forum (make-local "1" "Max" "how are you?") (make-Server #f "hi from server")))
(check-expect (receive "Error: Maintance")
              (make-forum (make-local "1" "Max" "how are you?") (make-Server #t "Maintance")))