;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |sample code|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data definitions

;; A World is a (make-posts String Image)
;; Interpretation: the String is the currently-being-typed post,
;; and the Image shows all the prior posts
(define-struct posts [cur history])

(define (posts-temp p)
  (... (posts-cur p) ... (posts-history p) ...))

;; Examples of worlds:
(define W1 (make-posts "" empty-image))
(define W2 (make-posts "hello" empty-image))
(define W3 (make-posts "world" empty-image))
;; You should make more examples here, with some images
;; filled in for the history of prior messages

(define WORLD-WIDTH 800)
(define WORLD-HEIGHT 600)
;; I picked this as my background.  You should pick something you
;; like better.
(define EMPTY-WORLD
  (rectangle WORLD-WIDTH WORLD-HEIGHT "solid" "lightgray"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main function of our program
(define (simple-forum init-world)
  (big-bang init-world
            [on-key  handle-key]
            [to-draw draw-world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling drawing

;; draw-world : World -> Image
;; Draws the current world
(check-expect (draw-world W1)
              (... "Fill this in with your expected drawing output"))
;; Make more examples here
(define (draw-world w)
  ;; Because this function consumes a World, we use the world template:
  ;; Use these pieces to draw something appropriate.
  ;; In my solution, I drew the current message in blue text,
  ;; in a wide white box (that looks kinda like a text-entry box),
  ;; underneath the collective image of the prior history of posts,
  ;; all of that drawn on top of the background.
  ;; So that this function doesn't get too complicated, I used a helper
  ;; function to draw the current text as a "prompt", below
  (... (posts-cur p) ... (posts-history p) ...))
  
;; Helper for drawing the current-post prompt
;; prompt : String -> Image
(check-expect (prompt "") (frame (rectangle WORLD-WIDTH 30 "solid" "white")))
(check-expect (prompt "hello")
              (frame (overlay/xy
                      (text "hello" 12 "blue")
                      -10 -5
                      (rectangle WORLD-WIDTH 30 "solid" "white"))))
;; Make more examples here
(define (prompt msg)
  (... "Fill this in such that the example above passes." ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling keys

;; handle-key : World String -> World
;; Responds to keypresses: "\b" backspaces, "\r" posts, and single
;; characters are typed
;; This is a sufficient set of tests for the key-handler for this assignment.
;; If you add behaviors, you'll need to add more tests.
(check-expect (handle-key W1 "s") (make-posts "s" empty-image))
(check-expect (handle-key W1 "\b") W1)
(check-expect (handle-key W1 "\r") W1)
(check-expect (handle-key W2 "\b") (make-posts "hell" empty-image))
(check-expect (handle-key W2 "\r")
              (make-posts
               ""
               (... "Fill this in with however you render the post 'hello'.")))
(check-expect (handle-key W3 "left") W3)


(define (handle-key w s)
  (cond
    [(string=? s "\b")
     (... "Figure out how to handle backspaces")]
    [(string=? s "\r")
     ;; Note: my solution used a helper function to add the current post
     ;; to the history of images
     (... "Figure out how to handle enters")]
    [(and (= 1 (string-length s)) ;; All single characters...
          (not (string=? s "\u007F"))) ;; except the Delete key
     (... "Figure out how to add a character to your world")]
    [else
     (... "What should happen if none of the above are true?")]))

;; add-post : String Image -> Image
;; Adds a given post to the existing image of posts
(check-expect (add-post "hello" (text "world" 12 "black"))
              (above/align "left"
                           (text "world" 12 "black")
                           (text "hello" 12 "black")))
(define (add-post msg history)
  (above/align "left" history (text msg 12 "black")))