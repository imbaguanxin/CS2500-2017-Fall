;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |simple formula|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct wordp [l r])
(define-struct wholetext [pre wordp])
(define BAR (rectangle 5 20 "solid" "red"))

(define (l-wordp str)
  (wordp-l(wholetext-wordp str)))

(define (r-wordp str)
  (wordp-r (wholetext-wordp str)))

(define (simple-forum str)
  (big-bang str
            [on-key move]
            [to-draw render]))

(define (render str)
  (overlay
   (above
   (text (wholetext-pre str) 20 "black")
   (lastline str))
   (empty-scene 500 500)))

(define (lastline str)
  (beside (text (l-wordp str) 20 "green")
          BAR
          (text (r-wordp str) 20 "green")))

(define (move str key)
  (cond [(key=? key "\b")
         (if (>= (- (string-length (l-wordp str)) 1)0)
             (make-wholetext
              (wholetext-pre str)
              (make-wordp (substring (l-wordp str) 0 (- (string-length (l-wordp str)) 1))
                          (r-wordp str)))
             str)]
        
        [(key=? key "\r") 
         (make-wholetext 
          (string-append (wholetext-pre str) " " (l-wordp str)(r-wordp str))
          (make-wordp "" ""))]
        
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
        
        [(key=? key "right")
         (if (>= (string-length (r-wordp str)) 1)
         (make-wholetext
          (wholetext-pre str)
          (make-wordp (string-append 
                       (l-wordp str)
                       (substring (r-wordp str)0 1))
                      (substring (r-wordp str) 1 (string-length (r-wordp str)))))
         str)]
        
        [else (make-wholetext
               (wholetext-pre str)
               (make-wordp (string-append (l-wordp str) key)
                           (r-wordp str)))]))

(simple-forum (make-wholetext "previous text" (make-wordp "hello" " BILIBILI")))