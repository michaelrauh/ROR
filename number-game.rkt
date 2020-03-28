#lang racket
(require 2htdp/universe 2htdp/image)

(define SIZE 100)
(define WIDTH 500)
(define HEIGHT 500)
(define TEXT-SIZE 15)
(struct state (small big count))

(define HELP-TEXT
  (text
   " up for larger numbers, down for smaller ones"
   TEXT-SIZE
   "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
  TEXT-SIZE
  "blue"))
(define COLOR "red")
(define TEXT-X 10)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 400)


(define (single? w)
  (= (state-small w) (state-big w)))

(define (render-last-scene w)
  (overlay (text (number->string (state-count w)) SIZE COLOR) MT-SC))

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (render w)
  (define str (string-append
               (number->string (guess w))
               ", "
               (number->string (state-count w)))) 
  (overlay (text str SIZE COLOR) MT-SC))

(define (guess w)
  (quotient (+ (state-small w) (state-big w)) 2))

(define (bigger w)
  (state (min (state-big w) (add1 (guess w)))
            (state-big w)
            (add1 (state-count w))))

(define (smaller w)
  (state (state-small w)
            (max (state-small w) (sub1 (guess w)))
            (add1 (state-count w))))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (start lower upper)
  (big-bang (state lower upper 0)
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scene)))

(start 0 100)