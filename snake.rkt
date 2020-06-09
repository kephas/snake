#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(require (file "données.rkt"))
(require (file "logique.rkt"))
(require (file "dessin.rkt"))



;;;;;;;;;;;;;;;;
; Debug
;;;;;;;;;;;;;;;;

(define (render-serpent serpent)
  (foldl (lambda (pt str) (string-append str "{" (number->string (point-x pt)) "," (number->string (point-y pt)) "} ")) "" serpent))



;;;;;;;;;;;;;;;;
; Game loop
;;;;;;;;;;;;;;;;

(define (m0)
  (let ((serpent0 (list (make-point 3 0) (make-point 2 0) (make-point 1 0) (make-point 0 0))))
    (make-monde
     serpent0
     (list (make-bonus serpent0) (make-bonus serpent0))
     "right"
     #f
     #f
     1
     #f
     #f)))

(define (clavier m touche)
  (cond
    [(member touche (list "up" "down" "left" "right") key=?)
     (struct-copy monde m (dir touche))]
    [(key=? touche "r")
     (if (monde-perdu? m)
         (m0)
         m)]
    [(key=? touche "q")
     (struct-copy monde m (stop #t))]
    [(key=? touche "o")
     (struct-copy monde m [menu-option? #t])]
    [else m]))

(big-bang (m0)
  (stop-when monde-stop)
  (close-on-stop #t)
  (on-key clavier)
  (on-tick suivant 0.5)
  (to-draw dessiner))


