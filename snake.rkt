#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define-struct monde
  (serpent   ; liste des points
   dir))     ; direction du serpent

(define-struct point (x y))


(define m0 (make-monde (list (make-point 3 0) (make-point 2 0) (make-point 1 0) (make-point 0 0)) "right"))

(define (déplacer-tête précédent dir)
  (cond
    [(string=? dir "up")
     (make-point (point-x précédent) (- (point-y précédent) 1))]
    [(string=? dir "down")
     (make-point (point-x précédent) (+ (point-y précédent) 1))]
    [(string=? dir "left")
     (make-point (- (point-x précédent) 1) (point-y précédent))]
    [(string=? dir "right")
     (make-point (+ (point-x précédent) 1) (point-y précédent))]))

(define taille-carreau 25)
(define scene (square (* taille-carreau 20) "solid" "pink"))


(define carreau (square taille-carreau "solid" "black"))
(define (dessiner-segment segment fond)
  (place-image carreau (* (+ 0.5 (point-x segment)) taille-carreau) (* (+ 0.5 (point-y segment)) taille-carreau) fond))

(define (dessiner-serpent fond serpent)
  (foldl dessiner-segment fond serpent))

(define (dessiner monde)
  (dessiner-serpent scene (monde-serpent monde)))

(big-bang m0
  (to-draw dessiner))