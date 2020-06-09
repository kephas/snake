#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;
; Données
;;;;;;;;;;;;;;;;

(define-struct monde
  (serpent           ; liste de points
   boni              ; liste de points
   dir               ; direction du serpent
   perdu?
   stop
   nouveaux-segments ; nombre de segments restant à ajouter
   menu-option?      ; afficher le menu des options
   bord-collision?   ; est-ce que toucher le bord fait perdre
                     ; (sinon, revenir de l'autre côté)
   ))

(define-struct point (x y))

(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))


(define taille-carreau 25)
(define taille-jeu 20)