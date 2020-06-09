#lang racket

(provide (all-defined-out))

(require (file "données.rkt"))
(require 2htdp/image)


;;;;;;;;;;;;;;;;
; Dessin
;;;;;;;;;;;;;;;;


(define (make-canvas color)
  (square (* taille-carreau taille-jeu) "solid" color))

(define scene (make-canvas "pink"))
(define canvas (make-canvas "transparent"))


(define carreau (square taille-carreau "solid" "black"))
(define (dessiner-carreau pt fond)
  (place-image carreau (* (+ 0.5 (point-x pt)) taille-carreau) (* (+ 0.5 (point-y pt)) taille-carreau) fond))


(define dessiner-segment dessiner-carreau)

(define (dessiner-serpent serpent)
  (foldl dessiner-segment canvas serpent))


(define dessiner-bonus dessiner-carreau)

(define (dessiner-boni boni)
  (foldl dessiner-bonus canvas boni))

(define half-width (/ (image-width canvas) 2))
(define half-height (/ (image-height canvas) 2))
(define third-height (/ (image-height canvas) 3))

(define (dessiner-fin perdu?)
  (if perdu?
      (place-image (text "FIN" 48 "red") half-width half-height canvas)
      canvas))

(define (dessiner-menu m)
  (place-image (text "1. collision avec les bords" 32 (if (monde-bord-collision? m) "green" "black"))
               half-width third-height
               (place-image (text "2. terrain infini" 32 (if (monde-bord-collision? m) "black" "green"))
                            half-width (* 2 third-height)
                            scene)))

(define (dessiner monde)
  (if (monde-menu-option? monde)
      (dessiner-menu monde)
      (overlay
       (dessiner-fin (monde-perdu? monde))
       (dessiner-boni (monde-boni monde))
       (dessiner-serpent (monde-serpent monde))
       scene)))

