#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define-struct monde
  (serpent   ; liste de points
   boni      ; liste de points
   dir       ; direction du serpent
   perdu?
   stop))

(define-struct point (x y))

(define (point=? p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))


(define m0 (make-monde
            (list (make-point 3 0) (make-point 2 0) (make-point 1 0) (make-point 0 0))
            (list)
            "right"
            #f
            #f))

(define m0+ (struct-copy monde m0 [boni (list (make-point 2 2))]))

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

(define (déplacer-corps précédent)
  (cons (first précédent)
        (drop-right (rest précédent) 1)))

(define (déplacer-serpent précédent dir)
  (cons (déplacer-tête (first précédent) dir)
        (déplacer-corps précédent)))


(define taille-carreau 25)
(define taille-jeu 20)

(define scene (square (* taille-carreau taille-jeu) "solid" "pink"))


(define carreau (square taille-carreau "solid" "black"))
(define (dessiner-carreau pt fond)
  (place-image carreau (* (+ 0.5 (point-x pt)) taille-carreau) (* (+ 0.5 (point-y pt)) taille-carreau) fond))


(define dessiner-segment dessiner-carreau)

(define (dessiner-serpent fond serpent)
  (foldl dessiner-segment fond serpent))


(define dessiner-bonus dessiner-carreau)

(define (dessiner-boni fond boni)
  (foldl dessiner-bonus fond boni))


(define (dessiner-fin perdu? dessin)
  (if perdu?
      (place-image (text "FIN" 48 "red") (/ (image-width dessin) 2) (/ (image-height dessin) 2) dessin)
      dessin))

(define (dessiner monde)
  (dessiner-fin (monde-perdu? monde)
                (dessiner-boni (dessiner-serpent scene (monde-serpent monde))
                               (monde-boni monde))))


(define (render-serpent serpent)
  (foldl (lambda (pt str) (string-append str "{" (number->string (point-x pt)) "," (number->string (point-y pt)) "} ")) "" serpent))

(define (collision? roger)
  (member (first roger) (rest roger) point=?))

(define (suivant m)
  (if (monde-perdu? m)
      m
      (let ((serpent2 (déplacer-serpent (monde-serpent m) (monde-dir m))))
        (struct-copy monde m
                     (serpent serpent2)
                     (perdu? (collision? serpent2))))))

(define (clavier m touche)
  (cond
    [(member touche (list "up" "down" "left" "right"))
     (struct-copy monde m (dir touche))]
    [(key=? touche "q")
     (struct-copy monde m (stop #t))]
    [#t m]))

(big-bang m0+
  (stop-when monde-stop)
  (close-on-stop #t)
  (on-key clavier)
  (on-tick suivant 0.5)
  (to-draw dessiner))


