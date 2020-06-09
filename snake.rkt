#lang racket
(require 2htdp/image)
(require 2htdp/universe)



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



;;;;;;;;;;;;;;;;
; Logique du jeu
;;;;;;;;;;;;;;;;


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

(define (grandir-serpent précédent dir)
  (cons (déplacer-tête (first précédent) dir)
        précédent))


(define (sortie? serpent)
  (let ([tête (first serpent)])
    (or (= (point-x tête) -1)
        (= (point-y tête) -1)
        (= (point-x tête) taille-jeu)
        (= (point-y tête) taille-jeu))))

(define (collision? serpent)
  (member (first serpent) (rest serpent) point=?))

(define (monde/serpent-déplacé m)
  (let* ((déplacer? (= (monde-nouveaux-segments m) 0))
         (serpent2 (if déplacer?
                       (déplacer-serpent (monde-serpent m) (monde-dir m))
                       (grandir-serpent (monde-serpent m) (monde-dir m)))))
    (struct-copy monde m
                 [serpent serpent2]
                 [perdu? (or (collision? serpent2) (sortie? serpent2))]
                 [nouveaux-segments (if déplacer?
                                        (monde-nouveaux-segments m)
                                        (sub1 (monde-nouveaux-segments m)))])))


(define (make-bonus serpent [boni (list)])
  (let ((bonus (make-point (random taille-jeu) (random taille-jeu))))
    (if (or (member bonus serpent point=?)
            (member bonus boni point=?))
        (make-bonus serpent boni)
        bonus)))
; (append (list 1 2) (list 3 4)) ==> (list 1 2 3 4)

(define (monde/bonus-mangé m)
  (let* ([tête (first (monde-serpent m))]
         [boni (monde-boni m)]
         [nouveau-bonus (make-bonus (monde-serpent m) boni)])
    (if (member tête boni point=?)
        (struct-copy monde m
                     [boni (cons nouveau-bonus (remove tête boni point=?))]
                     [nouveaux-segments (+ (monde-nouveaux-segments m) 2)])
        m)))




(define (suivant m)
  (if (or (monde-perdu? m) (monde-menu-option? m))
      m
      (monde/bonus-mangé (monde/serpent-déplacé m))))



;;;;;;;;;;;;;;;;
; Dessin
;;;;;;;;;;;;;;;;

(define taille-carreau 25)
(define taille-jeu 20)

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



;;;;;;;;;;;;;;;;
; Debug
;;;;;;;;;;;;;;;;

(define (render-serpent serpent)
  (foldl (lambda (pt str) (string-append str "{" (number->string (point-x pt)) "," (number->string (point-y pt)) "} ")) "" serpent))



;;;;;;;;;;;;;;;;
; Game loop
;;;;;;;;;;;;;;;;



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


