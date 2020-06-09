#lang racket

(provide (all-defined-out))

(require (file "données.rkt"))

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


