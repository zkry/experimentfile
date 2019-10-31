#lang racket
(require pict
         racket/draw)

(define circle-size 10)

(define (rgb r g b)
  (make-object color% (exact-round r) (exact-round g) (exact-round b)))

(define (rgb-list->color l)
  (rgb (first l) (second l) (third l)))

(define (random-color-offset val)
  (let ([variation 100])
    (max 0 (min 255 (+ val (- (random variation) (exact-round (/ variation 2))))))))

(define (vari-conf-circles n r [rgb (list (random 256) (random 256) (random 256))])
  (let* ([circles (for/list ((i (range n))) (rotate (vc-append
                                                     (filled-ellipse circle-size circle-size #:color (rgb-list->color (map random-color-offset rgb)))
                                                     (vline 10 (/ r 2))
                                                     (blank 10 (/ r 2)))
                                                    (* (/ (* 2 pi) n) i)))])
    (apply cc-superimpose circles)))

(define (logo) (cc-superimpose
                (vari-conf-circles 10 130)
                (vari-conf-circles 60 90)
                (filled-ellipse 45 45 #:draw-border? #f #:color "MediumTurquoise")
                (vari-conf-circles 15 60)
                (vari-conf-circles 7 30)
                (blank 880 70)))

(define (save-pict the-pict name kind)
  (define bm (pict->bitmap the-pict))
  (send bm save-file name kind))

(save-pict (logo) "logo.png" 'png)
