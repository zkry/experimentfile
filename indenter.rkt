#lang br
(require br/indent racket/contract)
(provide indent-experiment)

(define indent-width 2)
(define (indent-char? c) (member c '(#\: #\~)))

(define (indent-experiment tbox [posn 0])
  (define prev-line (previous-line tbox posn))
  (define current-line (line tbox posn))
  (cond
    [(not prev-line) 0]
    [else
     (define prev-indent (line-indent tbox prev-line))
     (cond
       [(indent-char? (line-last-visible-char tbox prev-line))
        (+ prev-indent indent-width)]
       [else prev-indent])]))


