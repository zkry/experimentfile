#lang br

(require "lexer.rkt" brag/support)
(provide experiment-colorer)

(define (experiment-colorer port)
  (define (handle-lexer-error excn)
    (define excn-srclocs (exn:fail:read-srclocs excn))
    (srcloc-token (token 'ERROR) (car excn-srclocs)))
  (define srcloc-tok
    (with-handlers ([exn:fail:read? handle-lexer-error])
      (exp-lexer port)))
  (match srcloc-tok
    [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
    [(srcloc-token (? eof-object?) _) (values srcloc-tok 'eof #f #f #f)]
    [else
     (match-define
       (srcloc-token
        (token-struct type val _ _ _ _ _)
        (srcloc _ _ _ posn span)) srcloc-tok)
     (define start posn)
     (define end (+ start span))
     (match-define (list cat paren)
       (match type
         ['STRING '(string #f)]
         ['COMMENT '(comment #f)]
         ['ERROR '(error #f)]
         ['DATE '(constant #f)]
         ['TIME '(constant #f)]
         ['INDENT '(parenthesis |{|)]
         ['DEDENT '(parenthesis |}|)]
         [else (match val
                 [(? number?) '(constant #f)]
                 [(? symbol?) '(symbol #f)]
                 ["(" '(parenthesis |(|)]
                 [")" '(parenthesis |)|)]
                 ["[" '(parenthesis |[|)]
                 ["]" '(parenthesis |]|)]
                 [else '(other #f)])]))
     (values val cat paren start end)]))
