#lang racket

(require "lexer.rkt" brag/support rackunit)

(define (lex str)
  (apply-port-proc exp-lexer str))

(check-equal? (lex "") empty)

(check-equal? (lex " ")
              (list (srcloc-token (token 'SPACE " ")
                                  (srcloc 'string 1 0 1 1))))

(check-equal? (lex "\t")
              (list (srcloc-token (token 'TAB "\t")
                                  (srcloc 'string 1 0 1 1))))

(check-equal? (lex ":")
              (list (srcloc-token (token ":" ":")
                                  (srcloc 'string 1 0 1 1))))

(check-equal? (lex ":=")
              (list (srcloc-token (token ":=" ":=")
                                  (srcloc 'string 1 0 1 2))))

(check-equal? (lex "experiment")
              (list (srcloc-token (token "experiment" "experiment")
                                  (srcloc 'string 1 0 1 10))))

(check-equal? (lex "ReactionTest1")
              (list (srcloc-token (token 'IDENT "ReactionTest1")
                                  (srcloc 'string 1 0 1 13))))

(check-equal? (lex "10.432")
              (list (srcloc-token (token 'DECIMAL 10.432)
                                  (srcloc 'string 1 0 1 6))))

(check-equal? (lex "`xyz`")
              (list (srcloc-token (token 'BT_STRING "xyz")
                                  (srcloc 'string 1 0 1 5))))

(check-equal? (lex "\"abc\"")
              (list (srcloc-token (token 'STRING "abc")
                                  (srcloc 'string 1 0 1 5))))
