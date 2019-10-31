#lang br/quicklang
(require "parser.rkt" "lexer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (lex-experiment port path)))
  (strip-bindings
   (with-syntax ((out-path #'"experiment.json")
                 (out-path-go #'"experiment.go"))
     #`(module experiment-mod experimentfile/expander
         (define-values (out-distrs all-experiments out-package) #,parse-tree)
         (define out-jsexpr (distributions->jsexpr out-distrs))
         (display (info-msg out-distrs all-experiments))
         (call-with-output-file out-path #:exists 'replace
           (lambda (out)
             (write-json out-jsexpr out)))
         (call-with-output-file out-path-go #:exists 'replace
           (lambda (out)
             (display (render-go (map flatten-distribution out-distrs) all-experiments out-package (jsexpr->string out-jsexpr)) out)))))))

(module+ reader
  (provide read-syntax get-info))

(define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'experimentfile/colorer 'experiment-colorer)]
        [(drracket:toolbar-buttons)
         (dynamic-require 'experimentfile/buttons 'button-list)]
        [(drracket:opt-out-toolbar-buttons)
         '(drracket:syncheck macro-stepper debug-tool)]
        [(drracket:indentation)
         (dynamic-require 'experimentfile/indenter 'indent-experiment)]
        [else default]))
    handle-query)
