#lang br

(require "parser.rkt" "lexer.rkt" "expander.rkt"
         brag/support
         (for-syntax syntax/parse))

(define str #<<HERE
experiment MyExpA:
  b = 2 * 3 * 4
  y := [1, 2.0, 3.0]
  z := [0 to 10 step 1]
  subexperiment Sub1:
    a := 0.0

  subexperiment Sub2:
    a := 1.0

  subexperiment Sub3:
    a := 2.0

distribution:
  [[20 MyExpA]
   [20 MyExpB]] by "UUID"
  
experiment MyExpA:
  b = 2
  y := b
  z := b
  a := 0

HERE
  )

(define simple #<<HERE
package bid_price


experiment MyExperimentA:
  alpha := [1ms to 10ms step 1ms]
  message := "hello world"
  
distribution:
  10: MyExperimentA when `always`
HERE
  )

(lex-experiment (open-input-string simple))
(define ast (parse-to-datum (lex-experiment (open-input-string simple))))

(begin-for-syntax
  (require racket/list)
  (define (find-unique-var-ids stmt-stxs)
    (remove-duplicates
     (for/list ([stx (in-list (stx-flatten stmt-stxs))]
                #:when (syntax-property stx 'e-id))
       stx)
     #:key syntax->datum)))
