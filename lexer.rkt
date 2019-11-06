#lang racket

(require brag/support "struct.rkt")

(provide lex-experiment exp-lexer)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev duration-units (:or "ns" "us" "µs" "ms" "s" "m" "h" "d"))

(define-lex-abbrev ident (:seq alphabetic (:* (:or alphabetic numeric "_"))))

(define-lex-abbrev reserved-terms
  (:or "experiment" "subexperiment" "deferred" "scheduled" "distribution" "package"
       "to" "step" ":=" "=" "<-" "+" "-" "/" "*" ":" "," "~" "when"))

(define exp-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [" " (token 'SPACE #:skip? #t)]
   ["\t" (token 'TAB lexeme)]
   ["(" (token 'L-PAREN lexeme)]
   [")" (token 'R-PAREN lexeme)]
   ["[" (token 'L-BRACKET lexeme)]
   ["]" (token 'R-BRACKET lexeme)]
   [(from/stop-before "#" "\n") (token 'COMMENT #:skip? #t)]
   [reserved-terms (token lexeme lexeme)]
   [(:seq numeric (:? numeric) "/" numeric (:? numeric) "/" (:** 2 4 numeric))
    (token 'DATE lexeme)]
   [(:seq numeric (:? numeric) ":" numeric numeric)
    (token 'TIME lexeme)]
   [(:seq digits (:? (:seq "e" digits))) (token 'INTEGER (string->number lexeme))]
   [(:seq digits duration-units) (token 'DURATION (parse-duration lexeme))]
   [(:seq (:or (:seq (:? digits) "." digits)
               (:seq digits "."))
          (:? (:seq "e" digits)))
    (token 'DECIMAL (string->number lexeme))]
   [(:or "true" "false") (token 'BOOLEAN lexeme)]
   [ident (token 'IDENTIFIER (string->symbol lexeme))]
   [(from/to "\"" "\"") (token 'STRING (trim-ends "\"" lexeme "\""))]
   [(from/to "`" "`") (token 'BT_STRING (trim-ends "`" lexeme "`"))]
   [(eof) eof]))

(define (lex-experiment in [path #f])
  ;;(define in (open-input-string str))
  (port-count-lines! in)
  (lexer-file-path path)
  (process-indentation
   (let loop ([v (exp-lexer in)])
     (cond [(void? (srcloc-token-token v)) (loop (exp-lexer in))] 
           [(eof-object? (srcloc-token-token v)) '()]
           [else (cons v (loop (exp-lexer in)))]))))

;; parse-duration parses a duration string (ex 10s) and converts it into
;; a duration struct
(define (parse-duration dur-str)
  (let* ([quant (string->number (first (regexp-match #rx"^[0-9]*" dur-str)))]
         [unit (first (regexp-match #rx"[a-z]*$" dur-str))]
         [multiplier (cond
                       [(equal? unit "ns") 1.0]
                       [(equal? unit "us") 1000.0]
                       [(equal? unit "µs") 1000.0]
                       [(equal? unit "ms") 1000000.0]
                       [(equal? unit "s") 1e+9]
                       [(equal? unit "m") 6e+10]
                       [(equal? unit "d") 8.64e+13])])
    (duration (* quant multiplier))))

;; calculate-tab-spaces calculates how many spaces a current level of
;; indentation should be at if followed by a tab. This is alwaysed
;; defined to be an offset of 8 spaces. So 7 spaces plus a tab will result
;; in an indentation level of 8.
(define (calculate-tab-spaces indent)
  (if (= (remainder indent 8) 0) indent (calculate-tab-spaces (add1 indent))))

;; dedent-ct indent-stack is list of indentation levels, n is the indentation count
;; currently at. Find how many indents we must "pop" off of indent stack to reach n.
(define (dedent-ct n indent-stack)
  (if (and (empty? indent-stack) (not (zero? n)))
      (error "bad indentation")
      (let ((indent-ct (apply + indent-stack)))
        (cond ((= indent-ct n) 0)
              (else (add1 (dedent-ct n (cdr indent-stack))))))))

;; process-indentation takes a list of tokens and inserts corresponding INDENT DEDENT
;; tokens as if they were { } braces.
(define (process-indentation toks)
  (define (srcloc-pos-diff l1 l2)
    (struct-copy srcloc l1 [span (- (srcloc-position l2) (srcloc-position l1))]))
  ;; conut-leading-indentation takes all of the spaces and tabs after a newline, converts tabs to
  ;; spaces and emits a (number? srcloc?) pair.
  (define (count-leading-indentation toks indent [sloc #f])
    (if (null? toks) '()
        (let* ((sl-tok (car toks))
               (new-sloc (srcloc-token-srcloc (car toks)))
               (tok (srcloc-token-token sl-tok))
               (type (token-struct-type tok)))
          (match type
            ['NEWLINE (cons sl-tok (count-leading-indentation (cdr toks) 0 new-sloc))]
            ['SPACE (if indent
                        (count-leading-indentation (cdr toks) (add1 indent) sloc)
                        (count-leading-indentation (cdr toks) #f))]
            ['TAB (if indent
                      (count-leading-indentation (cdr toks) (calculate-tab-spaces (add1 indent)) sloc)
                      (count-leading-indentation (cdr toks) #f))]
            [else (if indent
                      (cons (cons indent (srcloc-pos-diff sloc new-sloc))
                            (cons sl-tok (count-leading-indentation (cdr toks) #f)))
                      (cons sl-tok (count-leading-indentation (cdr toks) #f)))]))))
  (let ((indent-ct-toks (count-leading-indentation toks #f)))
    ;; insert-indent takes a list of tokens and counts the level of indentation at each line.
    ;; If the indentation goes up emit a INDENT token, if it goes down, insert the corresponding
    ;; number of DEDENT tokens.
    (define (insert-indent toks indent-stack)
      (if (null? toks)
          (make-list (length indent-stack)
                     (srcloc-token (token 'DEDENT " ") (srcloc-token-srcloc (last indent-ct-toks))))
          (let* ((tok (car toks)))
            (match tok
              [(cons (? number? n) sloc)
               (let ((stack-size (apply + indent-stack)))
                 (cond
                   [(= n stack-size) (insert-indent (cdr toks) indent-stack)]
                   [(> n stack-size)
                    (cons (srcloc-token (token 'INDENT " ") sloc)
                          (insert-indent (cdr toks)
                                         (cons (- n stack-size) indent-stack)))]
                   [else
                    (let ((dedent-n
                           (with-handlers ([exn:fail?
                                            (λ (e) (raise-user-error
                                                    (format "bad indentation in ~a:~a:~a"
                                                            (srcloc-source sloc) (srcloc-line sloc) (srcloc-column sloc))))])
                             (dedent-ct n indent-stack))))
                      (append (make-list dedent-n (srcloc-token (token 'DEDENT " ") sloc))
                              (list (token 'NEWLINE "")) ; NEWLINE after DEDENT to aid parasing
                              (insert-indent (cdr toks) (drop indent-stack dedent-n))))]))]
              [else (cons (car toks) (insert-indent (cdr toks) indent-stack))]))))
    (insert-indent indent-ct-toks '())))

