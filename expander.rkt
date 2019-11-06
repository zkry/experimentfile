#lang br

(require racket/stxparam
         (for-syntax syntax/parse racket/stxparam)
         json
         racket/hash
         racket/list
         racket/undefined
         gregor
         gregor/time

         (prefix-in gregor: gregor/period)
         "struct.rkt"
         "generator.rkt")

(provide (matching-identifiers-out #rx"^e-" (all-defined-out)))

(provide #%module-begin #%app #%datum #%top #%top-interaction)

(provide define define-values distributions->jsexpr jsexpr? jsexpr->string
         distribution-experiments distribution-end distribution-begin distribution-over
         car experiment-name experiment-values flatten-distribution

         sin cos tan asin acos atan modulo
         log
         sqrt
         (rename-out [expt exp])

         render-go

         get-experiment-names get-experiment-values

         map display quote println call-with-output-file write-json lambda

         info-msg)

(define-macro-cases e-neg
  [(_ VAL) #'VAL]
  [(_ "-" VAL) #'(dispatch-op-unary - VAL)])

(define-macro-cases e-product
  [(_ VAL) #'VAL]
  [(_ LEFT "*" RIGHT) #'(dispatch-op * LEFT RIGHT)]
  [(_ LEFT "/" RIGHT) #'(dispatch-op / LEFT RIGHT)]
  [(_ LEFT "%" RIGHT) #'(dispatch-op modulo LEFT RIGHT)])

(define-macro-cases e-sum
  [(_ VAL) #'VAL]
  [(_ LEFT "+" RIGHT) #'(dispatch-op + LEFT RIGHT)]
  [(_ LEFT "-" RIGHT) #'(dispatch-op - LEFT RIGHT)])

(define-macro-cases e-var-assign
  [(_ VARNAME EXPR) #'(set! VARNAME EXPR)])

(define-macro-cases e-func-app
  [(_ VAL) #'VAL]
  [(_ FUNC-NAME EXPR) #'(dispatch-op-unary FUNC-NAME EXPR)]
  [(_ FUNC-NAME EXPR1 EXPR2) #'(dispatch-op FUNC-NAME EXPR1 EXPR2)]
  [(_ FUNC-NAME EXPR ...) #'(FUNC-NAME EXPR ...)])

(define-macro-cases e-global-var
  [(_ VARNAME EXPR) #'(define VARNAME EXPR)])

(define-macro-cases e-package-stmt
  [(_ PKG-NAME) #'(set! out-package (symbol->string 'PKG-NAME))])

(define (e-date date-str)
  (parse-date date-str "d/M/y"))

(define (e-time time-str)
  (parse-time time-str "H:m"))

(define e-date-time
  (case-lambda
    [(y) (e-date-time y (time 0))]
    [(y t) (moment (->year y)
                   (->month y)
                   (->day y)
                   (->hours t)
                   (->minutes t)
                   #:tz "Europe/Berlin")]))

(define-syntax (e-relative-time stx)
  (syntax-case stx ()
    [(_ QUANT TYPE) #'((period-func 'TYPE) QUANT)]))

(define (period-func name)
  (let ((name (symbol->string name)))
    (cond
      ((member name '("minute" "minutes" "m" "min" "mins"))
       gregor:minutes)
      ((member name '("hour" "hours" "h"))
       gregor:hours)
      ((member name '("day" "days" "d"))
       gregor:days)
      ((member name '("month" "months" "m"))
       gregor:months)
      (else (raise-user-error "invalid duration" name)))))

(define (e-bool str)
  (cond
    ((equal? str "true") #t)
    ((equal? str "false") #f)))

(define (dispatch-op-unary op val)
  (if (list? val)
      (map (λ (x) (op val)) val)
      (op val)))

(define (dispatch-op op left right)
  (cond
    [(and (list? left) (not (list? right)))
     (map (λ (x) (op x right)) left)]
    [(and (not (list? left)) (list? right))
     (map (λ (x) (op left x)) right)]
    [(and (list? left) (list? right))
     (for/list ([l left]
                [r right])
       (op l r))]
    [else (op left right)]))

;; TODO: Possibly convert to exact/inexact
(define (e-expr expr)
  expr)

(define-macro-cases e-list-expr
  [(_ EXPR ...) #'(list EXPR ...)])

(define-macro-cases e-for-expr
  [(_ FROM "to" TO) #'(let ([from FROM]
                            [to TO])
                        (if (or (numberable? from) (numberable? to))
                            (raise-user-error "must provide step clause [_ to _ step _] for non-number (ex durations) types ")
                            (range from to)))]
  [(_ FROM "to" TO "step" STEP) #'(let ([from FROM]
                                        [to TO]
                                        [step STEP])
                                    (cond
                                      [(and (numberable? from)
                                            (numberable? to)
                                            (numberable? step))
                                           (range (get-number from) (get-number to) (get-number step))]
                                      [(or (numberable? from)
                                           (numberable? to)
                                           (numberable? step))
                                       (raise-user-error "can not mix number and non-number types (ex duration)")]
                                      [else (range from to step)]))])

;; e-multi-expr can be used to perform some cleanup
(define (e-multi-expr expr)
  expr)


(define (conflate-hash h)
  (let* ((items (hash->list h))
         ;; zipped-items: (Pair string? (list-of number?)) -> (list-of (Pair string? number?))
         (zipped-items
          (map (λ (kv) (for/list ([x (flatten (cdr kv))]) (cons (car kv) x)))
               items))
         (base-items (filter (λ (l) (= (length l) 1))
                             zipped-items))
         (perm-lists (filter (λ (l) (> (length l) 1))
                             zipped-items))
         ;; Cross product of all lists
         (perm-items (permutate-lists perm-lists)))
    (if (empty? perm-items) (apply hash (flatten base-items))
        (for/list ([x perm-items])
          (experiment (name-from-list x)
                      (apply hash (flatten (append base-items x))))))))

(define (name-from-list l)
  (define fmt-kv (λ (kv) (format "~a=~a" (car kv) (cdr kv))))
  (if (list? l)
      (string-join (map fmt-kv l) ",")
      (fmt-kv l)))

(define (permutate-lists ll)
  (cond
    ((null? ll) '())
    ((null? (cdr ll)) (car ll))
    (else (let ((l (car ll))
                (res-1 (permutate-lists (cdr ll))))
            (for*/list ([a l]
                        [b res-1])
              (cons a (list b)))))))

(define (conflate-subexperiments exps sub-exps)
  (if (= (length sub-exps) 0) exps
      (for*/list ([se sub-exps]
                  [ex (if (hash? exps) (list (experiment "" exps)) exps)])
        (experiment (string-append (experiment-name ex) ">"
                                   (experiment-name se))
                    (hash-union (experiment-values ex)
                                (experiment-values se))))))

(define-syntax-parameter exp-vars
  (λ (stx) (raise-syntax-error #f "Illegal outside experiment definition" stx)))

(define-syntax-parameter subexp-segments
  (λ (stx) (raise-syntax-error #f "Illegal outside experiment definition" stx)))

(define-syntax (e-experiment stx)
  (syntax-case stx ()
    [(_ exp-name stmt ...)
     (with-syntax ([(local-ids ...)
                    (find-unique-local-var-ids 'e-id-defn #'(stmt ...))])
       (unless (symbol? (syntax->datum #'exp-name))
         (raise-syntax-error #f "Experiment name invalid" #'exp-name))
       #`(begin (define exp-name
                  (let ((tmp-hash (make-hash)) ;; hash that will be the experiments values
                        (tmp-subexp-segments '()) ; store the subexperiment
                        ; bindings in a list. Each call
                        ; to subexperiments adds to this var.
                        (exp-name-str (symbol->string (quote exp-name))))
                    (define local-ids undefined) ...
                    (syntax-parameterize ([exp-vars (make-rename-transformer #'tmp-hash)]
                                          [subexp-segments (make-rename-transformer #'tmp-subexp-segments)])
                      stmt ...
                      (check-subexperiment-concistency subexp-segments)
                      (experiment exp-name-str (conflate-subexperiments (conflate-hash tmp-hash) subexp-segments)))))
                (unless is-subexperiment
                  (set! all-experiments (cons exp-name all-experiments)))))]))

;; Each subexperiment must define same set of values.
(define (check-subexperiment-concistency subs)
  (define keys (map (λ (key-list) (sort key-list symbol<?)) (map hash-keys (map experiment-values subs))))
  (unless (empty? subs)
    (for/list ([a (drop keys 1)]
               [b (drop-right keys 1)])
      (unless (equal? a b)
        (raise-user-error "subexperiments must have the same variables defined:" a  b)))))

(define-syntax-parameter is-subexperiment
  (λ (stx) #'#f))

(define-syntax (e-subexperiment stx)
  (syntax-case stx ()
    [(_ exp-name rest ...)
     #'(let ((tmp-is-subexperiment #t))
         (syntax-parameterize ([is-subexperiment (make-rename-transformer #'tmp-is-subexperiment)])
           (e-experiment exp-name rest ...)
           (set! subexp-segments (cons exp-name subexp-segments))))]))

(define-syntax (e-export-param stx)
  (syntax-case stx ()
    [(_ var-name expr)
     #'(begin
         (set! var-name expr)
         (hash-set! exp-vars (quote var-name) expr))]))

(define-macro-cases e-distr
  [(_ VAL NAME) #'(list VAL NAME "")]
  [(_ VAL NAME WHEN) #'(list VAL NAME WHEN)])

(define-syntax (e-option stx)
  (syntax-parse stx
    [(_ VAL NAME)
     #'(cons (quote VAL) NAME)]))

(define (get-option opt l) (let ((res (memf (λ (x) (equal? (car x) opt)) l)))
                             (if res (cdar res) res)))

(define e-distr-case
  ;; params is either (Pair Num Experiment) (Pair Symbol Value)
  (λ params
    ; returns false if option doesn't exist
    (let ((cases (filter (λ (x) (number? (car x))) params))
          (over-opt (get-option 'over params)))
      (let* ((probs (map car cases))
             (prob-sum (apply + probs)))
        (when (> prob-sum 100)
          (raise-user-error "sum of distribution probabilities is greater than 100: " prob-sum))
        (when (> (length (filter negative? probs)) 1)
          (raise-user-error "can not use negative number as distribution probability: " (filter negative? probs))))
      (distribution (map first cases) (map third cases) (map second cases) over-opt #f #f))))

(define-syntax (e-distribution stx)
  (syntax-case stx ()
    [(_ dst1 dst-rest ...)
     #'(set! out-distr (cons dst1 out-distr))]))

(define (e-time-stmt x)
  x)

;; exact-times calculates exact moments from a list of dates/periods
(define (exact-times times [prev-time #f])
  (when (and (gregor:period? at-time) (not prev-time))
    (raise-user-error "first time of a scheduled distribution must be an exact time"))
  (cond
    ((null? times) '())
    ((moment? (car times)) (cons (car times) (exact-times (cdr times) (car times))))
    (else (cons (+period prev-time (car times)) (exact-times (cdr times) (car times))))))

(define-syntax (e-scheduled-distribution stx)
  (syntax-case stx ()
    [(_ EXPR ...)
     #'(set! out-distr (get-scheduled-distribution EXPR ...))]))

(define get-scheduled-distribution
  (λ input
    (let ((params (memf moment? input))
          (options (takef input pair?)))
      (let* ((times (exact-times (for/list ([i (range (length params))]
                                            [p params]
                                            #:when (even? i))
                                   p)))
             (time-pairs (for/list ([beg (drop-right times 1)]
                                    [end (drop times 1)])
                           (cons beg end)))
             (distrs (for/list ([i (range (length params))]
                                [p params]
                                #:when (odd? i))
                       p)))
        (for/list ([distr distrs]
                   [beg-end time-pairs])
          (struct-copy distribution distr
                       [over (get-option 'over options)]
                       [begin (car beg-end)]
                       [end (cdr beg-end)]))))))

(define-syntax-parameter out-package
  (λ (stx) (raise-syntax-error #f "Illegal outside experiment definition" stx)))

(define-syntax-parameter out-distr
  (λ (stx) (raise-syntax-error #f "illegal use of out-distr" stx)))

(define-syntax-parameter all-experiments
  (λ (stx) (raise-syntax-error #f "illegal use of all-experiments " stx)))

(define-syntax (e-program stx)
  (syntax-parse stx
    [(_ stmt:expr ...)
     (define-values (grouped-stmts rest-stmts)
       (regroup-statements '(e-distribution e-scheduled-distribution) (syntax->list #'(stmt ...))))
     (with-syntax ([(grouped ...)
                    (datum->syntax #'(stmt ...) grouped-stmts)]
                   [(rest ...)
                    (datum->syntax #'(stmt ...) rest-stmts)])
       #'(let ((tmp-distr '())
               (tmp-experiments '())
               (tmp-package "experiment"))
           (syntax-parameterize ([out-distr
                                  (make-rename-transformer #'tmp-distr)]
                                 [all-experiments
                                  (make-rename-transformer #'tmp-experiments)]
                                 [out-package
                                  (make-rename-transformer #'tmp-package)])
             rest ...
             grouped ...

             (let ([flattened-experiments (filter
                                           experiment?
                                           (flatten
                                            (map (λ (e)
                                                   (divide-experiment e 0.0))
                                                 all-experiments)))])
               (check-experiments-types flattened-experiments)
               (values out-distr
                       flattened-experiments
                       out-package)))))]))

(begin-for-syntax
  (require racket/list)
  (define (find-unique-local-var-ids tag stmt-stxs)
    (remove-duplicates
     (for/list ([stx (in-list (stx-flatten stmt-stxs))]
                #:when (syntax-property stx tag))
       stx)
     #:key syntax->datum))

  (define (regroup-statements grp-syms stxs)
    (if (null? stxs)
        (values '() '())
        (let*-values ([(grouped rest) (regroup-statements grp-syms (cdr stxs))]
                      [(at-stx) (car stxs)]
                      [(at-stx-dat) (syntax->datum at-stx)])
          (if (member (car at-stx-dat) grp-syms)
              (values (cons at-stx grouped)
                      rest)
              (values grouped
                      (cons at-stx rest)))))))

(define dummy-exp
  (experiment "experimentA"
              (list (experiment "c=1_d=1" (list (experiment "e=3" (hash 'e 3))
                                                (experiment "e=4" (hash 'e 4))))
                    (experiment "c=1_d=2" (list (experiment "e=3" (hash 'e 3))
                                                (experiment "e=4" (hash 'e 4))))
                    (experiment "c=2_d=1" (list (experiment "e=3" (hash 'e 3))
                                                (experiment "e=4" (hash 'e 4))))
                    (experiment "c=2_d=1" (list (experiment "e=3" (hash 'e 3))
                                                (experiment "e=4" (hash 'e 4)))))))

;; divide-experiment takes an experiment that has values of either a hash or a list of
;; experiments. If the value of the hash is a list of experiments, distribute the initial
;; probability accordingly and "flatten" out all of the experiments into a single list.
;;
;; ex. (experiment "Exp1" (list (experiment "a=1" (hash 'a 1)) (experiment "a=2" (hash 'a 2))))
;;     with a probability of 20
;;
;;       will result in
;;
;;     (list
;;       ((experiment "Exp1_a=1" (hash 'a 1)) . 10)
;;       ((experiment "Exp1_a=2" (hash 'a 2)) . 10))
;;
;; TODO: Make a more general way to use this that doesn't show percentages
(define (divide-experiment e p)
  (define (divide-rec e p [prefix-name ""])
    (let* ((name (experiment-name e))
           (next-name (if (equal? "" prefix-name) name (string-append prefix-name ">" name)))
           (vals (experiment-values e)))
      (if (hash? vals) (cons (struct-copy experiment e
                                          [name next-name])
                             p)
          (map
           (λ (e) (divide-rec e (/ p (length vals)) next-name))
           vals))))
  (divide-rec e p))

(define (pair-up l)
  (if (null? l) '()
      (cons (list (car l) (cadr l) (caddr l)) (pair-up (cdddr l)))))

(define (flatten-distribution d)
  (let* ((probs (distribution-probabilities d))
         (conds (distribution-conditions d))
         (exps (distribution-experiments d))
         (over (distribution-over d))
         (conds-exps-probs (pair-up (flatten (for/list ([p probs]
                                                        [e exps]
                                                        [c conds])
                                               (let ((res (divide-experiment e p)))
                                                 (if (list? res)
                                                     (map (λ (x) (cons c x)) res)
                                                     (cons c res))))))))
    (distribution (map third conds-exps-probs) (map first conds-exps-probs) (map second conds-exps-probs)  over #f #f)))

(define (experiment-values->jsexpr vals)
  (define ret-hash (make-hash))
  (hash-for-each vals (λ (k v)
                        (hash-set! ret-hash
                                   k
                                   (if (jsonable? v)
                                       (->json v)
                                       v))))
  ret-hash)

(define (experiment->jsexpr e)
  (hash 'name (experiment-name e)
        'values (experiment-values->jsexpr (experiment-values e))))

(define (distribution->jsexpr d)
  (let ((probs (distribution-probabilities d))
        (conds (distribution-conditions d))
        (exps  (distribution-experiments d))
        (over  (distribution-over d)))
    (let ((conds-exp-probs (pair-up (flatten (for/list ([p probs]
                                                        [e exps]
                                                        [c conds])
                                               (let ((res (divide-experiment e p)))
                                                 (if (list? res)
                                                     (map (λ (x) (cons c x)) res)
                                                     (cons c res))))))))
      (hash 'probabilities (map exact->inexact (map third conds-exp-probs))
            'conditions (map first conds-exp-probs)
            'experiments (map experiment->jsexpr (map second conds-exp-probs))
            'over over))))

(define (distributions->jsexpr dd)
  (let ((dd-jsexprs (map distribution->jsexpr dd)))
    (hash 'distributions dd-jsexprs)))

(define (get-experiment-names out-distrs)
  (remove-duplicates
   (map experiment-name (flatten
                         (map distribution-experiments
                              (map flatten-distribution out-distrs))))))

(define (get-experiment-values out-distrs)
  (make-hash (let ((experiments (map
                                 distribution-experiments
                                 (map flatten-distribution
                                      out-distrs))))
               (for/list ([e (flatten experiments)])
                 (cons (experiment-name e)
                       (experiment-values e))))))

(define (info-msg out-distrs all-experiments)
  (string-join (flatten (list (format "Generated ~a experiments and ~a distributions:"
                                      (length all-experiments)
                                      (length out-distrs))
                              (for/list ([exp all-experiments])
                                (~a "-  " (experiment-name exp) "\n"
                                    (string-join
                                     (for/list ([vals (hash->list (experiment-values exp))])
                                       (~a "  - " (car vals) ": " (cdr vals)))
                                     "\n")))
                              "\nTraffic Dirtributions:"
                              (for/list ([dist out-distrs]
                                         [i (range (length out-distrs))])
                                (append
                                 (for/list ([c (distribution-conditions dist)]
                                            [prob (distribution-probabilities dist)]
                                            [exp (distribution-experiments dist)])
                                   (~a prob " -> " (experiment-name exp)))
                                 (list "")))))
               "\n"))
