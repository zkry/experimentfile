#lang racket

(require racket/generic)

(provide (struct-out experiment)
         (struct-out distribution)
         jsonable?
         ->json

         numberable? get-number wrap-func
         (struct-out duration)
         (contract-out [check-experiments-types (-> (listof experiment?) any)]))



;; experiment is a name with a hashmap of names and values.
(struct experiment (name values))

;; distribution is a list of probabilities corresponding to a list of experiments.
;; most experiments will only have one distribution but many are possible
(struct distribution (probabilities conditions experiments over begin end))

;; jsonable is for defining custom type that can be written to JSON
(define-generics jsonable
  (->json jsonable))

;; valable is for defining types that can be used with for expressions
(define-generics numberable
  (get-number numberable)
  (wrap-func numberable))

(struct duration (ns)
  #:methods gen:jsonable
  [(define (->json d) (duration-ns d))]
  #:methods gen:numberable
  [(define (get-number n) (duration-ns n))
   (define (wrap-func n) duration)])

(define (check-experiments-types experiments)
  (for/and ([base-exp (base-experiments-names experiments)])
    (let* ([exps (experiments-by-base-name experiments base-exp)])
      (if (> (length exps) 1)
          (let ([exp1 (first exps)]
                [exps-rest (rest exps)])
            (for/and ([exp exps-rest])
              (assert-value-types-equal exp1 exp)))
          #t))))

(module+ test
  (require rackunit)
  (define experiments-fixture
    (list (experiment "MyExperimentA>a=1>b=1" (hash "a" 1 "b" 1))
          (experiment "MyExperimentA>a=2>b=2" (hash "a" 2 "b" 2))
          (experiment "MyExperimentA>a=3>b=3" (hash "a" 3 "b" 3))
          (experiment "MyExperimentA>a=4>b=4" (hash "a" 4 "b" 4))
          (experiment "MyExperimentB" (hash "x" 1))
          (experiment "MyExperimentC" (hash "z" 1))))
  (define malformed-fixture
    (list (experiment "MyExperimentA>a=1>b=1" (hash "a" 1 "b" 1))
          (experiment "MyExperimentA>a=2>b=2" (hash "a" 1 "b" 2))
          (experiment "MyExperimentA>a=3>b=3" (hash "a" 3 "b" 3))
          (experiment "MyExperimentA>a=4>b=4" (hash "a" 4 "b" "xyz"))
          (experiment "MyExperimentB" (hash "x" 1))
          (experiment "MyExperimentC" (hash "z" 1)))))

(define (base-experiments-names experiments)
  (remove-duplicates
   (map first
        (map (λ (s) (string-split s ">"))
             (map experiment-name experiments)))))

(module+ test
  (check-equal? (list "MyExperimentA" "MyExperimentB" "MyExperimentC")
                (base-experiments-names experiments-fixture)))

(define (experiments-by-base-name experiments base-name)
  (filter (λ (exp) (string-prefix? (experiment-name exp) base-name))
          experiments))

(define (value-types-equal? h1 h2)
  (andmap identity
          (hash-map h1 (λ (k v1)
                         (let ([v2 (hash-ref h2 k null)]
                               [cmp-fns (list number? string? boolean? duration?)])
                           (ormap (λ (f) (and (f v1) (f v2))) cmp-fns))))))

(define (assert-value-types-equal e1 e2)
  (let ([h1 (experiment-values e1)]
        [h2 (experiment-values e2)])
    (andmap identity
            (hash-map h1 (λ (k v1)
                           (let ([v2 (hash-ref h2 k null)]
                                 [cmp-fns (list number? string? boolean? duration?)])
                             (unless (ormap (λ (f) (and (f v1) (f v2))) cmp-fns)
                               (raise-user-error
                                (format 
                                 "experiment type mismatch:\n Experiment ~a variable ~a\n dosen't match experiment ~a variable ~a"
                                 (experiment-name e1) k
                                 (experiment-name e2) k)))))))))
(module+ test
  (check-true (value-types-equal? (hash "a" 1 "b" 2 "c" 3)
                                  (hash "a" 100 "b" 3 "c" 10)))
  (check-false (value-types-equal? (hash "a" 1 "b" 2 "c" 3)
                                   (hash "a" #f "b" 3 "c" 10))))

(module+ test
  (check-not-exn (λ () (check-experiments-types experiments-fixture)))
  (check-exn exn:fail?
             (λ () (check-experiments-types malformed-fixture))))
