#lang racket

(require racket/draw racket/gui racket/list drracket/tool-lib "struct.rkt")
(provide button-list)

(define (render-experiment-analizer)
  ;; TODO: Get list of unique experiments
  (define exp-names (eval '(get-experiment-names out-distrs)))

  (define exp-values (eval '(get-experiment-values out-distrs)))
  
  (define frame (new frame%
                     [label "Experiment Analysis"]
                     [width 600]
                     [height 600]))
  
  (define panel (new horizontal-panel% [parent frame]
                     [alignment '(center center)]))

  (define exp-list-box (new list-box%
                            [parent panel]
                            [choices exp-names]
                            [label ""]
                            [callback (λ (lb event)
                                        (let ((selected (send lb get-selections)))
                                          (when (> (length selected) 0)
                                            (let ((idx (first selected)))
                                              (send text erase)
                                              (send text insert (format-hash (hash-ref exp-values (list-ref exp-names idx))))
                                              (send editor-canvas set-editor text)))))]))

  (define results-panel (new horizontal-panel% [parent panel]
                             [alignment '(center center)]))
  (define editor-canvas (new editor-canvas%
                             [parent results-panel]
                             [label "Editor Canvas"]
                             [style '(no-focus)]))
  
  (define text (new text%))
  (send text insert "Select an Experiment to\nview it's values.")
  (send editor-canvas set-editor text)
  
  (new text-field%
       [parent frame]
       [label "Search"]
       [callback (λ (tf event)
                   (define val (send tf get-value))
                   (when (not (equal? val ""))
                     (send exp-list-box set
                           (filter (λ (s) (string-contains? (string-downcase s) (string-downcase val))) exp-names))))])
  
  (send frame show #t))


(define (button-func drr-window)
  ;(define expr-string "@$  $@")
  (parameterize ([drracket:rep:after-expression
                  render-experiment-analizer])
    (send drr-window execute-callback)))

(define experiment-buttons
  (list
   "Analize Experiments"
   (make-object bitmap% "./experiment-analize-icon.png")
   button-func
   #f))

(define (format-hash h)
  (string-join
   (for/list ([key (hash-keys h)])
     (let ((val (hash-ref h key)))
       (format "~a: ~a" key val)))
   "\n"))

(define button-list (list experiment-buttons))
