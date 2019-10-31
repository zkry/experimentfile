#lang racket

(provide (struct-out experiment)
         (struct-out distribution))

;; experiment is a name with a hashmap of names and values.
(struct experiment (name values))

;; distribution is a list of probabilities corresponding to a list of experiments.
;; most experiments will only have one distribution but many are possible
(struct distribution (probabilities conditions experiments over begin end))
