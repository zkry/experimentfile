;; @ Wednesday, October 30th, 2019 7:48:51pm -----------
;; /Users/zromero/dev/racket/experiment/docs/program.rkt
#lang racket
(require "fact.rkt")
(fact 5)
;; @ Wednesday, October 30th, 2019 7:48:51pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang racket
(define (fact n)
  (if (= n 0) 1 (* n (fact (sub1 n)))))
(provide fact)
;; @ Wednesday, October 30th, 2019 7:48:53pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang racket
(define (fact n)
  (if (= n 0) 1 (* n (fact (sub1 n)))))
(provide fact)
;; @ Wednesday, October 30th, 2019 7:48:55pm -----------
;; /Users/zromero/dev/racket/experiment/docs/program.rkt
#lang racket
(require "fact.rkt")
(fact 5)
;; @ Wednesday, October 30th, 2019 7:48:55pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang racket
(define (fact n)
  (if (= n 0) 1 (* n (fact (sub1 n)))))
(provide fact)
;; @ Wednesday, October 30th, 2019 8:12:51pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:13:05pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10
  beta := 20

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:18:21pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:18:59pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:19:10pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10
  beta := 20

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:19:43pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10
  beta := 20

experiment MyExperimentB:
  gamma := 1

distribution:
  10: MyExperimentA

distribution:
  20: MyExperimentB

;; @ Wednesday, October 30th, 2019 8:21:02pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10
  beta := 20

experiment MyExperimentB:
  gamma := 1

distribution:
  10: MyExperimentA

distribution:
  20: MyExperimentB

;; @ Wednesday, October 30th, 2019 8:21:29pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10
  beta := 20

experiment MyExperimentB:
  gamma := 1

distribution:
  10: MyExperimentA

distribution:
  20: MyExperimentB

;; @ Wednesday, October 30th, 2019 8:36:09pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:36:22pm -----------
;; /Users/zromero/dev/racket/experiment/docs/fact.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10
  beta := 20

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:54:45pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 8:54:56pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment MyExperimentA:
  alpha = 10
  alphaInc := alpha + 5
  alphaDec := alpha - 5

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 9:22:15pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment StopBidderLogic:
  minCt := [1, 2, 3]
  stopCt := [5 to 10]

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 9:22:31pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment StopBidderLogic:
  minCt := [1, 2, 3]
  stopCt := [5 to 10]

distribution:
  10: StopBidderLogics
;; @ Wednesday, October 30th, 2019 9:22:36pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment StopBidderLogic:
  minCt := [1, 2, 3]
  stopCt := [5 to 10]

distribution:
  10: StopBidderLogic
;; @ Wednesday, October 30th, 2019 9:23:05pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment

package bid_logic

experiment StopBidderLogic:
  subexperiment A:
    minCt := 1
    maxCt := 5
  subexperiment B:
    minCt := 2
    maxCt := 7

distribution:
  10: MyExperimentA
;; @ Wednesday, October 30th, 2019 9:23:17pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment

package bid_logic

experiment StopBidderLogic:
  subexperiment A:
    minCt := 1
    maxCt := 5
  subexperiment B:
    minCt := 2
    maxCt := 7

distribution:
  10: StopBidderLogic
;; @ Wednesday, October 30th, 2019 9:23:43pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment

experiment StopBidderLogic:
  x := 1

experiment PiecewiseLinear:
  refCTR := 1

distribution:
  10: StopBidderLogic
  20: PiecewiseLinear
;; @ Wednesday, October 30th, 2019 9:32:03pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment

experiment StopBidderLogic:
  x := 7
  subexperiment A:
    minCt := 1
    maxCt := 5
  subexperiment B:
    minCt := 2
    maxCt := 7

distribution:
  10: StopBidderLogic:
;; @ Wednesday, October 30th, 2019 9:32:07pm -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment

experiment StopBidderLogic:
  x := 7
  subexperiment A:
    minCt := 1
    maxCt := 5
  subexperiment B:
    minCt := 2
    maxCt := 7

distribution:
  10: StopBidderLogic
;; @ Thursday, October 31st, 2019 11:17:10am -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment MyExperimentA:
  alpha := 10

distribution:
  10: MyExperimentA
;; @ Thursday, October 31st, 2019 11:17:25am -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment MyExperimentA:
  alpha = 10
  alphaInc := alpha + 5
  alphaDec := alpha - 5

distribution:
  10: MyExperimentA
;; @ Thursday, October 31st, 2019 11:17:38am -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment MyExperimentA:
  alphaInc := sqrt(alpha + 5)
  alphaDec := log(alpha - 5)

distribution:
  10: MyExperimentA
;; @ Thursday, October 31st, 2019 11:18:02am -----------
;; /Users/zromero/dev/racket/experiment/docs/example.rkt
#lang experiment
experiment StopBidderLogic:
  minCt := [1, 2, 3]
  stopCt := [5 to 10]

distribution:
  10: MyExperimentA
