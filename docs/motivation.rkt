#lang slideshow

(require pict
         pict/conditional
         slideshow/play
         slideshow/code
         slideshow/repl
         racket/gui/base
         racket/runtime-path
         racket/draw)

(define (rgb r g b)
  (make-object color% (exact-round r) (exact-round g) (exact-round b)))

(define bidder-logic-color (rgb 90 140 215))

(define (random-color)
  (rgb  (random 256) (random 256) (random 256)))

(define (labeled-rect txt w h [color #f] [border-color #f])
  (if color
      (ct-superimpose (filled-rectangle w h #:color color #:border-color border-color #:border-width (and border-color 3)) (text txt))
      (ct-superimpose (rectangle w h) (text txt))))

(define (multi-fan boxes dy)
  (cc-superimpose (first boxes)
                  (vc-append (second boxes) (blank 1 dy))
                  (vc-append (blank 1 dy) (third boxes))))

(define (gen-offer-selection-box [node-ct #f] [logic-ct 1] [suberimpose-fn cb-superimpose] )
  (suberimpose-fn (labeled-rect "Offer Selection" 160 40 "blue")
                  (let* ((config-nodes (for/list ((i (range (if node-ct node-ct (random 1 4))))) (filled-ellipse 10 10 #:color (random-color))))
                         (config-col (apply vc-append config-nodes))
                         (logic-boxes (for/list ((i (range logic-ct))) (filled-rectangle 10 10 #:color "red")))
                         (logic-col (apply vc-append logic-boxes)))
                    (foldl (lambda (cfg-node acc)
                             (foldl (lambda (logic-box acc) (pin-arrow-line 7
                                                                            acc
                                                                            cfg-node rc-find
                                                                            logic-box lc-find)) acc logic-boxes))
                           (hc-append config-col (blank 120 0) logic-col)
                           config-nodes))))

(begin
  (slide
   (t "AB Testing with the Bidder")))

(slide
 #:title "Problem"
 (item "Offer selection logic is too much mixed with bidder logic")
 (item "Ideally the \"optimization\" related logic would be separate from bidder logic.")
 (item "This would allow for " (bt "fast iterations") " reducing time from idea to implementation."))

(let ((os-boxes (for/list ((i (range 3))) (gen-offer-selection-box))))
  (play-n
   (lambda (n p ni pi crazy)
     (let* ((out 0)
            (nnet (- n (* ni 1.3)))
            (pnet (- p pi)))
       (define offer-selection-box (if (> crazy 0) (gen-offer-selection-box (exact-round (* crazy 10)) (exact-round (* crazy 7)) cc-superimpose) (multi-fan os-boxes (* pnet 100))))
       (define bidder-logic-box (filled-rectangle 600 200 #:color (rgb 90 140 215)))
       (define scene (pin-arrows-line 30 (ct-superimpose (cc-superimpose bidder-logic-box (hc-append 5
                                                                                                     (labeled-rect "Process Request" 120 80 "SkyBlue")
                                                                                                     (labeled-rect "Other logic" 120 60 "LightSkyBlue")
                                                                                                     (if (= crazy 0) (pin-over (blank) 0 (+ (* nnet 200) (* pi 50))
                                                                                                                               offer-selection-box)
                                                                                                         offer-selection-box)))
                                                         (text "Bidder Logic"))
                                      offer-selection-box lt-find
                                      bidder-logic-box cb-find
                                      #:alpha (max nnet 0)
                                      #:style 'long-dash
                                      #:label (if (= nnet 1) (text "GRPC Service") (blank))))
       (define scaled-scene (scale scene (+ 1 (* out 3))))
       (inset scaled-scene
              (- (* out 1300)) 0 0 0)))
   #:steps 10
   #:delay 0.05))

(let ([offer-selection-box (gen-offer-selection-box)]
      [bidder-logic-box (labeled-rect "bidder logic" 200 100 bidder-logic-color)])
  (slide
   #:title "Option 1: Separate the Offer Selection Logic to Microservice"
   (pin-arrows-line 30 (hc-append 200 bidder-logic-box offer-selection-box)
                    bidder-logic-box rc-find
                    offer-selection-box lc-find
                    #:style 'long-dash
                    #:label (text "GRPC"))
   (item "Latency would prove to be a problem.")
   (item "Offer selection code written in Python may would add more latency")
   (item "The management and upkeeup of an entirely new microservice would require time")))

(let* ([conf-file (cc-superimpose (file-icon 190 250 "bisque")
                                  (vc-append (text "Experiment 1:" '(bold) 15 0)
                                             (text "alpha := 1")
                                             (text "beta := 2")
                                             (text " ")
                                             (text "bid-price(baseCPM) =")
                                             (text "  alpha * baseCPM + beta")
                                             (text " ")
                                             (text "Experiment 2:" '(bold) 15 0)
                                             (text "alphaInc := 0.7")
                                             (text "alphaDec := 0.2")
                                             (text " ")
                                             (text "bid-price(baseCPM, winprice) =")
                                             (text "  ...")
                                             (text " ")
                                             (text "Traffic Distribution:" '(bold))
                                             (text "10% -> exp1, 10% -> exp2")))]
       [offer-selection-box (gen-offer-selection-box)]
       [bidder-logic-box (cc-superimpose (labeled-rect "bidder logic" 200 100 bidder-logic-color)
                                         offer-selection-box)])
  (slide
   #:title "Option 2: Have the bidderlogic stored in a config file"
   (pin-arrow-line 30 (hc-append 200 conf-file bidder-logic-box)
                   conf-file rc-find
                   bidder-logic-box lc-find)
   (item "Has the advantage of no network communication during bidder logic")
   (item "Problem: How do we dynamically load such logic into the bidder?")))



(let* ([conf-file (cc-superimpose (file-icon 190 210 "bisque")
                                  (vc-append (text "PiecewiseLinear:" '(bold) 15 0)
                                             (text "alphaInc := 0.7")
                                             (text "alphaDec := 0.2")
                                             (text " ")
                                             (text "Traffic Distribution:" '(bold))
                                             (text "10% -> exp1, 10% -> exp2")))]
       [offer-selection-box (gen-offer-selection-box)]
       [bidder-logic-box (cc-superimpose (labeled-rect "bidder logic" 200 100 bidder-logic-color)
                                         offer-selection-box)])
  (slide
   #:title "Option 3: Have experiment configuration stored in a config file"
   (pin-arrow-line 30 (hc-append 200 conf-file bidder-logic-box)
                   conf-file rc-find
                   bidder-logic-box lc-find)
   (item "Storing configuration is easy: JSON, YAML, TOML, XML...")
   (item "Problem: What happens when there are 20+ experiments?"
         (subitem "Keeping track of many experiments is difficult")
         (subitem "Managing multiple variations of an experiment is difficult"))))

(let* ([conf-file (cc-superimpose (file-icon 190 210 "bisque")
                                  (vc-append (text "10% -> PiecewiseLinear" '(bold) 15 0)
                                             (text "12% -> ExperimentB" '(bold) 15 0)))]
       [offer-selection-box (gen-offer-selection-box)]
       [bidder-logic-box (cc-superimpose (labeled-rect "bidder logic" 200 100 bidder-logic-color)
                                         offer-selection-box)])
  (slide
   #:title "Option 4: Store only names of experiments and traffic"
   (pin-arrow-line 30 (hc-append 200 conf-file bidder-logic-box)
                   conf-file rc-find
                   bidder-logic-box lc-find)
   (item "This is even easier!")
   (item "Problem: This is pretty much what we have already."
         (subitem "Any change will have to go through the realtime team, both configuration, and logic wise."))))

(define (name-block-stack names r g b [step 10] [hl-idx -1])
  (if (null? names) (blank)
      (let ([name (car names)])
        (cb-superimpose (labeled-rect name (+ (* 10 (apply max (map string-length names))) (* (length names) 10)) (* (length names) 20) (rgb r g b) (if (= 0 hl-idx) "red" #f))
                        (name-block-stack (cdr names) (min (+ r step) 255) (min (+ g step) 255) (min (+ b step) 255) step (sub1 hl-idx))))))

(slide
 #:title "Finding the sweet spot"
 (name-block-stack '("GRPC Service" "Logic Specification" "Configuration Specification" "Traffic Specification" "Experiment Enumeration") 70 120 120 15)
 (item "More separation between teams implies..."
       (subitem "↓ larger scope")
       (subitem "↓ more complicated configuration")
       (blank)
       (subitem "↑ faster iteration")
       (subitem "↑ less overhaed with Realtime team"))
 'next
 (item "Where do we see this project ending?"))

(slide
 #:title "Proposed Solution"
 'alts
 (list (list (name-block-stack '("GRPC Service" "Logic Specification" "Configuration Specification" "Traffic Specification" "Experiment Enumeration") 70 120 120 15 3)
             (item "This is what we currently have hard-coded into the bidder logic."))
       (list (name-block-stack '("GRPC Service" "Logic Specification" "Configuration Specification" "Traffic Specification" "Experiment Enumeration") 70 120 120 15 2)
             (item "We can first start decoupling"
                   (subitem "the experiment enumeration")
                   (subitem "the specification of the type of traffic")
                   (subitem "the provisioning of configuration values for experiments")))
       (list (name-block-stack '("GRPC Service" "Logic Specification" "Configuration Specification" "Traffic Specification" "Experiment Enumeration") 70 120 120 15 1)
             (item "And " (bt "after") " we have that done, we can work on figuring out how to decouple various aspects of the logic."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART II

(slide
 (t "Part II: Implementation"))

(define (implementation-stack n) (name-block-stack '("GRPC Service" "Logic Specification" "Configuration Specification" "Traffic Specification" "Experiment Enumeration") 120 90 100 15 n))

(define-syntax with-impl-stack
  (syntax-rules ()
    [(with-impl-stack n x ...) (rb-superimpose
                                (cc-superimpose
                                 (frame titleless-page #:color "white")
                                 (vc-append x ... (blank 1 100)))
                                (scale (implementation-stack n) 0.7))]))

(slide
 #:title "Experiment enumeration"
 (with-impl-stack 4
   (para "At a minumal level, the optimization team should be able to enumerate the various experiments that exist.")
   (codeblock-pict (string-join
                    '("{"
                      "  experiments: ["
                      "    \"piecewiseLinear\","
                      "    \"myExpA\","
                      "    \"myExpB\""
                      "  ]"
                      "}") "\n"))
   (t "simple!")))

(let ([json-example (codeblock-pict (string-join
                                     '("{"
                                       "  experiments: ["
                                       "    \"piecewiseLinear\","
                                       "    \"myExpA\","
                                       "    \"myExpB\""
                                       "  ]"
                                       "  distribution: ["
                                       "    {name: \"piecewiseLinear\", traffic: 10},"
                                       "    {name: \"myExpA\", traffic: 15},"
                                       "  ]"
                                       "}") "\n"))])
  (slide
   #:title "Traffic Specification"
   (with-impl-stack 3
     (para "This is what we had written in the Go code.")
     json-example
     (t "simple as well!")))

  (slide
   #:title "Traffic Specification"
   (with-impl-stack 3
     (item "It is impossible to create new experiments of offshoots of existing experiments without digging into the Go code.")
     (item (it "piecewiseLinear, myExpA") "and" (it "myExpB") "are just names."
           (subitem "What do these experiments mean?")
           (subitem "Where are these experiments configuration and logic?"))
     (scale json-example 0.5))))



;; TODO make an animation that changes with each item being added
(current-font-size 15)

(define-syntax small-item
  (syntax-rules ()
    [(small-item x ...) (item #:bullet (circle 10) x ...)]))
                        
(define-syntax show-when
  (syntax-rules ()
    [(show-when x y) (show y (> x 0))])) 

(define circle-size 10)

(define (conf-circle rgb)
  (filled-ellipse circle-size circle-size #:color (rgb-list->color rgb)))

(define (rgb-list->color l)
  (rgb (first l) (second l) (third l)))

(define (random-color-offset val)
  (let ([variation 100])
    (max 0 (min 255 (+ val (- (random variation) (exact-round (/ variation 2))))))))

(define (vari-conf-circles n r [rgb (list (random 256) (random 256) (random 256))])
  (let* ([circles (for/list ((i (range n))) (rotate (vc-append
                                                     (filled-ellipse circle-size circle-size #:color (rgb-list->color (map random-color-offset rgb)))
                                                     (vline 10 (/ r 2))
                                                     (blank 10 (/ r 2)))
                                                    (* (/ (* 2 pi) n) i)))])
    (apply cc-superimpose circles)))

(define (logic-ext-circles n h [rgb (list (random 256) (random 256) (random 256))])
  (if (= 0 h)
      (conf-circle rgb)
      (let* ([root-circle (conf-circle rgb)]
             [circles (for/list ((i (range n))) (conf-circle (map random-color-offset rgb)))])
        (foldl (lambda (circle acc)
                 (pin-arrow-line 1 acc circle ct-find root-circle cb-find))
               (vc-append root-circle (blank 1 h) (apply hc-append (cons (- h 10) circles)))
               circles))))

(define color-list
  (for/list ((i (range 100))) (list (random 256) (random 256) (random 256))))

(define ball-type-list
  (for/list ((i (range 100))) (random 3)))

(define ball-ct-list
  (for/list ((i (range 100))) (random 3 10)))

(define ball-rotation-list
  (for/list ((i (range 100))) (* 2 (random) pi)))

(define (config-scene many-iteration unique-params same-logic param-variation)
  (let* ([max-balls 18]
         [scene-w 600]
         [scene-h 300]
         [tree-len (* same-logic 20)]
         [group-r (* param-variation 60)]
         [ball-ct (exact-round (* unique-params max-balls))]
         [group-counts (take ball-ct-list ball-ct)]
         [ball-colors (take color-list ball-ct)]
         [ball-types (take ball-type-list ball-ct)]
         [ball-rotations (take ball-rotation-list ball-ct)]
         [scene-items (for/list ((color ball-colors)
                                 (type ball-types)
                                 (n group-counts)
                                 (theta ball-rotations))
                        (rotate (cond
                                  [(= 0 type) (conf-circle color)]
                                  [(= 1 type) (logic-ext-circles n tree-len color)]
                                  [(= 2 type) (vari-conf-circles n group-r color)]
                                  [else (error "unknown type")])
                                theta))]
         [row1-item-ct (exact-round (/ ball-ct 2))]
         [row1-items (take scene-items row1-item-ct)]
         [row2-items (drop scene-items row1-item-ct)])
    (cc-superimpose
     (blank scene-w scene-h)
     (vc-append 10
                (apply hc-append (cons 10 row1-items))
                (apply hc-append (cons 10 row2-items))))))

(play-n
 (lambda (many-iteration unique-params same-logic param-variation a1)
   (with-impl-stack 2
     (config-scene many-iteration unique-params same-logic param-variation)
     (small-item "Adding the configurations to the mix is where the complexity")
     (show-when many-iteration (small-item "Fast iterations imply that many tests will be created"))
     (show-when unique-params (small-item "Each test will have it's own set of parameters which are" (it "not") "universal across experiments"))
     (show-when same-logic (small-item "Some experiments should share the same logic of other experiments."))
     (show-when param-variation (small-item "Multiple experiments with slight variations of a value may exist. If handled poorly, this could lead to an explosion in complexity"))
     (show-when a1 (small-item (bt "Any configuration file, let alone JSON would be able to handle this.")))))
 #:title "Configuration Specification")


(current-font-size 32)

(slide
 #:title "Configuration Specification: Managing Complexity"
 (with-impl-stack 2
   (item "Number of experiments + variation of parameters + mixed schemas → Extreme complexity")
   (item "JSON would be a very bad option no matter what.")
   (item "YAML or TOML would be an improvement.")
   (item "I think there can be something better however.")
   (item "It is hard to go up to \"Logic Specification\" without solving this problem.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART III
;(start-at-recent-slide)

(slide
 (t "Part III: The Experiment File"))

(slide
 #:title "Goal"
 (para "What would be the " (it "easiest") "way of representing multiple experiment configurations?")
 (item "Needs to be easy to understand.")
 (item "Should be able to \"contain\" all of the complexity, both now and in the future.")
 (item "Should require a small learning curve (like learning YAML) and be intuative.")
 (item "Should solve a user interface problem to allow optimization team to write clear correct experiments."
       (subitem "Functionality should be handled by a format such as JSON.")))

(define experimentfile1 (codeblock-pict #:keep-lang-line? #f
                                        (string-join
                                         '("#lang experiment"
                                           "experiment PiecewiseLinearExperiment:"
                                           "  refCTR :=  0.001"
                                           "  alphaDec := 1"
                                           "  alphaInc := 0.25"
                                           "  minCampaignCPMFraction := 0.6"
                                           "  maxCampaignCPMFraction := 0.9"
                                           ""
                                           "experiment DiversityExperiment:"
                                           "  minReq := [0, 1, 2, 3]"
                                           "  maxReq := [5, 6, 7]"
                                           ""
                                           "distribution:"
                                           "  10: PiecewiseLinearExperiment when `sspid == 1`"
                                           "  10: DiversityExperiment"
                                           "") "\n")))


(define experimentfile1-json (codeblock-pict (string-join (list "{"
                                                                "    \"distributions\": ["
                                                                "        {"
                                                                "            \"over\": false,"
                                                                "            \"conditions\": ["
                                                                "                \"sspid == 1\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\","
                                                                "                \"\""
                                                                "            ],"
                                                                "            \"experiments\": ["
                                                                "                {"
                                                                "                    \"name\": \"PiecewiseLinearExperiment\","
                                                                "                    \"values\": {"
                                                                "                        \"minCampaignCPMFraction\": 0.6,"
                                                                "                        \"alphaDec\": 1,"
                                                                "                        \"refCTR\": 0.001,"
                                                                "                        \"alphaInc\": 0.25,"
                                                                "                        \"maxCampaignCPMFraction\": 0.9"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=0,maxReq=5\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 5,"
                                                                "                        \"minReq\": 0"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=0,maxReq=6\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 6,"
                                                                "                        \"minReq\": 0"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=0,maxReq=7\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 7,"
                                                                "                        \"minReq\": 0"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=1,maxReq=5\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 5,"
                                                                "                        \"minReq\": 1"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=1,maxReq=6\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 6,"
                                                                "                        \"minReq\": 1"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=1,maxReq=7\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 7,"
                                                                "                        \"minReq\": 1"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=2,maxReq=5\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 5,"
                                                                "                        \"minReq\": 2"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=2,maxReq=6\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 6,"
                                                                "                        \"minReq\": 2"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=2,maxReq=7\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 7,"
                                                                "                        \"minReq\": 2"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=3,maxReq=5\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 5,"
                                                                "                        \"minReq\": 3"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=3,maxReq=6\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 6,"
                                                                "                        \"minReq\": 3"
                                                                "                    }"
                                                                "                },"
                                                                "                {"
                                                                "                    \"name\": \"DiversityExperiment>minReq=3,maxReq=7\","
                                                                "                    \"values\": {"
                                                                "                        \"maxReq\": 7,"
                                                                "                        \"minReq\": 3"
                                                                "                    }"
                                                                "                }"
                                                                "            ],"
                                                                "            \"probabilities\": ["
                                                                "                10.0,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334,"
                                                                "                0.8333333333333334"
                                                                "            ]"
                                                                "        }"
                                                                "    ]"
                                                                "}") "\n")))


(slide
 #:title "Example"
 (hc-append 10
            (cc-superimpose (file-icon 400 300 "honeydew")
                            (scale experimentfile1 0.4))
            (arrow 30 0)
            (scale experimentfile1-json 0.1)))

(define (open-json-btn filename) (clickback (cc-superimpose (filled-rectangle 180 60 #:color "PaleGreen")
                                                            (text (~a "Open " filename)))
                                            (lambda ()
                                              (let* ([f (new frame% [label (path-element->string
                                                                            (file-name-from-path filename))]
                                                             [width 600] [height 100]
                                                             [x 10] [y 10])]
                                                     [e (new text%)]
                                                     [c (new editor-canvas% [parent f] [editor e])])
                                                (send e load-file filename)
                                                (send e change-style
                                                      (make-object style-delta% 'change-family 'modern)
                                                      0 'end)
                                                (send f show #t)))))

(define code-examples '(("Defining Values"
                         "#lang experiment"
                         "experiment MyExperimentA:"
                         "  alpha := 10"
                         ""
                         "distribution:"
                         "  10: MyExperimentA")
                        ("Variables"
                         "#lang experiment"
                         "experiment MyExperimentA:"
                         "  alpha = 10"
                         "  alphaInc := alpha + 5"
                         "  alphaDec := alpha - 5"
                         ""
                         "distribution:"
                         "  10: MyExperimentA")
                        ("Functions"
                         "#lang experiment"
                         "experiment MyExperimentA:"
                         "  alphaInc := sqrt(alpha + 5)"
                         "  alphaDec := log(alpha - 5)"
                         ""
                         "distribution:"
                         "  10: MyExperimentA")
                        ("Ranges"
                         "#lang experiment"
                         "experiment StopBidderLogic:"
                         "  minCt := [1, 2, 3]"
                         "  stopCt := [5 to 10]"
                         ""
                         "distribution:"
                         "  10: MyExperimentA")
                        ("Sub-experiments"
                         "#lang experiment"
                         ""
                         "experiment StopBidderLogic:"
                         "  x := 7"
                         "  subexperiment A:"
                         "    minCt := 1"
                         "    maxCt := 5"
                         "  subexperiment B:"
                         "    minCt := 2"
                         "    maxCt := 7" 
                         ""
                         "distribution:"
                         "  10: StopBidderLogic:")
                        
                        ("Distributions"
                         "#lang experiment"
                         ""
                         "experiment StopBidderLogic:"
                         "  x := 1"
                         ""
                         "experiment PiecewiseLinear:"
                         "  refCTR := 1"
                         ""
                         "distribution:"
                         "  10: StopBidderLogic"
                         "  20: PiecewiseLinear")
                        
                        ("'when' cases"
                         "#lang experiment"
                         ""
                         "experiment StopBidderLogic:"
                         "  x := 1"
                         ""
                         "experiment GoogleSSPBehaviorTest:"
                         "  refCTR := 1"
                         ""
                         "distribution:"
                         "  10: MyExperimentA when `auctiontype == 1`"
                         "  20: GoogleSSPBehaviorTest when `sspid == 1 && auctiontype == 2`"
                         "")
                        
                        ("Logic offshoots (work in progress)"
                         "#lang experiment"
                         ""
                         "experiment MainBidderLogic:"
                         "  alpha := 0.9"
                         "  beta := 1.15"
                         "  maxBidLimit := 21000000"
                         "  bidThreshold := 6000000"
                         ""
                         "experiment BetaTest logicof MainBidderLogic:"
                         "  beta := 1.30"
                         ""
                         "distribution:"
                         "  5: BetaTest"
                         "")
                        
                        ("Code Generation Directives"
                         "#lang experiment"
                         ""
                         "package bid_logic"
                         ""
                         "experiment StopBidderLogic:"
                         "  x := 1"
                         ""
                         "distribution:"
                         "  10: StopBidderLogic:")
                        
                        ("Logic (Considering idea) 1"
                         "#lang experiment"
                         ""
                         "experiment StopOfferSelection:"
                         "  subexperiment static:"
                         "    base_bid := 1000"
                         "  subexperiment fromWinprice"
                         "    base_bid := `winprice / 2`"
                         ""
                         "distribution:"
                         "  10: StopOfferSelection:")

                        ("Logic (Considering idea) 2"
                         "#lang experiment"
                         ""
                         "experiment BidPriceCalculation:"
                         "  refCTR := 0.001"
                         "  alphaInc := 0.25"
                         "  alphaDec := 1"
                         "  minCampaignCPMFraction := 0.6"
                         "  maxCampaignCPMFraction := 0.9"
                         "  base_bid := -> CalculateOfferCPMWithPiecewiseLinear"
                         "                 ApplyRelativeBidPriceLimits"
                         "distribution:"
                         "  10: BidPriceCalculation:")
                        ))

(for ([example code-examples])
  (let* ([title (car example)]
         [code (cdr example)]
         [rg (make-repl-group)]
         [backing (keyword-apply make-module-backing '(#:module-name) '("example.rkt")  (append (list rg) code))])
    (slide
     #:title title
     (module-area backing
                  #:width 800
                  #:height 270
                  #:font-size 15
                  #:background "LightBlue")
     (result-area rg
                  #:width 800
                  #:height 270
                  #:background "PaleTurquoise")
     (hc-append (open-json-btn "experiment.json")
                (open-json-btn "experiment.go")))))
    
