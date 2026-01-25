;; title:   Feed-a-Startup
;; author:  Andrew Nichols <andrew@frenata.net>
;; desc:    Try to keep the startup from dying until it goes unicorn!
;; site:    www.frenata.net
;; license: MIT License
;; version: 0.1
;; script:  fennel
;; strict:  true

(local fennel (require :fennel))

(local H 136)
(local W 240)
(local C 2)

;; Utilities
(fn trib [{: x : y : w : h : border : bgc : col}]
  (let [border (or border 2)
        bgc    (or bgc 0)
        col    (or col C)
        y-dir  (if (> 0 h) 1 -1)]

    (tri 
      (- x (// w 2)) 
      (- y (// h 2))

      (+ x (// w 2)) 
      (- y (// h 2))

      x 
      (+ y (// h 2))

      col)

    (tri 
      (- x (// w 2) (* border -2.5)) 
      (- y (// h 2) (* y-dir border))

      (+ x (// w 2) (* border -2.5)) 
      (- y (// h 2) (* y-dir border))

      x 
      (+ y (// h 2) (* 2 y-dir border))

      bgc)))

(fn min-by [vals f def]
  (let [res 
         (accumulate [min def
                      _ k (ipairs vals)]
           (if (< (f k) min.v) 
               {:v (f k) :k k} 
               min))]
    res.k))

;; State Management
(local needs 
       {:reset (fn [self]
                (set self.people 20)
                (set self.pmf 40)
                (set self.funds 80)
                (set self.over-cared 0))
       :loss (fn [self] 
               (let [rate (if (> self.over-cared 0) 3 1)]
                 (when (> self.over-cared 0) (set self.over-cared (- self.over-cared 1)))
                 (set self.people (- self.people (* 2 rate)))
                 (set self.pmf (- self.pmf (* 4 rate)))
                 (set self.funds (- self.funds (* 6 rate)))))
       :draw (fn [self x need]
         (rectb x 15 50 10 C)
         (rect  x 15 (/ (. self need) 2) 10 C)
         (print need x 5 C))
       :render (fn [self]
                 (self:draw 50 :people)
                 (self:draw 110 :pmf)
                 (self:draw 170 :funds))
       :failed? (fn [self]
                  (or 
                    (> 0 self.people)
                    (> 0 self.pmf)
                    (> 0 self.funds)))
       :add (fn [self need val?]
              (let [inc (or val? 30)
                    new-val (+ (. self need) inc)]
                (when (> new-val 100)
                  (set self.over-cared 5)
                  (cls 1))
                (set (. self need)
                     (math.min 100 (+ (. self need) inc)))
                ))
       :state (fn [self]
                (case (min-by [:people :pmf :funds] 
                              (fn [k] (. self k))
                              {:v 50 :k :healthy})
                  :people :under-hired
                  :pmf    :bad-product
                  :funds  :broke
                  _ :healthy))
       })

(fn button [x y text key f]
  (let [pressed (keyp key 60 60)
                shape (if pressed rect rectb)
                action (if pressed f (fn []))
                ]
    (shape x y (+ 4 (* 6 (length text))) 10 C)
    (print text (+ x 2) (+ y 2) C true)
    (action)))


(fn left-panel []

  ;; Message
  (print (needs:state) 5 60 C)

  ;; Player Buttons
  (button 5 90 "Hire (z)" 26 (fn [] 
                               (trace "Hire")
                               (needs:add :people)))
  (button 5 105 "Research (x)" 24 (fn [] 
                                    (trace "Research")
                                    (needs:add :pmf)))
  (button 5 120 "Fundraise (c)" 3 (fn [] 
                                    (trace "Fundraise")
                                    (needs:add :funds))))

(fn logo [state]
  (trib {:col C :x 140 :y 80 :w 180 :h 100})

  (let [mouth (case state
                :under-hired {:w 50 :h 8}
                :bad-product {:w 30 :h -30}
                :broke       {:w 50 :h -50}
                :healthy     {:w 30 :h 30}
                )]
    (trib {:x 140 :y 90 :w mouth.w :h mouth.h}))


  (let [eyes (case state
                :under-hired {:w 30 :h 5}
                :bad-product {:w 10 :h 10}
                :broke       {:w 30 :h -30}
                :healthy     {:w 30 :h 20}
                )]
    (trib {:x 90 :y 50 :w eyes.w :h eyes.h})
    (trib {:x 190 :y 50 :w eyes.w :h eyes.h})))

(fn right-panel []
  (logo (needs:state))
  (needs:render))

(fn instructions [msg sub-msg]
  (print msg 
         (- (// W 2) (// (* (length msg) 6) 2)) (- (// H 2) 20) C)
  (print sub-msg 
         (- (// W 2) (// (* (length sub-msg) 6) 2)) (- (// H 2) 10) C))

(var t 0)
(fn _G.TIC []
  (set t (+ t 1))
  (cls 0)
  (rectb 0 0 240 136 C)

  (print "Digi" 5 5 C false 2)
  (print "Tec" 5 17 C false 2)

  (when (keyp 48) (needs:reset))

  (if (= nil needs.people)
      (instructions "Can you keep the startup fed?" "<space> to start")

      (needs:failed?)
      (instructions "Out of Business!" "<space> to restart")

      (> t 60000)
      (instructions "Unicorn Exit!" "<space> to restart")

      (do
        (left-panel)
        (right-panel)

        (when (= 0 (% t 60))
          (needs:loss)))))

;; <TILES>
;; 032:2222222222222222220000002200000022200000022200000022200000022200
;; 033:2222222222222222000000000000000000000000000000000000000000000000
;; 034:2222222222222222000000220000002200000222000022200002220000222000
;; 048:0000222000000222000000220000000200000000000000000000000000000000
;; 049:0000000000000000200000022200002222200222022222200022220000022000
;; 050:0222000022200000220000002000000000000000000000000000000000000000
;; </TILES>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
;; </SFX>

;; <TRACKS>
;; 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

