;; title:   game title
;; author:  game developer, email, etc.
;; desc:    short description
;; site:    website link
;; license: MIT License (change this to your license of choice)
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
               bgc (or bgc 0)]
    (tri x y
         (+ x w) y
         (+ x (/ w 2)) (+ y h)
         col)

    (tri (+ x (* border 2.5)) (+ y border)
         (- (+ x w) (* border 2.5)) (+ y border)
         (+ x (/ w 2)) (- (+ y h) (* border 2))
         bgc)))

;; State Management
(local needs 
       {:people 20 :pmf 30 :funds 100
       :loss (fn [self] 
               (set self.people (- self.people 5))
               (set self.pmf (- self.pmf 10))
               (set self.funds (- self.funds 15)))
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
              (let [inc (or val? 30)]
              (set (. self need)
                   (math.min 100 (+ (. self need) inc)))))
       })

(fn button [x y text key f]
  (let [pressed (keyp key 60 60)
        shape (if pressed rect rectb)
        action (if pressed f (fn []))
        ]
    (shape x y (+ 4 (* 6 (length text))) 10 C)
    (print text (+ x 2) (+ y 2) C true)
    (action)
  ))

(fn buttons []
  (button 5 70 "Hire (z)" 26 (fn [] 
                               (trace "Hire")
                               (needs:add :people)))
  (button 5 90 "Research (x)" 24 (fn [] 
                                   (trace "Research")
                                   (needs:add :pmf)))
  (button 5 110 "Fundraise (c)" 3 (fn [] 
                                    (trace "Fundraise")
                                    (needs:add :funds))))


(var t 0)
(fn _G.TIC []
  (set t (+ t 1))
  (cls 0)
  (rectb 0 0 240 136 C)

  (if (needs:failed?)
      (print "Failure!" (- (// W 2) 20) (// H 2) C)
      (do
        (buttons)
        (trib {:col C :x 50 :y 30 :w 180 :h 100})
        (needs:render)

        (when (= 0 (% t 60))
          (needs:loss))

        )))

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

