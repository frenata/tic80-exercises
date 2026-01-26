;; title:   Fox Busters
;; author:  Andrew Nichols <andrew@frenata.net>
;; desc:    The chickens take revenge on the foxes.
;; site:    www.frenata.net
;; license: MIT License
;; version: 0.1
;; script:  fennel
;; strict:  true

(var t 0)
(var eggs [])

(fn lay-egg [x y vec]
  {:x x :y y :vec vec})

(local chicken 
       {:x 30 :y 30
        :render (fn [self t]
                  (let [frame (% t 20)]
                    (if (> 10 frame)
                        (spr 256 self.x self.y 0)
                        (spr 257 self.x self.y 0)
                        )))
        :move-x (fn [self x]
                 (let [dx (+ self.x x)
                       tx (if (> 5 dx) 5 (> dx 225) 225 dx)]
                   (set self.x tx)))

        :move-y (fn [self y]
                 (let [dy (+ self.y y)
                       ty (if (> 5 dy) 5 (> dy 121) 121 dy)]
                   (set self.y ty)))

        :shoot (fn [self vec]
                   (sfx 0 "E-4")
                   (table.insert eggs (lay-egg self.x self.y vec))
                 )
        })

(fn controls [player] 
  (when (btn 0) (player:move-y -1))
  (when (btn 1) (player:move-y  1))
  (when (btn 2) (player:move-x -1))
  (when (btn 3) (player:move-x  1))
  (when (btnp 4 30 20) (player:shoot [2 0]))
  (when (btnp 5 30 20) (player:shoot [0 2]))
  )

(fn render-eggs [] 
  (set eggs
       (icollect [_ egg (ipairs eggs)]
    (do
      (elli egg.x egg.y 1 2 13)
      {:x (+ egg.x (. egg.vec 1))
       :y (+ egg.y (. egg.vec 2))
       :vec egg.vec}))))

(fn _G.TIC []
  (cls 1)
  (set t (+ t 1))

  (chicken:render t)
  (controls chicken)
  (render-eggs eggs)
  )

;; <TILES>
;; 000:0000000000777700776667007766674007666775007667700077770000070000
;; </TILES>

;; <SPRITES>
;; 000:0000000000777700776667007766674007666775000667700007770000070000
;; 001:0000000000777700006667000766674077666775770667707007770000070000
;; 002:0000020000002200000022030002233333a33333333333330023333000023330
;; </SPRITES>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:720052a022700270027002801290129022a042a0628082209200d200f200f200f200f200f200f200f200f200f200f200f200f200f200f200f200f200324000000000
;; </SFX>

;; <TRACKS>
;; 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </TRACKS>

;; <PALETTE>
;; 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
;; </PALETTE>

