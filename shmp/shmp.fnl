;; title:   Fox Busters
;; author:  Andrew Nichols <andrew@frenata.net>
;; desc:    The chickens take revenge on the foxes.
;; site:    www.frenata.net
;; license: MIT License
;; version: 0.1
;; script:  fennel
;; strict:  true

(local fennel (require :fennel))

(var t 0)
(var eggs [])
(var foxes [])
(var score {:hits 0 :missed 0 :laid 0})

(fn collision-check [buf]
  (var collision nil)
  (icollect [i fox (ipairs foxes)]
    (icollect [j egg (ipairs eggs)]

      (when 
        ;; TODO: improve this to use the bounds of the sprites
        ; (> buf (+ 
        ;       (math.abs (- egg.x fox.x))
        ;       (math.abs (- egg.y fox.y))))
        (and
          (> 2 (- fox.x egg.x))
          (> 1 (- fox.y egg.y))
          (< -4 (- fox.x egg.x))
          (< -3 (- fox.y egg.y))
          )

        (set collision [i j])
        
      )
    )
  )
  (when collision
    (table.remove eggs (. collision 2))
    ; (trace (fennel.view (. foxes (. collision 1))))
    (tset (. foxes (. collision 1)) :dead true)
    (set score.hits (+ score.hits 1)
    ; (trace (fennel.view (. foxes (. collision 1))))
    ; (table.remove foxes (. collision 1))
  ; (trace (fennel.view collision))
  )
  ))

(fn spawn-fox [t]
  (when (= 0 (% t 60))
    (table.insert 
      foxes 
      {:x 300 :y 120 :vec [-1 0]
      :render 
      (fn [self] (spr (if self.dead 259 258) self.x self.y 0))
      :dead false
      }
      )))

(fn lay-egg [x y vec]
  {:x x :y y :vec vec
  :render (fn [self] (elli self.x self.y 1 2 13))
  })

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
                (table.insert eggs 
                              (lay-egg self.x self.y vec))
                (set score.laid (+ score.laid 1))
                )
       })

(fn controls [player] 
  (var vec-x 0)
  (var vec-y 2)

  (when (btn 0) (player:move-y -1)
    (set vec-y (- vec-y 1)))
  (when (btn 1) 
    (player:move-y 1) 
    (set vec-y (+ vec-y 1)))
  (when (btn 2) (player:move-x -1)
    (set vec-x (- vec-x 1)))
  (when (btn 3) (player:move-x  1)
    (set vec-x (+ vec-x 1)))
  (when (btnp 4 30 20) 
    (player:shoot [vec-x vec-y])))


(fn render-moving [ents]
  (icollect [_ ent (ipairs ents)]
    (do
      ; (trace (fennel.view ent))
      (: ent :render)
      (set ent.x (+ ent.x (. ent.vec 1)))
      (set ent.y (+ ent.y (. ent.vec 2)))
      ;; TODO: drop the entity when off the screen by too much
      (if (and (> ent.x -50) (< ent.x 350)
               (> ent.y -50) (< ent.y 200))
          ent
          (when (~= ent.dead nil)
            (set score.missed (+ score.missed 1)))
          )
      )))

(fn background [t]
  "to give a sense of horizontal movement, a little jank"
  (let [ b1 (- (% (- 100 t) 1400) 700)
            b2 (- (% (- 300 t) 1400) 700)
            b3 (- (% (- 500 t) 1400) 700)
            b4 (- (% (- 700 t) 1400) 700) ]
    (for [i -40 256]
      (line i 0 (+ i 30) 150 
            (if (> b1 i) 0
                (> b2 i) 1
                (> b3 i) 2
                (> b4 i) 1
                )))))

(fn _G.TIC []
  (cls 10)
  (set t (+ t 1))

  (background t)
  (print (.. "Foxes Squashed: " score.hits) 5 5 9 true)
  (print (.. "Foxes Missed  : " score.missed) 5 15 9 true)
  (print (.. "Eggs  Laid    : " score.laid) 5 25 9 true)

  (chicken:render t)
  (controls chicken)
  (spawn-fox t)

  (set foxes (render-moving foxes))
  (set eggs (render-moving eggs))
  (collision-check 4)
  )

;; <SPRITES>
;; 000:0000000000777700776667007766674007666775000667700007770000070000
;; 001:0000000000777700006667000766674077666775770667707007770000070000
;; 002:0000020000002200000022030002233333a33333333333330023333000023330
;; 003:00000000000000000000000000000000000000000000000000a3112233333333
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

