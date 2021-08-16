(local js (require :js ))
(local lume (require :lume ))

(local board-min-x -2000)
(local board-max-x 2000)
(local board-min-y -2000)
(local board-max-y 2000)

(local board-width (- board-max-x board-min-x))
(local board-height (- board-max-y board-min-y))

(local grid-spacing 20)
(local half-grid-spacing (/ grid-spacing 2))
(local neg-half-grid-spacing (- half-grid-spacing))

(lambda inc [x] (+ x 1))
(lambda dec [x] (- x 1))

(fn nil? [x]
  (= x nil)
)
(fn table? [x]
  (= (type x) "table")
)
(fn fn? [x]
  (= (type x) "function")
)

(lambda fst [list] (. list 1))
(lambda snd [list] (. list 2))

(fn table-cat [table-1 table-2]
  (var result [])
  (each [_ tbl (ipairs [table-1 table-2])]
    (each [_ v (ipairs tbl)]
      (table.insert result v)
    )
  )
  result
)

(lambda table-append [tbl item]
  (table.insert tbl item)
  tbl
)

(lambda replicate [item times]
  (if (<= times 0)
    []
    (let [value (if (fn? item) (item) item)]
      (table-append (replicate item (dec times)) value)
    )
  )
)
(lambda join [list ?separator]
  (let [separator (or ?separator "")]
    (lume.reduce list #(.. $1 separator $2) "")
  )
)

(lambda pairs-list [tbl]
  (var result [])
  (each [k v (pairs tbl)]
    (table.insert result [k v])
  )
  result
)
(fn fmt [x level]
  (let [level (or level 0)
        indent (join (replicate "  " level))
        key #(. $1 1)
        value #(. $1 2)]
    (if
      (table? x)
        (join
          (lume.map
            (pairs-list x)
            #(.. indent (key $1) ":\n" (fmt (value $1) (inc level)) "\n")
          )
        )
      (.. indent (tostring x))
    )
  )
)

(lambda list-flatmap [list f]
  (var result [])
  (each [_ v (ipairs list)]
    (let [mapped (f v)]
      (when (not (nil? mapped))
        (table.insert result mapped)
      )
    )
  )
  result
)

(lambda clear [ctx width height]
  (ctx:clearRect 0 0 width height)
)
(lambda grid [ctx width height x-space y-space x-offset y-offset]
  (set ctx.strokeStyle "#fff")
  (set ctx.lineWidth 1.5)
  (let [h-gridline-count (/ width x-space)
        v-gridline-count (/ height y-space)]
    (for [i (- (/ h-gridline-count 2)) (/ h-gridline-count 2)]
      ; Draw vertical line
      (ctx:beginPath)
      (let [x (+ (* i x-space) (/ x-space 2) x-offset)]
        (ctx:moveTo x 0)
        (ctx:lineTo x height)
      )
      (ctx:stroke)
    )
    (for [i (- (/ v-gridline-count 2)) (/ v-gridline-count 2)]
      ; Draw horizontal line
      (ctx:beginPath)
      (let [y (+ (* i y-space) (/ y-space 2) y-offset)]
        (ctx:moveTo 0 y)
        (ctx:lineTo width y)
      )
      (ctx:stroke)
    )
  )
)
(lambda trace [ctx origin-x origin-y dx-dy]
  (each [_ [dx dy] (ipairs dx-dy)]
    (ctx:lineTo (+ origin-x dx) (+ origin-y dy))
  )
)
(lambda square [ctx x1 y1 x2 y2 ?style]
  (set ctx.strokeStyle (or (?. ?style :stroke) "#000"))
  (when (?. ?style :fill)
    (set ctx.fillStyle (?. ?style :fill))
  )
  (set ctx.lineWidth (or (?. ?style :line-width) 2))
  
  (ctx:beginPath)
  (ctx:moveTo x1 y1)
  (let [width (- x2 x1)
        height (- y2 y1)]
    (trace ctx x1 y1 (lume.map [[0 0] [0 1] [1 1] [1 0] [0 0]]
      #(let [i (. $1 1) j (. $1 2)]
        [(* i width) (* j height)]
      )
    ))
  )
  (ctx:stroke)
  (when (?. ?style :fill)
    (ctx:fill)
  )
)

; BEGIN GAME OF LIFE

(global advanceToken nil)
(global running? true)
(global advance-interval-ms 500)

(global set-advance-interval nil)

(math.randomseed (os.time))

(let [window js.global
      document window.document]
  (let [canvas (document:querySelector "#board")
        ctx (canvas:getContext "2d")
        canvas-width 1024
        canvas-height 720]
    (set canvas.width canvas-width)
    (set canvas.height canvas-height)
    
    (local home {
      :make-style (fn [] { :stroke "#777" :fill "#777" })
    })
    (local automaton {
      :make-style (fn [] { :stroke "#bbb" :fill "#bbb" })
    })
    
    (fn reset []
    
      (var board
        (lume.map
          (table-cat
            ; [[home 0 0]]
            (replicate #[automaton (math.random -10 -1) (math.random -10 -1)] 60)
            (replicate #[automaton (math.random  1  10) (math.random  1  10)] 60)
          )
          (fn [c] (let [ty (. c 1) x (. c 2) y (. c 3)]
            { :type ty :x x :y y }
          ))
        )
      )
      (fn build-board [matrix]
        (var board [])
        (each [_ l (pairs matrix)]
          (each [_ obj (pairs l)]
            (table.insert board obj)
          )
        )
        board
      )
      (fn obj-at [matrix x y]
        (?. (. matrix x) y)
      )
      (fn set-in-board-matrix [matrix obj]
        (tset matrix obj.x (or (. matrix obj.x) []))
        (tset (. matrix obj.x) obj.y obj)
      )
      (fn clear-in-board-matrix [matrix x y]
        (tset matrix x (or (. matrix x) []))
        (tset (. matrix x) y nil)
      )
      (fn toggle-in-board-matrix [matrix obj]
        (if (nil? (obj-at matrix obj.x obj.y))
          (set-in-board-matrix matrix obj)
          (clear-in-board-matrix matrix obj.x obj.y)
        )
      )
      (fn build-board-matrix [board]
        (var matrix [])
        (lume.each board #(set-in-board-matrix matrix $1))
        matrix
      )
      (var board-matrix (build-board-matrix board))
  
      (var x (/ canvas-width 2))
      (var y (/ canvas-height 2))
    
      (fn draw [ctx board]
        (clear ctx canvas-width canvas-height)
        (grid ctx board-width board-height grid-spacing grid-spacing x y)
        (print (.. "Drawing " (length board) " objects"))
        (each [_ obj (ipairs board)]
          ; (print (.. "Draw " (fmt obj)))
          (let [obj-canvas-x (* grid-spacing obj.x)
                obj-canvas-y (* grid-spacing obj.y)]
            (square ctx
              (+ neg-half-grid-spacing x obj-canvas-x) (+ neg-half-grid-spacing y obj-canvas-y)
              (+ half-grid-spacing x obj-canvas-x) (+ half-grid-spacing y obj-canvas-y)
              (obj.type.make-style)
            )
          )
        )
      )
      
      (var dragging? false)
      (var in-bounds? false)
      (var dragged? false)
      (canvas.classList:add "cursor-draggable")
      (fn canvas.onmousedown []
        (set dragging? true)
        (set in-bounds? true)
        (set dragged? false)
        (canvas.classList:remove "cursor-draggable")
        (canvas.classList:add "cursor-dragging")
      )
      (fn canvas.onmouseup [canvas event]
        (set dragging? false)
        (canvas.classList:remove "cursor-dragging")
        (canvas.classList:add "cursor-draggable")
        
        (fn round [x] (math.floor (+ x 0.5)))
        (when (not dragged?)
          (let [board-x (round (/ (+ (- x) event.offsetX) grid-spacing))
                board-y (round (/ (+ (- y) event.offsetY) grid-spacing))]
            (print (.. "Adding at " board-x " " board-y))
            (toggle-in-board-matrix board-matrix { :type automaton :x board-x :y board-y })
            (set board (build-board board-matrix))
      
            (draw ctx board)
          )
        )
      )
      (fn canvas.onmouseleave []
        (set in-bounds? false)
        (set dragged? true)
        (canvas.classList:remove "cursor-dragging")
        (canvas.classList:add "cursor-draggable")
      )
      (fn canvas.onmouseenter [canvas event]
        (set in-bounds? true)
        (set dragging? (and dragging? (= (band event.buttons 1) 1)))
      )
      (fn canvas.onmousemove [canvas event]
        (when (and dragging? in-bounds?)
          (set dragged? (or dragged? (> event.movementX 1) (> event.movementY 1)))
          
          ; Move grid.
          (set x (lume.clamp (+ x event.movementX) board-min-x board-max-x))
          (set y (lume.clamp (+ y event.movementY) board-min-y board-max-y))
      
          (draw ctx board)
        )
      )

      (fn neighbor [obj dx dy]
        (obj-at board-matrix (+ obj.x dx) (+ obj.y dy))
      )
      (local neighbor-deltas [
        [-1 -1]
        [0 -1]
        [1 -1]
        [-1 0]
        [1 0]
        [-1 1]
        [0 1]
        [1 1]
      ])
      (fn neighbors [obj]
        ; (print (.. "Neighs for " (fmt obj)))
        (local a (lume.map
          neighbor-deltas
          #(neighbor obj (fst $1) (snd $1))
        ))
        ; (print "Done neighs")
        a
      )
      (fn count-neighbors [neighbors]
        (length (lume.keys neighbors))
      )
      (fn survives [neighbors]
        (let [count (count-neighbors neighbors)]
          (or
            (= count 2)
            (= count 3)
          )
        )
      )
      (fn should-be-produced [obj]
        (= (count-neighbors (neighbors obj)) 3)
      )
    
      (fn advance []
        ; (print (fmt board-matrix))
        (var new-matrix [])
        (each [_ obj (pairs board)]
          ; Check surrounding cells for potential newborns.
          (for [i 1 8]
            (let [deltas (. neighbor-deltas i)
                  candidate { :type automaton :x (+ obj.x (fst deltas)) :y (+ obj.y (snd deltas)) }]
              (when (should-be-produced candidate)
                (set-in-board-matrix new-matrix candidate)
              )
            )
          )
        
          ; Check if cell stays alive.
          (if
            (= obj.type automaton)
              (if
                (and
                  (survives (neighbors obj))
                )
                (set-in-board-matrix new-matrix obj)
              )
          
            (set-in-board-matrix new-matrix obj)
          )
        )
        (set board-matrix new-matrix)
        (set board (build-board board-matrix))
        ; (print (fmt board-matrix))
      
        (draw ctx board)
      )

      (global set-advance-interval (fn [interval]
        (when _G.advanceToken
          (window:clearInterval advanceToken)
        )
        (global advanceToken (window:setInterval #(when running? (advance)) interval))
      ))
    
      (draw ctx board)
      (set-advance-interval advance-interval-ms)
    )
    
    (reset)
    
    (window:addEventListener "keydown" (fn [_ event]
      (match event.key
        "r" (reset)
        "p" (global running? (not running?))
      )
    ))
    (let [reset-button (document:querySelector "#button-reset")]
      (set reset-button.onclick reset)
    )
    (let [pause-button (document:querySelector "#button-pause")]
      (set pause-button.onclick (fn []
        (global running? (not running?))
      ))
    )
    (let [speed-slider (document:querySelector "#slider-speed")]
      (set speed-slider.oninput (fn [speed-slider]
        (let [attrs speed-slider.attributes
              max-attr (attrs:getNamedItem "max")
              max (js.tonumber max-attr.value)]
          (global advance-interval-ms (- max speed-slider.value))
          (set-advance-interval advance-interval-ms)
        )
      ))
    )
  )
)
