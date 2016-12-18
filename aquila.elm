import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)


aquila: Float -> Form

aquila t = 
          let 
          rt=t/1000
          dw=sin(rt*5)
          in
          group [ polygon [(0,50), (300,50), (250,0),(15,0), (25,25), (12,27)]
                  |> filled (rgb 64 40 3)
                  |> move (0,0)
          
                , rect 2 250 
                  |> filled (rgb 64 40 3)
                  |> move (100,150)
            
                , rect 2 200 
                  |> filled (rgb 64 40 3)
                  |> move (200,150)
                
                , rect 2 75
                  |> filled (rgb 64 40 3)
                  |> move (335,56)
                  |> rotate (100*pi/180)
                
                , polygon [(0,20),(-40+sin(rt*16),20+cos(rt*15)),(-40+sin(rt*16+0.5),0+cos(rt*15+0.2)),(0,0)]
                  |> filled black
                  |> move (100,250)
                
                , assassin
                  |>scale 0.04
                  |> move (82+sin(rt*16),255)
                  
                  
                , sail dw
                  |> scale 1.5
                  |> move (0,20)
                , sail dw
                  |> scale 1.8
                  |> move (0,-50)
                , sail dw
                  |> scale 1.2
                  |> move (0,80)
                , sail dw
                  |> scale 1.5
                  |> move (-100,20)
                , sail dw
                  |> scale 1.8
                  |> move (-100,-50)
                , sail dw
                  |> scale 1.2
                  |> move (-100,80)
                  
                , polygon [(0,50),(50,20),(20,0)]
                  |> filled white
                  |> move (250,60)
                  |> scale 1.5
                , polygon [(0,50),(50,20),(20,0)]
                  |> filled white
                  |> move (245,64)
                  |> scale 1.4
                , polygon [(0,50),(50,20),(20,0)]
                  |> filled white
                  |> move (240,66)
                  |> scale 1.35
                  
                , polygon [(0,50),(60,25),(60,0),(0,-10)]
                  |> filled white
                  |> move (120,70)
                  |> scale 1.2
                , polygon [(0,50),(50,25),(50,0),(0,2)]
                  |> filled white
                  |> move (20,70)
                  |> scale 1.5
               
                 
                 ,polygon [(0,50),(300,50),(288,40),(7,40)]
                  |> filled (rgba 22 36 41 0.8)
                 
                  
                 , rect 220 12
                  |> filled 
                  --(rgba 180 180 180 1.8)
                  (rgba 250 36 36 0.8)
                  |> move (144,35)
                
                , rect 200 12
                  |> filled 
                  --(rgba 180 180 180 1.8)
                  (rgba 250 36 36 0.8)
                  |> move (148,20)
                  
                , rect 210 5
                  |> filled (rgba 22 36 41 0.9)
                  |> move (145,27.5)
                 
                , polygon [(21,15),(265,15),(250,0),(15,0)]
                  |> filled (rgba 22 36 41 0.8)
                 
                , rect 240 1
                  |> filled (rgba 255 215 0 0.8)
                  |> move (141,10)
                , rect 238 1
                  |> filled (rgba 255 215 0 0.8)
                  |> move (139,7)
                
                , cannon
                  |> move (45,35)
                , cannon
                  |> move (65,35)
                , cannon
                  |> move (85,35)
                , cannon
                  |> move (105,35)  
                , cannon
                  |> move (125,35)
                , cannon
                  |> move (145,35)
                , cannon
                  |> move (165,35)
                , cannon
                  |> move (185,35)
                , cannon
                  |> move (205,35)
                , cannon
                  |> move (225,35)
                , cannon
                  |> move (245,35)
                , cannon
                  |> move (270,35)
                , cannon
                  |> move (237.5,20)
                , cannon
                  |> move (217.5,20)
                , cannon
                  |> move (197.5,20)
                , cannon
                  |> move (177.5,20)
                , cannon
                  |> move (157.5,20)
                , cannon
                  |> move (137.5,20)
                , cannon
                  |> move (117.5,20)
                , cannon
                  |> move (97.5,20)
                , cannon
                  |> move (77.5,20)
                , cannon
                  |> move (57.5,20) --`trace` "moved to (57.5,20)"
                  
                 
              ]
              
              
sail: Float -> Form

sail dw = polygon [(0,50),(8+dw,35),(10+dw,25),(8+dw,15),(0,0),(6+dw,15),(7+dw,25),(6+dw,35)]
                    |> filled white
                    |> move (200,100)



cannon = group [square 10
                |> filled (rgb 51 12 8)
                
                ,circle 4 
                 |> filled black
                ]


assassin : Form
assassin = group [
            filled white(polygon[(0, 490), (752, 162), (697, 20), (924, 25), (1104, 455), (940, 226), (189, 485), (907, 787), (1104, 547), (921, 979), (695, 987), (750, 829)])|> scale 0.4 |> rotate (degrees 270) |> move(-200, 350)
            ]


grad : Gradient
grad =
  linear (0,360) (0,0)
    [ (0, blue)
    , (0.5, rgb 135 206 235)
    , (0.9, white)
    , (1, white)
    ]


main = Signal.map render_col <| Signal.foldp update initial_render (every millisecond)


initial_render : (Form,Float,Float,Float,Float,List Form)
initial_render = 
                  let x = 30
                      y = 15
                  in
                 (
                  aquila 0
                  |> move(-200,0)
                  
                  ,x ,y ,(700/x) ,(100/y)
    
                ,(List.map ini_opg (oceanini x y 700 100 0)))
                  
ini_opg : Form -> Form
ini_opg opg = opg |> move (-380,-100)
     
update : Float -> (Form,Float,Float,Float,Float,List Form) -> (Form,Float,Float,Float,Float,List Form)
update t (_,x,y,dx,dy,waves) =(
                                    (aquila t |> move (-200+5*sin(t/5000),0+5*cos(t/2500)))
                                    ,x ,y ,dx ,dy
                                    ,(oceansimlr waves x x y dx dy (t/1000))
                               )

render_col : (Form,Float,Float,Float,Float,List Form) -> Element
render_col (aquila,x,y,dx,dy,waves) = 
                collage 700 700
                 [rect 700 700 |> gradient grad
    
                , rect 700 400 
                  |> filled blue
                  |> move(0,-180)
    
                , circle 15
                  |> filled (rgb 255 203 51)
                  |> move (-100,300)
                  
                , aquila
                , group waves
                
                ]


oceansimlr : List Form -> Float -> Float -> Float -> Float -> Float -> Float -> List Form
oceansimlr ocean ox x y dx dy t = case ocean of [] -> []
                                                ti::ts -> (ti
                                                |>move(sin(t*1.5+x+y/3)*dx/120+(rand t),cos(2*t+x/2+y)*dy/80+(rand t)))
                                                ::
                                                (if x==1 
                                                    then oceansimlr ts ox ox (y-1) dx dy t
                                                    else oceansimlr ts ox (x-1) y dx dy t )


opgon : Float -> Float -> Form
opgon x y = group[polygon [(0,0),(x/2,y),(x,0)]
                  |> filled (rgba 52 101 164 0.2)
                 ,polygon [(0,0),(x/2,y),(x,0)]
                  |> filled (rgba 255 255 255 0.4)
                  |> rotate 0.05
                  |> move (x/10,y/10)
                 ,polygon [(0,0),(x/2,y),(x,0)]
                  |> filled (rgba 52 101 164 0.8)
                  |> rotate -0.05
                  |> move (x/10+x/10,y/10+y/10)
                 ]

oceanini: Float -> Float -> Float -> Float -> Float -> List Form
oceanini x y ox oy t = oceansamplr (opgon (2*ox/x) (oy/y)) x x y (ox/x) (oy/y) t

oceansamplr : Form -> Float -> Float -> Float -> Float -> Float -> Float -> List Form
oceansamplr opgon ox x y dx dy t = case y of 
                                      0 -> []
                                      y -> (opgon
                                            |>move((x+(rand t)+sin(t/2+x+y))*dx,(y+(rand t)+sin(t+y/2))*dy)) :: (if x==1 
                                                                      then oceansamplr opgon ox ox (y-1) dx dy t
                                                                      else oceansamplr opgon ox (x-1) y dx dy t )




rand t = 0


--float -1 1







