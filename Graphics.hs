{-
INSTRUCTIONS: Start up the board with "main" in terminal.
Use "W,A,S,D" keys to move a player around.
Use "1,2,3,4" to switch between playerA, playerB, playerC and playerD
-}


module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

-- Main function taking in all the functions necessary. write "main" in terminal to execute the program.
main :: IO ()

-- Prints to a display
print :: String -> IO ()

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

main = play window background fps initialState render keyPress update

-- TODO: Fix this function
print str = do
  putStrLn str

-- Generates a window with name and size.
window :: Display
window = InWindow "Monopoly Polacksbacken" (720,720) (10, 10)

-- gives a backgroundcolor to the window.
background :: Color
background = colorOfBoard

-- Our own defined colors
brown = makeColor 0.39453125 0.26171875 0.12890625 1 
lightBlue = light (light (light blue))
colorOfBoard = dark (makeColor 0.804 0.902 0.816 1)


{-gameUpdater game
    Takes the current gamestate and returns a new updated one 
    PRE: ?
    RETURNS: a new gamestate with updated status.
    SIDE EFFECTS: ?
-}
gameUpdater :: MonopolyGame -> MonopolyGame
gameUpdater game = game { playerALoc = playerALoc game
                          ,playerBLoc = playerBLoc game
                          ,playerCLoc = playerCLoc game
                          ,playerDLoc = playerDLoc game
                         ,currentPlayer = currentPlayer game
                         }



{- keyPress event game
    A function designed to intake keybindings and make changes in the game.
    PRE:?
    RETURNS: A completely new gamestate which depending on which key pressed has changed the gamestate.
    SIDE EFFECTS: The players move their characters on "W,A,S,D" keypresses and switches between character on "1,2,3,4".
    EXAMPLES: keyPress (EventKey (Char 'a') _ _ _) { playerALoc = ((x, y) } = { playerALoc = ((x-15), y) }
-}
keyPress :: Event -> MonopolyGame -> MonopolyGame
keyPress (EventKey (Char '1') _ _ _) game =  game { currentPlayer = PlayerA }
keyPress (EventKey (Char '2') _ _ _) game =  game { currentPlayer = PlayerB }
keyPress (EventKey (Char '3') _ _ _) game =  game { currentPlayer = PlayerC }
keyPress (EventKey (Char '4') _ _ _) game =  game { currentPlayer = PlayerD }
    
-- Pressing key 'a' will move the currentPlayer left half a step.
keyPress (EventKey (Char 'a') _ _ _) game =
    
    case currentPlayer game of

        PlayerA -> game { playerALoc = ((x-15), y) }
            where
                (x, y) = playerALoc game

        PlayerB -> game { playerBLoc = ((x-15), y) }
            where
                (x, y) = playerBLoc game

        PlayerC -> game { playerCLoc = ((x-15), y) }
            where
                (x, y) = playerCLoc game 

        PlayerD -> game { playerDLoc = ((x-15), y) }
            where
                (x, y) = playerDLoc game

-- Pressing key 'd' will move the currentPlayer right half a step
keyPress (EventKey (Char 'd') _ _ _) game =
    
    case currentPlayer game of

        PlayerA -> game { playerALoc = ((x+15), y) }
            where
                (x, y) = playerALoc game
        
        PlayerB -> game { playerBLoc = ((x+15), y) }
            where
                (x, y) = playerBLoc game
        
        PlayerC -> game { playerCLoc = ((x+15), y) }
            where
                (x, y) = playerCLoc game 
        
        PlayerD -> game { playerDLoc = ((x+15), y) }
            where
                (x, y) = playerDLoc game

--Pressing key 'w' will move the currentPlayer upwards half a step
keyPress (EventKey (Char 'w') _ _ _) game =
    
    case currentPlayer game of

        PlayerA -> game { playerALoc = (x, (y+15)) }
            where
                (x, y) = playerALoc game

        PlayerB -> game { playerBLoc = (x, (y+15)) }
            where
                (x, y) = playerBLoc game

        PlayerC -> game { playerCLoc = (x, (y+15)) }
            where
                (x, y) = playerCLoc game 

        PlayerD -> game { playerDLoc = (x, (y+15)) }
            where
                (x, y) = playerDLoc game

-- Pressing key 's' will move the currentPlayer downwards half a step
keyPress (EventKey (Char 's') _ _ _) game =
    
    case currentPlayer game of

        PlayerA -> game { playerALoc = (x, (y-15)) }
            where
                (x, y) = playerALoc game

        PlayerB -> game { playerBLoc = (x, (y-15)) }
            where
                (x, y) = playerBLoc game

        PlayerC -> game { playerCLoc = (x, (y-15)) }
            where
                (x, y) = playerCLoc game 

        PlayerD -> game { playerDLoc = (x, (y-15)) }
            where
                (x, y) = playerDLoc game

keyPress _ game = game

-- Frames per second
fps :: Int
fps = 60

-- Needed in the main function to keep updating the game with the help of gameUpdater.
update :: Float -> MonopolyGame -> MonopolyGame
update _ = gameUpdater





data Player = PlayerA | PlayerB | PlayerC | PlayerD deriving (Eq, Show)





-- | A data structure to hold the state of the Monopoly game.
data MonopolyGame = Game
    { playerALoc :: (Float, Float)  --  First players (x, y) location.
    , playerBLoc :: (Float, Float)  --  Second players (x, y) location. 
    , playerCLoc :: (Float, Float)  --  Third players (x, y) location.
    , playerDLoc :: (Float, Float) --  Fourth players (x, y) location. 
    , currentPlayer :: Player -- Has four choices of players and holding one as selected currentPlayer

    } 
    deriving (Eq, Show) 

-- | Draw a Monopoly game state (convert it to a picture).
render :: MonopolyGame -> Picture
render game = pictures [walls, centerSquare, board, playerA, playerB, playerC, playerD]
    
    where
        
        -- centerSquare is the square with the same color as the background to cover the center of the board.
        centerSquare :: Picture
        centerSquare = color colorOfBoard $ rectangleSolid 530 530
        
        -- Red box at center with black outlines
        redSquare :: Color -> Float -> Float -> Picture
        redSquare col wid hei = translate (5) (15) $ scale 0.5 0.5 $ color col $ (rotate (320) (rectangleSolid wid hei))

        -- All horizontal walls/lines
        hzwall :: Float -> Picture
        hzwall x = translate x 0 $ rectangleSolid 10 720
        
        -- All vertical walls/lines   
        vrwall :: Float -> Picture
        vrwall y = translate 0 y $ rectangleSolid 720 10     


        -- The Monopoly center text
        monopolyText :: Float -> Float -> Color -> Picture
        monopolyText x y col = translate x y $ scale 0.5 0.5 $ color col $ (rotate (320) (text "Monopoly"))
     
        -- The Polacksbacken center text  
        polacksbackenText :: Float -> Float -> Color -> Picture
        polacksbackenText x y col = translate x y $ scale 0.2 0.2 $ color col $ (rotate (320) (text "Polacksbacken"))
      
        -- The community chest central rectangle
        communityChestBox :: Float -> Picture
        communityChestBox x = translate x 165 $ scale 0.5 0.5 $ color black $ (rotate (320) (rectangleWire (270) (140)))
        
        -- The chance central rectangle
        chanceBox :: Float -> Picture
        chanceBox x = translate x (-150) $ scale 0.5 0.5 $ color black $ (rotate (320) (rectangleWire (270) (140)))
        
        -- The chance central text
        chanceText :: Float -> Picture
        chanceText x = translate x (-170) $ scale 0.1 0.1 $ color black $ (rotate (320) (text "Chance"))

        -- The community chest central text
        communityChestText :: Float -> Float -> String -> Picture
        communityChestText x y str = translate x y $ scale 0.1 0.1 $ color black $ (rotate (320) (text str))
        
         -- The GO text
        goText :: Float -> Picture
        goText x = translate x (-325) $ scale 0.2 0.2 $ color black $ (rotate (320) (text "GO"))


        -- Part of the GO arrow
        goArrow :: Float -> Picture
        goArrow x = translate x (-345) $ scale 0.19 0.19 $ color black $ text "--"


        -- The arrow tip
        goArrowTip :: Picture
        goArrowTip = translate (300) (-335) $ scale 0.2 0.2 $ color black $ (rotate (270) (polygon [ (-50,-50), ( 0, 0), ( 50,-50), ( 0,100) ]))
        
        
        -- The street colors
        streetColors x y col rot= translate x y $ scale 0.2 0.2 $ color col $ rotate rot $ (rectangleSolid (250) (-100))


        -- The chance on the streets
        streetChance :: Float -> Float -> Float -> Picture
        streetChance x y rot = translate x y $ scale 0.3 0.3 $ color rose $ (rotate rot (text "?"))
        

        -- Drawings for jail
        jailDrawer :: Float -> Float -> Float -> Color -> Float -> Float -> Float -> Picture
        jailDrawer x y sca col rot width height = translate x y $ scale sca sca $ color col $ rotate rot $(rectangleSolid (width) (height))
    

        -- The street names
        streetNames :: Float -> Float -> Float -> String -> Picture
        streetNames x y rot str = translate x y $ scale 0.08 0.08 $ rotate rot $ text str

        
        -- Player A which is a red car.
        playerA = Pictures [ uncurry translate (a, b) $ color black $ rectangleSolid 30 10,
                             uncurry translate ((a+2), (b+4)) $ color black $ rectangleSolid 20 10,
                             uncurry translate ((a+2), (b+4)) $ color playerACol $ rectangleSolid 14 5,
                             uncurry translate (a, b) $ color playerACol $ rectangleSolid 25 5 ,
                             uncurry translate (a-8, b-5) $ color black $ circleSolid 4,
                             uncurry translate (a+8, b-5) $ color black $ circleSolid 4
                           ]
                    where
                        (a, b) = playerALoc game
        -- Player B which is a blue car.
        playerB = Pictures [ uncurry translate (c, d) $ color black $ rectangleSolid 30 10,
                             uncurry translate ((c+2), (d+4)) $ color black $ rectangleSolid 20 10,
                             uncurry translate ((c+2), (d+4)) $ color playerBCol $ rectangleSolid 14 5,
                             uncurry translate (c, d) $ color playerBCol $ rectangleSolid 25 5 ,
                             uncurry translate (c-8, d-5) $ color black $ circleSolid 4,
                             uncurry translate (c+8, d-5) $ color black $ circleSolid 4
                           ]
            where
                (c, d) = playerBLoc game
        -- Player C which is a green car.
        playerC = Pictures [ uncurry translate (e, f) $ color black $ rectangleSolid 30 10,
                             uncurry translate ((e+2), (f+4)) $ color black $ rectangleSolid 20 10,
                             uncurry translate ((e+2), (f+4)) $ color playerCCol $ rectangleSolid 14 5,
                             uncurry translate (e, f) $ color playerCCol $ rectangleSolid 25 5 ,
                             uncurry translate (e-8, f-5) $ color black $ circleSolid 4,
                             uncurry translate (e+8, f-5) $ color black $ circleSolid 4
                           ]
            where
                (e, f) = playerCLoc game
        
        -- Player D which is an orange car.
        playerD = Pictures [ uncurry translate (g, h) $ color black $ rectangleSolid 30 10,
                             uncurry translate ((g+2), (h+4)) $ color black $ rectangleSolid 20 10,
                             uncurry translate ((g+2), (h+4)) $ color playerDCol $ rectangleSolid 14 5,
                             uncurry translate (g, h) $ color playerDCol $ rectangleSolid 25 5 ,
                             uncurry translate (g-8, h-5) $ color black $ circleSolid 4,
                             uncurry translate (g+8, h-5) $ color black $ circleSolid 4
                           ]
            where
                (g, h) = playerDLoc game
        
        -- All the player colors
        playerACol = dark red
        playerBCol = light blue
        playerCCol = dark green
        playerDCol = light orange
        

        -- All the wall drawings needed to separate the streets.
        walls = pictures [
                        hzwall 355, hzwall (-355), vrwall 355, vrwall (-355),
                        hzwall 270, hzwall (-270), vrwall 270, vrwall (-270),
                        hzwall 210, hzwall (-210), vrwall 210, vrwall (-210),
                        hzwall 150, hzwall (-150), vrwall 150, vrwall (-150),
                        hzwall 90, hzwall (-90), vrwall 90, vrwall (-90),
                        hzwall 30, hzwall (-30), vrwall 30, vrwall (-30)
                        ]
        
        -- These are all the graphics that is needed for aesthetics. All the texts, colors, symbols etc.
        board = Pictures [
                                redSquare black 830 230,
                                redSquare red 800 200,
                                
                                monopolyText (-85) (-70) black,
                                monopolyText (-85.5) (-70) black,
                                monopolyText (-86) (-70) black,
                                monopolyText (-86.5) (-70) black,
                                monopolyText (-87) (-70) black,
                                monopolyText (-87.5) (-70) black,
                                monopolyText (-88) (-70) white,
                                monopolyText (-88.5) (-70) white,
                                monopolyText (-89) (-70) white,
                                polacksbackenText (-23) (-67) black,
                                polacksbackenText (-23.5) (-67) black,
                                polacksbackenText (-24) (-67) black,
                                polacksbackenText (-24.5) (-67) white,
                                polacksbackenText (-25) (-67) white,
                                
                            
                                
                                communityChestBox (-105),
                                communityChestBox (-105.5),
                                communityChestBox (-106),
                                communityChestBox (-106.5),
                                communityChestText (-135) (145) "Community",
                                communityChestText (-135.5) (145) "Community",
                                communityChestText (-136) (145) "Community",
                                communityChestText (-110) (140) "Chest",
                                communityChestText (-110.5) (140) "Chest",
                                communityChestText (-111) (140) "Chest",
                                
                                chanceBox (150),
                                chanceBox (150.5),
                                chanceBox (151),
                                chanceBox (151.5),
                                chanceText (135),
                                chanceText (135.5),
                                chanceText (136),
                                
                                goText (300),
                                goText (300.5),
                                goText (301),
                                goText (301.5),
                                goArrow (300),
                                goArrow (301),
                                goArrow (302),
                                goArrow (303),
                                goArrowTip,
                                
                                streetColors (240) (-285) brown 0,
                                streetColors (120) (-285) brown 0,
                                streetColors (-60) (-285) lightBlue 0,
                                streetColors (-180) (-285) lightBlue 0,
                                streetColors (-240) (-285) lightBlue 0,
                                streetColors (-285) (-240) rose 90,
                                streetColors (-285) (-120) rose 90,
                                streetColors (-285) (-60) rose 90,
                                streetColors (-285) (60) orange 90,
                                streetColors (-285) (180) orange 90,
                                streetColors (-285) (240) orange 90,
                                streetColors (-240) (285) red 0,
                                streetColors (-120) (285) red 0,
                                streetColors (-60) (285) red 0,
                                streetColors (60) (285) yellow 0,
                                streetColors (120) (285) yellow 0,
                                streetColors (240) (285) yellow 0,
                                streetColors (285) (240) (dark (dark green)) 90,
                                streetColors (285) (180) (dark (dark green)) 90,
                                streetColors (285) (60) (dark (dark green)) 90,
                                streetColors (285) (-120) (dark blue) 90,
                                streetColors (285) (-240) (dark blue) 90,
                                
                                streetChance (-130) (-330) 0,
                                streetChance (-130.5) (-330) 0,
                                streetChance (-131) (-330) 0,
                                streetChance (-170) (330) (180),
                                streetChance (-170.5) (330) (180),
                                streetChance (-171) (330) (180),

                                jailDrawer (-300) (-300) 0.23 black 0 250 250,
                                jailDrawer (-300) (-300) 0.2 orange 0 250 250,
                                jailDrawer (-300) (-300) 0.22 black 320 150 150,
                                jailDrawer (-300) (-300) 0.2 white 320 150 150,
                                jailDrawer (-300) (-300) 0.2 black 50 10 150,
                                jailDrawer (-305) (-295) 0.2 black 50 10 150,
                                jailDrawer (-295) (-305) 0.2 black 50 10 150,

                                streetNames (218) (-310) 0 "ITC-1145",
                                streetNames (225) (-345) 0 "$ 60",

                                streetNames (160) (-310) 0 "Comm.",
                                streetNames (160) (-325) 0 "Chest",
                                
                                streetNames (98) (-310) 0 "ITC-1213",
                                streetNames (105) (-345) 0 "$ 60",

                                streetNames (38) (-310) 0 "Income",
                                streetNames (38) (-320) 0 "Tax",
                                streetNames (38) (-330) 0 "Pay",
                                streetNames (38) (-345) 0 "200 $",

                                streetNames (-22) (-310) 0 "Polacks",
                                streetNames (-22) (-325) 0 "Backen",
                                streetNames (-20) (-345) 0 "$ 200",

                                streetNames (-82) (-310) 0 "Angstrom",
                                streetNames (-82) (-325) 0 "6K1113",
                                streetNames (-82) (-345) 0 "$ 100",

                                streetNames (-202) (-310) 0 "Angstrom",
                                streetNames (-202) (-325) 0 "2004",
                                streetNames (-202) (-345) 0 "$ 100",

                                streetNames (-262) (-310) 0 "Angstrom",
                                streetNames (-262) (-325) 0 "4007",
                                streetNames (-262) (-345) 0 "$ 120",
                                
                                streetNames (-345) (-300) 90 "JUST",
                                streetNames (-320) (-345) 0 "VISITING",

                                streetNames (-310) (-220) 90 "ITC-1111",
                                streetNames (-345) (-225) 90 "$ 140",

                                streetNames (-310) (-160) 90 "Electric",
                                streetNames (-325) (-160) 90 "Comp.",
                                streetNames (-345) (-165) 90 "$ 150",

                                streetNames (-310) (-100) 90 "ITC-1211",
                                streetNames (-345) (-105) 90 "$ 140",
                            
                                streetNames (-310) (-38) 90 "ITC-2146",
                                streetNames (-345) (-42) 90 "$ 160",

                                streetNames (-310) (20) 90 "Grind-",
                                streetNames (-325) (20) 90 "stugan",
                                streetNames (-345) (15) 90 "$ 200",

                                streetNames (-310) (82) 90 "ITC-1549",
                                streetNames (-345) (77) 90 "$ 180",

                                streetNames (-310) (140) 90 "Comm.",
                                streetNames (-325) (140) 90 "Chest",

                                streetNames (-310) (202) 90 "ITC-2510",
                                streetNames (-345) (197) 90 "$ 180",

                                streetNames (-310) (262) 90 "ITC-1515",
                                streetNames (-345) (257) 90 "$ 200",

                                streetNames (-292) (312) 135 "FREE",
                                streetNames (-305) (335) 135 "PARKING",

                                streetNames (-220) (310) 180 "Lilla",
                                streetNames (-220) (325) 180 "Mikro.",
                                streetNames (-220) (345) 180 "$ 220",

                                streetNames (-100) (310) 180 "Stora",
                                streetNames (-100) (325) 180 "Mikro.",
                                streetNames (-100) (345) 180 "$ 220",

                                streetNames (-40) (310) 180 "Foobar",
                                streetNames (-40) (345) 180 "$ 240",

                                streetNames (20) (310) 180 "Uppsala",
                                streetNames (20) (320) 180 "Science",
                                streetNames (20) (330) 180 "Park",
                                streetNames (20) (345) 180 "$ 200",

                                streetNames (80) (310) 180 "Konfer.",
                                streetNames (80) (320) 180 "Rum",
                                streetNames (80) (345) 180 "$ 260",

                                streetNames (140) (310) 180 "Massen",
                                streetNames (140) (345) 180 "$ 260",

                                streetNames (200) (310) 180 "Water",
                                streetNames (200) (320) 180 "Works",
                                streetNames (200) (345) 180 "$ 150",

                                streetNames (260) (310) 180 "Aulan",
                                streetNames (260) (345) 180 "$ 280",

                                streetNames (315) (290) 225 "GO TO ",
                                streetNames (325) (315) 225 "JAIL",

                                streetNames (310) (220) 270 "Polhem.",
                                streetNames (320) (220) 270 "Salen",
                                streetNames (345) (220) 270 "$ 300",

                                streetNames (310) (160) 270 "Hagg.",
                                streetNames (325) (160) 270 "Salen.",
                                streetNames (345) (160) 270 "$ 300",

                                streetNames (310) (100) 270 "Comm.",
                                streetNames (320) (100) 270 "Chest.",

                                streetNames (310) (38) 270 "Siegbahn.",
                                streetNames (322) (38) 270 "Salen.",
                                streetNames (345) (38) 270 "$ 320",

                                streetNames (310) (-24) 270 "Lundellska",
                                streetNames (320) (-24) 270 "Skolan",
                                streetNames (345) (-22) 270 "$ 200",

                                streetNames (325) (-75) 270 "?",
                                streetNames (325.5) (-75) 270 "?",
                                streetNames (326) (-75) 270 "?",

                                streetNames (310) (-140) 270 "UTH-",
                                streetNames (320) (-140) 270 "Gard",
                                streetNames (345) (-140) 270 "$ 350",

                                streetNames (310) (-200) 270 "Luxury",
                                streetNames (325) (-200) 270 "Tax",
                                streetNames (345) (-200) 270 "$ 100",

                                streetNames (310) (-262) 270 "Skrubben",
                                streetNames (345) (-262) 270 "$ 400"
                                ]


-- | The starting state for the game of Monopoly.
initialState :: MonopolyGame
initialState = Game
  { playerALoc = (300, (-300))
  , playerBLoc = (300, (-330))
  , playerCLoc = (330, (-300))
  , playerDLoc = (330, (-330))
  , currentPlayer = PlayerA
  }
