module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Data.IORef

import TypesMonopoly
import qualified Graphics
import qualified Monopoly
import qualified Property
import qualified Railroad
import qualified Utility

brown = makeColor 0.39453125 0.26171875 0.12890625 1 
lightBlue = light (light (light blue))
colorOfBoard = dark (makeColor 0.804 0.902 0.816 1)

fps :: Int
fps = 60

main :: IO()
main = do
    hasRenderedSinceUpdate <- newIORef False
    playIO window background fps initialState (render hasRenderedSinceUpdate) input (update hasRenderedSinceUpdate)

{-
draw :: Status -> IO Picture
draw s = return . scale 300 300 ∘ pictures $
  [ text' $ s^.value.to show
  , translate 0 0.2 . text' $ "step #" ++ s^.stepNo.to show
  ]
  where
    text' = scale 0.0005 0.0005 . text
    -}


window :: Display
window = InWindow "Monopoly Polacksbacken" (720,720) (10, 10)

background :: Color
background = colorOfBoard



playersEmpty = [("",0,0,0),("",0,0,0),("",0,0,0),("",0,0,0)]

initialState :: Status
initialState =  (playersEmpty, Property.empty, Railroad.empty, Utility.empty)

render :: IORef Bool -> Status -> IO Picture
render hasRenderedSinceUpdate s = do
    writeIORef hasRenderedSinceUpdate True
    Graphics.render (statusToLocation s)


input :: Event -> Status -> IO Status
input _ status = return status

{-
update :: Float -> Float -> IO Float
update dt angle =  do
    getLine
    return (dt+angle)
-}

monopolyRound :: Status -> IO Status
monopolyRound (players, pro, rai, uti) = do
    -- If no players have been added
    if players == playersEmpty then do
        putStrLn "Welcome to Monopoly Polacksbacken."
        putStrLn "When all players are added please write \"Play\" or \"\" to start the game."
        addedPlayers <- Monopoly.addPlayers [] 0
        return (addedPlayers, Property.empty, Railroad.empty, Utility.empty)
    else do
        putStrLn ""
        putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
        Monopoly.printGameState players
        putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
        playersLeft <- Monopoly.removeBankrupt players []
        let statusUpdated = (playersLeft, Property.clear pro playersLeft, Railroad.clear rai playersLeft, Utility.clear uti playersLeft)
        case length playersLeft of
            0 -> do
                putStrLn "Game over. It's a tie."
                return initialState
            1 -> do
                putStrLn $ (winner playersLeft) ++ " won the game! Well played."
                return initialState
                where
                    winner [(n, b, p, j)] = n
            _ -> do
                newStatus <- Monopoly.playerRound playersLeft statusUpdated 0
                return newStatus

update :: IORef Bool -> Float -> Status -> IO Status
update hasRenderedSinceUpdate _ status = do
    hasRendered <- readIORef hasRenderedSinceUpdate
    if hasRendered then do
        writeIORef hasRenderedSinceUpdate False
        monopolyRound status
    else do
        return status

statusToLocation :: Status -> Graphics.MonopolyGame
statusToLocation ([(_, _, pos1, _), (_, _, pos2, _), (_, _, pos3, _), (_, _, pos4, _)], _, _, _) =
    Graphics.Game
    { Graphics.playerALoc = ((posToX pos1)   ,   (posToY pos1)   )
    , Graphics.playerBLoc = ((posToX pos2)   ,   (posToY pos2)-30)
    , Graphics.playerCLoc = ((posToX pos3)+30,   (posToY pos3)   )
    , Graphics.playerDLoc = ((posToX pos4)+30,   (posToY pos4)-30)
    , Graphics.currentPlayer = Graphics.PlayerA
    , Graphics.currentString = "Hej"
    }

posToX :: Int -> Float
posToX pos
    | pos == 0  = 300
    | pos < 10  = 285  - fromIntegral (60*pos)
    | pos <= 20 = -330
    | pos < 30  = -315 + fromIntegral (60*(pos-20))
    | pos < 40  = 300
    | otherwise = 0

posToY :: Int -> Float
posToY pos
    | pos <= 10 = -300
    | pos < 20  = -285 + fromIntegral (60*(pos-10))
    | pos <= 30 = 330
    | pos < 40  = 315  - fromIntegral (60*(pos-30))
    | otherwise = 0
