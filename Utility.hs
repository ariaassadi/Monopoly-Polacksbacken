module Utility(empty, clear, print, find, buy) where

import Prelude hiding(print)
import TypesMonopoly
import PlayerOperations

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

{- Represents a fresh list of utilities where "Nobody" owns all the utilities
 -}
empty :: [Utility]

{- clear utilities players
   Clears all utilities that are not owned by a player in 'players'
   RETURNS: 'utilities' with cleared values for all utilities with an owner that is not in 'players'
   EXAMPLE: clear [("Electric Company","Faizan",2),("Water Works","Faizan",2)] [("Aria",1350,17,0),("Kevin",1130,10,3),("Filip",900,35,0)] == [("Electric Company","Nobody",0),("Water Works","Nobody",0)]
 -}
clear :: [Utility] -> [Player] -> [Utility]

{- print name
   Prints everthing you need to know about the utility called 'name'
   SIDE-EFFECTS: Displays information about a choosen railroad
 -}
print :: Name -> IO ()

{- find (players, properties, railroads, utilities) name
   Finds the utility called 'name' in status
   PRE: 'name' is the name of a utility in status
   RETURNS: the utility called 'name' in status
   EXAMPLES: find ([],[],[("Polacksbacken","Nobody",0),("Grindstugan","Nobody",0),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Nobody",0)],[]) "Grindstugan" == ("Grindstugan","Nobody",0)
 -}
-- VARIANT: length utilities
find :: Status -> Name -> Utility


{- buy status player utilityName
   Updates the owner of utlityName
   RETURNS: 'status' where the owner of 'utiltyName' is the name of 'player'
   EXAMPLES: buy ([("Aria",1350,28,0),("Kevin",1500,15,0)],[],[],[("Electric Company","Nobody",0),("Water Works","Aria",1)]) ("Aria",1350,28,0) "Electric Company" == ([("Aria",1200,28,0),("Kevin",1500,15,0)],[],[],[("Electric Company","Aria",2),("Water Works","Aria",2)])
 -}
buy :: Status -> Player -> Name -> Status
                           
-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

empty =
  [("Electric Company","Nobody",0),
   ("Water Works",     "Nobody",0)]

clear utilities players = clear' utilities players players
  where
    clear' [] _ _ = []
    clear' ((nameU, own, sta):xs) [] allPlayers = (nameU, "Nobody", 0) : clear' xs allPlayers allPlayers
    clear' ((nameU, own, sta):xs) ((nameP, _, _, _):ys) allPlayers
      | own == nameP = (nameU, own, sta) : clear' xs allPlayers allPlayers                           | otherwise    = clear' ((nameU, own, sta):xs) ys allPlayers

print name = do
  putStrLn "..................................................."
  putStrLn $ "Utility " ++ name
  putStrLn "Price: $150"
  --All utilities cost $150
  putStrLn "Rent if the owner has 1 utility:    4*(sum of dice)"
  putStrLn "Rent if the owner has 2 utilities: 10*(sum of dice)"
  putStrLn "..................................................."

find (pla, pro, rai, (nam, own, sta):xs) name
                               | nam == name = (nam, own, sta)
                               | otherwise   = find (pla, pro, rai, xs) name

{- currentStage utilities name
   Returns the current stage of a players utility
   RETURNS: How many utilities the player called 'name' currently owns
   EXAMPLE: currentStage [("Electric Company","Nobody",0),("Water Works","Aria",1)] "Aria" == 1
 -}
    -- VARIANT: length utilitiea
currentStage :: [Utility] -> Name -> Stage
currentStage [] _ = 0
currentStage ((_, owner, stage):xs) nameP | owner == nameP = stage
                                          | otherwise      = currentStage xs nameP

{- updateUtilities utilities namePlayer nameUtility newStage
   Updates the values of all utilities owned by 'playerName', inlcuding 'nameUtility'
   RETURNS: utilities with updated values for all utilities owned by 'playerName', inlcuding 'nameUtility'
   EXAMPLE: updateUtilities [("Electric Company","Nobody",0),("Water Works","Aria",1)] "Aria" "Electric Company" 2 == [("Electric Company","Aria",2),("Water Works","Aria",2)]
 -}
-- VARIANT: length utilities
updateUtilities :: [Utility] -> Name -> Name -> Stage -> [Utility]
updateUtilities [] _ _ _ = []
updateUtilities ((name, owner, stage):xs) nameP nameU newStage
                     | name == nameU || owner == nameP = (name, nameP, newStage) : updateUtilities xs nameP nameU newStage
                     | otherwise = (name, owner, stage) : updateUtilities xs nameP nameU newStage

buy (pla, pro, rai, uti) (nameP, bal, pos, jai) nameU =
  updatePlayer (pla, pro, rai, (updateUtilities uti nameP nameU (1 + currentStage uti nameP))) (nameP, bal-150, pos, jai)
