module Railroad(empty, clear, print, find, buy) where

import Prelude hiding(print)
import TypesMonopoly
import PlayerOperations

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

{- Represents a fresh list of railroads where "Nobody" owns all the railroads
 -}
empty :: [Railroad]

{- clear railroads players
   Clears all railroads that are not owned by a player in 'players'
   RETURNS: 'railroads' with cleared values for all railroads with an owner that is not in 'players'
   EXAMPLE: clear [("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Faizan",1),("Lundellska Skolan","Filip",1)] [("Kevin",1130,10,3),("Filip",900,35,0)] == [("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Filip",1)]
 -}
clear :: [Railroad] -> [Player] -> [Railroad]

{- print name
   Prints everything you need to know about the railroad called 'name'
   SIDE-EFFECTS: Displays information about a choosen railroad
 -}
print :: Name -> IO ()

{- find (players, properties, railroads, utilities) name
   Finds the railroad called 'name' in status
   PRE: 'name' is the name of a railroad in status
   RETURNS: the railroad called 'name' in status
   EXAMPLES: find ([],[],[("Polacksbacken","Nobody",0),("Grindstugan","Nobody",0),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Nobody",0)],[]) "Grindstugan" == ("Grindstugan","Nobody",0)
 -}
-- VARIANT: length railroads
find :: Status -> Name -> Railroad


{- buy (players, properties, railroads, utilities) player railroadName
   Updates the owner of the railroad called 'railroadName'
   RETURNS: status where the value for 'railroads' is updated
   EXAMPLES: buy ([("Kevin",1000,25,0),("Faizan",600,10,3)],[],[("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Faizan",1)],[]) ("Kevin",1000,25,0) "Uppsala Science Park" == ([("Kevin",800,25,0),("Faizan",600,10,3)],[],[("Polacksbacken","Kevin",3),("Grindstugan","Kevin",3),("Uppsala Science Park","Kevin",3),("Lundellska Skolan","Faizan",1)],[])
 -}
buy :: Status -> Player -> Name -> Status       

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

empty =
  [("Polacksbacken",       "Nobody",0),
   ("Grindstugan",         "Nobody",0),
   ("Uppsala Science Park","Nobody",0),
   ("Lundellska Skolan",   "Nobody",0)]

clear railroads players = clear' railroads players players
  where
    clear' [] _ _ = []
    clear' ((nameR, own, sta):xs) [] allPlayers = (nameR, "Nobody", 0) : clear' xs allPlayers allPlayers
    clear' ((nameR, own, sta):xs) ((nameP, _, _, _):ys) allPlayers
      | own == nameP = (nameR, own, sta) : clear' xs allPlayers allPlayers                            | otherwise    = clear' ((nameR, own, sta):xs) ys allPlayers

print name = do
  putStrLn "..................................................."
  putStrLn $ "Bus station " ++ name
  putStrLn "Price: $200"
  --All stations cost $200
  putStrLn "Rent:                             $25"
  putStrLn "Rent if the owner has 2 stations: $50"
  putStrLn "Rent if the owner has 3 stations: $100"
  putStrLn "Rent if the owner has 4 stations: $200"
  putStrLn "..................................................."

find (pla, pro, (nam, own, sta):xs, uti) name
                               | nam == name = (nam, own, sta)
                               | otherwise   = find (pla, pro, xs, uti) name

{- currentStage railroad name
   Returns the current stage of a railroad
   RETURNS: How many railroads the player called 'name' currently owns
   EXAMPLE: currentStage [("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Faizan",1)] "Kevin" == 2
 -}
    -- VARIANT: length railroads
currentStage :: [Railroad] -> Name -> Stage
                      
currentStage [] _ = 0
currentStage ((_, owner, stage):xs) nameP | owner == nameP = stage
                                          | otherwise      = currentStage xs nameP

{- updateRailroads railroads namePlayer nameRailroad newStage
   Updates the values of all railroads owned by 'playerName', inlcuding 'nameRailroad'
   RETURNS: railroads with updated values for all railroads owned by 'playerName', inlcuding 'nameRailroad'
   EXAMPLE: updateRailroads [("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Faizan",1)] "Kevin" "Uppsala Science Park" 3 == [("Polacksbacken","Kevin",3),("Grindstugan","Kevin",3),("Uppsala Science Park","Kevin",3),("Lundellska Skolan","Faizan",1)]
 -}
-- VARIANT: length railroads
updateRailroads :: [Railroad] -> Name -> Name -> Stage -> [Railroad]
updateRailroads [] _ _ _ = []
updateRailroads ((name, owner, stage):xs) nameP nameR newStage
                     | name == nameR || owner == nameP = (name, nameP, newStage) : updateRailroads xs nameP nameR newStage
                     | otherwise = (name, owner, stage) : updateRailroads xs nameP nameR newStage

buy (pla, pro, rai, uti) (nameP, bal, pos, jai) nameR =
  updatePlayer (pla, pro, (updateRailroads rai nameP nameR (1 + currentStage rai nameP)), uti) (nameP, bal-200, pos, jai)
