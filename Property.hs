module Property(empty, clear, print, find, colorMonopoly, update, buy, buildHouse) where

import Prelude hiding(print)
import TypesMonopoly
import PlayerOperations

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

{- Represents a fresh list of properties where "Nobody" owns all the properties
 -}
empty :: [Property]

{- clear properties players
   Clears all properties that are not owned by a player in 'players'
   RETURNS: 'properties' with cleared values for all properties with an owner that is not in 'players'
   EXAMPLE: clear [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] [("Filip",900,35,0)] == [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])]
 -}
clear :: [Property] -> [Player] -> [Property]

{- find (players, properties, railroads, utilities) nameProperty
   Finds the current status about the property called nameProperty
   PRE: 'nameProperty' is a property in 'status'
   RETURNS: property called 'name' in 'status'
   EXAMPLES: find ([],[("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Nobody",Green,False,0,0,320,200,[28,150,450,1000,1200,1400])],[],[]) "Haggsalen" == ("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275])
 -}
-- VARIANT: length properties
find :: Status -> Name -> Property

{- print property
   Prints everthing you need to know about a property
   SIDE-EFFECTS: Displays information about the chosen property
 -}
print :: Property -> IO ()

{- update (players, properties, railroads, utilities) property nameProperty
   Creates a list of properties where it updates the values for the chosen property
   PRE: 'nameProperty' exists as the name of a property in status
   RETURNS: all properties from status with updated values for the property called 'nameProperty' such that it has the value 'property'
   EXAMPLES: update ([],[("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Nobody",Green,False,0,0,320,200,[28,150,450,1000,1200,1400])],[],[]) ("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400]) "Siegbahnsalen" == [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])]
 -}
--VARIANT: length properties
update :: Status -> Property -> Name -> [Property]

{- colorMonpoly properties color playerName
   Checks if a player has monopoly over a color group
   RETURNS: True iff all 'properties' of the color group 'color' is owned by the player called 'playerName'
   EXAMPLES: colorMonopoly [("Polhemssalen","Faizan",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Faizan",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Faizan",Green,True,28,0,320,200,[28,150,450,1000,1200,1400]),("ITC 1549","Kevin",Orange,False,14,0,180,100,[14,70,200,550,750,950])] Green "Faizan" == True,
             colorMonopoly [("Polhemssalen","Faizan",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Kevin",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Faizan",Green,True,28,0,320,200,[28,150,450,1000,1200,1400]),("ITC 1549","Kevin",Orange,False,14,0,180,100,[14,70,200,550,750,950])] Green "Faizan" == False
 -}
--VARIANT: length properties
colorMonopoly :: [Property] -> ColorGroup -> Name -> ColorMonopoly
      
{- buy(players, properties, railroads, utilities) updatedProperties colorMonopoly colorGroup
   This is how the status of the properties change when a players buys a property
   RETURNS: 'status' with updated values for 'propeties' after a property in the color group 'colorGroup' is bought
   EXAMPLES: buy ([],[("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Nobody",Green,False,0,0,320,200,[28,150,450,1000,1200,1400])],[],[]) [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] False Green == ([],[("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])],[],[]),
             buy ([],[("Polhemssalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])],[],[]) [("Polhemssalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] True Green == ([],[("Polhemssalen","Aria",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,True,28,0,320,200,[28,150,450,1000,1200,1400])],[],[])
 -}
buy :: Status -> [Property] -> ColorMonopoly -> ColorGroup -> Status

{- buildHouse property (players, properties, railroads, utilities) player
   This is how status changes when a player lands on his/ her own property with color monopoly
   RETURNS: 'status' with updated values accordning to the input of the player
   SIDE-EFFECTS: Displays information on the screen. Takes input from the player
 -}
buildHouse :: Property -> Status -> Player -> IO Status

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

empty =
  [("ITC 1145",         "Nobody",Brown, False,0,0,60,50,[2,10,30,90,160,250]),
   ("ITC 1213",         "Nobody",Brown, False,0,0,60,50,[4,20,60,180,320,450]),
   ("Angstrom 6K1113",  "Nobody",Cyan,  False,0,0,100,50,[6,30,90,270,400,550]),
   ("Angstrom 2004",    "Nobody",Cyan,  False,0,0,100,50,[6,30,90,270,400,550]),
   ("Angstrom 4007",    "Nobody",Cyan,  False,0,0,120,50,[8,40,100,300,450,600]),
   ("ITC 1111",         "Nobody",Pink,  False,0,0,140,100,[10,50,150,450,625,750]),
   ("ITC 1211",         "Nobody",Pink,  False,0,0,140,100,[10,50,150,450,625,750]),
   ("ITC 2146",         "Nobody",Pink,  False,0,0,160,100,[12,60,180,500,700,900]),
   ("ITC 1549",         "Nobody",Orange,False,0,0,180,100,[14,70,200,550,750,950]),
   ("ITC 2510",         "Nobody",Orange,False,0,0,180,100,[14,70,200,550,750,950]),
   ("ITC 1515",         "Nobody",Orange,False,0,0,200,100,[16,80,220,600,800,1000]),
   ("Lilla Mikrorummet","Nobody",Red,   False,0,0,220,150,[18,90,250,700,875,1050]),
   ("Stora Mikrorummet","Nobody",Red,   False,0,0,220,150,[18,90,250,700,875,1050]),
   ("Foobar",           "Nobody",Red,   False,0,0,240,150,[20,100,300,750,925,1100]),
   ("Konferensrummet",  "Nobody",Yellow,False,0,0,260,150,[22,110,330,800,975,1150]),
   ("Massen",           "Nobody",Yellow,False,0,0,260,150,[22,110,330,800,975,1150]),
   ("Aulan",            "Nobody",Yellow,False,0,0,280,150,[24,120,360,850,1025,1200]),
   ("Polhemssalen",     "Nobody",Green, False,0,0,300,200,[26,130,390,900,1100,1275]),
   ("Haggsalen",        "Nobody",Green, False,0,0,300,200,[26,130,390,900,1100,1275]),
   ("Siegbahnsalen",    "Nobody",Green, False,0,0,320,200,[28,150,450,1000,1200,1400]),
   ("UTH-Gard",         "Nobody",Blue,  False,0,0,350,200,[35,175,500,1100,1300,1500]),
   ("Skrubben",         "Nobody",Blue,  False,0,0,400,200,[50,200,600,1400,1700,2000])]

clear properties players = clear' properties players players
  where
    clear' [] _ _ = []
    clear' ((name, owner, cG, cM, rent, stage, price, priceH, rents):xs) [] allPlayers = (name, "Nobody", cG, False, 0, 0, price, priceH, rents) : clear' xs allPlayers allPlayers
    clear' ((name, owner, cG, cM, rent, stage, price, priceH, rents):xs) ((nameP, _, _, _):ys) allPlayers
      | owner == nameP = (name, owner, cG, cM, rent, stage, price, priceH, rents) : clear' xs allPlayers allPlayers
      | otherwise    = clear' ((name, owner, cG, cM, rent, stage, price, priceH, rents):xs) ys allPlayers

find (pla, (name, owner, cG, cM, rent, stage, price, priceU, rents):xs, rai, uti) nameProperty
             | name == nameProperty = (name, owner, cG, cM, rent, stage, price, priceU, rents)
             | otherwise            = find (pla, xs, rai, uti) nameProperty


print (name, owner, cG, cM, _, _, price, priceU, [r1,r2,r3,r4,r5,r6]) = do
  putStrLn "..................................................."
  putStrLn $ "Property " ++ name
  putStrLn $ "Color group: " ++ show cG
  putStrLn $ "Price: $" ++ show price
  putStrLn $ "Price per house/ hotel: " ++ show priceU
  putStrLn $ "Rent:               $" ++ show r1
  putStrLn $ "Rent with 1 house:  $" ++ show r2
  putStrLn $ "Rent with 2 houses: $" ++ show r3
  putStrLn $ "Rent with 3 houses: $" ++ show r4
  putStrLn $ "Rent with 4 houses: $" ++ show r5
  putStrLn $ "Rent with hotel:    $" ++ show r6
  putStrLn "..................................................."
  putStrLn $ "Current owner: " ++ owner
  if cM then do putStrLn $ owner ++ " owns every property in the color group " ++ show cG ++ "."
        else do putStr ""

update (pla, ((name, owner, cG, cM, rent, stage, price, priceH, rents):xs), rai, uti) property nameProperty
  | name == nameProperty = property:xs
  | otherwise            = (name, owner, cG, cM, rent, stage, price, priceH, rents) : update (pla, xs, rai, uti) property nameProperty

colorMonopoly [] _ _ = True
colorMonopoly ((_, owner, cG, _, _, _, _, _, _):xs) color playerName
  --Return false iff there is a property in the same color group with a different owner
  | cG == color && owner /= playerName = False
  | otherwise                          = colorMonopoly xs color playerName

buy (pla, _, rai, uti)   properties False _ = (pla, properties, rai, uti)
buy (pla, pro, rai, uti) properties True cG = (pla, buyProperty' properties cG, rai, uti)
    where
      {- buyProperty' propeties colorGroup
         Changes the colorMonopoly to True in all properties in 'colorGroup'
         RETURNS: 'porperties' where the colorMonopoly value for all properties in 'colorGroup' is True
         EXAMPLE: buyProperty' [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] Green == [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])],
             buyProperty' [("Polhemssalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] Green == [("Polhemssalen","Aria",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,True,28,0,320,200,[28,150,450,1000,1200,1400])]
       -}
      -- VARIANT: length properties
      buyProperty' :: [Property] -> ColorGroup -> [Property]
      buyProperty' [] _ = []
      buyProperty' ((name, owner, color, cM, rent, stage, price, priceU, rents):xs) cG
        | color == cG = (name, owner, color, True, rent, stage, price, priceU, rents) : buyProperty' xs cG
        | otherwise   = (name, owner, color, cM, rent, stage, price, priceU, rents) : buyProperty' xs cG
     
{- housesAmount property
   Asks the player how many houses he/ she wants to build
   RETURNS: current stage + amount
   SIDE-EFFECTS: Displays information on the screen. Takes input from the player
 -}
housesAmount :: Property -> IO Int
housesAmount (name, owner, cG, cM, rent, stage, price, priceU, rents) = do
  putStrLn "How many houses do you want to build? (5 houses = 1 hotel)"
  maybeAmount <- getLine
  if validAmount maybeAmount then do let amount = (read maybeAmount :: Int)
                                     if (amount + stage) > 5
                                       then do putStrLn $ "Invalid amount. Please enter an integer between 1 and " ++ show (5-stage) ++ "."
                                               housesAmount (name, owner, cG, cM, rent, stage, price, priceU, rents)
                                       else return $ stage + amount
                             else do putStrLn $ "Invalid amount. Please enter an integer between 1 and " ++ show (5-stage) ++ "."
                                     housesAmount (name, owner, cG, cM, rent, stage, price, priceU, rents)


{- validAmount amount
   Checks wether amount is a valid number of houses
   RETURNS: True iff amount is an integer from 1 to 5
   EXAMPLES: validAmount "1" == True, validAmount "-1" == False, validAmount "1337" == False, validAmount "Hello" == False
 -}
validAmount :: String -> Bool
validAmount input = case input of
                      "1" -> True
                      "2" -> True
                      "3" -> True
                      "4" -> True
                      "5" -> True
                      _   -> False

buildHouse house s p = do
  putStrLn "Do you want to build a house? \"Yes\" or \"No\"."
  answer <- getLine
  case answer of
    "Yes" -> do newStage <- housesAmount house
                let newStatus = buildHouse' house s p newStage
                putStrLn "Purchase successful."
                return $ newStatus
                  where
                    {- buildHouse' property (players, properties, railroads, utilities) player newStage
                       This is how status changes when a player buys a house(s)/ a hotel
                       RETURNS: Status with updated values
                       EXAMPLE: buildHouse' ("Haggsalen","Filip",Green,True,26,0,300,200,[26,130,390,900,1100,1275]) ([("Filip",1400,28,0)],[("Haggsalen","Filip",Green,False,26,0,300,200,[26,130,390,900,1100,1275])],[],[]) ("Filip",1400,32,0) 3 == ([("Filip",800,32,0)],[("Haggsalen","Filip",Green,True,900,3,300,200,[26,130,390,900,1100,1275])],[],[])
                     -}
                    buildHouse' :: Property -> Status -> Player -> Int -> Status
                    buildHouse' (namePro, owner, cG, cM, rent, stage, price, priceU, rents) s (namePla, bal, pos, jai) newStage =
                      let propertiesUpdated = update s (namePro, owner, cG, True, rents !! newStage, newStage, price, priceU, rents) namePro
                      in buy (updatePlayer s (namePla, bal-(priceU*(newStage-stage)), pos, jai)) propertiesUpdated True cG
    "No"  -> return s
    _     -> buildHouse house s p

buildHouse' :: Property -> Status -> Player -> Int -> Status
buildHouse' (namePro, owner, cG, cM, rent, stage, price, priceU, rents) s (namePla, bal, pos, jai) newStage =
                      let propertiesUpdated = update s (namePro, owner, cG, True, rents !! newStage, newStage, price, priceU, rents) namePro
                      in buy (updatePlayer s (namePla, bal-(priceU*(newStage-stage)), pos, jai)) propertiesUpdated True cG
