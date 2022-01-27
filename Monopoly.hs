module Monopoly(addPlayers, printGameState, removeBankrupt, playerRound) where

import System.Random
import Test.HUnit

import TypesMonopoly
import PlayerOperations
import qualified Property
import qualified Railroad
import qualified Utility


-------------------------------------------------------------------------------
-- interface
-------------------------------------------------------------------------------

{- addPlayers players counts
   Adds 2-4 players to the game
   RETURNS: A list of players with $1500 standing on poistion 0
   SIDE-EFFECTS: Displays "Add player: " and takes input from the players
 -}
addPlayers :: [Player] -> Int -> IO [Player]

{- printGameState players
   Prints the current status of each player
   SIDE-EFFECTS: Displays each players state on the screen
 -}
-- VARIANT: length players
printGameState :: [Player] -> IO ()

{- removeBankrupt players nonBankruptPlayers
   Removes all players with an incomce lower than $0
   RETURNS: players without the bankrupt ones
   SIDE-EFFECTS: Displays information on the screen
 -}
-- VARIANT: length players
removeBankrupt :: [Player] -> [Player] -> IO [Player]

{- playerRound playersTurn (players, properties, railroads, utilities) doubleCounts
   This is what happens every single round (around the table).
   RETURNS: status with updated values
   SIDE-EFFECTS: Displays information on the screen. Takes input from the players
 -}
-- VARIANT: length playersTurn
playerRound :: [Player] -> Status -> Int -> IO Status 

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

addPlayers players 4 = do
  putStrLn "You have reached the maximal amount of players."
  return players
addPlayers players n = do
  putStr "Add player: "
  name <- getLine
  if elem (name, 1500, 0, 0) players
    then do putStrLn $ "Player " ++ name ++ " is already added."
            addPlayers players n            
    else do case name of
              "Play" -> if n < 2
                          then do
                            putStrLn "You need at least 2 players to play the game."
                            addPlayers players n
                          else return players
              "" -> if n < 2
                      then do
                        putStrLn "You need at least 2 players to play the game."
                        addPlayers players n
                      else return players
              "Nobody" -> do putStrLn "Invalid name."
                             addPlayers players n
              x -> addPlayers (players ++ [(x, 1500, 0, 0)]) (n+1)

{- run
   Runs the game and adds all the players
   SIDE-EFFECTS: Displays information on the screen. Takes inputs from the players
 -}
run :: IO ()
run = do
  putStrLn "Welcome to Monopoly Polacksbacken."
  putStrLn "When all players are added please write \"Play\" or \"\" to start the game."
  players <- addPlayers [] 0
  play (players, Property.empty, Railroad.empty, Utility.empty)


printGameState [] = do
  return ()
printGameState ((n, b, p, _):xs) = do
  putStrLn $ n ++ " has $" ++ show b ++ " and is standing on " ++ show p
  printGameState xs

-- G�r om funktionen removeBankrupt s� den tar in status och clearar alla properties, railroads och utilities som den spelaren �ger.


removeBankrupt [] notBankrupt = return notBankrupt
removeBankrupt ((nam, bal, pos, jai):xs) notBankrupt
  | bal > 0 = removeBankrupt xs (notBankrupt ++ [(nam, bal, pos, jai)])
  | otherwise = do
      putStrLn $ nam ++ " just got bankrupt and has to leave the game immediately."
      removeBankrupt xs notBankrupt
     
{- play (players, properties, railroads, utilities)
   This is what happens every new round
   SIDE-EFFECTS: Displays information on the screen. Takes inputs from the players
 -}
play :: Status -> IO ()
play (players, pro, rai, uti) = do
  putStrLn ""
  putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
  printGameState players
  putStrLn "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
  playersLeft <- removeBankrupt players []
  let statusUpdated = (playersLeft, Property.clear pro playersLeft, Railroad.clear rai playersLeft, Utility.clear uti playersLeft)
  case length playersLeft of
    0 -> do putStrLn "Game over. It's a tie."
            return ()
    1 -> do putStrLn $ (winner playersLeft) ++ " won the game! Well played."
            return ()
              where
                winner [(n, b, p, j)] = n
    _ -> do newStatus <- playerRound playersLeft statusUpdated 0
            play newStatus

{- findPlayers playerName status
   Creates a list of players starting with the one called 'playerName'
   RETURNS: The player called 'playerName' and all the players after him/ her in status
   EXAMPLES: findPlayers "Aria" ([("Aria",1350,17,0),("Kevin",1500,0,0)],[],[],[]) == [("Aria",1350,17,0),("Kevin",1500,0,0)]
             findPlayers "Kevin" ([("Aria",1350,17,0),("Kevin",1500,0,0)],[],[],[]) == [("Kevin",1500,0,0)]
 -}
findPlayers :: Name -> Status -> [Player]
findPlayers _ ([], _, _, _) = []
findPlayers name (((nam, bal, pos, jai):xs), pro, rai, uti)
                      | name == nam = (nam, bal, pos, jai):xs
                      | otherwise = findPlayers name (xs, pro, rai, uti)
                                                     
{- rollDice
   Generates a random number from 1 to 6
   RETURNS: A random number from 1 to 6
   EXAMPLES: rollDice == 5
             rollDice == 1
 -}
rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))
            

playerRound []                      s _ = return (s)
--Throwing a double 3 times leads to Jail
playerRound (cPla:xs)               s 3 = do
  goToJail <- jail s cPla
  playerRound xs goToJail 0 
playerRound ((nam, bal, pos, 0):xs) s c = do
  putStrLn ""
  putStr $ nam ++ "'s turn. Press Enter to throw."
  pressEnter <- getLine
  d1 <- rollDice
  d2 <- rollDice
  putStrLn $ "You threw " ++ show d1 ++ "+" ++ show d2 ++ " and landed on " ++ show (if (pos+d1+d2) < 40 then (pos+d1+d2) else (pos+d1+d2-40))
  if d1 == d2 && c == 2
    then playerRound ((nam, bal, pos, 0):xs) s 3
    else if d1 == d2
           then do newStatus <- gameRound (nam, bal, pos+d1+d2, 0) s (d1+d2)
                   playerRound (findPlayers nam newStatus) newStatus (c+1) --The same player gets to play again
           else do newStatus <- gameRound (nam, bal, pos+d1+d2, 0) s (d1+d2)
                   playerRound (tail (findPlayers nam newStatus)) newStatus 0
--If the player is in Jail
playerRound ((nam, bal, pos, jai):xs) s _ = do
  putStrLn ""
  putStr $ nam ++ "'s turn. You're currently in Jail. Press Enter to throw."
  pressEnter <- getLine
  d1 <- rollDice
  d2 <- rollDice
  putStr $ "You threw " ++ show d1 ++ "+" ++ show d2
  if d1 == d2
    then do putStrLn $ ". You escaped and landed on " ++ show (pos+d1+d2) ++ "."
            newStatus <- gameRound (nam, bal, pos+d1+d2, 0) s (d1+d2)
            playerRound xs newStatus 0
    else if jai == 1
           then do putStrLn $ " and landed on " ++ show (pos+d1+d2) ++ ". You are free but you have to pay $50 since you didn't throw a double."
                   newStatus <- gameRound (nam, bal-50, pos+d1+d2, 0) s (d1+d2)
                   playerRound (tail (findPlayers nam newStatus)) newStatus 0
           else do putStrLn ". Better luck next time."
                   playerRound xs (updatePlayer s (nam, bal, pos, jai-1)) 0

{- randomInt
   Generates a random number from 1 to 10
   RETURNS: A random number from 1 to 10
   EXAMPLES: rollDice == 2
             rollDice == 7
 -}
randomInt :: IO Int
randomInt = getStdRandom (randomR (1,10))
                 
{- communityChest status player
   Randomly picks one of ten cards and changes status according to the instruction on the card
   RETURNS: status were player has new values
   SIDE-EFFECTS: Prints the information on the card to the screen
 -}
communityChest :: Status -> Player -> IO Status
communityChest s p = do
    putStr "Community chest. "
    random <- randomInt
    chest' s p random
      where
        {- communityChest status player card
           Randomly picks one of ten cards and changes status according to the instruction on the card
           RETURNS: status were player has new values
         -}
        chest' :: Status -> Player -> Int -> IO Status
        chest' s (n, b, p, 0) 1 = do   
            putStrLn "You parked illegaly while being in a hurry to an exam, pay a fine of $40." 
            return $ updatePlayer s (n, b-40, p, 0)
        chest' s (n, b, p, 0) 2 = do   
            putStrLn "Someone forgot their lunch in the microwave, you recieve $60."
            return $ updatePlayer s (n, b+60, p, 0)
        chest' s (n, b, p, 0) 3 = do
            putStrLn "You have to renew your nation card, pay $90."
            return $ updatePlayer s (n, b-90, p, 0)
        chest' s (n, b, p, 0) 4 = do    
            putStrLn "You recieved a scholarship from UU, you recieve $80." 
            return $ updatePlayer s (n, b+80, p, 0)
        chest' s (n, b, p, 0) 5 = do
            putStrLn "You helped your roommate with an assignment, you recieve $50."  
            return $ updatePlayer s (n, b+50, p, 0)
        chest' s (n, b, p, 0) 6 = do
            putStrLn "You broke your table in rage while studying to your final exam, pay $60 for a new one."
            return $ updatePlayer s (n, b-60, p, 0)
        chest' s (n, b, p, 0) 7 = do
            putStrLn "You got a speeding ticket on your way to UU, pay a fine of $70."
            return $ updatePlayer s (n, b-70, p, 0)
        chest' s (n, b, p, 0) 8 = do
            putStrLn "You won the UU prize for best SPEX, you recieve $100."
            return $ updatePlayer s (n, b+100, p, 0)
        chest' s (n, b, p, 0) 9 = do
            putStrLn "You need to help renovate Skrubben, pay $60."
            return $ updatePlayer s (n, b-60, p, 0)
        chest' s (n, b, p, 0) 10 = do
            putStrLn "You attend to a lunch lecture, you recieve $50."
            return $ updatePlayer s (n, b+50, p, 0)

{- chance status player
   Randomly picks one of ten cards and changes status according to the instruction on the card
   RETURNS: status were player, and possibly other players as well, has new values
   SIDE-EFFECTS: Prints the information on the card to the screen
 -}
chance :: Status -> Player -> IO Status
chance s p = do
    putStr "Chance. "
    random <- randomInt
    chance' s p random
      where
        {- chance' status player card
           Randomly picks one of ten cards and changes status according to the instruction on the card
           RETURNS: status were player, and possibly other players as well, has new values
           SIDE-EFFECTS: Prints the information on the card to the screen
         -}
        chance' :: Status -> Player -> Int -> IO Status
        chance' s (n, b, p, 0) 1 = do
            putStrLn "You forgot your computer in Siegbahnssalen, go there immediately. If you pass GO, recieve $200."
            if p > 34 
                then do newStatus <- gameRound (n, b, 74, 0) s 0
                        --This makes you pass go, and recieve $200.
                        return $ newStatus
                else do newStatus <- gameRound (n, b, 34, 0) s 0
                        return $ newStatus
        chance' s (n, b, p, 0) 2 = do
            putStrLn "You got caught cheating on your final exam, go to Jail. If you pass GO, you don't recieve $200."
            newStatus <- jail s (n, b, p, 0)
            return newStatus
        chance' s player 3 = do
            putStrLn "It is your birthday, recieve $20 from all other players."
            return $ payEveryone s player (-20) 
        chance' s (n, b, p, 0) 4 = do
            putStrLn "You accidentaly dropped your pen, go back 4 steps."
            newStatus <- gameRound (n, b, p-4, 0) s 0
            return newStatus
        chance' s (n, b, p, 0) 5 = do
            putStrLn "You are late to a lecture in Aulan, go there immediately. If you pass GO, recieve $200."
            if p > 29
                then do newStatus <- gameRound (n, b, 69, 0) s 0
                        return newStatus
                else do newStatus <- gameRound (n, b, 29, 0) s 0
                        return newStatus
        chance' s (n, b, p, 0) 6 = do
            putStrLn "You got too drunk last night and spent a bit more than you anticipated, pay $50."
            return $ updatePlayer s (n, b-50, p, 0)
        chance' s player 7 = do
            putStrLn "You have debts to pay after yesterdays pub crawl. Pay everyone $15."
            return $ payEveryone s player 15 
        chance' s (n, b, p, 0) 8 = do
            putStrLn "You recieved your tax refund, you recieve $100."
            return $ updatePlayer s (n, b+100, p, 0)
        chance' s (n, b, p, 0) 9 = do
            putStrLn "It's friday and you are heading home to see your family, go to the next station based on your current location immediately. If you pass GO, recieve $200."
            case p of
              36 -> do newStatus <- gameRound (n, b, 45, 0) s 0
                       return newStatus
              22 -> do newStatus <- gameRound (n, b, 25, 0) s 0
                       return newStatus
              _  -> do newStatus <- gameRound (n, b, 15, 0) s 0
                       return newStatus
        chance' s (n, b, p, 0) 10 = do
            putStrLn "Your computer has stopped working but you have to send in your assignment, go to room 1515 immediately. If you pass GO, recieve $200."
            if p > 19
                then do newStatus <- gameRound (n, b, 19, 0) s 0
                        return newStatus
                else do newStatus <- gameRound (n, b, 59, 0) s 0
                        return newStatus

{- property (players, properties, railroad, utilities) player propertyName player
   This happens when you land on a property
   PRE: None of the the players has the name "Nobody"
   RETURNS: 'status' with updated values accordning to the input from the player
   SIDE-EFFECTS: Displays information on the screen. Takes inputs from the player
 -}
property :: Status -> Player -> Name -> IO Status
property s p n = do
  currentProperty <- return $ Property.find s n
  Property.print currentProperty
  property' currentProperty s p
    where
      {- property' property (players, properties, railroads, utilities) player
         This happens when you land on a property
         SIDE-EFFECTS: Displays information on the screen. Takes inputs from the player
       -}
      property' :: Property -> Status -> Player -> IO Status
      property' (name, "Nobody", cG, cM, 0, 0, price, priceU, rents) s (nam, bal, pos, jai) = do
        putStrLn $ "Do you want to buy " ++ name ++ "? \"Yes\" or \"No\"."
        answer <- getLine
        case answer of
          "Yes" -> do propertiesUpdated <- return $ Property.update s (name, nam, cG, cM, head rents, 0, price, priceU, rents) name
                      putStrLn "Purchase successful."
                      return $ Property.buy (updatePlayer s (nam, bal-price, pos, jai)) propertiesUpdated (Property.colorMonopoly propertiesUpdated cG nam) cG
          "No"  -> return s
             --Ask again
          _     -> property' (name, "Nobody", cG, cM, 0, 0, price, priceU, rents) s (nam, bal, pos, jai)
      property' (name, owner, cG, cM, rent, stage, price, priceU, rents) s (nam, bal, pos, jai)
        | owner == nam = do
            putStrLn "This is your property."
            --If you own all houses in the current color group
            if cM then do Property.buildHouse (name, owner, cG, cM, rent, stage, price, priceU, rents) s (nam, bal, pos, jai)
                  else return s
        | otherwise = do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show rent ++ "."
                         return $ transaction s owner nam rent

{- railroad status player name
   This happens when you land on a railroad
   PRE: None of the the players has the name "Nobody"
   RETURNS: 'status' with updated values accordning to the input from the player
   SIDE-EFFECTS: Displays information on the screen. Takes input from the player
 -}
railroad :: Status -> Player -> Name -> IO Status
railroad s p n = do
  Railroad.print n
  (name, owner, stage) <- return $ Railroad.find s n
  putStrLn $ "Current owner: " ++ show owner
  railroad' (name, owner, stage) s p
    where
      {- railroad' railroad status player
         This happens when you land on a railroad
         PRE: None of the the players has the name "Nobody"
         RETURNS: 'status' with updated values accordning to the input from the player
         SIDE-EFFECTS: Displays information on the screen. Takes input from the player
       -}   
      railroad' :: Railroad -> Status -> Player -> IO Status
      railroad' (name, "Nobody", 0) s p = do
        putStrLn $ "Do you want to buy " ++ name ++ "? \"Yes\" or \"No\"."
        answer <- getLine
        case answer of
          "Yes" -> do putStrLn "Purchase successful."
                      return (Railroad.buy s p name)
          "No"  -> return s
          _     -> railroad' (name, "Nobody", 0) s p
                   --Stage isn�t relevant when owner is Nobody
      railroad' (_, owner, stage) s (pN, _, _, _)
                         | owner == pN = do
                             putStrLn "This is your property."
                             return (s)
                         | otherwise   = case stage of
                             1 -> do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show (25) ++ "."
                                     return $ transaction s owner pN (25)
                             2 -> do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show (50) ++ "."
                                     return $ transaction s owner pN (50)
                             3 -> do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show (100) ++ "."
                                     return $ transaction s owner pN (100)
                             4 -> do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show (200) ++ "."
                                     return $ transaction s owner pN (200)


{- utility status player name dieRoll
   This happens when you land a utility
   RETURNS: 'status' with updates values
   SIDE-EFFECTS: Displays information on screen. Takes inputs from the player
 -}
utility :: Status -> Player -> Name -> DieRoll -> IO Status
utility s p n d = do
  Utility.print n
  (name, owner, stage) <- return $ Utility.find s n
  putStrLn $ "Current owner: " ++ show owner
  utility' (name, owner, stage) s p d
    where
      {- utility' utility status player dieRoll
         This happens when you land a utility
         RETURNS: 'status' with updates values accordning to the input from the player
         SIDE-EFFECTS: Displays information on screen. Takes inputs from the player
       -}
      utility' :: Utility -> Status -> Player -> DieRoll -> IO Status
      utility' (name, "Nobody", 0) s p _ = do
        putStrLn $ "Do you want to buy " ++ name ++ "? \"Yes\" or \"No\"."
        answer <- getLine
        case answer of
          "Yes" -> do putStrLn "Purchase successful."
                      return (Utility.buy s p name)
          "No"  -> return s
          _     -> utility' (name, "Nobody", 0) s p 0
                   --Stage and dieroll aren�t relevant when owner is Nobody
      utility' (_, owner, stage) s (pN, _, _, _) d
                         | owner == pN = do
                             putStrLn "This is your property."
                             return (s)
                         | otherwise   = case stage of
                                           1 -> do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show (4*d) ++ "."
                                                   return $ transaction s owner pN (4*d)
                                           2 -> do putStrLn $ "You have to pay " ++ owner ++ " $" ++ show (10*d) ++ "."
                                                   return $ transaction s owner pN (10*d)

{- gameRound player (players, properties, railroads, utilities) dieRoll
   How the status of the game changes upon landing on a square
   RETURNS: status with updated values according to wich square player landed on
   SIDE-EFFECTS: Displays information on the screen. Takes input from the players
 -}
gameRound :: Player -> Status -> DieRoll -> IO Status
gameRound (n, b, position, 0)  s d = case position of
  0  -> return (updatePlayer s (n, b, 0, 0))
  1  -> do property (updatePlayer s (n, b, 1, 0)) (n, b, 1, 0) "ITC 1145"
  2  -> do communityChest (updatePlayer s (n, b, 2, 0)) (n, b, 2, 0)
  3  -> do property (updatePlayer s (n, b, 3, 0)) (n, b, 3, 0) "ITC 1213"
  4  -> do putStrLn "Income tax. Pay $200."
           return $ updatePlayer s (n, b-200, 4, 0)
  5  -> do railroad (updatePlayer s (n, b, 5, 0)) (n, b, 5, 0) "Polacksbacken"
  6  -> do property (updatePlayer s (n, b, 6, 0)) (n, b, 6, 0) "Angstrom 6K1113"
  7  -> do chance s (n, b, 7, 0)
  8  -> do property (updatePlayer s (n, b, 8, 0)) (n, b, 8, 0) "Angstrom 2004"
  9  -> do property (updatePlayer s (n, b, 9, 0)) (n, b, 9, 0) "Angstrom 4007"
  10 -> do putStrLn "Just visiting Jail"
           return $ updatePlayer s (n, b, 10, 0)
  11 -> do property (updatePlayer s (n, b, 11, 0)) (n, b, 11, 0) "ITC 1111"
  12 -> do utility (updatePlayer s (n, b, 12, 0)) (n, b, 12, 0) "Electric Company" d
  13 -> do property (updatePlayer s (n, b, 13, 0)) (n, b, 13, 0) "ITC 1211"
  14 -> do property (updatePlayer s (n, b, 14, 0)) (n, b, 14, 0) "ITC 2146"
  15 -> do railroad (updatePlayer s (n, b, 15, 0)) (n, b, 15, 0) "Grindstugan"
  16 -> do property (updatePlayer s (n, b, 16, 0)) (n, b, 16, 0) "ITC 1549"
  17 -> do communityChest s (n, b, 17, 0)
  18 -> do property (updatePlayer s (n, b, 18, 0)) (n, b, 18, 0) "ITC 2510"
  19 -> do property (updatePlayer s (n, b, 19, 0)) (n, b, 19, 0) "ITC 1515"
  20 -> do putStrLn "Free parking."
           return $ updatePlayer s (n, b, 20, 0)
  21 -> do property (updatePlayer s (n, b, 21, 0)) (n, b, 21, 0) "Lilla Mikrorummet"
  22 -> do chance s (n, b, 22, 0)
  23 -> do property (updatePlayer s (n, b, 23, 0)) (n, b, 23, 0) "Stora Mikrorummet"
  24 -> do property (updatePlayer s (n, b, 24, 0)) (n, b, 24, 0) "Foobar"
  25 -> do railroad (updatePlayer s (n, b, 25, 0)) (n, b, 25, 0) "Uppsala Science Park"
  26 -> do property (updatePlayer s (n, b, 26, 0)) (n, b, 26, 0) "Konferensrummet"
  27 -> do property (updatePlayer s (n, b, 27, 0)) (n, b, 27, 0) "Massen"
  28 -> do utility (updatePlayer s (n, b, 28, 0)) (n, b, 28, 0) "Water Works" d
  29 -> do property (updatePlayer s (n, b, 29, 0)) (n, b, 29, 0) "Aulan"
  30 -> do (jail s (n, b, 30, 0))
  31 -> do property (updatePlayer s (n, b, 31, 0)) (n, b, 31, 0) "Polhemssalen"
  32 -> do property (updatePlayer s (n, b, 32, 0)) (n, b, 32, 0) "Haggsalen"
  33 -> do communityChest s (n, b, 33, 0)
  34 -> do property (updatePlayer s (n, b, 34, 0)) (n, b, 34, 0) "Siegbahnsalen"
  35 -> do railroad (updatePlayer s (n, b, 35, 0)) (n, b, 35, 0) "Lundellska Skolan"
  36 -> do chance s (n, b, 36, 0)
  37 -> do property (updatePlayer s (n, b, 37, 0)) (n, b, 37, 0) "UTH-Gard"
  38 -> do putStrLn "Luxury tax. Pay $100."
           return $ updatePlayer s (n, b-100, 38, 0)
  39 -> do property (updatePlayer s (n, b, 39, 0)) (n, b, 39, 0) "Skrubben"
  pos -> do putStrLn "You passed go. Recieve $200."
            gameRound (n, b+200, pos-40, 0) s d 
                                                   
------------------------------------------------------------------
--Test Cases
------------------------------------------------------------------
playersExample = [("Aria",1350,17,0),("Kevin",1130,10,3),("Faizan",1780,2,0),("Filip",900,35,0)]

statusExample = (playersExample, Property.empty, Railroad.empty, Utility.empty)

test0 = TestCase $ assertEqual "findPlayers 1"
  playersExample $ findPlayers "Aria" statusExample

test1 = TestCase $ assertEqual "findPlayers 2"
  [("Faizan",1780,2,0),("Filip",900,35,0)] $ findPlayers "Faizan" statusExample

test2 = TestCase $ assertEqual "updatePlayer 1"
  ([("Aria",1350,17,0),("Kevin",1130,10,3),("Faizan",1660,9,0),("Filip",900,35,0)], Property.empty, Railroad.empty, Utility.empty) $ updatePlayer statusExample ("Faizan",1660,9,0)

test3 = TestCase $ assertEqual "updatePlayer 2"
  ([("Aria",1350,10,3),("Kevin",1130,10,3),("Faizan",1780,2,0),("Filip",900,35,0)], Property.empty, Railroad.empty, Utility.empty) $ updatePlayer statusExample ("Aria",1350,10,3)

test4 = TestCase $ assertEqual "payEveryone 1"
  ([("Aria",1450,17,0),("Kevin",1230,10,3),("Faizan",1880,2,0),("Filip",600,35,0)], Property.empty, Railroad.empty, Utility.empty) $ payEveryone statusExample ("Filip",900,35,0) 100

test5 = TestCase $ assertEqual "payEveryone 2"
  ([("Aria",1250,17,0),("Kevin",1030,10,3),("Faizan",1680,2,0),("Filip",1200,35,0)], Property.empty, Railroad.empty, Utility.empty) $ payEveryone statusExample ("Filip",900,35,0) (-100)

test6 = TestCase $ assertEqual "transaction"
  ([("Aria",1350,17,0),("Kevin",1030,10,3),("Faizan",1780,2,0),("Filip",1000,35,0)], Property.empty, Railroad.empty, Utility.empty) $ transaction statusExample "Filip" "Kevin" 100

test7 = TestCase $ assertEqual "Property.find"
  ("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]) $ Property.find statusExample "Haggsalen"

test8 = TestCase $ assertEqual "Property.update & Property.colorMonopoly"
  False $ Property.colorMonopoly (Property.update (playersExample,Property.update statusExample ("UTH-Gard","Kevin",Blue,False,35,0,350,200,[35,175,500,1100,1300,1500]) "UTH-Gard", Railroad.empty,Utility.empty) ("Skrubben","Filip",Blue,False,50,0,400,200,[50,200,600,1400,1700,2000]) "Skrubben") Blue "Kevin"

test9 = TestCase $ assertEqual "Property.update & Property.colorMonopoly"
  True $ Property.colorMonopoly (Property.update (playersExample,Property.update statusExample ("UTH-Gard","Kevin",Blue,False,35,0,350,200,[35,175,500,1100,1300,1500]) "UTH-Gard", Railroad.empty,Utility.empty) ("Skrubben","Kevin",Blue,False,50,0,400,200,[50,200,600,1400,1700,2000]) "Skrubben") Blue "Kevin"

test10 = TestCase $ assertEqual "Property.buy 1"
  (playersExample,[("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])],Railroad.empty,Utility.empty) $ Property.buy (playersExample,[("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Nobody",Green,False,0,0,320,200,[28,150,450,1000,1200,1400])],Railroad.empty,Utility.empty) [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] False Green

test11 = TestCase $ assertEqual "Property.buy 2"
  (playersExample,[("Polhemssalen","Aria",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,True,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,True,28,0,320,200,[28,150,450,1000,1200,1400])],Railroad.empty,Utility.empty) $ Property.buy (playersExample,[("Polhemssalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])],Railroad.empty,Utility.empty) [("Polhemssalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Aria",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] True Green

test12 = TestCase $ assertEqual "Railroad.find"
  ("Grindstugan","Nobody",0) $ Railroad.find statusExample "Grindstugan"

test13 = TestCase $ assertEqual "Railroad.buy"
  ([("Aria",1350,17,0),("Kevin",1130,10,3),("Faizan",1580,5,0),("Filip",900,35,0)],Property.empty,[("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Faizan",2),("Lundellska Skolan","Faizan",2)],Utility.empty) $ Railroad.buy (playersExample,Property.empty,[("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Faizan",1)],Utility.empty) ("Faizan",1780,5,0) "Uppsala Science Park"

test14 = TestCase $ assertEqual "Utility.find"
  ("Electric Company","Nobody",0) $ Utility.find statusExample "Electric Company"

test15 = TestCase $ assertEqual "Utility.buy"
  ([("Aria",1350,17,0),("Kevin",1130,10,3),("Faizan",1630,5,0),("Filip",900,35,0)],Property.empty,Railroad.empty,[("Electric Company","Faizan",2),("Water Works","Faizan",2)]) $ Utility.buy (playersExample,Property.empty,Railroad.empty,[("Electric Company","Nobody",0),("Water Works","Faizan",1)]) ("Faizan",1780,5,0) "Electric Company"

test16 = TestCase $ assertEqual "Property.clear"
  [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] $ Property.clear [("Polhemssalen","Nobody",Green,False,0,0,300,200,[26,130,390,900,1100,1275]),("Haggsalen","Aria",Green,False,26,0,300,200,[26,130,390,900,1100,1275]),("Siegbahnsalen","Filip",Green,False,28,0,320,200,[28,150,450,1000,1200,1400])] [("Filip",900,35,0)]

test17 = TestCase $ assertEqual "Railroad.clear"
  [("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Nobody",0),("Lundellska Skolan","Filip",1)] $ Railroad.clear [("Polacksbacken","Kevin",2),("Grindstugan","Kevin",2),("Uppsala Science Park","Faizan",1),("Lundellska Skolan","Filip",1)] [("Kevin",1130,10,3),("Filip",900,35,0)]

test18 = TestCase $ assertEqual "Utility.clear"
  [("Electric Company","Nobody",0),("Water Works","Nobody",0)] $ Utility.clear [("Electric Company","Faizan",2),("Water Works","Faizan",2)] [("Aria",1350,17,0),("Kevin",1130,10,3),("Filip",900,35,0)]

--For running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18]
