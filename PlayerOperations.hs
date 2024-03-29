module PlayerOperations where

import TypesMonopoly

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

{- updatePlayer (players, properties, railroads, utilities) player
   Updates the status of player in status
   PRE: player is a player in status
   RETURNS: status with the updated values for player
   EXAMPLES: updatePlayer ([("Faizan",1680,17,0),("Aria",950,25,0)],[],[],[]) ("Faizan",1480,17,0) == ([("Faizan",1480,17,0),("Aria",950,25,0)],[],[],[]),
             updatePlayer ([("Faizan",1680,17,0),("Aria",950,25,0)],[],[],[]) ("Aria",950,10,3) == ([("Faizan",1680,17,0),("Aria",950,10,3)],[],[],[])
 -}
updatePlayer :: Status -> Player -> Status

{- payEveryone (players, properties, railroads, utilities) player bill
   Lets player pay a bill to all the other players in status
   PRE: player is the name of a player in status
   RETURNS: status were player has payed all the other players $bill
   EXAMPLES: payEveryone ([("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)],[],[],[]) ("Faizan",1680,17,0) 100 == ([("Faizan",1480,17,0),("Aria",1050,25,0),("Filip",1200,2,0)],[],[],[]),
             payEveryone ([("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)],[],[],[]) ("Faizan",1680,17,0) (-100) == ([("Faizan",1880,17,0),("Aria",850,25,0),("Filip",1000,2,0)],[],[],[])
 -}
payEveryone :: Status -> Player -> Int -> Status

{- transaction (players, properties, railroads, utilities) reciever giver bill
   Transfers money from one players balance to another
   PRE: reciever and giver are names of two players in status 
   RETURNS: 'status' with updated player balances such that 'bill' is taken from 'giver' and is given to 'reciever'
   EXAMPLES: transaction ([("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)],[],[],[]) "Filip" "Faizan" 100 == ([("Faizan",1580,17,0),("Aria",950,25,0),("Filip",1200,2,0)],[],[],[])
 -}
transaction :: Status -> Name -> Name -> Int -> Status

{- jail status player
   Moves a player to jail
   RETURNS: status where player is in jail
   SIDE-EFFECTS: Display what is going on
 -}
jail :: Status -> Player -> IO Status

-------------------------------------------------------------------------------
-- implementation
-------------------------------------------------------------------------------

updatePlayer (players, pro, rai, uti) player =
  ((updatePlayer' players player), pro, rai, uti)
  where
   {- updatePlayer' players player
      Updates the status of player
      RETURNS: players with the updated values for player
      EXAMPLES: updatePlayer' [("Faizan",1680,17,0),("Aria",950,25,0)] ("Faizan",1480,17,0) == [("Faizan",1480,17,0),("Aria",950,25,0)],
                updatePlayer' [("Faizan",1680,17,0),("Aria",950,25,0)] ("Aria",950,10,3) == [("Faizan",1680,17,0),("Aria",950,10,3)]
    -}
   -- VARIANT: length players
   updatePlayer' :: [Player] -> Player -> [Player]
   updatePlayer' ((n1, b1, p1, j1):xs) (n, b, p, j)
         | n == n1   = (n, b, p, j):xs
         | otherwise = ((n1, b1, p1, j1):(updatePlayer' xs (n, b, p, j)))

payEveryone (pla, pro, rai, uti) player bill = payEveryone' pla (pla, pro, rai, uti) player bill
  where
    {- payEveryone' playersList (players, properties, railroads, utilities) player bill
   Lets player pay a bill to all the other players in status
   RETURNS: status were player has payed all the other players $bill
   EXAMPLES: payEveryone' [("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)] ([("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)],[],[],[]) ("Faizan",1680,17,0) 100 == ([("Faizan",1480,17,0),("Aria",1050,25,0),("Filip",1200,2,0)],[],[],[])
     -}
    --VARIANT: length playersList
    payEveryone' :: [Player] -> Status -> Player -> Int -> Status
    payEveryone' [] s player _ = updatePlayer s player  
    payEveryone' ((n1, b1, p1, j1):xs) s (n, b, p, j) bill
      | n1 == n   = payEveryone' xs s (n, b, p, j) bill
      | otherwise = payEveryone' xs (updatePlayer s (n1, b1+bill, p1, j1)) (n, b-bill, p, j) bill

transaction (pla, pro, rai, uti) p1 p2 bill = transaction' pla (pla, pro, rai, uti) p1 p2 bill
  where
    {- transaction' playersList (players, properties, railroads, utilities) reciever giver bill
       Transfers money from one players balance to another
       RETURNS: 'status' with updated player balances such that 'bill' is taken from 'giver' and is given to 'reciever'
       EXAMPLES: transaction' [("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)] ([("Faizan",1680,17,0),("Aria",950,25,0),("Filip",1100,2,0)],[],[],[]) "Filip" "Faizan" 100 == ([("Faizan",1580,17,0),("Aria",950,25,0),("Filip",1200,2,0)],[],[],[])
     -}
    -- VARIANT: length playersList
    transaction' :: [Player] -> Status -> Name -> Name -> Int -> Status
    transaction' [] s _ _ _ = s
    transaction' ((n, b, p, j):xs) s name1 name2 bill
                        | n == name1   = transaction' xs (updatePlayer s (n, b+bill, p, j)) p1 p2 bill
                        | n == name2   = transaction' xs (updatePlayer s (n, b-bill, p, j)) p1 p2 bill
                        | otherwise = transaction' xs s name1 name2 bill

jail s (n, b, _, _) = do
  putStrLn "Go to Jail."
  return $ updatePlayer s (n, b, 10, 3)
