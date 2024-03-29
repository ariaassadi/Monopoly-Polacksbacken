module TypesMonopoly where

{- Represents the name of a player, house, railroad or utility.
   As a String.
   INVARIANT: the name of player can never be "" or "Nobody"
              name must be a unique String i.e. two players/houses/railroad/utilities can�t have the same value.
 -}
type Name = String

{- Represents how much money a player has.
   As an Int that starts at $1500.
 -}
type Balance = Int

{- Represents on wich position on the board a player is standing on.
 -}
type Position = Int

{- Represents how many rounds a player max has left in jail. If jail == 0 the player isn�t in jail.
   0 == Not in Jail
   1 == Max 1 round left in Jail
   2 == Max 2 round left in Jail
   3 == Max 3 round left in Jail
   INVARIANT: jail >= 0 && jail <= 3
 -}
type Jail = Int

{- Contains everything you need to know about a player.
   Thier name, balance, position and jail. All in the same tuple.
 -}
type Player = (Name, Balance, Position, Jail)

{- Represents how much a property, property upgrade, railroad or utility cost.
   As an Int.
 -}
type Price = Int

{- Represent the name of the owner of a property.
   INVARIANT: if owner == "Nobody" that property has no owner.
 -}
type Owner = String

{- Represents how much money you need to pay if you land on someones property.
 -}
type Rent = Int

{- Represents wich stage a property, railroad or utility currently is in. The stage of a property is number of houses on that property. The stage of a railroad/ utility is the total number of railroads/ utilities that are owned by the same player.
   INVARIANT: Stage (property): stage >= 0 && stage <= 5
              Stage (railroad): stage >= 0 && stage <= 4
              Stage (utility):  stage >= 0 && stage <= 2
 -}
type Stage = Int

{- Represent wich color group a house belongs to.
 -}
data ColorGroup = Brown | Cyan | Pink | Orange | Red | Yellow | Green
  | Blue deriving (Show, Eq)

{- Represent wether a player owns all houses in a color group or not.
   True iff all properties in the same ColorGroup is owned by the same player.
 -}
type ColorMonopoly = Bool

{- Contains everything you need to know about a property.
   The name, owner, color group, colorMonopoly, current rent, current stage, price, price to upgrade and the rent for each stage. All in the same tuple.
 -}
type Property = (Name, Owner, ColorGroup, ColorMonopoly, Rent, Stage, Price, Price, [Rent])

{- Contains everything you need to know about a railroad.
   The name, owner and current stage. All in the same tuple.
 -}
type Railroad = (Name, Owner, Stage)

{- Contains everything you need to know about a utility.
   The name, owner and current stage. All in the same tuple.
 -}
type Utility = (Name, Owner, Stage)

{- Represent the sum of the dots on the two dice
   INVARIANT: dieRoll >= 2 && dieRoll <= 12
 -}
type DieRoll = Int

{- Contains all the information about the status of the game.
   The players, houses, railroads and utilites. All in the same tuple.
 -}
type Status = ([Player], [Property], [Railroad], [Utility])
