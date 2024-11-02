module Actions where
import GameState
import Cards
data Action = Move | Teleport | Guess | Accuse | Respond
data Player = Player{hand::[Card], knowledge::Knowledge, location::Location, teleported::Bool,lost::Bool} deriving(Show)
type Knowledge = [Card]
data Move = Move Player Location
data Guess = Guess Player Weapon Character
data Accuse = Accuse Player Weapon Room Character
data Teleport = Teleport Player Location

moveOptions :: Player -> [Player] -> [Move]
moveOptions player players = map (Move player) filter (\x-> x `elem` map location players) getAdjacent (location player)

teleport :: Player -> Location -> Player 
teleport player loc = player{location=loc}