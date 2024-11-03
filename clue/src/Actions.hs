module Actions where
import GameState
import Cards
import Data.List.NonEmpty (NonEmpty)
data Action = MoveAction Move | TeleportAction Teleport | GuessAction Guess | AccuseAction Accuse | RespondAction Respond| EndTurn 
data Player = Player{hand::[Card],piece::Character, knowledge::Knowledge, location::Location, teleported::Bool,lost::Bool} deriving(Show)
type Knowledge = [Card]
data Move = Move Location
data Guess = Guess Weapon Room Character
data Accuse = Accuse Weapon Room Character
data Respond = Respond Player (Maybe Card) 
data Teleport = Teleport Player Location
data GameState = GameState{players::[Player],win_con::(Character,Weapon,Room),turn_order::NonEmpty Character}

moveOptions :: Player -> [Player] -> [Move]
moveOptions player players = map (Move player) filter (\x-> x `elem` map location players) getAdjacent (location player)
move:: Player -> Move -> Player
move player (Move loc) = player{location = loc}
teleport :: Player -> Location -> Player
teleport player loc = player{location=loc,teleported=True}
accuse :: Player-> GameState -> Character -> Weapon -> Room -> Player
accuse player gs murderer weapon room = player{lost= win_con gs == (murderer,weapon,room)}
wGuess :: Player -> Character -> Weapon -> Player 
wGuess self murderer weapon = guess self Guess murderer weapon (location self)
guess :: Player-> Guess -> Player
guess player guess = player{knowledge=getResponse (knowledge player) guess}
doSomething :: GameState->Player->(Player->Player)->GameState
doSomething gs player action 
    | action == Move loc = gs{players = (players gs)//[index players gs player,(move player loc)]}
    | action == Teleport p loc 
    | action = Accuse p loc 
    | action = Respond p card 
    | action = Guess weapon room char
    | action = EndTurn gs{turn_order=tail turn_order gs}