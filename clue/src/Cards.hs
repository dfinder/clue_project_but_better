module Cards where 
import GameState
initCards = [Plum,Mustard,Green,Scarlet,White,Peacock]++[Candlestick,Dagger,Pipe,Revolver,Rope,Wrench]++[Kitchen,Ball,Conservatory,Billard,Library,Study,Dining,Hall,Lounge]
distributeCards:: [Card] -> Int->([Card],[[Card]])
characters = [Plum,Mustard,Green,Scarlet,White,Peacock]
weapons = [Candlestick,Dagger,Pipe,Revolver,Rope,Wrench]
rooms = [Kitchen,Ball,Conservatory,Billard,Library,Study,Dining,Hall,Lounge]

seed::Int
seed = 40

giveList :: [Int]
giveList = [8,9,4,5,2]


selectWinCon::[Character]->[Weapon]->[Room]->((Character,Weapon,Room),[Card])
selectWinCon (char:chars) (weapon:weapons) (room:rooms)  = (char,weapon,room,[chars+weapons+rooms]) 
constructHands:: Int-> [[Card]]->[[Card]]
constructHands i [] = i-1 [[]]
constructHands 0 l = l 
constructHands i l = i-1 []:l


--Cyclic player list
rotate:: Int->[a]
rotate n xs = bs ++ as where (as, bs) = splitAt n xs
-- (constructHands (length player_list) [])

shuffleCards:: [Card]->Int->[Card]
shuffleCards [] _ shuffled = shuffled 
shuffleCards (top_card:deck) seed shuffled = rotate deck seed ((seed+1) *2 `mod` length deck) top_card:shuffled
setupHands:: Int->[[a]]
setupHands count [] = count-1 [[]]
setupHands count hands = []::hands
setupHands 0 hands = hands
distributeRemainingCards:: [Card]->Int->[[Card]]
distributeRemainingCards deck player_count [] = distributeRemainingCards deck hands (setupHands player_count)
distributeRemainingCards (top:heap) player_count (top_hand:hands)  = distributeRemainingCards heap player_count hands ++ (top:top_hand)

