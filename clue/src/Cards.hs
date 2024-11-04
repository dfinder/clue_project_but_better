module Cards where
import GameState ( Room(..), Weapon(..), Character(..), Card (RoomCard, WeaponCard, CharCard) )
import GHC.Num

import qualified Data.List.NonEmpty as NonEmpty
--initCards :: [Card]
--initCards =   map CharCard characters ++ map WeaponCard weapons ++ map RoomCard rooms 
--distributeCards:: [Card] -> Int->([Card],[[Card]])
characters :: [Character]
characters = [Plum,Mustard,Green,Scarlet,White,Peacock]
charCards ::[Character]
charCards = shuffleCards characters seed []
weapons :: [Weapon]
weapons = [Candlestick,Dagger,Pipe,Revolver,Rope,Wrench]
weaponCards :: [Weapon]
weaponCards  = shuffleCards weapons seed []
rooms :: [Room]
rooms = [Kitchen,Ball,Conservatory,Billard,Library,Study,Dining,Hall,Lounge]
roomCards :: [Room]
roomCards  = shuffleCards rooms seed []
seed::Int
seed = 40

selectWinCon::((Character,Weapon,Room),[Card])
selectWinCon  =  let
    theweapons = weaponCards
    therooms = roomCards
    thecharacters = charCards in ((head thecharacters, head theweapons, head therooms), map WeaponCard (tail theweapons) ++ map RoomCard (tail therooms) ++ map CharCard (tail thecharacters))

constructHands:: Int-> [[Card]]->[[Card]]
constructHands i [[]] = constructHands (i-1) [[]]
constructHands 0 l = l
constructHands i l = constructHands (i-1) [[]]++l

--Cyclic player list
rotate:: Int->[a]->[a]
rotate n xs = bs ++ as where (as, bs) = splitAt n xs

shuffleCards:: [a]->Int->[a]->[a]
shuffleCards [] _ shuffled = shuffled
shuffleCards (top_card:deck) seed shuffled = shuffleCards (rotate seed deck) ((seed+1) *2 `mod` length deck) (top_card:shuffled)
setupHands:: Int-> [[Card]]
setupHands 0 = [[]]
setupHands count = setupHands (count-1)++[[]]
distributeRemainingCards:: [Card]->Int->[[Card]]
distributeRemainingCardsRec:: [Card]->[[Card]]->[[Card]]
distributeRemainingCards deck player_count = distributeRemainingCardsRec deck (setupHands player_count)
distributeRemainingCardsRec (top:heap) (top_hand:hands) = distributeRemainingCardsRec heap (hands++[top:top_hand])
distributeRemainingCardsRec [] hands  = hands
distributeRemainingCardsRec _ _ = undefined 
--[[CharCard Plum]]