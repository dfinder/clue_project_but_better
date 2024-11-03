{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}

module GameState where
import Data.Maybe
data Card = CharCard Character | WeaponCard Weapon | RoomCard Room 
data Passage = Passage Room Room Bool
data Location = RoomLoc Room | PasLoc Passage | StartLoc Character Room Room
data Character = Plum|Mustard|Green|Scarlet|White|Peacock
data Weapon = Candlestick|Dagger|Pipe|Revolver|Rope|Wrench
data Room = Kitchen|Ball|Conservatory|Billard|Library|Study|Dining|Hall|Lounge deriving (Eq)
getStarts :: [Location]
getStarts = [     StartLoc Scarlet Hall Lounge ,
                  StartLoc Mustard Dining Lounge,
                  StartLoc White Ball Kitchen ,
                  StartLoc Green Ball Conservatory,
                  StartLoc Plum Study Library ,
                  StartLoc Peacock Library Conservatory]
getAdjacency :: [Passage]
getAdjacency = [  Passage Study Kitchen True,   --There should be 20 passages. 2 secrets, 12 normals, and 6 starting locations
                   Passage Lounge Conservatory True,
                   Passage Study Hall False,
                   Passage Study Library False,
                   Passage Hall Lounge False,
                   Passage Hall Billard False,
                   Passage Lounge Dining False,
                   Passage Dining Kitchen False,
                   Passage Dining Billard False ,
                   Passage Billard Ball False ,
                   Passage Billard Library False,
                   Passage Library Conservatory False ,
                   Passage Ball Conservatory False ,
                   Passage Ball Kitchen False ]
getAdjacent :: Location -> [Location]
--getAdjacent r@(RoomLoc a) = filter (\x-> let (start,end,secret)=x in x )
matchPassage :: Room -> Passage -> Maybe Room
matchPassage room p@(Passage start end b)
  | room == start = Just end
  | room == end = Just start
  | otherwise = Nothing

getAdjacent (RoomLoc room) = map RoomLoc (mapMaybe (matchPassage room) getAdjacency)
getAdjacent (PasLoc (Passage  from to _)) = [RoomLoc from,RoomLoc to]
getAdjacent (StartLoc _ from to) = [PasLoc$Passage from to False]

inRoom :: Location -> Bool 
inRoom (RoomLoc _) = True 
inRoom _ = False 