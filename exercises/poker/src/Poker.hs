module Poker (bestHands) where

import Data.List (sort, sortBy, nub, group, groupBy, length)
import Data.List.Extra (groupSortOn)
import Data.List.Split (splitOn)
import Control.Monad (msum)

data Suit = Hearts | Spades | Diamonds | Clubs deriving (Eq)
instance Ord Suit where
  compare _ _ = EQ

instance Show Suit where
  show Hearts = "H"
  show Spades = "S"
  show Diamonds = "D"
  show Clubs = "C"

data FaceValue
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum)

instance Show FaceValue where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card = Card {
  face :: FaceValue,
  suit :: Suit
}

instance Eq Card where
  (Card f1 _) == (Card f2 _) = f1 == f2
instance Ord Card where
  compare (Card f1 _) (Card f2 _) = compare f1 f2

instance Show Card where
  show (Card f s) = (show f) ++ (show s)

data Hand = Hand Card Card Card Card Card

instance Eq Hand where
  hand1 == hand2 = (handToList hand1) == (handToList hand2)
instance Ord Hand where
  compare hand1 hand2 = compare (handToList hand1) (handToList hand2)

instance Show Hand where
  show (Hand a b c d e) = (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e)

parseSuit :: Char -> Maybe Suit
parseSuit 'H' = Just Hearts
parseSuit 'D' = Just Diamonds
parseSuit 'C' = Just Clubs
parseSuit 'S' = Just Spades
parseSuit _ = Nothing

parseFace :: String -> Maybe FaceValue
parseFace "2" = Just Two
parseFace "3" = Just Three
parseFace "4" = Just Four
parseFace "5" = Just Five
parseFace "6" = Just Six
parseFace "7" = Just Seven
parseFace "8" = Just Eight
parseFace "9" = Just Nine
parseFace "10" = Just Ten
parseFace "J" = Just Jack
parseFace "Q" = Just Queen
parseFace "K" = Just King
parseFace "A" = Just Ace
parseFace _ = Nothing

parseCard :: String -> Maybe Card
parseCard ('1' : '0' : s : []) = Card <$> (parseFace "10") <*> (parseSuit s)
parseCard (f : s : []) = Card <$> (parseFace [f]) <*> (parseSuit s)
parseCard _ = Nothing

parseHand :: String -> Maybe Hand
parseHand s = do
  x <- sequence $ parseCard <$> splitOn " " s
  toHand x
  where
    toHand (a : b : c : d : e : []) = Just $ Hand a b c d e
    toHand _ = Nothing

-- (Works)
isStraight :: Hand -> Maybe HandRank
isStraight hand = case faces sortedHand of
    (Two : Three : Four : Five : Ace : []) -> Just (Straight Five) -- Ace is high in our Ord instance, but it can be low.
    (f1 : f2 : f3 : f4 : f5 : []) ->
        if (succ f1 == f2
		&& succ f2 == f3
		&& succ f3 == f4
		&& succ f4 == f5)
        then Just (Straight f5)
        else Nothing
  where
  sortedHand :: [Card]
  sortedHand = sort $ handToList hand
  faces :: [Card] -> [FaceValue]
  faces cards = face <$> cards


handToList :: Hand -> [Card]
handToList (Hand a b c d e) = sort [a, b, c, d, e]

-- (Done)
isFlush :: Hand -> Maybe HandRank
isFlush hand = case length (nub suits) of
    1 -> Just (Flush hand)
    _ -> Nothing
  where
  suits :: [Suit]
  suits = (map suit (handToList hand))

isStraightFlush :: Hand -> Maybe HandRank
isStraightFlush hand = isFlush hand >> isStraight hand >> (Just $ StraightFlush (face highestCard))
  where highestCard = head $ (handToList hand)

-- [ [3H, 3S, 3C, 3D], [4S] ]
isGroupedHand :: Hand -> Maybe HandRank
isGroupedHand hand = case sortBy (\g1 g2 -> compare (length g2) (length g1)) groupedHand of
  ( [c1, _, _, _] : [c2] : []) -> Just (FourOfAKind (face c1) (face c2))
  ( [c1, _, _]    : [c2, _] : [] ) -> Just (FullHouse (face c1) (face c2))
  ( [c1, _, _]    : [c2] : [c3] : []) -> Just (ThreeOfAKind (face c1) (face c2) (face c3))
  ( [c1, _] : [c2, _] : [c3] : [] ) -> Just (TwoPair (face c1) (face c2) (face c3))
  ( [c1, _] : [c2] : [c3] : [c4] : [] ) -> Just (Pair (face c1) (face c2) (face c3) (face c4))
  _ -> Nothing
  where
  groupedHand :: [[Card]]
  groupedHand = reverse . sort $ group (handToList hand)

data HandRank
  = HighCard Hand
  | Pair FaceValue FaceValue FaceValue FaceValue
  | TwoPair FaceValue FaceValue FaceValue
  | ThreeOfAKind FaceValue FaceValue FaceValue
  | Straight FaceValue
  | Flush Hand
  | FullHouse FaceValue FaceValue
  | FourOfAKind FaceValue FaceValue
  | StraightFlush FaceValue
  deriving (Show, Eq, Ord)


getHandRank :: Hand -> HandRank
getHandRank hand =
  case msum ([ isGroupedHand, isStraightFlush, isFlush, isStraight ] <*> pure hand) of
    (Just x) -> x
    Nothing -> HighCard hand

-- GOAL Function
bestHands :: [String] -> Maybe [String]
bestHands hands = fmap show <$> topHands <$> handRanks <$> parseHands hands
  where
  topHands :: [[Hand]] -> [Hand]
  topHands = head . reverse
  handRanks :: [Hand] -> [[Hand]]
  handRanks = groupSortOn getHandRank
  parseHands :: [String] -> Maybe [Hand]
  parseHands hands = sequence $ parseHand <$> hands
-- GOAL Function
