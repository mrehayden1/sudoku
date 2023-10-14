module Main (
  main
) where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

data Digit = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
 deriving (Enum, Eq, Ord, Show)

type Row = Digit
type Col = Digit

toChar :: Digit -> Char
toChar d =
  case d of {
    One -> '1'; Two -> '2';   Three -> '3'; Four -> '4'; Five -> '5';
    Six -> '6'; Seven -> '7'; Eight -> '8'; Nine -> '9' }

type Puzzle = Map (Col, Row) Digit

data PuzzlePrinter = PP Puzzle

instance Show PuzzlePrinter where
  show (PP p) = showPuzzle p

toString :: Puzzle -> String
toString m = do
  j <- [One .. Nine]
  i <- [One .. Nine]
  return . maybe ' ' toChar . Map.lookup (i, j) $ m

fromString :: String -> Either String Puzzle
fromString = fmap puzzleFromMaybes . mapM readDigit
 where
  puzzleFromMaybes =
    let ks = [ (i, j) | j <- [One .. Nine], i <- [One .. Nine] ]
    in Map.fromList . catMaybes . zipWith (fmap . (,)) ks

  readDigit :: Char -> Either String (Maybe Digit)
  readDigit c =
    case c of {
      '1' -> Right $ Just One;   '2' -> Right $ Just Two;
      '3' -> Right $ Just Three; '4' -> Right $ Just Four;
      '5' -> Right $ Just Five;  '6' -> Right $ Just Six;
      '7' -> Right $ Just Seven; '8' -> Right $ Just Eight;
      '9' -> Right $ Just Nine;  ' ' -> Right Nothing;
      _   -> Left $ "Invalid char '" ++ (c:[]) ++ "'." }

solve :: Puzzle -> [Puzzle]
solve = solve' [ (i, j) | j <- [One .. Nine], i <- [One .. Nine] ]

solve' :: [(Col, Row)] -> Puzzle -> [Puzzle]
solve' []            m = [ m ]
solve' (k@(c, r):ks) m = do
  g <- case Map.lookup k m of
         Just d  -> [ d ]
         Nothing -> Set.toList . Set.difference allCandidates . Set.unions
                      . fmap Set.fromList $ [ row, col, box ]
  let m' = Map.insert k g m
  solve' ks m'
 where
  allCandidates = Set.fromList [One .. Nine]
 
  row :: [Digit]
  row =
    let ks = [ (i, r) | i <- [One .. Nine] ]
    in catMaybes . map (flip Map.lookup m) $ ks

  col :: [Digit]
  col =
    let ks = [ (c, j) | j <- [One .. Nine] ]
    in catMaybes . map (flip Map.lookup m) $ ks

  -- Get the cell addresses for the box of cell (c, r)
  box :: [Digit]
  box =
   let ks = [ (i, j) | j <- box' r, i <- box' c ]
   in catMaybes . map (flip Map.lookup m) $ ks
   where
    box' n =
      let b = (flip div 3 . fromEnum $ n)
      in [ toEnum (b * 3) .. toEnum (b * 3 + 2) ]

puzzle :: Puzzle
puzzle = either error id . fromString $ "7    2  4   1573   31    5  4   1   8       9   6   4  5    72   8736   1  4    6"

showPuzzle :: Puzzle -> String
showPuzzle p =
  "┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓\n"
    ++ (intercalate "\n" . map (formatRow p) $ [One .. Nine])
    ++ "\n┗━━━┷━━━┷━━━┻━━━┷━━━┷━━━┻━━━┷━━━┷━━━┛"
 where
  formatRow :: Puzzle -> Row -> String
  formatRow m j = intercalate "\n" $ [
      "┃ " ++ concatMap formatCell [ One .. Nine ]
    ] ++ (
      if j == One || j == Two || j == Four || j == Five || j == Seven
           || j == Eight
        then [ "┠───┼───┼───╂───┼───┼───╂───┼───┼───┨" ] 
        else (
          if j == Three || j == Six
            then [ "┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫" ] 
            else []
        )
    )
   where
    formatCell :: Col -> String
    formatCell i = (maybe ' ' toChar . flip Map.lookup m $ (i, j)) : (
        if i == Three || i == Six || i == Nine then " ┃ " else " │ "
      )
  
main = do
  --putStrLn "Enter partial solution:"
  let solutions = solve puzzle
  putStrLn "Solutions: "
  mapM_ (putStrLn . showPuzzle) solutions
