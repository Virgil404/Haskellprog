module Leadando where 
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
--import Data.List (deleteFirstsBy)
import Data.List (delete, intersect, sort,(\\), nub, dropWhileEnd)
import Data.Maybe (isNothing, fromMaybe)
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Char (toLower)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Színek
data Color = Purple | DarkGreen | Orange | Pink | Yellow | Red | Gray | DarkBlue | Green | Blue
  deriving (Eq, Show)

-- Tipp
type Guess = [Color]

-- Kitalálandó színsor
type ColorRow = [Color]

-- Jelölők
data Mark = Black | White
  deriving (Eq, Show)


-- Visszajelzés
data GuessResp = Resp Guess [Mark]
  deriving (Eq, Show)

-- Lehetőségek száma
type AvailableGuesses = Int

-- Játék vége státusz
data Completion = Success Int | Failure String | Ongoing
  deriving (Eq, Show)


data Game = Game ColorRow (AvailableGuesses, [GuessResp]) Completion
  deriving (Eq, Show)

--data Response = Respo [Color] [Feedback]


emptyGame1 :: Game
emptyGame1 = Game [Red, DarkGreen, Pink, Orange] (10, []) Ongoing

emptyGame2 :: Game
emptyGame2 = Game [Yellow, Gray, Green, DarkBlue] (3, []) Ongoing

wrongGame1 :: Game
wrongGame1 = Game [Red] (10,[]) Ongoing

wrongGame2 :: Game
wrongGame2 = Game (repeat Purple) (2,[]) Ongoing




matchingColorsRightPlace :: Guess -> ColorRow -> Maybe Int
matchingColorsRightPlace guess colorRow
  | isListSmallerOrEqual4 guess || isListSmallerOrEqual4cr colorRow = Nothing
  | isListBiggerThan4cr colorRow || isListBiggerThan4 guess = Nothing
  | isEmpty guess = Nothing
  | isEmptycr colorRow = Nothing
  | otherwise = Just $ countMatchingColors 0 (zipGuessAndColor guess colorRow)
  where
    isListSmallerOrEqual4 :: Guess -> Bool
    isListSmallerOrEqual4 xs = go 0 xs
      where
      go n []     = n < 4
      go n (_:ys) = n < 4 && go (n + 1) ys

    isListSmallerOrEqual4cr :: ColorRow -> Bool
    isListSmallerOrEqual4cr xs = go 0 xs
      where
      go n []     = n < 4
      go n (_:ys) = n < 4 && go (n + 1) ys

    isListBiggerThan4cr :: ColorRow -> Bool
    isListBiggerThan4cr xs = go 0 xs
       where
       go n []     = n > 4
       go n (_:ys) | n >= 4    = True
                | otherwise = go (n + 1) ys

    isListBiggerThan4 :: Guess -> Bool
    isListBiggerThan4 xs = go 0 xs
       where
       go n []     = n > 4
       go n (_:ys) | n >= 4    = True
                | otherwise = go (n + 1) ys

    isEmpty :: Guess -> Bool
    isEmpty [] = True
    isEmpty _  = False

    
    isEmptycr :: ColorRow -> Bool
    isEmptycr [] = True
    isEmptycr _  = False

   


    zipGuessAndColor :: Guess -> ColorRow -> [(Color, Color)]
    zipGuessAndColor xs ys = take 4 (zip xs ys)

    countMatchingColors :: Int -> [(Color, Color)] -> Int
    countMatchingColors count [] = count
    countMatchingColors count ((x, y):rest)
      | x == y = countMatchingColors (count + 1) rest
      | otherwise = countMatchingColors count rest



matchingColorsWrongPlace :: Guess -> ColorRow -> Maybe Int
matchingColorsWrongPlace guess colorRow
  | isListSmallerOrEqual4 guess ||  isListSmallerOrEqual4cr colorRow = Nothing
  | isListBiggerThan4cr colorRow || isListBiggerThan4 guess = Nothing
  | isEmpty guess = Nothing
  | isEmptycr colorRow = Nothing
  | otherwise =
      let matchingRightPlace = matchingColorsRightPlace guess colorRow
          totalMatches = length (intersect guess colorRow)
      in case matchingRightPlace of
           Just rightPlace -> Just (totalMatches - rightPlace)
           Nothing -> Just totalMatches
  where
    isListSmallerOrEqual4 :: Guess -> Bool
    isListSmallerOrEqual4 xs = go 0 xs
      where
      go n []     = n < 4
      go n (_:ys) = n <= 4 && go (n + 1) ys

    isListSmallerOrEqual4cr :: ColorRow -> Bool
    isListSmallerOrEqual4cr xs = go 0 xs
      where
      go n []     = n < 4
      go n (_:ys) = n <= 4 && go (n + 1) ys

    isListBiggerThan4cr :: ColorRow -> Bool
    isListBiggerThan4cr xs = go 0 xs
       where
       go n []     = n > 4
       go n (_:ys) | n >= 4    = True
                | otherwise = go (n + 1) ys

    isListBiggerThan4 :: Guess -> Bool
    isListBiggerThan4 xs = go 0 xs
       where
       go n []     = n > 4
       go n (_:ys) | n >= 4    = True
                | otherwise = go (n + 1) ys


    isEmpty :: Guess -> Bool
    isEmpty [] = True
    isEmpty _  = False

    isEmptycr :: ColorRow -> Bool
    isEmptycr [] = True
    isEmptycr _  = False

    matchingColorsRightPlace :: Guess -> ColorRow -> Maybe Int
    matchingColorsRightPlace [] [] = Just 0
    matchingColorsRightPlace (x:xs) (y:ys)
      | x == y = fmap (+1) (matchingColorsRightPlace xs ys)
      | otherwise = matchingColorsRightPlace xs ys
    matchingColorsRightPlace _ _ = Nothing

isListBiggerThan4 :: [a] -> Bool
isListBiggerThan4 xs = go 0 xs
       where
       go n []     = n > 4
       go n (_:ys) | n >= 4    = True
                | otherwise = go (n + 1) ys


guessOnce :: Guess -> ColorRow -> AvailableGuesses -> Maybe (AvailableGuesses, GuessResp)
guessOnce guess colorRow availableGuesses
  | availableGuesses <= 0 = Nothing
  | isListSmallerOrEqual4 guess || isListSmallerOrEqual4 colorRow = Nothing
  | isListBiggerThan4 colorRow || isListBiggerThan4 guess = Nothing
  | length guess /= length colorRow || null guess = Nothing
  | otherwise =
      case matchingColorsRightPlace guess colorRow of
        Nothing -> Nothing
        Just blackMarkers -> 
          case matchingColorsWrongPlace guess colorRow of
            Nothing -> Nothing
            Just whiteMarkers ->
              let remainingGuesses = availableGuesses - 1
              in Just (remainingGuesses, Resp guess (replicate blackMarkers Black ++ replicate whiteMarkers White))
  where
   isListSmallerOrEqual4 :: [a] -> Bool
   isListSmallerOrEqual4 xs = go 0 xs
      where
      go n []     = n < 4
      go n (_:ys) = n <= 4 && go (n + 1) ys


   isListBiggerThan4 :: [a] -> Bool
   isListBiggerThan4 xs = go 0 xs
       where
       go n []     = n > 4
       go n (_:ys) | n >= 4    = True
                | otherwise = go (n + 1) ys


   removeBlackMarkers :: Guess -> Guess
   removeBlackMarkers = filter (/= head colorRow)



matchingColorsWrongPlace guess colorRow
  | not (isFourElements guess) || not (isFourElements colorRow) = Nothing
  | otherwise = Just $ go2 guess colorRow [] 0
  where
    isFourElements :: [a] -> Bool
    isFourElements lst = length (take 5 lst) == 4

go2 :: Guess -> ColorRow -> [Color] -> Int -> Int
go2 _ _ _ count | count > 4 = 4  
go2 [] _ _ count = count
go2 _ [] _ count = count
go2 (g:gs) (c:cs) usedColors count
  | g == c = go2 gs cs usedColors count
  | g elem cs && g notElem usedColors = go2 gs cs (delete g usedColors) (count + 1)
  | otherwise = go2 gs cs usedColors count



countMatchingColors :: Guess -> ColorRow -> Int
countMatchingColors [] _ = 0
countMatchingColors _ [] = 0
countMatchingColors (x:xs) (y:ys)
  | x == y = 1 + countMatchingColors xs ys
  | otherwise = countMatchingColors xs ys



deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst x (y:ys)
  | x == y = ys
  | otherwise = y : deleteFirst x ys










--isListBiggerThan4 :: [a] -> Bool
--isListBiggerThan4 xs = go 0 xs
 -- where
  --  go n _ | n > 4 = True
  --  go _ [] = False
   -- go n (_:rest) = go (n + 1) rest


isListSmallerOrEqual4 :: [a] -> Bool
isListSmallerOrEqual4 xs = length xs <= 4

--isListBiggerThan4:: [a] -> Bool
--isListBiggerThan4 xs = length xs > 4





gameUpdate :: Maybe Guess -> Either String Game -> Either String Game
gameUpdate _ (Left msg) = Left msg
gameUpdate _ (Right (Game _ _ (Success _))) = Left "Ezt a jatekot mar korabban megnyerted!"
gameUpdate _ (Right (Game _ _ (Failure _))) = Left "Ezt a jatekot mar korabban elvesztetted!"
gameUpdate Nothing (Right (Game colorRow (availableGuesses, guessResps) Ongoing)) = Right (Game colorRow (availableGuesses, guessResps) (Failure "Feladtad a jatekot!"))
gameUpdate (Just guess) (Right (Game colorRow (availableGuesses, guessResps) Ongoing)) =
  case guessOnce guess colorRow availableGuesses of
    Nothing -> Left "A tippeles nem sikerult!"
    Just (remainingGuesses, resp@(Resp _ marks)) ->
      let newGuessResps = guessResps ++ [resp]
          newCompletion =
            if length marks == length colorRow && all (== Black) marks
              then Success $ length newGuessResps
              else if remainingGuesses == 0
                then Failure "Elfogytak a tippek!"
                else Ongoing
           in trace ("newGuessResps: " ++ show newGuessResps) $ Right $ Game colorRow (remainingGuesses, newGuessResps) newCompletion
       --in Right $ Game colorRow (remainingGuesses, tracedGuessResps) newCompletion





gameState :: Either String Game -> String
gameState (Right (Game _ _ (Success tips))) = "A jatekot megnyerted " ++ show tips ++ " tippbol."
gameState (Right (Game _ _ (Failure reason))) = "A jatekot elvesztetted. " ++ reason
gameState (Right (Game _ (remaining, _) Ongoing)) = "A jatek meg folyamatban van, meg " ++ show remaining ++ " tipped van hatra."
gameState (Left msg)
  | null msg = "HIBA"
  | otherwise = "HIBA: " ++ msg



stringToColor :: String -> Maybe Color
stringToColor str = case map toLower str of
  "purple" -> Just Purple
  "orange" -> Just Orange
  "pink" -> Just Pink
  "yellow" -> Just Yellow
  "red" -> Just Red
  "gray" -> Just Gray
  "darkblue" -> Just DarkBlue
  "green" -> Just Green
  "blue" -> Just Blue
  _ -> Nothing

-- Helper function to convert a string to lowercase
toLowerCase :: String -> String
toLowerCase = map toLower



stringToGuess :: String -> Maybe Guess
stringToGuess str = case splitColors str of
  Just colors -> traverse stringToColor colors
  Nothing -> Nothing

splitColors :: String -> Maybe [String]
splitColors str = case dropWhile isSpace str of
  "" -> Just []
  trimmedStr -> case spanColors trimmedStr of
    (color, rest) -> case dropWhile isSpace rest of
      ',' : rest' -> (color :) <$> splitColors rest'
      "" -> Just [color]
      _ -> Nothing

spanColors :: String -> (String, String)
spanColors = span (\c -> not (isSpace c) && c /= ',')

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace













