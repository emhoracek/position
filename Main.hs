module Main where 

import Data.Char (toLower, isAlpha, toTitle)
import Data.List (intercalate)
import System.Random (randomRIO)
import Control.Monad (replicateM, unless)
import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents, removeFile)
import System.FilePath (takeExtensions)

type Word = String
type Sentence = String

main :: IO ()
main = do
  files <- getDirectoryContents "."
  let textFiles = filter (\x -> ".txt" == takeExtensions x) files
  unless (null textFiles) $ do
    let file = head textFiles
    garblage <- garble file
    putStrLn garblage
    removeFile file        
  main

garble :: FilePath -> IO String
garble file = do
  f <- readFile file
  let randomWords = replicateM 10 (randomElem (justWords f))
  let sentence = fmap formatSentence randomWords
  sentences <- replicateM 10 sentence
  return $ formatParagraph sentences

justWords :: String -> [Word]
justWords text = map (map toLower . filter isAlpha) (words text)

randomElem :: [a] -> IO a
randomElem xs = (xs !!) <$> randomRIO (0, length xs - 1)

formatSentence :: [Word] -> Sentence
formatSentence xs = unwords capitalized ++ "."
  where firstWord = firstLetter : tail (head xs)
        firstLetter = toTitle (head $ head xs)
        capitalized = firstWord : tail xs

formatParagraph :: [Sentence] -> String
formatParagraph xs = intercalate "\n" (toLines $ concatMap words xs)

toLines :: [Word] -> [Sentence]
toLines [] = []
toLines ws = firstLine : toLines rest
  where 
    firstWords = filter (\(_,n) -> n < 80) (withCount ws)
    firstLine = unwords $ map fst firstWords
    rest = map fst $ filter (\(_,n) -> n >= 80 ) (withCount ws)

-- This creates a list of tuples that counts how long a line is by adding 
-- the length of the current word plus one to all the previous length plus 
-- ones. You have to add the one to account for spaces, which is kinda 
-- terrible?? Also I don't know that fold is the right tool here.
withCount :: [Word] -> [(Word, Int)]
withCount xs = tail $ foldl f [("", 0)] xs
  where f acc x = acc ++ [(x, length x + snd (last acc) + 1)]

