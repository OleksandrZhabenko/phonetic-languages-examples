-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Analyzes a poetic text in Ukrainian, for every line prints statistic data and
-- then for the whole poem prints the hypothesis evaluation information.
--
-- To enable parallel computations (potentially, they can speed up the work), please, run the @propertiesText@ executable with
-- @+RTS -threaded -RTS@ command line options with possibly @-N@ option inside.
--

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Main where


import Data.SubG
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Parallel.Strategies
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Text.Read (readMaybe)
import qualified Data.Vector as VB
import Languages.UniquenessPeriods.Vector.General.Simplified
import Languages.UniquenessPeriods.Vector.General.DebugG hiding (newLineEnding,maximumElBy)
import Languages.UniquenessPeriods.Vector.PropertiesG
import Languages.UniquenessPeriods.Vector.PropertiesFuncRepG
import Languages.UniquenessPeriods.Vector.PropertiesSyllablesG
import Melodics.ByteString.Ukrainian
import System.Environment
import Languages.Phonetic.Ukrainian.PrepareText
import Languages.UniquenessPeriods.Vector.DataG
import Languages.UniquenessPeriods.Vector.AuxiliaryG
import Languages.UniquenessPeriods.Vector.StrictVG
import Numeric (showFFloat)
import Languages.UniquenessPeriods.Vector.Filters
import Data.Char (isAlpha)
import Data.Statistics.RulesIntervals
import Languages.UniquenessPeriods.Vector.FuncRepRelatedG


main :: IO ()
main = do
 args0 <- getArgs
 let args = filter (\xs -> all (/= ':') xs && all (/= '@') xs) args0
     !coeffs = readCF . concat . take 1 $ args -- The first command line argument. If not sure, just enter \"1_\".
     !lInes = filter (any (== ':')) args0
     !numbersJustPrint =  filter (== "@n") args0
 if isPair coeffs then do
  let !file = concat . drop 1 . take 2 $ args  -- The second command line argument except those ones that are RTS arguments
  if null numbersJustPrint then do
   let !gzS = concat . take 1 . drop 2 $ args -- The third command line argument that controls the choice of the number of intervals
       !printLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 3 $ args)::(Maybe Int)) -- The fourth command line argument except those ones that are  RTS arguments. Set to 1 if you would like to print the current line within the information
       !toOneLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 4 $ args)::(Maybe Int)) -- The fifth command line argument except those ones that are RTS arguments. Set to 1 if you would like to convert the text into one single line before applying to it the processment (it can be more conceptually consistent in such a case)
       !choice = concat . drop 5 . take 6 $ args -- The sixth command line argument that controls what properties are used.
   generalProc lInes coeffs file gzS printLine toOneLine choice
  else do
   contents <- readFile file
   fLinesIO contents
 else do
  let !file = concat . take 1 $ args
  if null numbersJustPrint then do
   let !gzS = concat . take 1 . drop 1 $ args
       !printLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 2 $ args)::(Maybe Int))
       !toOneLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 3 $ args)::(Maybe Int))
       !choice = concat . drop 4 . take 5 $ args
   generalProc lInes coeffs file gzS printLine toOneLine choice
  else do
   contents <- readFile file
   fLinesIO contents

generalProc :: [String] -> Coeffs2 -> FilePath -> String -> Int -> Int -> String -> IO ()
generalProc lInes coeffs file gzS printLine toOneLine choice
 | null lInes = do
    contents <- readFile file
    let !flines = fLines toOneLine contents
    getData3 coeffs (getIntervalsN gzS flines) printLine choice flines
 | otherwise = do
    contents <- readFile file
    let flines = fLines toOneLine . unlines . linesFromArgsG lInes . map VB.toList . fLines 0 $ contents
    getData3 coeffs (getIntervalsN gzS flines) printLine choice flines

linesFromArgs1 :: Int -> String -> [String] -> [String]
linesFromArgs1 n xs yss =
  let (!ys,!zs) = (\(x,z) -> (x, drop 1 z)) . break (== ':') $ xs
      !ts = sort . map (min n . abs) $ [fromMaybe 1 (readMaybe ys::Maybe Int), fromMaybe n (readMaybe zs::Maybe Int)] in
        drop (head ts - 1) . take (last ts) $ yss

linesFromArgsG :: [String] -> [String] -> [String]
linesFromArgsG xss yss = let n = length yss in concatMap (\ts -> linesFromArgs1 n ts yss) xss

getIntervalsN :: String -> [VB.Vector Char] -> Int
getIntervalsN xs ys
  | xs == "s" = sturgesH (length ys)
  | xs == "l" = levynskyiMod (length ys)
  | otherwise = fromMaybe 9 (readMaybe xs::(Maybe Int))
{-# INLINE getIntervalsN #-}

getData3 :: Coeffs2 -> Int -> Int -> String -> [VB.Vector Char] -> IO ()
getData3 coeffs gz printLine choice zs = let !permsV4 = genPermutationsV in mapM_ (process1Line coeffs gz printLine choice permsV4) zs

process1Line :: Coeffs2 -> Int -> Int -> String -> VB.Vector (VB.Vector (VB.Vector Int)) -> VB.Vector Char -> IO ()
process1Line coeffs gz printLine choice !permsV5 v = bracket (do
 myThread <- forkIO (do
   let !whspss = VB.fromList " 01-"
       !v2 = subG whspss v
       !l2 = (subtract 2) . VB.length $ v2
       (!maxE,!minE,!data2)
          | compare l2 0 /= LT = runEval (parTuple3 rpar rpar rpar ((\k -> if k == 0.0 then 1.0 else k) . (\ls -> if null ls then 0.0 else head ls) .
              firstFrom3 . maximumElBy 1 (VB.singleton oneProperty) .
                uniquenessVariants2GNB ' ' id id id (VB.unsafeIndex permsV5 l2) (VB.singleton oneProperty) (chooseMax coeffs choice) $ v2,
                  (\k -> if k == 0.0 then 1.0 else k) . abs . (\ls -> if null ls then 0.0 else head ls) .
                     firstFrom3 . maximumElBy 1 (VB.singleton oneProperty) .
                       uniquenessVariants2GNB ' ' id id id (VB.unsafeIndex permsV5 l2) (VB.singleton oneProperty) (chooseMin coeffs choice) $ v2,
                         (\k -> if k == 0.0 then 1.0 else k) . head . getAC (chooseMax coeffs choice) $ v))
          | otherwise = let !mono = (\k -> if k == 0.0 then 1.0 else k) . head . getAC (chooseMax coeffs choice) $ v in (mono,mono,mono)
       (!wordsN,!intervalN)
         | maxE == 1.0 = (0, 0)
         | otherwise = runEval (parTuple2 rpar rpar (l2 + 2, intervalNRealFrac minE maxE gz data2))
       !ratio = if maxE == 1.0 then 0.0 else 2.0 * data2 / (minE + maxE) in do
         hPutStr stdout . showFFloat (precChoice choice) minE $ "\t"
         hPutStr stdout . showFFloat (precChoice choice) data2 $ "\t"
         hPutStr stdout . showFFloat (precChoice choice) maxE $ "\t"
         hPutStr stdout . showFFloat (Just 4) (data2 / minE) $ "\t"
         hPutStr stdout . showFFloat (Just 4) (maxE / minE) $ "\t"
         hPutStr stdout . showFFloat (Just 4) (maxE / data2) $ "\t"
         hPutStr stdout . showFFloat Nothing ratio $ "\t"
         hPutStr stdout ('\t':show (wordsN::Int))
         hPutStr stdout ('\t':show (intervalN::Int))
         hPutStrLn stdout (if printLine == 1 then '\t':(VB.toList v) else ""))
 return myThread) (killThread) (\_ -> putStr "")

fLines :: Int -> String -> [VB.Vector Char]
fLines !toOneLine ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText . (\z -> if toOneLine == 1 then unwords . words $ z else z) $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in map VB.fromList . g preText $ wss

fLinesIO :: String -> IO ()
fLinesIO ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in VB.mapM_ putStrLn . VB.map (\(i,x) -> show (i + 1) ++ "\t" ++ x) . VB.indexed . VB.fromList . g preText $ wss
