-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Inspired by: https://functional-art.org/2020/papers/Poetry-OleksandrZhabenko.pdf from the https://functional-art.org/2020/performances ;
-- Allows to rewrite the given text (usually a poetical one).

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Main where

import Data.SubG
import Control.Parallel.Strategies
import qualified Data.Vector as VB
import Data.List (sort)
import Languages.UniquenessPeriods.Vector.PropertiesG
import Languages.UniquenessPeriods.Vector.PropertiesFuncRepG
import Languages.UniquenessPeriods.Vector.PropertiesSyllablesG
import Languages.UniquenessPeriods.Vector.General.Simplified
import Languages.UniquenessPeriods.Vector.General.DebugG hiding (maximumElBy)
import Languages.UniquenessPeriods.Vector.StrictVG
import Languages.UniquenessPeriods.Vector.Filters (unsafeSwapVecIWithMaxI)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Melodics.ByteString.Ukrainian (isUkrainianL)
import System.Environment
import Languages.Phonetic.Ukrainian.PrepareText
import Languages.UniquenessPeriods.Vector.DataG
import Languages.UniquenessPeriods.Vector.AuxiliaryG
import Data.Char (isDigit,isAlpha)
import Languages.UniquenessPeriods.Vector.FuncRepRelatedG
import Data.Monoid (mappend)

-- | The function allows to rewrite the Ukrainian text in the file given as the first command line argument to a new file. In between, it is rewritten
-- so that every last word on the lines is preserved at its position, and the rest of the line is rearranged using the specified other command line
-- arguments. They are general for the whole program. The first command line argument is a FilePath to the file with a Ukrainian text to be rewritten.
-- The second one is a variant of the metrics (\"properties\") used to evaluate the variants.
-- The further command line arguments are: the number of the intervals and the numbers of the intervals
-- that are swapped with the maximum one so that they are available for further usage by the program. See documentation for @uniqueness-periods-vector-filters@
-- package
-- 'https://hackage.haskell.org/package/uniqueness-periods-vector-filters'
--
main :: IO ()
main = do
 args <- getArgs
 let coeffs = readCF . concat . take 1 $ args -- The first command line argument. If not sure, pass just \"1_\".
 if isPair coeffs then do
  let !numericArgs = filter (all isDigit) . drop 3 $ args
      !choice = concat . drop 2 . take 3 $ args
      !numberI = fromMaybe 1 (readMaybe (concat . take 1 $ numericArgs)::Maybe Int)
      !file = concat . drop 1 . take 2 $ args
  generalProcessment coeffs numericArgs choice numberI file
 else do
  let !numericArgs = filter (all isDigit) . drop 2 $ args
      !choice = concat . drop 1 . take 2 $ args
      !numberI = fromMaybe 1 (readMaybe (concat . take 1 $ numericArgs)::Maybe Int)
      !file = concat . take 1 $ args
  generalProcessment coeffs numericArgs choice numberI file

generalProcessment :: Coeffs2 -> [String] -> String -> Int -> FilePath -> IO ()
generalProcessment coeffs numericArgs choice numberI file = do
  let !permsV = VB.force genPermutationsV
  contents <- readFile file
  let !flines = fLines contents
      !lasts = map (\ts -> if null . words $ ts then [] else last . words $ ts) flines
      !l = (subtract 3) . length . words . concat . take 1 $ flines
      !whspss = VB.fromList " 01-"
  if compare numberI 2 == LT then do
    let !zs = if compare l 0 == LT then concat . take 1 $ flines else VB.toList . lastFrom3 . headU2 . fst . get22 .
               uniqNProperties2GN ' ' whspss id id id (VB.unsafeIndex permsV l) (PA VB.empty (VB.fromList . concat . take 1 $ lasts)) 1 1 (VB.singleton oneProperty) (chooseMax coeffs choice) . VB.fromList . unwords . init . words . concat . take 1 $ flines
    toFileStr (file ++ ".new.txt") (zs:(noDoubleWords . circle2 coeffs permsV (concat . take 1 $ lasts) choice [] . drop 1 $ flines))
  else do
    let !intervalNmbrs = (\vs -> if null vs then VB.singleton numberI else VB.uniq . VB.fromList $ vs) . sort . filter (<= numberI) .
           map (\t -> fromMaybe numberI (readMaybe t::Maybe Int)) . drop 2 $ numericArgs
        !us = words . concat . take 1 $ flines
        !l2 = (subtract 3) . length $ us
    if compare l2 0 /= LT then do
      let !perms2 = VB.force . VB.unsafeIndex permsV $ l2
          !v = if compare l2 0 == LT then VB.empty else VB.fromList . unwords . init $ us
          !v2 = subG whspss v
          (!maxE,!minE) = runEval (parTuple2 rpar rpar ((\k -> if k == 0.0 then 1.0 else k) . (\rs -> if null rs then 0.0 else head rs) . firstFrom3 .
            maximumElBy 1 (VB.singleton oneProperty) . uniquenessVariants2GNB ' ' id id id perms2 (VB.singleton oneProperty) (chooseMax coeffs choice) $
              v2, (\k -> if k == 0.0 then 1.0 else k) . abs . (\rs -> if null rs then 0.0 else head rs) . firstFrom3 .
               maximumElBy 1 (VB.singleton oneProperty) . uniquenessVariants2GNB ' ' id id id perms2 (VB.singleton oneProperty) (chooseMin coeffs choice) $
                 v2))
          !zs = if compare l2 0 == LT then concat . take 1 $ flines else VB.toList . lastFrom3 . headU2 . fst . get22 .
            uniqNProperties2GN ' ' whspss id id id perms2 (PA VB.empty (VB.fromList . concat . take 1 $ lasts)) 1 1 (VB.singleton (unsafeSwapVecIWithMaxI minE maxE numberI intervalNmbrs . oneProperty)) (chooseMax coeffs choice) $ v
      toFile (file ++ ".new.txt") (zs:(noDoubleWords . circle2I coeffs permsV whspss (concat . take 1 $ lasts) choice [] numberI intervalNmbrs minE maxE . drop 1 $ flines))
    else toFileStr (file ++ ".new.txt") ((concat . take 1 $ flines):(noDoubleWords . circle2I coeffs permsV whspss (concat . take 1 $ lasts) choice [] numberI intervalNmbrs 0.0 0.0 . drop 1 $ flines))

fLines :: String -> [String]
fLines ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in g preText wss

-- | Processment without rearrangements.
circle2 :: Coeffs2 -> VB.Vector (VB.Vector (VB.Vector Int)) -> String -> String -> [String] -> [String] -> [String]
circle2 coeffs permsG1 xs choice yss xss
 | null xss = yss
 | otherwise = circle2 coeffs permsG1 (if null rs then [] else last rs) choice (yss `mappend` [ws]) tss
      where (!zss,!tss) = splitAt 1 xss
            !rs = words . concat $ zss
            !ws = if compare (length rs) 3 == LT then unwords (xs:rs) else VB.toList . lastFrom3 . headU2 . fst . get22 .
                    uniqNProperties2GN ' ' (VB.fromList " 01-") id id id (VB.unsafeIndex permsG1 $ length rs - 3) (PA (VB.fromList xs) (VB.fromList . last $ rs)) 1 1 (VB.singleton oneProperty) (chooseMax coeffs choice) . VB.fromList . unwords . init $ rs

-- | Processment with rearrangements.
circle2I :: Coeffs2 -> VB.Vector (VB.Vector (VB.Vector Int)) -> VB.Vector Char -> String -> String -> [String] -> Int -> VB.Vector Int -> Float -> Float -> [String] -> [String]
circle2I coeffs permsG1 whspss xs choice yss numberI vI minE maxE xss
 | null xss = yss
 | otherwise = circle2I coeffs permsG1 whspss (if null rs then [] else last rs) choice (yss `mappend` [ws]) numberI vI minE1 maxE1 tss
      where (!zss,!tss) = splitAt 1 xss
            !w2s = words . concat . take 1 $ tss
            !l3 = (subtract 3) . length $ w2s
            !rs = words . concat $ zss
            !ws = if compare (length rs) 3 == LT then unwords (xs:rs) else VB.toList . lastFrom3 . headU2 . fst . get22 .
                    uniqNProperties2GN ' ' (VB.fromList " 01-") id id id (VB.unsafeIndex permsG1 $ length rs - 3) (PA (VB.fromList xs) (VB.fromList . last $ rs)) 1 1 (VB.singleton (unsafeSwapVecIWithMaxI minE maxE numberI vI .
                      oneProperty)) (chooseMax coeffs choice) . VB.fromList . unwords . init $ rs
            (!maxE1,!minE1)
             | compare l3 0 /= LT =
              let !perms3 = VB.force . VB.unsafeIndex permsG1 $ l3
                  !v3 = VB.fromList . unwords . init $ w2s
                  !v4 = subG whspss v3 in runEval (parTuple2 rpar rpar ((\k -> if k == 0.0 then 1.0 else k) . (\ls -> if null ls then 0.0 else head ls) .
                         firstFrom3 . maximumElBy 1 (VB.singleton oneProperty) .
                           uniquenessVariants2GNB ' ' id id id perms3 (VB.singleton oneProperty) (chooseMax coeffs choice) $ v4,
                             (\k -> if k == 0.0 then 1.0 else k) . abs . (\ls -> if null ls then 0.0 else head ls) . firstFrom3 .
                               maximumElBy 1 (VB.singleton oneProperty) .
                                 uniquenessVariants2GNB ' ' id id id perms3 (VB.singleton oneProperty) (chooseMin coeffs choice) $ v4))
             | otherwise = (0.0,0.0)

headU2
  :: (Foldable t, Foldable t2, InsertLeft t a, Monoid (t a), Monoid (t (t a)), Monoid (t2 b)) => VB.Vector (UniquenessG1T2 t t2 a b)
  -> UniquenessG1T2 t t2 a b
headU2 v
 | VB.null v = (mempty, VB.empty,mempty)
 | otherwise = VB.unsafeIndex v 0
{-# INLINE headU2 #-}

noDoubleWords :: [String] -> [String]
noDoubleWords = map (unwords . drop 1 . words)
{-# INLINE noDoubleWords #-}
