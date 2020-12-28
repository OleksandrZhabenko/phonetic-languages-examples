-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Prints the rearrangements with the \"property\" information for the Ukrainian language text.
-- The most interesting is the first line of the output. But other ones also are noteworthy.

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies
import Languages.UniquenessPeriods.Vector.Constraints.Encoded (EncodedCnstrs(..),decodeLConstraints,readMaybeECG)
import qualified Data.Vector as VB
import Data.Print.Info
import Languages.UniquenessPeriods.Vector.General.Simplified
import Languages.UniquenessPeriods.Vector.General.DebugG hiding (maximumElBy)
import Languages.UniquenessPeriods.Vector.AuxiliaryG
import Languages.UniquenessPeriods.Vector.PropertiesG
import Languages.UniquenessPeriods.Vector.PropertiesFuncRepG
import Languages.UniquenessPeriods.Vector.PropertiesSyllablesG
import Languages.UniquenessPeriods.Vector.Filters
import Languages.UniquenessPeriods.Vector.StrictVG
import Languages.Phonetic.Ukrainian.PrepareText
import Languages.UniquenessPeriods.Vector.DataG
import Data.Char (isDigit,isAlpha)
import Melodics.ByteString.Ukrainian (isUkrainianL)
import qualified Data.List  as L (span,sort,permutations)
import Languages.UniquenessPeriods.Vector.FuncRepRelatedG
import Data.SubG hiding (takeWhile,dropWhile)
import System.Environment
import Data.Maybe
import Text.Read (readMaybe)


-- | Prints the rearrangements with the \"property\" information for the Ukrainian language text. The first command line argument must be a
-- positive 'Int' number and is a number of printed variants for the line (if they are present, otherwise just all possible variants are printed).
-- The second one is the number of the intervals into which the all range of possible metrics values are divided. The next numeric arguments that must be
-- sequenced without interruptions further are treated as the numbers of the intervals (counting is started from 1) which values are moved to the maximum
-- values of the metrics interval using the 'unsafeSwapVecIWithMaxI' function. The first textual command line argument should be in the form either \"y0\",
-- or \"0y\", or \"yy\", or \"y\" and specifies, which property or properties is or are evaluated.
-- The rest of the command line arguments is the Ukrainian text.
--
-- The most interesting is the first line of the output. But other ones also are noteworthy.
--
-- You can specify constraints according to the 'decodeLConstraints' function between +A and -A command line arguments. If so, the program will
-- ask you additional question before proceeding.
main :: IO ()
main = do
 args0 <- getArgs
 let args = takeWhile (/= "+A") args0 `mappend` (drop 1 . dropWhile (/= "-A") $ args0)
     coeffs = readCF . concat . take 1 $ args -- The first command line argument. If not sure, just pass \"1_\".
 if isPair coeffs then generalProc2 args0 coeffs (drop 1 args)
 else generalProc2 args0 coeffs args


generalProc2 :: [String] -> Coeffs2 -> [String] -> IO ()
generalProc2 args0 coeffs args = do
  let (!numericArgs,!textualArgs) = L.span (all isDigit) $ args
      !xs = concat . take 1 . fLines . unwords . drop 1 $ textualArgs
      !l = length . words $ xs
      argCs = catMaybes (fmap (readMaybeECG l) . drop 1 . dropWhile (/= "+A") . takeWhile (/= "-A") $ args0)
      !arg0 = fromMaybe 1 $ (readMaybe (concat . take 1 $ numericArgs)::Maybe Int)
      !numberI = fromMaybe 1 $ (readMaybe (concat . drop 1 . take 2 $ numericArgs)::Maybe Int)
      !choice = concat . take 1 $ textualArgs
      !whspss = VB.fromList " 01-"
      !v = VB.fromList xs
      !subs = subG whspss v
  if null argCs then let !perms = genPermutations l in generalProc1 coeffs numericArgs arg0 numberI choice l perms subs v
  else do
   putStr "Please, check whether the line below corresponds and is consistent with the data you have specified between the +A and -A options. "
   putStrLn "If it is inconsistent then enter further \"n\", press Enter and then run the program again with better arguments. "
   putStrLn "If the line is consistent with your input between +A and -A then just press Enter to proceed further. "
   putStrLn xs
   correct <- getLine
   if correct == "n" then putStrLn "You stopped the program, please, if needed, run it again with better arguments. "
   else let !perms = decodeLConstraints argCs . genPermutations $ l in generalProc1 coeffs numericArgs arg0 numberI choice l perms subs v


generalProc1 :: Coeffs2 -> [String] -> Int -> Int -> String -> Int -> VB.Vector (VB.Vector Int) -> VB.Vector (VB.Vector Char) -> VB.Vector Char -> IO ()
generalProc1 coeffs numericArgs arg0 numberI choice l perms subs v = do
  if compare numberI 2 == LT then printUniquenessG1VChar (I1 H) . fst . get22 . uniqNProperties2GN ' ' (VB.fromList " 01-") id id id perms K arg0 1 (VB.singleton (oneProperty))
    (chooseMax coeffs choice) $ v
  else do
    let !intervalNmbrs = (\zs -> if null zs then VB.singleton numberI else VB.uniq . VB.fromList $ zs) . L.sort . filter (<= numberI) . map (\t -> fromMaybe numberI $ (readMaybe t::Maybe Int)) . drop 2 $ numericArgs
        (!maxE,!minE) = runEval ((parTuple2 rpar rpar) ((\k -> if k == 0.0 then 1.0 else k) . (\rs -> if null rs then 0.0 else head rs) . firstFrom3 .
          maximumElBy 1 (VB.singleton (oneProperty)) . uniquenessVariants2GNB ' ' id id id perms (VB.singleton (oneProperty)) (chooseMax coeffs choice) $
           subs, (\k -> if k == 0.0 then 1.0 else k) . abs . (\rs -> if null rs then 0.0 else head rs) . firstFrom3 .
             maximumElBy 1 (VB.singleton (oneProperty)) . uniquenessVariants2GNB ' ' id id id perms (VB.singleton (oneProperty))
               (chooseMin coeffs choice) $ subs))
    printUniquenessG1VChar (I1 H) . fst . get22 . uniqNProperties2GN ' ' (VB.fromList " 01-") id id id perms K arg0 1 (VB.singleton (unsafeSwapVecIWithMaxI minE maxE numberI intervalNmbrs .
      oneProperty)) (chooseMax coeffs choice) $ v


fLines :: String -> [String]
fLines ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in g preText wss
