-- |
-- Module      :  Languages.UniquenessPeriods.Vector.FuncRepRelatedG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions to choose from the 'FuncRep' variants.

{-# LANGUAGE BangPatterns #-}

module Languages.UniquenessPeriods.Vector.FuncRepRelatedG where

import Data.Maybe (isNothing,fromMaybe)
import Text.Read
import CaseBi (getBFst')
import qualified Data.Vector as VB
import Languages.UniquenessPeriods.Vector.DataG
import String.Languages.UniquenessPeriods.VectorG
import Languages.UniquenessPeriods.Vector.PropertiesFuncRepG
import Languages.UniquenessPeriods.Vector.PropertiesSyllablesG
import Languages.UniquenessPeriods.Vector.PropertiesG

-- | Allows to choose the variant of the computations in case of usual processment.
chooseMax :: Coeffs2 -> String -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
chooseMax coeffs choice
 | isPair coeffs =
     let !k2 = fromMaybe 1.0 . fstCF $ coeffs
         !k3 = fromMaybe 1.0 . sndCF $ coeffs in getBFst' (procBoth2InvF coeffs, VB.fromList [("02y",procRhythmicity232F "02y" coeffs),
            ("0y",procRhythmicity23F "0y" coeffs),("y",procBothF coeffs),("y0",procDiverse2F),("y2",procBoth2F coeffs),("yy",procBothInvF coeffs)]) choice
 | otherwise = getBFst' (procBoth2InvF coeffs, VB.fromList [("02y",procRhythmicity232F "02y" coeffs),("0y",procRhythmicity23F "0y" coeffs),
     ("y",procBothF coeffs),("y0",procDiverse2F),("y2",procBoth2F coeffs),("yy",procBothInvF coeffs)]) choice

-- | Allows to choose the variant of the computations in case of minimum lookup. Uses @-neg@ variants.
chooseMin :: Coeffs2 -> String -> FuncRep (VB.Vector Char) (UniquenessGeneral2 Char) [Float]
chooseMin coeffs choice
 | isPair coeffs =
     let !k2 = fromMaybe 1.0 . fstCF $ coeffs
         !k3 = fromMaybe 1.0 . sndCF $ coeffs in getBFst' (procBoth2InvFneg coeffs, VB.fromList [("02y",procRhythmicity232Fneg "02y" coeffs),
            ("0y",procRhythmicity23Fneg "0y" coeffs),("y",procBothFneg coeffs),("y0",procDiverse2Fneg),("y2",procBoth2Fneg coeffs),
              ("yy",procBothInvFneg coeffs)]) choice
 | otherwise = getBFst' (procBoth2InvFneg coeffs, VB.fromList [("02y",procRhythmicity232Fneg "02y" coeffs),("0y",procRhythmicity23Fneg "0y" coeffs),
     ("y",procBothFneg coeffs),("y0",procDiverse2Fneg),("y2",procBoth2Fneg coeffs),("yy",procBothInvFneg coeffs)]) choice

-- | Allows to choose precision in the Numeric.showFFloat function being given a choice parameter.
precChoice :: String -> Maybe Int
precChoice = getBFst' (Just 4, VB.fromList [("02y",Just 0),("0y",Just 0),("y",Just 0),("y0",Just 0),("y2",Just 0)])
