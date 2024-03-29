{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      : Discord.BenjiBot.Plugins.Roll.Dice.DiceStatsBase
-- Description : The basics for dice stats
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The basics for dice stats. Functions for creating and manipulating
-- `Distribution`s.
module Discord.BenjiBot.Plugins.Roll.Dice.DiceStatsBase
  ( Distribution,
    distributionByteString,
  )
where

import Codec.Picture (PngSavable (encodePng))
import Control.Monad.Exception (MonadException)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as B
import qualified Data.Distribution as D
import Data.List (genericLength)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Diagrams (Diagram, dims2D, renderDia)
import Diagrams.Backend.Rasterific
import Graphics.Rendering.Chart.Axis.Int
import Graphics.Rendering.Chart.Backend.Diagrams (runBackendR, DEnv, createEnv)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.Rendering.Chart.Easy
import Discord.BenjiBot.Plugins.Roll.Dice.DiceEval (evaluationException)
import Discord.BenjiBot.Utility.Font (FontMap)
import qualified Graphics.SVGFonts.ReadFont as F

-- | A wrapper type for mapping values to their probabilities.
type Distribution = D.Distribution Integer

-- | Default x and y values for the output chart.
diagramX, diagramY :: Double
(diagramX, diagramY) = (1400.0, 400.0)

-- | Get the ByteString representation of the given distribution, setting the
-- string as its title.
distributionByteString :: MonadException m => FontMap Double -> [(Distribution, T.Text)] -> m B.ByteString
distributionByteString fontMap d = encodePng . renderDia Rasterific opts <$> distributionDiagram fontMap d
  where
    opts = RasterificOptions (dims2D diagramX diagramY)

-- | Get the Diagram representation of the given distribution, setting the
-- string as its title.
distributionDiagram :: MonadException m => FontMap Double -> [(Distribution, T.Text)] -> m (Diagram B)
distributionDiagram fontMap d = do
  if null d
    then evaluationException "empty distribution" []
    else return . fst $ runBackendR defEnv r
  where
    r = distributionRenderable d
    defEnv = makeSansSerifEnv diagramX diagramY

    makeSansSerifEnv :: Double -> Double -> DEnv Double
    makeSansSerifEnv diX diY = createEnv (AlignmentFns id id) diX diY fontSelector
      where
        alterFontFamily :: String -> F.PreparedFont n -> F.PreparedFont n
        alterFontFamily n (fd, om) = (fd {F.fontDataFamily = n}, om)
        localSansSerif = M.filterWithKey (\(k, _, _) _ -> k == "sans-serif") fontMap
        localAltered = M.mapWithKey (\(s, _, _) v -> alterFontFamily s v) localSansSerif
        -- we simplify the map so that other font types become sans-serif as well
        localKeySimple = M.mapKeys (\(_, fs, fw) -> (fs, fw)) localAltered
        -- we use an unsafe lookup method because what do we do if this isn't correct?
        fontSelector :: FontStyle -> F.PreparedFont Double
        fontSelector FontStyle {..} = localKeySimple M.! (_font_slant, _font_weight)

-- | Get the Renderable representation of the given distribution, setting the
-- string as its title.
distributionRenderable :: [(Distribution, T.Text)] -> Renderable ()
distributionRenderable d = toRenderable $ do
  layout_title .= T.unpack (title' d)
  layout_x_axis . laxis_title .= "value"
  layout_y_axis . laxis_title .= "probability (%)"
  layout_x_axis . laxis_generate .= scaledIntAxis' r
  layout_y_axis . laxis_override .= \ad@AxisData {_axis_labels = axisLabels} -> ad {_axis_labels = (second (\s -> if '.' `elem` s then s else s ++ ".0") <$>) <$> axisLabels}
  layout_all_font_styles .= defFontStyle
  pb <- (bars @Integer @Double) (barNames d) pts
  let pb' = set plot_bars_spacing (BarsFixGap 10 5) pb
  plot $ return $ plotBars pb'
  where
    removeNullMap m
      | M.null m = M.singleton 0 0
      | otherwise = m
    ds = removeNullMap . D.toMap . fst <$> d
    allIntegers = let s = S.unions $ M.keysSet <$> ds in [S.findMin s .. S.findMax s]
    insertEmpty k = M.insertWith (\_ a -> a) k 0
    ds' = M.unionsWith (++) $ M.map (: []) <$> (applyAll (insertEmpty <$> allIntegers) <$> ds)
    pts = second (fromRational . (* 100) <$>) <$> M.toList ds'
    r = (fst $ M.findMin ds', fst $ M.findMax ds')
    applyAll [] = id
    applyAll (f : fs) = f . applyAll fs
    defFontStyle = def {_font_size = 2 * _font_size def}
    barNames [_] = [""]
    barNames xs = T.unpack . snd <$> xs
    title' [(_, t)] = t
    title' xs = "Range of " <> T.intercalate ", " (snd <$> xs)

-- | Custom scaling function due to some difficulties for drawing charts.
--
-- Using
-- https://hackage.haskell.org/package/Chart-1.9.3/docs/src/Graphics.Rendering.Chart.Axis.Int.html#scaledIntAxis
-- for pointers.
scaledIntAxis' :: (Integer, Integer) -> AxisFn Integer
scaledIntAxis' r@(minI, maxI) _ = makeAxis (_la_labelf lap) ((minI - 1) : (maxI + 1) : labelvs, tickvs, gridvs)
  where
    lap = defaultIntAxis
    labelvs = stepsInt' (fromIntegral $ _la_nLabels lap) r
    tickvs =
      stepsInt'
        (fromIntegral $ _la_nTicks lap)
        ( fromIntegral $ minimum labelvs,
          fromIntegral $ maximum labelvs
        )
    gridvs = labelvs

-- | Taken and modified from
-- https://hackage.haskell.org/package/Chart-1.9.3/docs/src/Graphics.Rendering.Chart.Axis.Int.html#stepsInt
stepsInt' :: Integer -> (Integer, Integer) -> [Integer]
stepsInt' nSteps (minV, maxV) = bestSize (goodness alt0) alt0 alts
  where
    bestSize n a (a' :|| as) =
      let n' = goodness a'
       in if n' < n then bestSize n' a' as else a

    goodness vs = abs (genericLength vs - nSteps)

    (alt0 :|| alts) = fmap steps sampleSteps'

    -- throw away sampleSteps that are definitely too small as
    -- they takes a long time to process
    sampleSteps' =
      let rangeMag = maxV - minV
          (s1, s2) = spanStream (< (rangeMag `div` nSteps)) sampleSteps
       in mkStreamWith s2 ((reverse . take 5 . reverse) s1)

    -- generate all possible step sizes
    sampleSteps = mkStreamWith sampleSteps1 [1, 2, 5]
    sampleSteps1 = flip mkStreamWith [10, 20, 25, 50] $ fmap (* 10) sampleSteps1

    steps :: Integer -> [Integer]
    steps size' = takeWhile (< b) [a, a + size' ..] ++ [b]
      where
        a = floor @Double (fromIntegral minV / fromIntegral size') * size'
        b = ceiling @Double (fromIntegral maxV / fromIntegral size') * size'

data Stream a = a :|| (Stream a) deriving (Functor)

mkStreamWith :: Stream a -> [a] -> Stream a
mkStreamWith = foldr (:||)

spanStream :: (a -> Bool) -> Stream a -> ([a], Stream a)
spanStream p (a :|| as)
  | p a = first (a:) $ spanStream p as
  | otherwise = ([], a :|| as)
