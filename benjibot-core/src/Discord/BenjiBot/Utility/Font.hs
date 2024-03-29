{-# LANGUAGE ScopedTypeVariables #-}

module Discord.BenjiBot.Utility.Font (FontMap, makeFontMap) where

import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as M
import Graphics.Rendering.Chart.Backend.Types
import Graphics.SVGFonts (loadFont)
import qualified Graphics.SVGFonts.ReadFont as F
import System.Environment (lookupEnv)
import System.FilePath (replaceFileName)

-- | A type to map between some basic font characteristics and some loaded fonts.
type FontMap n = M.Map (String, FontSlant, FontWeight) (F.PreparedFont n)

makeFontMap :: (Read n, RealFloat n, MonadIO m, MonadException m) => m (FontMap n)
makeFontMap = do
  exec <- liftIO $ lookupEnv "FONT_PATH"
  case exec of
    Nothing -> liftIO $ putStrLn "could not find env var FONT_PATH" >> pure M.empty
    Just exec' -> do
      let local = localFonts exec'
      mapM (liftIO . loadFont) local

-- thanks to https://stackoverflow.com/questions/21549082/how-do-i-deploy-an-executable-using-chart-diagrams-standard-fonts-without-cabal
localFonts :: FilePath -> M.Map (String, FontSlant, FontWeight) FilePath
localFonts exec =
  let serifR = replaceFileName exec "fonts/LinLibertine_R.svg"
      serifRB = replaceFileName exec "fonts/LinLibertine_RB.svg"
      serifRBI = replaceFileName exec "fonts/LinLibertine_RBI.svg"
      serifRI = replaceFileName exec "fonts/LinLibertine_RI.svg"
      sansR = replaceFileName exec "fonts/SourceSansPro_R.svg"
      sansRB = replaceFileName exec "fonts/SourceSansPro_RB.svg"
      sansRBI = replaceFileName exec "fonts/SourceSansPro_RBI.svg"
      sansRI = replaceFileName exec "fonts/SourceSansPro_RI.svg"
      monoR = replaceFileName exec "fonts/SourceCodePro_R.svg"
      monoRB = replaceFileName exec "fonts/SourceCodePro_RB.svg"
   in M.fromList
        [ (("serif", FontSlantNormal, FontWeightNormal), serifR),
          (("serif", FontSlantNormal, FontWeightBold), serifRB),
          (("serif", FontSlantItalic, FontWeightNormal), serifRI),
          (("serif", FontSlantOblique, FontWeightNormal), serifRI),
          (("serif", FontSlantItalic, FontWeightBold), serifRBI),
          (("serif", FontSlantOblique, FontWeightBold), serifRBI),
          (("sans-serif", FontSlantNormal, FontWeightNormal), sansR),
          (("sans-serif", FontSlantNormal, FontWeightBold), sansRB),
          (("sans-serif", FontSlantItalic, FontWeightNormal), sansRI),
          (("sans-serif", FontSlantOblique, FontWeightNormal), sansRI),
          (("sans-serif", FontSlantItalic, FontWeightBold), sansRBI),
          (("sans-serif", FontSlantOblique, FontWeightBold), sansRBI),
          (("monospace", FontSlantNormal, FontWeightNormal), monoR),
          (("monospace", FontSlantNormal, FontWeightBold), monoRB),
          (("monospace", FontSlantItalic, FontWeightNormal), monoR),
          (("monospace", FontSlantOblique, FontWeightNormal), monoR),
          (("monospace", FontSlantItalic, FontWeightBold), monoRB),
          (("monospace", FontSlantOblique, FontWeightBold), monoRB)
        ]
