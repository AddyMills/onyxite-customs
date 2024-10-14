{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
module Onyx.Import.Encore where

import           Control.Monad.Extra              (guard, mapMaybeM, when)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe)
import qualified Data.Text                        as T
import           Onyx.Audio                       (Audio (..))
import           Onyx.Encore
import           Onyx.Guitar                      (applyStatus)
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   joinEdgesSimple,
                                                   splitEdgesSimple)
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.Mania
import           Onyx.PhaseShift.Dance            (NoteType (..))
import           Onyx.Project                     hiding (Difficulty)
import qualified Onyx.Project                     as P
import           Onyx.StackTrace                  (SendMessage, lg, stackIO)
import           Onyx.Util.Files                  (fixFileCase)
import           Onyx.Util.Handle                 (Readable, fileReadable)
import qualified Sound.MIDI.Util                  as U
import qualified System.Directory                 as Dir
import           System.FilePath                  (takeExtension, (<.>), (</>))
import           Text.Read                        (readMaybe)

-- TODO maybe import lead to PartKeys if sig/icon_guitar == Keyboard

importEncore :: (SendMessage m, MonadIO m) => FilePath -> Import m
importEncore dir level = do
  when (level == ImportFull) $ lg $ "Importing Encore song from: " <> dir

  info <- loadEncoreInfo $ dir </> "info.json"

  mid <- case level of
    ImportQuick -> return emptyChart
    ImportFull  -> F.loadMIDIReadable @EncoreFile $ fileReadable info.midi

  let existingPaths :: [FilePath] -> IO [FilePath]
      existingPaths paths = flip mapMaybeM paths $ \p -> do
        p' <- fixFileCase p
        e <- Dir.doesFileExist p'
        return $ guard e >> Just p'
      audioFor :: T.Text -> IO (Maybe (Audio t (FilePath, Readable)))
      audioFor key = mapM existingPaths (HM.lookup key info.stems) >>= return . \case
        Just [x]    -> Just $ Input (T.unpack key <.> takeExtension x, fileReadable x)
        Just (x : xs) -> Just $ Mix $ NE.zipWith
          (\i y -> Input ((T.unpack key <> "-" <> show i) <.> takeExtension y, fileReadable y))
          ((1 :: Int) NE.:| [2..])
          (x NE.:| xs)
        _ -> Nothing
  audioDrums   <- stackIO $ audioFor "drums"
  audioBass    <- stackIO $ audioFor "bass"
  audioLead    <- stackIO $ audioFor "lead"
  audioVocals  <- stackIO $ audioFor "vocals"
  audioBacking <- stackIO $ audioFor "backing"

  return SongYaml
    { metadata = Metadata
      { title         = Just info.title
      , titleJP       = Nothing
      , artist        = Just info.artist
      , artistJP      = Nothing
      , album         = info.album
      , genre         = case info.genres of
        [] -> Nothing
        xs -> Just $ T.intercalate ", " xs
      , subgenre      = Nothing
      , year          = info.release_year >>= readMaybe . T.unpack
      , fileAlbumArt  = flip fmap info.art $ \f ->
        SoftFile ("album" <.> takeExtension f) $ SoftReadable $ fileReadable f
      , trackNumber   = Nothing
      , comments      = []
      , difficulty    = Tier 1
      , key           = Nothing
      , author        = case info.charters of
        [] -> Nothing
        xs -> Just $ T.intercalate ", " xs
      , rating        = Unrated
      , previewStart  = PreviewSeconds . (/ 1000) . fromIntegral <$> info.preview_start_time
      , previewEnd    = Nothing
      , languages     = []
      , convert       = False
      , rhythmKeys    = False
      , rhythmBass    = False
      , catEMH        = False
      , expertOnly    = False
      , cover         = False
      , loadingPhrase = info.loading_phrase
      }
    , global = let Global{..} = def' in Global
      { fileMidi = SoftFile "notes.mid" $ SoftChart $ encoreToOnyxMIDI mid
      , fileSongAnim = Nothing
      , backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , ..
      }
    , audio = HM.fromList $ do
      (f, r) <- [audioDrums, audioBass, audioLead, audioVocals, audioBacking] >>= toList >>= toList
      return (T.pack f, AudioFile AudioInfo
        { md5      = Nothing
        , frames   = Nothing
        , filePath = Just $ SoftFile f $ SoftReadable r
        , commands = []
        , rate     = Nothing
        , channels = 2 -- TODO verify?
        })
    , jammit = HM.empty
    , plans = HM.singleton "encore" $ StandardPlan StandardPlanInfo
      { song        = fmap (fmap $ (\(f, _) -> Named $ T.pack f)) audioBacking
      , parts       = Parts $ HM.fromList $ do
        (part, audio) <-
          [ (F.PartDrums , audioDrums )
          , (F.PartBass  , audioBass  )
          , (F.PartGuitar, audioLead  )
          , (F.PartVocal , audioVocals)
          ]
        a <- toList audio
        return (part, PartSingle $ fmap (\(f, _) -> Named $ T.pack f) a)
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    , targets = HM.empty
    , parts = encorePartData info.diff
    }

encoreToOnyxMIDI :: F.Song (EncoreFile U.Beats) -> F.Song (F.OnyxFile U.Beats)
encoreToOnyxMIDI = fmap $ \encore -> mempty
  { F.onyxParts    = Map.fromList
    [ (F.PartGuitar, mempty
      { F.onyxPartGuitar = encore.plasticGuitar
      , F.onyxPartMania = encorePart encore.partGuitar.part
      })
    , (F.PartBass, mempty
      { F.onyxPartGuitar = encore.plasticBass
      , F.onyxPartMania = encorePart encore.partBass.part
      })
    , (F.PartDrums, mempty
      { F.onyxPartDrums = encore.plasticDrums
      , F.onyxPartMania = encorePart encore.partDrums.part
      })
    , (F.PartVocal, mempty
      { F.onyxPartMania = encorePart encore.partVocals
      })
    ]
  , F.onyxEvents   = encore.events
  , F.onyxBeat     = encore.beat
  } where
    encorePart :: EncorePart U.Beats -> Map.Map T.Text (ManiaTrack U.Beats)
    encorePart fp = Map.fromList $ do
      diff <- [Easy, Medium, Hard, Expert]
      let name = T.toLower $ T.pack $ show diff
      fd <- toList $ Map.lookup diff fp.difficulties
      return (name, ManiaTrack
        { maniaNotes     = encoreToMania fd
        , maniaOverdrive = fp.overdrive
        })

encoreToMania :: EncoreDifficulty U.Beats -> RTB.T U.Beats (Edge () (Int, NoteType))
encoreToMania fd = let
  base = joinEdgesSimple fd.gems
  liftStatus = fmap
    (\case
      EdgeOn () color -> (color, True )
      EdgeOff   color -> (color, False)
    ) fd.lifts
  in splitEdgesSimple $ fmap
    (\(liftsHere, ((), color, len)) -> let
      noteType = if elem color liftsHere then NoteLift else NoteNormal
      in ((), (fromEnum color, noteType), len)
    ) (applyStatus liftStatus base)

encorePartData :: HM.HashMap T.Text Int -> Parts (Part f)
encorePartData diffs = let

  modePad :: P.Difficulty -> ModeMania
  modePad tier = ModeMania $ do
    -- TODO should check the midi to see what's actually present
    name <- "easy" NE.:| ["medium", "hard", "expert"]
    return ManiaChart
      { name = name
      , keys = if name == "expert" then 5 else 4
      , style = ManiaEncore
      , difficulty = tier
      }

  getTier :: [T.Text] -> Maybe P.Difficulty
  getTier options = listToMaybe $ do
    o <- options
    case HM.lookup o diffs of
      Just d | d >= 0 -> [Tier $ fromIntegral d + 1] -- 0..6 -> 1..7
      _               -> []

  in Parts $ HM.fromList
    [ (F.PartGuitar, let Part{..} = emptyPart in Part
      -- Run It (Epic Games) has no pg/gr difficulties but seems to have guitar, huh?
      { grybo = do
        tier <- getTier ["pg", "plastic_guitar"]
        Just ModeFive
          { difficulty = tier
          , hopoThreshold = 170
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
      , mania = do
        tier <- getTier ["gr", "guitar"]
        Just $ modePad tier
      , ..
      })
    , (F.PartBass, let Part{..} = emptyPart in Part
      { grybo = do
        tier <- getTier ["pb", "plastic_bass"]
        Just ModeFive
          { difficulty = tier
          , hopoThreshold = 170
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
      , mania = do
        tier <- getTier ["ba", "bass"]
        Just $ modePad tier
      , ..
      })
    , (F.PartDrums, let Part{..} = emptyPart in Part
      { drums = do
        tier <- getTier ["pd", "plastic_drums"]
        Just $ let ModeDrums{..} = emptyPartDrums DrumsPro Kicks1x in ModeDrums
          { difficulty = tier
          , ..
          }
      , mania = do
        tier <- getTier ["ds", "drums"]
        Just $ modePad tier
      , ..
      })
    , (F.PartVocal, let Part{..} = emptyPart in Part
      { mania = do
        tier <- getTier ["vl", "vocals"]
        Just $ modePad tier
      , ..
      })
    ]
