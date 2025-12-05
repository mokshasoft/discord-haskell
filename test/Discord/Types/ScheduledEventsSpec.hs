{-# LANGUAGE OverloadedStrings #-}

module Discord.Types.ScheduledEventsSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Discord.Internal.Types.ScheduledEvents

spec :: Spec
spec = do
  describe "ScheduledEvent FromJSON" $ do
    it "parses entity_type 1 (stage instance) successfully" $ do
      let json = scheduledEventJson 1 (Just "123456789")
      case eitherDecode json :: Either String ScheduledEvent of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right (ScheduledEventStage {}) -> return ()  -- Success
        Right _ -> expectationFailure "Expected ScheduledEventStage"

    it "parses entity_type 2 (voice) successfully" $ do
      let json = scheduledEventJson 2 (Just "123456789")
      case eitherDecode json :: Either String ScheduledEvent of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right (ScheduledEventVoice {}) -> return ()  -- Success
        Right _ -> expectationFailure "Expected ScheduledEventVoice"

    it "parses entity_type 3 (external) successfully" $ do
      let json = scheduledEventJsonExternal 3
      case eitherDecode json :: Either String ScheduledEvent of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right (ScheduledEventExternal {}) -> return ()  -- Success
        Right _ -> expectationFailure "Expected ScheduledEventExternal"

    it "returns Nothing for unknown entity_type (e.g. 99)" $ do
      let json = scheduledEventJson 99 (Just "123456789")
      (decode json :: Maybe ScheduledEvent) `shouldBe` Nothing

    it "returns Nothing for entity_type 0 (NONE - reserved by Discord)" $ do
      let json = scheduledEventJson 0 (Just "123456789")
      (decode json :: Maybe ScheduledEvent) `shouldBe` Nothing

  describe "CreateScheduledEventData FromJSON" $ do
    it "parses entity_type 3 as External (not Voice)" $ do
      let json = createScheduledEventDataJsonExternal
      case eitherDecode json :: Either String CreateScheduledEventData of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right (CreateScheduledEventDataExternal loc _ _ _ _ _ _) ->
          loc `shouldBe` "Test Location"
        Right _ ->
          expectationFailure "Expected CreateScheduledEventDataExternal"

    it "returns Nothing for unknown entity_type" $ do
      let json = createScheduledEventDataJsonUnknown
      case (decode json :: Maybe CreateScheduledEventData) of
        Nothing -> return ()  -- Success
        Just _ -> expectationFailure "Expected Nothing for unknown entity_type"

-- Test JSON data
scheduledEventJson :: Int -> Maybe String -> BL.ByteString
scheduledEventJson entityType mChannelId = BL.pack $
  "{\"id\": \"123\", \"guild_id\": \"456\", \"name\": \"Test Event\", " ++
  "\"scheduled_start_time\": \"2024-01-01T00:00:00Z\", " ++
  "\"privacy_level\": 2, \"status\": 1, \"entity_type\": " ++ show entityType ++
  channelIdField mChannelId ++ "}"
  where
    channelIdField Nothing = ""
    channelIdField (Just cid) = ", \"channel_id\": \"" ++ cid ++ "\""

scheduledEventJsonExternal :: Int -> BL.ByteString
scheduledEventJsonExternal entityType = BL.pack $
  "{\"id\": \"123\", \"guild_id\": \"456\", \"name\": \"Test Event\", " ++
  "\"scheduled_start_time\": \"2024-01-01T00:00:00Z\", " ++
  "\"scheduled_end_time\": \"2024-01-01T02:00:00Z\", " ++
  "\"privacy_level\": 2, \"status\": 1, \"entity_type\": " ++ show entityType ++
  ", \"entity_metadata\": {\"location\": \"Test Location\"}}"

createScheduledEventDataJsonExternal :: BL.ByteString
createScheduledEventDataJsonExternal = BL.pack $
  "{\"name\": \"Test Event\", \"privacy_level\": 2, " ++
  "\"scheduled_start_time\": \"2024-01-01T00:00:00Z\", " ++
  "\"scheduled_end_time\": \"2024-01-01T02:00:00Z\", " ++
  "\"entity_type\": 3, \"entity_metadata\": {\"location\": \"Test Location\"}}"

createScheduledEventDataJsonUnknown :: BL.ByteString
createScheduledEventDataJsonUnknown = BL.pack $
  "{\"name\": \"Test Event\", \"privacy_level\": 2, " ++
  "\"scheduled_start_time\": \"2024-01-01T00:00:00Z\", " ++
  "\"entity_type\": 99}"
