{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Discord Channel types JSON parsing
--
-- Property tests verify JSON roundtrip: decode (encode x) == Just x
--
-- Unit tests verify regression for BUG-004:
--   The FromJSON instance for Overwrite used `error` instead of `fail`
--   when encountering an invalid "type" field value.
module Discord.Types.ChannelSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Discord.Internal.Types.Channel (Overwrite(..))
import Discord.Types.Arbitrary ()


spec :: Spec
spec = do
  describe "Overwrite" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \overwrite ->
        decode (encode overwrite) == Just (overwrite :: Overwrite)

    describe "FromJSON (regression tests)" $ do
      it "parses type 0 (role) successfully" $ do
        let json = overwriteJson 0 "123456"
        case eitherDecode json :: Either String Overwrite of
          Left err -> expectationFailure $ "Failed to parse: " ++ err
          Right ow -> case overwriteId ow of
            Left _roleId -> pure ()
            Right _userId -> expectationFailure "Expected Left (RoleId), got Right (UserId)"

      it "parses type 1 (user) successfully" $ do
        let json = overwriteJson 1 "789012"
        case eitherDecode json :: Either String Overwrite of
          Left err -> expectationFailure $ "Failed to parse: " ++ err
          Right ow -> case overwriteId ow of
            Left _roleId -> expectationFailure "Expected Right (UserId), got Left (RoleId)"
            Right _userId -> pure ()

      -- BUG-004: invalid type should return Nothing, not crash
      it "returns Nothing for invalid type (e.g. 99)" $ do
        let json = overwriteJson 99 "123456"
        (decode json :: Maybe Overwrite) `shouldBe` Nothing

      it "returns Nothing for type 2 (invalid)" $ do
        let json = overwriteJson 2 "123456"
        (decode json :: Maybe Overwrite) `shouldBe` Nothing


-- | Generate an Overwrite JSON payload for testing FromJSON
overwriteJson :: Int -> String -> BL.ByteString
overwriteJson owType owId = BL.pack $
  "{\"id\": \"" ++ owId ++ "\", " ++
  "\"type\": " ++ show owType ++ ", " ++
  "\"allow\": \"0\", " ++
  "\"deny\": \"0\"}"
