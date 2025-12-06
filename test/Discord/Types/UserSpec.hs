{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Discord User JSON serialization
--
-- Property tests verify JSON roundtrip: decode (encode x) == Just x
--
-- Unit tests verify regression for BUG-003:
--   The ToJSON instance for User incorrectly serialized `userPublicFlags`
--   instead of `userMember` for the "member" field.
module Discord.Types.UserSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (isJust)

import Discord.Internal.Types.User
import Discord.Types.Arbitrary ()


spec :: Spec
spec = do
  describe "User" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \user ->
        decode (encode user) == Just (user :: User)

    describe "ToJSON (regression tests)" $ do
      -- BUG-003: member field should roundtrip correctly
      it "serializes member field correctly (not as public_flags)" $ do
        let json = userJsonWithMember
        case eitherDecode json :: Either String User of
          Left err -> expectationFailure $ "Failed to parse: " ++ err
          Right user -> do
            userMember user `shouldSatisfy` isJust
            let reencoded = encode user
            BL.unpack reencoded `shouldContain` "\"member\""
            case eitherDecode reencoded :: Either String User of
              Left err' -> expectationFailure $ "Failed to re-parse: " ++ err'
              Right user' -> userMember user' `shouldSatisfy` isJust

      it "serializes user without member field" $ do
        let json = userJsonSimple
        case eitherDecode json :: Either String User of
          Left err -> expectationFailure $ "Failed to parse: " ++ err
          Right user -> do
            userMember user `shouldBe` Nothing
            let reencoded = encode user
            case eitherDecode reencoded :: Either String User of
              Left err' -> expectationFailure $ "Failed to re-parse: " ++ err'
              Right user' -> userMember user' `shouldBe` Nothing

  describe "GuildMember" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \member ->
        decode (encode member) == Just (member :: GuildMember)


-- | Simple user JSON without a member field
userJsonSimple :: BL.ByteString
userJsonSimple = BL.pack $
  "{\"id\": \"123456789\", \"username\": \"testuser\", " ++
  "\"discriminator\": \"1234\", \"avatar\": null, \"bot\": false}"

-- | User JSON with a member field (for BUG-003 regression test)
userJsonWithMember :: BL.ByteString
userJsonWithMember = BL.pack $
  "{\"id\": \"123456789\", \"username\": \"testuser\", " ++
  "\"discriminator\": \"1234\", \"avatar\": null, \"bot\": false, " ++
  "\"public_flags\": 64, " ++
  "\"member\": {" ++
    "\"roles\": [\"111\", \"222\"], " ++
    "\"joined_at\": \"2024-01-01T00:00:00Z\", " ++
    "\"deaf\": false, " ++
    "\"mute\": false" ++
  "}}"
