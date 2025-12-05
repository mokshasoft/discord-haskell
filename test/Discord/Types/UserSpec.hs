{-# LANGUAGE OverloadedStrings #-}

module Discord.Types.UserSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (isJust)

import Discord.Internal.Types.User

spec :: Spec
spec = do
  describe "User ToJSON" $ do
    it "serializes member field correctly (not as public_flags)" $ do
      let json = userJsonWithMember
      case eitherDecode json :: Either String User of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right user -> do
          -- The user should have a member field
          userMember user `shouldSatisfy` isJust
          -- Re-encode and check the member field is preserved
          let reencoded = encode user
          -- The reencoded JSON should contain "member" with the guild member data
          BL.unpack reencoded `shouldContain` "\"member\""
          -- Decode again and verify roundtrip
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

-- Test JSON data
userJsonSimple :: BL.ByteString
userJsonSimple = BL.pack $
  "{\"id\": \"123456789\", \"username\": \"testuser\", " ++
  "\"discriminator\": \"1234\", \"avatar\": null, \"bot\": false}"

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
