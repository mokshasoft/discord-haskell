{-# LANGUAGE OverloadedStrings #-}

module Discord.Types.ChannelSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Discord.Internal.Types.Channel (Overwrite(..))

spec :: Spec
spec = do
  describe "Overwrite FromJSON" $ do
    it "parses type 0 (role) successfully" $ do
      let json = overwriteJson 0 "123456"
      case eitherDecode json :: Either String Overwrite of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right ow -> case overwriteId ow of
          Left _roleId -> return ()  -- Success - it's a role ID
          Right _userId -> expectationFailure "Expected Left (RoleId), got Right (UserId)"

    it "parses type 1 (user) successfully" $ do
      let json = overwriteJson 1 "789012"
      case eitherDecode json :: Either String Overwrite of
        Left err -> expectationFailure $ "Failed to parse: " ++ err
        Right ow -> case overwriteId ow of
          Left _roleId -> expectationFailure "Expected Right (UserId), got Left (RoleId)"
          Right _userId -> return ()  -- Success - it's a user ID

    it "returns Nothing for invalid type (e.g. 99)" $ do
      let json = overwriteJson 99 "123456"
      (decode json :: Maybe Overwrite) `shouldBe` Nothing

    it "returns Nothing for type 2 (invalid)" $ do
      let json = overwriteJson 2 "123456"
      (decode json :: Maybe Overwrite) `shouldBe` Nothing

-- Test JSON data
overwriteJson :: Int -> String -> BL.ByteString
overwriteJson owType owId = BL.pack $
  "{\"id\": \"" ++ owId ++ "\", \"type\": " ++ show owType ++
  ", \"allow\": \"0\", \"deny\": \"0\"}"
