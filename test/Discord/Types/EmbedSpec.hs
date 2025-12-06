{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Discord Embed types JSON serialization
--
-- Property tests verify JSON roundtrip: decode (encode x) == Just x
module Discord.Types.EmbedSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Aeson (decode, encode)

import Discord.Internal.Types.Embed
import Discord.Types.Arbitrary ()


spec :: Spec
spec = do
  describe "Embed" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \embed ->
        decode (encode embed) == Just (embed :: Embed)

  describe "EmbedFooter" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \footer ->
        decode (encode footer) == Just (footer :: EmbedFooter)

  describe "EmbedImage" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \image ->
        decode (encode image) == Just (image :: EmbedImage)

  describe "EmbedThumbnail" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \thumbnail ->
        decode (encode thumbnail) == Just (thumbnail :: EmbedThumbnail)

  describe "EmbedVideo" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \video ->
        decode (encode video) == Just (video :: EmbedVideo)

  describe "EmbedProvider" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \provider ->
        decode (encode provider) == Just (provider :: EmbedProvider)

  describe "EmbedAuthor" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \author ->
        decode (encode author) == Just (author :: EmbedAuthor)

  describe "EmbedField" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \field ->
        decode (encode field) == Just (field :: EmbedField)
