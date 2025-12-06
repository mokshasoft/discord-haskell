{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Discord AutoModeration types JSON serialization
--
-- Property tests verify JSON roundtrip: decode (encode x) == Just x
--
-- These tests uncovered and verify fixes for:
--   BUG-005: AutoModerationRule ToJSON used wrong field for "id"
--   BUG-006: AutoModerationRuleTriggerMetadataPreset ToJSON/FromJSON value mismatch
--   BUG-007: AutoModerationRuleActionType ToJSON/FromJSON value mismatch
module Discord.Types.AutoModerationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Aeson (decode, encode)

import Discord.Internal.Types.AutoModeration
import Discord.Types.Arbitrary ()


spec :: Spec
spec = do
  describe "AutoModerationRule" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \rule ->
        decode (encode rule) == Just (rule :: AutoModerationRule)

  describe "AutoModerationRuleEventType" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \eventType ->
        decode (encode eventType) == Just (eventType :: AutoModerationRuleEventType)

  describe "AutoModerationRuleTriggerType" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \triggerType ->
        decode (encode triggerType) == Just (triggerType :: AutoModerationRuleTriggerType)

  describe "AutoModerationRuleTriggerMetadata" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \metadata ->
        decode (encode metadata) == Just (metadata :: AutoModerationRuleTriggerMetadata)

  describe "AutoModerationRuleTriggerMetadataPreset" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \preset ->
        decode (encode preset) == Just (preset :: AutoModerationRuleTriggerMetadataPreset)

  describe "AutoModerationRuleAction" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \action ->
        decode (encode action) == Just (action :: AutoModerationRuleAction)

  describe "AutoModerationRuleActionType" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \actionType ->
        decode (encode actionType) == Just (actionType :: AutoModerationRuleActionType)

  describe "AutoModerationRuleActionMetadata" $ do
    describe "JSON roundtrip" $ do
      prop "decode . encode == Just" $ \metadata ->
        decode (encode metadata) == Just (metadata :: AutoModerationRuleActionMetadata)
