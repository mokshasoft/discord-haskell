{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances for discord-haskell types
--
-- These instances are used for property-based testing with QuickCheck.
-- They generate random Discord API types for roundtrip JSON tests.
module Discord.Types.Arbitrary
  ( -- * Re-exports for convenience
    module Test.QuickCheck
  ) where

import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()

import Data.Word (Word64)
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Channel (Overwrite(..))
import Discord.Internal.Types.User (User(..), GuildMember(..))
import Discord.Internal.Types.ScheduledEvents
import Discord.Internal.Types.Embed
import Discord.Internal.Types.Color (DiscordColor(..))
import Discord.Internal.Types.AutoModeration


-- =============================================================================
-- Base Discord types
-- =============================================================================

instance Arbitrary Snowflake where
  arbitrary = Snowflake <$> arbitrary
  shrink (Snowflake w) = Snowflake <$> shrink w

instance Arbitrary (DiscordId a) where
  arbitrary = DiscordId <$> arbitrary
  shrink (DiscordId s) = DiscordId <$> shrink s


-- =============================================================================
-- Channel types
-- =============================================================================

instance Arbitrary Overwrite where
  arbitrary = Overwrite
    <$> arbitrary                    -- Either RoleId UserId
    <*> genPermissionBits            -- allow
    <*> genPermissionBits            -- deny
  shrink (Overwrite eid allow deny) =
    [ Overwrite eid' allow' deny'
    | (eid', allow', deny') <- shrink (eid, allow, deny)
    ]

-- | Generate permission bit strings (Discord uses string-encoded integers)
genPermissionBits :: Gen T.Text
genPermissionBits = T.pack . show <$> (arbitrary :: Gen Word64)


-- =============================================================================
-- Color types
-- =============================================================================

instance Arbitrary DiscordColor where
  -- Note: Only RGB colors roundtrip correctly because FromJSON always
  -- converts to DiscordColorRGB via convertToRGB. Named colors like
  -- DiscordColorAqua become DiscordColorRGB 26 188 156 after roundtrip.
  arbitrary = DiscordColorRGB
    <$> chooseInteger (0, 255)
    <*> chooseInteger (0, 255)
    <*> chooseInteger (0, 255)


-- =============================================================================
-- User types
-- =============================================================================

instance Arbitrary User where
  arbitrary = User
    <$> arbitrary                    -- userId
    <*> genUsername                  -- userName
    <*> arbitrary                    -- userDiscrim
    <*> arbitrary                    -- userGlobalName
    <*> arbitrary                    -- userAvatar
    <*> arbitrary                    -- userIsBot
    <*> pure False                   -- userIsWebhook (not in JSON, set by library for webhook users)
    <*> arbitrary                    -- userIsSystem
    <*> arbitrary                    -- userMfa
    <*> arbitrary                    -- userBanner
    <*> arbitrary                    -- userAccentColor
    <*> arbitrary                    -- userLocale
    <*> arbitrary                    -- userVerified
    <*> arbitrary                    -- userEmail
    <*> arbitrary                    -- userFlags
    <*> arbitrary                    -- userPremiumType
    <*> arbitrary                    -- userPublicFlags
    <*> arbitrary                    -- userMember
  shrink user =
    [ user { userMember = m }
    | m <- shrink (userMember user)
    ]

instance Arbitrary GuildMember where
  arbitrary = GuildMember
    <$> pure Nothing                 -- memberUser (avoid recursion)
    <*> arbitrary                    -- memberNick
    <*> arbitrary                    -- memberAvatar
    <*> listOf arbitrary             -- memberRoles
    <*> arbitrary                    -- memberJoinedAt
    <*> arbitrary                    -- memberPremiumSince
    <*> arbitrary                    -- memberDeaf
    <*> arbitrary                    -- memberMute
    <*> arbitrary                    -- memberPending
    <*> arbitrary                    -- memberPermissions
    <*> arbitrary                    -- memberTimeoutEnd
  shrink member =
    [ member { memberRoles = r }
    | r <- shrink (memberRoles member)
    ]

-- | Generate valid Discord usernames (2-32 chars, alphanumeric + underscore)
genUsername :: Gen T.Text
genUsername = do
  len <- chooseInt (2, 32)
  chars <- vectorOf len $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  pure $ T.pack chars


-- =============================================================================
-- Scheduled Event types
-- =============================================================================

instance Arbitrary ScheduledEventPrivacyLevel where
  arbitrary = pure ScheduledEventPrivacyLevelGuildOnly

instance Arbitrary ScheduledEventStatus where
  arbitrary = elements
    [ ScheduledEventStatusScheduled
    , ScheduledEventStatusActive
    , ScheduledEventStatusCompleted
    , ScheduledEventStatusCancelled
    ]

instance Arbitrary ScheduledEvent where
  arbitrary = oneof
    [ genScheduledEventStage
    , genScheduledEventVoice
    , genScheduledEventExternal
    ]
  shrink _ = []  -- Complex sum type, skip shrinking

genScheduledEventStage :: Gen ScheduledEvent
genScheduledEventStage = ScheduledEventStage
  <$> arbitrary                      -- id
  <*> arbitrary                      -- guildId
  <*> arbitrary                      -- channelId
  <*> arbitrary                      -- creatorId
  <*> genEventName                   -- name
  <*> arbitrary                      -- description
  <*> arbitrary                      -- startTime
  <*> arbitrary                      -- endTime
  <*> arbitrary                      -- privacyLevel
  <*> arbitrary                      -- status
  <*> arbitrary                      -- entityId
  <*> arbitrary                      -- creator
  <*> arbitrary                      -- userCount
  <*> arbitrary                      -- image

genScheduledEventVoice :: Gen ScheduledEvent
genScheduledEventVoice = ScheduledEventVoice
  <$> arbitrary                      -- id
  <*> arbitrary                      -- guildId
  <*> arbitrary                      -- channelId
  <*> arbitrary                      -- creatorId
  <*> genEventName                   -- name
  <*> arbitrary                      -- description
  <*> arbitrary                      -- startTime
  <*> arbitrary                      -- endTime
  <*> arbitrary                      -- privacyLevel
  <*> arbitrary                      -- status
  <*> arbitrary                      -- entityId
  <*> arbitrary                      -- creator
  <*> arbitrary                      -- userCount
  <*> arbitrary                      -- image

genScheduledEventExternal :: Gen ScheduledEvent
genScheduledEventExternal = ScheduledEventExternal
  <$> arbitrary                      -- id
  <*> arbitrary                      -- guildId
  <*> genLocation                    -- location (required for external)
  <*> arbitrary                      -- creatorId
  <*> genEventName                   -- name
  <*> arbitrary                      -- description
  <*> arbitrary                      -- startTime
  <*> arbitrary                      -- endTime (required for external)
  <*> arbitrary                      -- privacyLevel
  <*> arbitrary                      -- status
  <*> arbitrary                      -- entityId
  <*> arbitrary                      -- creator
  <*> arbitrary                      -- userCount
  <*> arbitrary                      -- image

-- | Generate valid event names (1-100 chars)
genEventName :: Gen T.Text
genEventName = do
  len <- chooseInt (1, 100)
  T.pack <$> vectorOf len (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ [' '])

-- | Generate a location string for external events
genLocation :: Gen T.Text
genLocation = do
  len <- chooseInt (1, 100)
  T.pack <$> vectorOf len (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ [' ', ','])


-- =============================================================================
-- Embed types
-- =============================================================================

instance Arbitrary Embed where
  arbitrary = Embed
    <$> arbitrary                    -- embedAuthor
    <*> arbitrary                    -- embedTitle
    <*> arbitrary                    -- embedUrl
    <*> arbitrary                    -- embedThumbnail
    <*> arbitrary                    -- embedDescription
    <*> listOf arbitrary             -- embedFields
    <*> arbitrary                    -- embedImage
    <*> arbitrary                    -- embedFooter
    <*> arbitrary                    -- embedColor
    <*> arbitrary                    -- embedTimestamp
    <*> arbitrary                    -- embedVideo
    <*> arbitrary                    -- embedProvider

instance Arbitrary EmbedFooter where
  arbitrary = EmbedFooter
    <$> genShortText                 -- embedFooterText
    <*> arbitrary                    -- embedFooterIconUrl
    <*> arbitrary                    -- embedFooterProxyIconUrl

instance Arbitrary EmbedImage where
  arbitrary = EmbedImage
    <$> arbitrary                    -- embedImageUrl
    <*> arbitrary                    -- embedImageProxyUrl
    <*> arbitrary                    -- embedImageHeight
    <*> arbitrary                    -- embedImageWidth

instance Arbitrary EmbedThumbnail where
  arbitrary = EmbedThumbnail
    <$> arbitrary                    -- embedThumbnailUrl
    <*> arbitrary                    -- embedThumbnailProxyUrl
    <*> arbitrary                    -- embedThumbnailHeight
    <*> arbitrary                    -- embedThumbnailWidth

instance Arbitrary EmbedVideo where
  arbitrary = EmbedVideo
    <$> arbitrary                    -- embedVideoUrl
    <*> arbitrary                    -- embedVideoProxyUrl
    <*> arbitrary                    -- embedVideoHeight
    <*> arbitrary                    -- embedVideoWidth

instance Arbitrary EmbedProvider where
  arbitrary = EmbedProvider
    <$> arbitrary                    -- embedProviderName
    <*> arbitrary                    -- embedProviderUrl

instance Arbitrary EmbedAuthor where
  arbitrary = EmbedAuthor
    <$> genShortText                 -- embedAuthorName
    <*> arbitrary                    -- embedAuthorUrl
    <*> arbitrary                    -- embedAuthorIconUrl
    <*> arbitrary                    -- embedAuthorProxyIconUrl

instance Arbitrary EmbedField where
  arbitrary = EmbedField
    <$> genShortText                 -- embedFieldName
    <*> genShortText                 -- embedFieldValue
    <*> arbitrary                    -- embedFieldInline

-- | Generate short text for embed fields (1-50 chars)
genShortText :: Gen T.Text
genShortText = do
  len <- chooseInt (1, 50)
  T.pack <$> vectorOf len (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ [' '])


-- =============================================================================
-- AutoModeration types
-- =============================================================================

instance Arbitrary AutoModerationRule where
  arbitrary = AutoModerationRule
    <$> arbitrary                    -- autoModerationRuleId
    <*> arbitrary                    -- autoModerationRuleGuildId
    <*> genAlphaNum                  -- autoModerationRuleName
    <*> arbitrary                    -- autoModerationRuleCreatorId
    <*> arbitrary                    -- autoModerationRuleEventType
    <*> arbitrary                    -- autoModerationRuleTriggerType
    <*> arbitrary                    -- autoModerationRuleTriggerMetadata
    <*> listOf arbitrary             -- autoModerationRuleActions
    <*> arbitrary                    -- autoModerationRuleEnabled
    <*> listOf arbitrary             -- autoModerationRuleExemptRoles
    <*> listOf arbitrary             -- autoModerationRuleExemptChannels

instance Arbitrary AutoModerationRuleEventType where
  arbitrary = pure MessageSent

instance Arbitrary AutoModerationRuleTriggerType where
  arbitrary = elements [Keyword, Spam, KeywordPreset, MentionSpam]

instance Arbitrary AutoModerationRuleTriggerMetadata where
  arbitrary = AutoModerationRuleTriggerMetadata
    <$> listOf genAlphaNum           -- keywordFilter
    <*> listOf genAlphaNum           -- regexPatterns
    <*> arbitrary                    -- presets
    <*> listOf genAlphaNum           -- allowList
    <*> arbitrary                    -- mentionLimit
    <*> arbitrary                    -- raidProtection

instance Arbitrary AutoModerationRuleTriggerMetadataPreset where
  arbitrary = elements [Profanity, SexualContent, Slurs]

instance Arbitrary AutoModerationRuleAction where
  arbitrary = AutoModerationRuleAction
    <$> arbitrary                    -- actionType
    <*> arbitrary                    -- actionMetadata

instance Arbitrary AutoModerationRuleActionType where
  arbitrary = elements [BlockMessage, SendAlertMessage, Timeout]

instance Arbitrary AutoModerationRuleActionMetadata where
  arbitrary = AutoModerationRuleActionMetadata
    <$> arbitrary                    -- channelId
    <*> arbitrary                    -- timeoutDuration
    <*> (fmap getASCIIString <$> arbitrary)  -- customMessage (ASCII only)

-- | Generate alphanumeric strings for rule names, keywords, etc.
genAlphaNum :: Gen String
genAlphaNum = do
  len <- chooseInt (1, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

