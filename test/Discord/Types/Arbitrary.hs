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

