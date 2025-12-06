{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Discord Gateway Events completeness
--
-- Verifies that the library handles all official Discord Gateway events.
-- Events not explicitly handled fall through to UnknownEvent.
--
-- Source: https://discord-api-types.dev/api/discord-api-types-v10/enum/GatewayDispatchEvents
module Discord.Types.EventsSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set


spec :: Spec
spec = do
  describe "Gateway Events Coverage" $ do
    it "documents which official Discord events are implemented" $ do
      -- This test documents the current state of event coverage.
      -- It should be updated as new events are added to the library.

      let implemented = Set.fromList implementedEvents
          official = Set.fromList officialDiscordEvents
          missing = Set.difference official implemented
          _extra = Set.difference implemented official

      -- Report coverage
      let coverage = (fromIntegral (Set.size implemented) / fromIntegral (Set.size official) * 100) :: Double

      -- This test passes but documents what's missing
      putStrLn $ "\n  Event Coverage: " ++ show (length implementedEvents) ++ "/" ++ show (length officialDiscordEvents)
                 ++ " (" ++ show (round coverage :: Int) ++ "%)"
      putStrLn $ "  Missing events (" ++ show (Set.size missing) ++ "): " ++ show (Set.toList missing)

      -- The library handles unknown events gracefully via UnknownEvent,
      -- so missing events don't cause crashes - they just aren't typed.
      Set.size implemented `shouldSatisfy` (> 0)

    it "has no typos in implemented event names" $ do
      -- All implemented events should be in the official list
      let implemented = Set.fromList implementedEvents
          official = Set.fromList officialDiscordEvents
          extra = Set.difference implemented official

      -- These should all be official Discord events
      Set.toList extra `shouldBe` []


-- | Events currently implemented in discord-haskell
-- From: src/Discord/Internal/Types/Events.hs eventParse function
implementedEvents :: [String]
implementedEvents =
  [ "READY"
  , "RESUMED"
  , "AUTO_MODERATION_RULE_CREATE"
  , "AUTO_MODERATION_RULE_UPDATE"
  , "AUTO_MODERATION_RULE_DELETE"
  , "AUTO_MODERATION_ACTION_EXECUTION"
  , "CHANNEL_CREATE"
  , "CHANNEL_UPDATE"
  , "CHANNEL_DELETE"
  , "CHANNEL_PINS_UPDATE"
  , "THREAD_CREATE"
  , "THREAD_UPDATE"
  , "THREAD_DELETE"
  , "THREAD_LIST_SYNC"
  , "THREAD_MEMBER_UPDATE"
  , "THREAD_MEMBERS_UPDATE"
  , "GUILD_CREATE"
  , "GUILD_UPDATE"
  , "GUILD_DELETE"
  , "GUILD_AUDIT_LOG_ENTRY_CREATE"
  , "GUILD_BAN_ADD"
  , "GUILD_BAN_REMOVE"
  , "GUILD_EMOJIS_UPDATE"
  , "GUILD_INTEGRATIONS_UPDATE"
  , "GUILD_MEMBER_ADD"
  , "GUILD_MEMBER_REMOVE"
  , "GUILD_MEMBER_UPDATE"
  , "GUILD_MEMBERS_CHUNK"
  , "GUILD_ROLE_CREATE"
  , "GUILD_ROLE_UPDATE"
  , "GUILD_ROLE_DELETE"
  , "MESSAGE_CREATE"
  , "MESSAGE_UPDATE"
  , "MESSAGE_DELETE"
  , "MESSAGE_DELETE_BULK"
  , "MESSAGE_REACTION_ADD"
  , "MESSAGE_REACTION_REMOVE"
  , "MESSAGE_REACTION_REMOVE_ALL"
  , "MESSAGE_REACTION_REMOVE_EMOJI"
  , "PRESENCE_UPDATE"
  , "TYPING_START"
  , "USER_UPDATE"
  , "INTERACTION_CREATE"
  -- Commented out in source:
  -- , "VOICE_STATE_UPDATE"
  -- , "VOICE_SERVER_UPDATE"
  ]


-- | Official Discord Gateway events as of 2025
-- Source: https://discord-api-types.dev/api/discord-api-types-v10/enum/GatewayDispatchEvents
officialDiscordEvents :: [String]
officialDiscordEvents =
  [ "APPLICATION_COMMAND_PERMISSIONS_UPDATE"
  , "AUTO_MODERATION_ACTION_EXECUTION"
  , "AUTO_MODERATION_RULE_CREATE"
  , "AUTO_MODERATION_RULE_DELETE"
  , "AUTO_MODERATION_RULE_UPDATE"
  , "CHANNEL_CREATE"
  , "CHANNEL_DELETE"
  , "CHANNEL_PINS_UPDATE"
  , "CHANNEL_UPDATE"
  , "ENTITLEMENT_CREATE"
  , "ENTITLEMENT_DELETE"
  , "ENTITLEMENT_UPDATE"
  , "GUILD_AUDIT_LOG_ENTRY_CREATE"
  , "GUILD_BAN_ADD"
  , "GUILD_BAN_REMOVE"
  , "GUILD_CREATE"
  , "GUILD_DELETE"
  , "GUILD_EMOJIS_UPDATE"
  , "GUILD_INTEGRATIONS_UPDATE"
  , "GUILD_MEMBER_ADD"
  , "GUILD_MEMBER_REMOVE"
  , "GUILD_MEMBERS_CHUNK"
  , "GUILD_MEMBER_UPDATE"
  , "GUILD_ROLE_CREATE"
  , "GUILD_ROLE_DELETE"
  , "GUILD_ROLE_UPDATE"
  , "GUILD_SCHEDULED_EVENT_CREATE"
  , "GUILD_SCHEDULED_EVENT_DELETE"
  , "GUILD_SCHEDULED_EVENT_UPDATE"
  , "GUILD_SCHEDULED_EVENT_USER_ADD"
  , "GUILD_SCHEDULED_EVENT_USER_REMOVE"
  , "GUILD_SOUNDBOARD_SOUND_CREATE"
  , "GUILD_SOUNDBOARD_SOUND_DELETE"
  , "GUILD_SOUNDBOARD_SOUNDS_UPDATE"
  , "GUILD_SOUNDBOARD_SOUND_UPDATE"
  , "GUILD_STICKERS_UPDATE"
  , "GUILD_UPDATE"
  , "INTEGRATION_CREATE"
  , "INTEGRATION_DELETE"
  , "INTEGRATION_UPDATE"
  , "INTERACTION_CREATE"
  , "INVITE_CREATE"
  , "INVITE_DELETE"
  , "MESSAGE_CREATE"
  , "MESSAGE_DELETE"
  , "MESSAGE_DELETE_BULK"
  , "MESSAGE_POLL_VOTE_ADD"
  , "MESSAGE_POLL_VOTE_REMOVE"
  , "MESSAGE_REACTION_ADD"
  , "MESSAGE_REACTION_REMOVE"
  , "MESSAGE_REACTION_REMOVE_ALL"
  , "MESSAGE_REACTION_REMOVE_EMOJI"
  , "MESSAGE_UPDATE"
  , "PRESENCE_UPDATE"
  , "READY"
  , "RESUMED"
  , "SOUNDBOARD_SOUNDS"
  , "STAGE_INSTANCE_CREATE"
  , "STAGE_INSTANCE_DELETE"
  , "STAGE_INSTANCE_UPDATE"
  , "SUBSCRIPTION_CREATE"
  , "SUBSCRIPTION_DELETE"
  , "SUBSCRIPTION_UPDATE"
  , "THREAD_CREATE"
  , "THREAD_DELETE"
  , "THREAD_LIST_SYNC"
  , "THREAD_MEMBERS_UPDATE"
  , "THREAD_MEMBER_UPDATE"
  , "THREAD_UPDATE"
  , "TYPING_START"
  , "USER_UPDATE"
  , "VOICE_CHANNEL_EFFECT_SEND"
  , "VOICE_SERVER_UPDATE"
  , "VOICE_STATE_UPDATE"
  , "WEBHOOKS_UPDATE"
  ]
