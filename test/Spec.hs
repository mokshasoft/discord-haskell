-- | Main test entry point using hspec
--
-- Tests include:
--   - Property-based roundtrip tests (decode . encode == Just)
--   - Regression tests for specific bugs (BUG-001 through BUG-004)
module Main (main) where

import Test.Hspec

import qualified Discord.Types.ScheduledEventsSpec as ScheduledEvents
import qualified Discord.Types.UserSpec as User
import qualified Discord.Types.ChannelSpec as Channel

main :: IO ()
main = hspec $ do
  describe "Discord.Internal.Types.ScheduledEvents" ScheduledEvents.spec
  describe "Discord.Internal.Types.User" User.spec
  describe "Discord.Internal.Types.Channel" Channel.spec
