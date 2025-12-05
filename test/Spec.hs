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
