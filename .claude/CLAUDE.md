# discord-haskell Development Guide

## Build System

This project uses **Stack with Nix** for dependency management. The `stack.yaml` configures Nix to provide system libraries (zlib, gmp).

### Common Commands

```bash
# Build the library and all executables
stack build --nix

# Build and run tests
stack test --nix

# Build without running tests
stack build --test --no-run-tests --nix

# Run a specific test pattern
stack test --nix --test-arguments="--match '/User/JSON roundtrip/'"

# Build documentation
stack haddock --nix

# Clean build artifacts
stack clean
```

### Important: Always use `--nix`

The `stack.yaml` has Nix integration configured:
```yaml
nix:
  packages: [ zlib, gmp ]
```

Without `--nix`, builds will fail with missing system library errors (e.g., `zlib.h not found`).

## Project Structure

```
discord-haskell/
├── src/Discord/                    # Main library source
│   ├── Internal/Types/             # Discord API type definitions
│   │   ├── User.hs                 # User, GuildMember types
│   │   ├── Channel.hs              # Channel, Overwrite types
│   │   ├── ScheduledEvents.hs      # Scheduled event types
│   │   └── ...
│   └── Internal/Rest/              # REST API request handlers
├── test/                           # Test suite
│   ├── Spec.hs                     # Main test entry point
│   └── Discord/Types/
│       ├── Arbitrary.hs            # QuickCheck Arbitrary instances
│       ├── UserSpec.hs             # User type tests
│       ├── ChannelSpec.hs          # Channel type tests
│       └── ScheduledEventsSpec.hs  # ScheduledEvent tests
├── examples/                       # Example bot implementations
├── discord-haskell.cabal           # Package definition
└── stack.yaml                      # Stack configuration with Nix
```

## Testing

The test suite uses **hspec** with **QuickCheck** for property-based testing.

### Test Types

1. **Property-based roundtrip tests**: Verify `decode (encode x) == Just x` with 100 random values
2. **Regression unit tests**: Specific test cases for previously-fixed bugs (BUG-001 through BUG-008)
3. **Event coverage tests**: Document which Gateway events are implemented vs official Discord events

### Adding New Tests

1. Add `Arbitrary` instances to `test/Discord/Types/Arbitrary.hs`
2. Create a spec file in `test/Discord/Types/`
3. Import the spec in `test/Spec.hs`
4. Add the module to `other-modules` in `discord-haskell.cabal` under `test-suite`

### Test Dependencies

Located in `discord-haskell.cabal` under `test-suite discord-haskell-tests`:
- `hspec` - Test framework
- `QuickCheck` - Property-based testing
- `quickcheck-instances` - Arbitrary instances for common types (Text, UTCTime)

## Code Style

### JSON Serialization Pattern

The library uses custom helpers for optional JSON fields:

```haskell
import Discord.Internal.Types.Prelude ((.==), (.=?), objectFromMaybes)

instance ToJSON Foo where
  toJSON Foo{..} = objectFromMaybes
    [ "required_field" .== requiredField    -- Always included
    , "optional_field" .=? optionalField    -- Only if Just
    ]
```

### Parser Safety

Always use `fail` instead of `error` in `FromJSON` instances for graceful error handling:

```haskell
-- Good: returns Nothing on invalid input
_ -> fail $ "Unknown type: " <> show t

-- Bad: crashes the program
_ -> error "unreachable"
```

## GHC Versions

The library supports GHC 8.10.7 through 9.12 (see `tested-with` in cabal file). The current Stack resolver (`lts-24.0`) uses GHC 9.10.2.

## Compiler Warnings

The `warnings` common stanza enables strict warnings:
- `-Wall`
- `-Wincomplete-uni-patterns`
- `-Wincomplete-record-updates`

All library code, executables, and tests import this stanza.

## JSON Roundtrip Test Coverage

### Tested Types (with property-based roundtrip tests)

**User.hs**: User, GuildMember
**Channel.hs**: Overwrite
**ScheduledEvents.hs**: ScheduledEvent, ScheduledEventPrivacyLevel, ScheduledEventStatus
**Embed.hs**: Embed, EmbedFooter, EmbedImage, EmbedThumbnail, EmbedVideo, EmbedProvider, EmbedAuthor, EmbedField
**AutoModeration.hs**: AutoModerationRule, AutoModerationRuleEventType, AutoModerationRuleTriggerType, AutoModerationRuleTriggerMetadata, AutoModerationRuleTriggerMetadataPreset, AutoModerationRuleAction, AutoModerationRuleActionType, AutoModerationRuleActionMetadata
**Color.hs**: DiscordColor (RGB only - named colors don't roundtrip)

### Untested Types (have both ToJSON and FromJSON)

**Channel.hs** (14 types):
- Simple: Attachment, Nonce, MessageReference, MessageActivity, MessageReaction, MessageFlags, MessageInteraction, ThreadMetadata, ThreadMember, ThreadMemberUpdateFields
- Enums: MessageType, MessageActivityType
- Complex: Channel, Message

**Components.hs** (7 types):
- Simple: SelectOption, TextInput
- Enum: ButtonStyle
- Complex: ActionRow, Button, SelectMenu, SelectMenuData

**Emoji.hs** (3 types):
- Simple: Emoji, StickerItem
- Enum: StickerFormatType

**ApplicationCommands.hs** (6 types):
- Simple: GuildApplicationCommandPermissions, ApplicationCommandPermissions
- Complex: Options, OptionSubcommandOrGroup, OptionSubcommand, OptionValue

**ScheduledEvents.hs** (4 types):
- Enum: ScheduledEventType
- Complex: CreateScheduledEventImage, CreateScheduledEventData, ModifyScheduledEventData

**Guild.hs** (2 types):
- Complex: Activity, GuildWidget

**Prelude.hs** (2 types):
- Simple: Snowflake
- Enum: ChannelTypeOption

**Events.hs** (1 type):
- Complex: AutoModerationActionExecuteInfo

**Gateway.hs** (1 type):
- Complex: GatewaySendable

**Interactions.hs** (1 type):
- Complex: ResolvedData

## Gateway Event Coverage

The library implements 43 of 75 official Discord Gateway events (57%). Missing events fall through to `UnknownEvent` and don't cause crashes.

**Tested in**: `test/Discord/Types/EventsSpec.hs`

The test verifies:
1. Implemented events are documented correctly
2. Event names match official Discord API (no typos)
