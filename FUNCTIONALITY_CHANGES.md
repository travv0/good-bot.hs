# Functionality Changes Since Commit 6549f72

## Overview
This document summarizes the changes between commit 6549f72 ("switch to megaparsec") and the current state of the codebase.

## Commands

### Removed Commands
1. **Sum Command** - Removed before commit 6549f72 (commit 3804d4b)
   - Previously allowed summing multiple numbers with `!sum <num1> <num2> ...`
   - Functionality replaced by the more powerful `calc` command

### Modified Commands
1. **Calc Command**
   - Added command aliases: `calculate` and `math`
   - Now properly handles error cases:
     - Division by zero returns "Error: Division by zero"
     - Invalid operations return descriptive error messages
   - Fixed double factorial operator (`!!`) which was previously mapped to single factorial

2. **RR (Russian Roulette) Command**
   - Added alias: `roulette`

3. **Meanness Command**
   - Range extended from 0-10 to 0-11

## Architecture Changes

### New Modules
1. **ApiClient.hs** - API client functionality extracted from Lib.hs
2. **CommandHandlers.hs** - Command handling logic extracted from Lib.hs
3. **Config.hs** - Configuration handling extracted from Lib.hs
4. **Database.hs** - Database operations extracted from Lib.hs
5. **RateLimit.hs** - New rate limiting functionality for commands
6. **Types.hs** - Common types, including MeannessLevel with validation

### Modified Modules
1. **Commands.hs**
   - Added `handleCommandWithAliases` function to support command aliases
   - Changed message access from `messageText` to `messageContent`

2. **Calculator.hs**
   - Added proper error handling with `CalcError` type
   - Fixed double factorial operator parsing order (now correctly before single factorial)
   - Calculator now returns `Either CalcError Double` instead of just `Double`

3. **Lib.hs**
   - Significantly refactored for better modularity
   - Added rate limiting for commands
   - Moved most command logic to CommandHandlers module

## New Features

### Rate Limiting
- Commands are now rate-limited to prevent spam
- Users who send commands too quickly receive: "You're sending commands too quickly! Please slow down."
- Implemented in the new RateLimit module

### Command Aliases
- Commands can now have multiple names/aliases
- Calc: `calc`, `calculate`, `math`
- RR: `rr`, `roulette`

### Better Error Handling
- Calculator properly handles division by zero and modulo by zero
- Error messages are user-friendly

## Bug Fixes
1. Fixed double factorial operator (`!!`) in calculator
2. Fixed Discord.Types import issues for ActivityType (commit bb70a1f)
3. Improved error handling throughout the application

## Testing
- Added comprehensive test suite:
  - CalculatorSpec.hs - Tests for calculator functionality
  - TypesSpec.hs - Tests for type validation (MeannessLevel)
  - All tests passing

## Configuration
- Added .env.example file for environment configuration template
- Configuration handling is now more robust with proper error messages