{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}

module Commands
    ( ArgParser
    , parseArgs
    , arg
    , optArg
    , restArg
    , optRestArg
    , multiArg
    , optMultiArg
    , int
    , num
    , str
    , defaultArgErrorText
    , defaultParseErrorText
    , defaultHelpText
    , handleCommand
    , customRestArg
    ) where

import           Control.Monad                  ( filterM )
import           Data.Char                      ( isSpace )
import           Data.Foldable                  ( find )
import           Data.Maybe
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Discord                       as D
import qualified Discord.Internal.Types        as D
import           DiscordHelper                  ( isCommand
                                                , replyTo
                                                )
import qualified Text.Parsec                   as P
import           Text.Parsec                    ( (<?>)
                                                , ParseError
                                                )
import qualified Text.Parsec.Number            as P
import           Text.Parsec.Text               ( Parser )

data ArgArity = Single | Multi
data ArgReq = Required | Optional
type ArgType = (ArgReq, ArgArity)
data Arg = Arg
    { argType        :: ArgType
    , argName        :: Text
    , argDescription :: Text
    }
data ArgParser a = ArgParser
    { argParserArgs :: [Arg]
    , argParser     :: Parser a
    }
    deriving Functor
type CommandArgInfo = [Arg]

instance Applicative ArgParser where
    pure a = ArgParser [] (pure a)
    ArgParser { argParserArgs = args1, argParser = p1 } <*> ArgParser { argParserArgs = args2, argParser = p2 }
        = ArgParser { argParserArgs = args1 <> args2, argParser = p1 <*> p2 }

type CommandName = Text
type HelpText = Text

arg :: Text -> Text -> Parser a -> ArgParser a
arg name desc p = ArgParser
    [ Arg { argType        = (Required, Single)
          , argName        = name
          , argDescription = desc
          }
    ]
    (P.spaces *> p)

optArg :: Text -> Text -> Parser a -> ArgParser (Maybe a)
optArg name desc p = ArgParser
    [ Arg { argType        = (Optional, Single)
          , argName        = name
          , argDescription = desc
          }
    ]
    (P.spaces *> P.optionMaybe p)

restArg :: Text -> Text -> ArgParser Text
restArg name desc = ArgParser
    [Arg { argType = (Required, Multi), argName = name, argDescription = desc }]
    (P.spaces *> restStr1)

optRestArg :: Text -> Text -> ArgParser (Maybe Text)
optRestArg name desc = ArgParser
    [Arg { argType = (Optional, Multi), argName = name, argDescription = desc }]
    (P.spaces *> P.optionMaybe restStr1)

customRestArg :: Text -> Text -> Parser a -> ArgParser a
customRestArg name desc p = ArgParser
    [Arg { argType = (Required, Multi), argName = name, argDescription = desc }]
    (P.spaces *> p)

multiArg :: Text -> Text -> Parser a -> ArgParser [a]
multiArg name desc p = ArgParser
    [Arg { argType = (Required, Multi), argName = name, argDescription = desc }]
    (P.spaces *> manyArgs1 p)

optMultiArg :: Text -> Text -> Parser a -> ArgParser (Maybe [a])
optMultiArg name desc p = ArgParser
    [Arg { argType = (Optional, Multi), argName = name, argDescription = desc }]
    (P.spaces *> P.optionMaybe (manyArgs1 p))

restStr1 :: Parser Text
restStr1 = T.pack <$> (P.spaces *> P.many1 P.anyChar)

int :: Integral a => Parser a
int = (P.spaces *> P.int) <?> "integer"

num :: Floating a => Parser a
num = (P.spaces *> P.floating3 False) <?> "number"

str :: Parser Text
str = T.pack <$> (P.spaces *> P.many1 (P.satisfy (not . isSpace))) <?> "string"

manyArgs1 :: Parser a -> Parser [a]
manyArgs1 p = P.many1 (P.spaces *> p)

parseArgs
    :: Text
    -> (command -> CommandName)
    -> (command -> ArgParser a)
    -> command
    -> Text
    -> Either ParseError a
parseArgs prefix commandName commandArgs command = P.parse
    (  P.string (T.unpack prefix)
    *> P.string (T.unpack $ commandName command)
    *> argParser (commandArgs command)
    <* P.eof
    )
    ""

showUsage :: Text -> Text -> ArgParser a -> Text
showUsage prefix name args =
    "Usage: " <> prefix <> name <> argStr (argParserArgs args)

defaultHelpText
    :: Text
    -> (command -> CommandName)
    -> (command -> HelpText)
    -> (command -> ArgParser a)
    -> Maybe Text
    -> [command]
    -> Text
defaultHelpText prefix commandName commandHelpText commandArgs mcommand commands
    = case mcommand of
        Just commandStr ->
            case find ((== commandStr) . commandName) commands of
                Nothing -> "Command not found: **" <> commandStr <> "**"
                Just command ->
                    let name = commandName command
                        args = commandArgs command
                    in  "```\n"
                            <> commandStr
                            <> " help\n-----------------------------\n"
                            <> commandHelpText command
                            <> "\n\n"
                            <> showUsage prefix name args
                            <> case argParserArgs args of
                                   [] -> ""
                                   argInfo ->
                                       "\n\n"
                                           <> (T.intercalate "\n" . map
                                                  (\Arg { argName, argDescription } ->
                                                      let arg' = T.toUpper
                                                              argName
                                                      in  " "
                                                              <> arg'
                                                              <> " - "
                                                              <> argDescription
                                                  )
                                              )
                                                  argInfo
                            <> "\n```"
        Nothing ->
            "```\nCommands (prefix with "
                <> prefix
                <> ")\n-----------------------------\n"
                <> T.intercalate
                       "\n"
                       (map
                           (\command ->
                               commandName command
                                   <> argStr
                                          (argParserArgs $ commandArgs command)
                                   <> " - "
                                   <> commandHelpText command
                           )
                           commands
                       )
                <> "\n```"

argStr :: CommandArgInfo -> Text
argStr argInfoArgs = mconcat $ map
    (\Arg { argType, argName } -> " " <> modifyArg argType (T.toUpper argName))
    argInfoArgs

modifyArg :: (Semigroup a, IsString a) => (ArgReq, ArgArity) -> a -> a
modifyArg (r, s) arg0 =
    let arg1 = case s of
            Single -> arg0
            Multi  -> arg0 <> "..."
    in  case r of
            Required -> arg1
            Optional -> "[" <> arg1 <> "]"

type ErrorHandler command a
    =  Text
    -> (command -> CommandName)
    -> (command -> ArgParser a)
    -> command
    -> Text
    -> ParseError
    -> Text

defaultParseErrorText :: Text -> ParseError -> Text
defaultParseErrorText message e =
    message
        <> "\n"
        <> T.pack (replicate (P.sourceColumn (P.errorPos e) - 1) ' ')
        <> "^\n\n"
        <> T.pack (show e)

defaultArgErrorText :: ErrorHandler command a
defaultArgErrorText prefix commandName commandArgs command message e =
    "```\nInvalid args:\n\n"
        <> defaultParseErrorText message e
        <> "\n\n"
        <> showUsage prefix (commandName command) (commandArgs command)
        <> "\n```"

handleCommand
    :: Text
    -> (command -> Text)
    -> (command -> ArgParser a)
    -> (a -> D.Message -> D.DiscordHandler ())
    -> Maybe (ErrorHandler command a)
    -> D.Message
    -> [command]
    -> D.DiscordHandler Bool
handleCommand prefix commandName commandArgs commandHandler errorHandler message commands
    = do
        commandMatches <- filterM
            (\command -> isCommand prefix (commandName command) message)
            commands
        case commandMatches of
            (command : _) -> do
                case
                        parseArgs prefix
                                  commandName
                                  commandArgs
                                  command
                                  (D.messageText message)
                    of
                        Left e -> replyTo message $ fromMaybe
                            defaultArgErrorText
                            errorHandler
                            prefix
                            commandName
                            commandArgs
                            command
                            (D.messageText message)
                            e
                        Right cas -> commandHandler cas message
                pure True
            _ -> pure False

