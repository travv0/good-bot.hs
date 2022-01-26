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
    , str
    , defaultErrorText
    , defaultHelpText
    ) where

import           Data.Char                      ( isSpace )
import           Data.Foldable                  ( find )
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Text.Parsec                   as P
import           Text.Parsec                    ( ParseError )
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

multiArg :: Text -> Text -> Parser a -> ArgParser [a]
multiArg name desc p = ArgParser
    [Arg { argType = (Required, Multi), argName = name, argDescription = desc }]
    (P.spaces *> manyArgs1 p)

optMultiArg :: Text -> Text -> Parser a -> ArgParser (Maybe [a])
optMultiArg name desc p = ArgParser
    [Arg { argType = (Optional, Multi), argName = name, argDescription = desc }]
    (P.spaces *> P.optionMaybe (manyArgs1 p))

restStr1 :: Parser Text
restStr1 = do
    cs <- P.spaces *> P.many1 P.anyChar
    return $ T.pack cs

int :: Parser Int
int = do
    d <- P.spaces *> P.many1 P.digit
    return $ read d

str :: Parser Text
str = do
    cs <- P.spaces *> P.many1 (P.satisfy (not . isSpace))
    return $ T.pack cs

manyArgs1 :: Parser a -> Parser [a]
manyArgs1 p = P.many1 (P.spaces *> p)

parseArgs
    :: Text
    -> (command -> Text)
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
    -> (command -> Text)
    -> (command -> Text)
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

defaultErrorText
    :: Text
    -> (command -> Text)
    -> (command -> ArgParser a)
    -> command
    -> Text
    -> ParseError
    -> Text
defaultErrorText prefix commandName commandArgs command message e =
    "```\nInvalid args:\n\n"
        <> message
        <> "\n"
        <> T.pack (replicate (P.sourceColumn (P.errorPos e) - 1) ' ')
        <> "^\n\n"
        <> T.pack (show e)
        <> "\n\n"
        <> showUsage prefix (commandName command) (commandArgs command)
        <> "\n```"
