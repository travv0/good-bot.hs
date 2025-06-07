{-# LANGUAGE OverloadedStrings #-}

module ApiClient
    ( Definition(..)
    , getDefineOutput
    , getUrbanOutput
    ) where

import           Control.Lens                   ( (&)
                                                , (.~)
                                                , view
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON
                                                , eitherDecode
                                                , parseJSON
                                                , withObject
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Discord                       as D
import           DiscordHelper                  ( writeError )
import           Network.Wreq                   ( Response
                                                , defaults
                                                , get
                                                , getWith
                                                , header
                                                , param
                                                , responseBody
                                                )
import qualified Network.Wreq                  as W

data Definition = Definition
    { defPartOfSpeech :: Maybe Text
    , defDefinitions  :: [Text]
    }
    deriving Show

instance FromJSON Definition where
    parseJSON = withObject "Definition" $ \v -> do
        partOfSpeech <- v .: "fl"
        definitions  <- v .: "shortdef"
        pure Definition { defPartOfSpeech = partOfSpeech
                        , defDefinitions  = definitions
                        }

newtype UrbanDefinition = UrbanDefinition {urbanDefDefinition :: [Text]}
    deriving (Show)

instance FromJSON UrbanDefinition where
    parseJSON = withObject "UrbanDefinition" $ \v -> do
        list <- v .: "list"
        defs <- traverse (.: "definition") list
        pure UrbanDefinition { urbanDefDefinition = defs }

type App a = ReaderT AppContext D.DiscordHandler a

data AppContext = AppContext
    { appDictKey  :: Maybe Text
    , appUrbanKey :: Maybe Text
    }

buildDefineOutput :: Text -> Definition -> Text
buildDefineOutput word definition =
    let definitions = case defDefinitions definition of
            [def] -> def
            defs  -> T.intercalate "\n\n" $ zipWith
                (\i def -> T.pack (show i) <> ". " <> def)
                [1 :: Int ..]
                defs
    in  "**"
            <> word
            <> "**"
            <> (case defPartOfSpeech definition of
                   Just partOfSpeech -> " *" <> partOfSpeech <> "*"
                   Nothing           -> ""
               )
            <> "\n"
            <> definitions

getDefineOutput :: Maybe Text -> Text -> D.DiscordHandler (Maybe Text)
getDefineOutput dictKey word = do
    response <- getDictionaryResponse dictKey word
    buildDefineOutputHandleFail
            word
            (response >>= eitherDecode . view responseBody)
        $ Just
        $ getUrbanOutput Nothing word

getUrbanOutput :: Maybe Text -> Text -> D.DiscordHandler (Maybe Text)
getUrbanOutput urbanKey word = do
    urbanResponse <- getUrbanResponse urbanKey word
    buildDefineOutputHandleFail
        word
        (urbanResponse >>= decodeUrban . view responseBody)
        Nothing

buildDefineOutputHandleFail
    :: Text
    -> Either String [Definition]
    -> Maybe (D.DiscordHandler (Maybe Text))
    -> D.DiscordHandler (Maybe Text)
buildDefineOutputHandleFail word (Right defs) _ | not (null defs) =
    pure $ Just $ T.intercalate "\n\n" $ map (buildDefineOutput word) defs
buildDefineOutputHandleFail _ (Left err) Nothing =
    writeError (T.pack err) >> pure Nothing
buildDefineOutputHandleFail _ (Left err) (Just fallback) =
    writeError (T.pack err) >> fallback
buildDefineOutputHandleFail _ _         (Just fallback) = fallback
buildDefineOutputHandleFail _ (Right _) Nothing         = pure Nothing

getDictionaryResponse :: Maybe Text -> Text -> D.DiscordHandler (Either String (Response BSL.ByteString))
getDictionaryResponse Nothing _ = pure $ Left "no dictionary.com api key set"
getDictionaryResponse (Just apiKey) word =
    liftIO
        $   fmap Right
        <$> get
        $   T.unpack
        $ "https://dictionaryapi.com/api/v3/references/collegiate/json/"
        <>  word
        <>  "?key="
        <>  apiKey

getUrbanResponse :: Maybe Text -> Text -> D.DiscordHandler (Either String (Response BSL.ByteString))
getUrbanResponse Nothing _ = pure $ Left "no urban dictionary api key set"
getUrbanResponse (Just apiKey) word = liftIO $ Right <$> getWith
    (urbanOpts apiKey word)
    "https://mashape-community-urban-dictionary.p.rapidapi.com/define"

urbanOpts :: Text -> Text -> W.Options
urbanOpts apiKey term =
    defaults
        &  header "x-rapidapi-key"
        .~ [T.encodeUtf8 apiKey]
        &  header "x-rapidapi-host"
        .~ ["mashape-community-urban-dictionary.p.rapidapi.com"]
        &  header "useQueryString"
        .~ ["true"]
        &  param "term"
        .~ [term]

decodeUrban :: BSL.ByteString -> Either String [Definition]
decodeUrban = fmap urbanToDictionary . eitherDecode

urbanToDictionary :: UrbanDefinition -> [Definition]
urbanToDictionary (UrbanDefinition def) =
    [ Definition Nothing def | not (null def) ]