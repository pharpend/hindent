{-# LANGUAGE OverloadedStrings #-}

-- | User configuration

module HIndent.UserConfig where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import HIndent.Styles.ChrisDone (chrisDone)
import HIndent.Styles.Fundamental (fundamental)
import HIndent.Styles.Gibiansky (gibiansky)
import HIndent.Styles.JohanTibell (johanTibell)
import HIndent.Types

-- 
-- for reference
-- 
-- data Style =
--   forall s. Style {styleName :: !Text -- ^ Name of the style, used in the commandline interface.
--                   ,styleAuthor :: !Text -- ^ Author of the printer (as opposed to the author of the style).
--                   ,styleDescription :: !Text -- ^ Description of the style.
--                   ,styleInitialState :: !s -- ^ User state, if needed.
--                   ,styleExtenders :: ![Extender s] -- ^ Extenders to the printer.
--                   ,styleDefConfig :: !Config -- ^ Default config to use for this style.
--                   }

-- -- | Configurations shared among the different styles. Styles may pay
-- -- attention to or completely disregard this configuration.
-- data Config =
--   Config {configMaxColumns :: !Int64 -- ^ Maximum columns to fit code into ideally.
--          ,configIndentSpaces :: !Int64 -- ^ How many spaces to indent?
--          ,configClearEmptyLines :: !Bool  -- ^ Remove spaces on lines that are otherwise empty?
--          }
-- 

-- |The type we will extract from the YAML file
data UserConfig =
  UserConfig {ucStyle :: !Style
             ,ucConfig :: !Config}

instance FromJSON UserConfig where
  parseJSON (Object o) =
    do ustyl <- o .:? "style" .!= fundamental
       ucols <- o .:? "max-columns" .!=
                (configMaxColumns . styleDefConfig $ ustyl)
       uidnt <- o .:? "indent-spaces" .!=
                (configIndentSpaces . styleDefConfig $ ustyl)
       uclrl <- o .:? "clear-empty-lines" .!=
                (configClearEmptyLines . styleDefConfig $ ustyl)
       let uconf = Config ucols uidnt uclrl
       return $ UserConfig ustyl uconf
  parseJSON _ = fail "User config must be a JSON object."
  
instance FromJSON Style where
  parseJSON (String s) =
    return $
    case alpha s of
      "chrisdone" -> chrisDone
      "gibiansky" -> gibiansky
      "johantibell" -> johanTibell
      _ -> fundamental
    where alpha :: Text -> Text
          alpha =
            T.filter (`elem` ['a' .. 'z']) .
            T.toLower
  parseJSON _ = return fundamental
