module Component.Helpers where

import Prelude

import Data.Array (filter, intercalate)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Web.HTML.Common (ClassName(..))

classList :: Array (Tuple String Boolean) -> ClassName
classList = ClassName <<< intercalate " " <<< map Tuple.fst <<< filter Tuple.snd