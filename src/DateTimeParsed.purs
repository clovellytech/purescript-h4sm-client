module DateTimeParsed where 

import Prelude

import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (note)
import Data.JSDate (JSDate, toDateTime, parse)
import Data.List.NonEmpty as NE
import Data.Newtype (class Newtype, unwrap)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign, ForeignError(..), F, readString)
import Simple.JSON (class ReadForeign)

newtype DateTimeParsed = DateTimeParsed DateTime

readJsDate :: Foreign -> F JSDate
readJsDate s = transJsDate <$> (readString s)  where
  transJsDate :: String -> JSDate
  transJsDate d = unsafePerformEffect $ parse d

readDateTime :: Foreign -> F DateTimeParsed
readDateTime s = do
  let dt = readJsDate s
  let dto = toDateTime <$> dt
  let dte = dto >>= (\d -> except $ lmap (NE.singleton <<< ForeignError) (note "Could not convert to Purescript DateTime" d))
  DateTimeParsed <$> dte

instance readJsDateInst :: ReadForeign DateTimeParsed where
  readImpl = readDateTime

derive instance dateTimeParsedNt :: Newtype DateTimeParsed _

instance dateTimeShow :: Show DateTimeParsed where 
  show = unwrap >>> show
