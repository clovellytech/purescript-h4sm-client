module Client.Headers where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseHeader (ResponseHeader, responseHeaderName, responseHeaderValue)
import Data.Array (filter)
import Data.Set (Set, member)

convertHeaders :: ResponseHeader -> RequestHeader
convertHeaders h = RequestHeader (responseHeaderName h) (responseHeaderValue h)

convertHeadersFilter :: (Set String) -> (Array ResponseHeader) -> (Array RequestHeader)
convertHeadersFilter names hs = convertHeaders <$> filter (\h -> member (responseHeaderName h) names) hs
