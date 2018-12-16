module Client.Auth where

import Prelude

import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseHeader (ResponseHeader)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import DateTimeParsed (DateTimeParsed)
import Effect.Aff (Aff)
import Simple.Ajax (HTTPError, AjaxError, get, getR, post_, postH_, delete_)

type Empty = {}

type UserRequest = {username :: String, password :: String}
type User = {username :: String, userId :: String}
type UserDetail = {username :: String, joinTime :: DateTimeParsed}

type UserSession = {token :: String}

baseUrl :: String
baseUrl = "http://127.0.0.1:8080"

url :: String -> String
url = (<>) baseUrl

isTest :: Aff (Either AjaxError Boolean)
isTest = get $ url "/users/istest"

registerUser :: UserRequest -> Aff (Either HTTPError Unit)
registerUser ur = post_ (url "/users/user") (Just ur)

loginUser :: UserRequest -> Aff (Either HTTPError (Tuple (Array ResponseHeader) Unit))
loginUser ur = postH_ (url "/users/login") (Just ur)

getUser :: (Array RequestHeader) -> String -> Aff (Either AjaxError UserDetail)
getUser headers username = getR {headers : headers} $ url ("/users/user/" <> username)

getAuthedUser :: (Array RequestHeader) -> Aff (Either AjaxError UserDetail)
getAuthedUser headers = getR {headers: headers} $ url "/users/user"

deleteUser :: String -> Aff (Either HTTPError Unit)
deleteUser username = delete_ $ url "/users/" <> username
