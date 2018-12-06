module AuthClient where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (logShow)
import Simple.Ajax (HTTPError, AjaxError, get, post, post_)

type Empty = {}

type UserRequest = {username :: String, password :: String}
type User = {username :: String, userId :: String}

baseUrl :: String
baseUrl = "http://localhost:8080"

url :: String -> String
url = (<>) baseUrl

registerUser :: UserRequest -> Aff (Either HTTPError Unit)
registerUser ur = post_ (url "/users/user") (Just ur)

loginUser :: UserRequest -> Aff (Either HTTPError Unit)
loginUser ur = post_ (url "/users/login") (Just ur)

getUser :: String -> Aff (Either AjaxError User)
getUser username = get (url "/users/user/" <> username)

run :: Aff Unit
run = do 
    res <- registerUser {username : "pattersonzak2@gmail.com", password : "test" }
    case res of
        Right (_) -> do
            logShow "WORKED?"
            logShow res
        Left err ->
            logShow err *> logShow res


