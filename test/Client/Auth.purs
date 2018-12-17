module Test.Client.Auth where
  
import Prelude

import Client.Auth (UserRequest, deleteUser, getAuthedUser, isTest, loginUser, registerUser, userExists)
import Client.Headers (convertHeadersFilter)
import Data.Date (year, month)
import Data.DateTime (date)
import Data.Either (Either(..), isRight)
import Data.List (List(..), (:))
import Data.Newtype (unwrap)
import Data.Set (singleton)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Now (nowDate)
import Test.Assert (assert')
import Type.Data.Boolean (kind Boolean)

user1 :: UserRequest
user1 = {username : "attempt1", password : "attempt2"}

testIsTest :: Aff Unit
testIsTest = do 
  res <- isTest
  case res of 
    Right (testMode) -> liftEffect (assert' "Not in test mode" testMode)
    Left (err) -> do
      logShow "It appears test mode is not enabled on the server."
      logShow err
      liftEffect $ assert' "Error querying testmode, route must not be enabled." false

testRegister :: Aff Unit
testRegister = do
  res <- registerUser user1
  _ <- deleteUser user1.username
  liftEffect $ assert' "Register failed" (isRight res)

testLogin :: Aff Unit
testLogin = do
  _ <- registerUser user1
  res <- loginUser user1
  _ <- deleteUser user1.username
  liftEffect $ assert' "Login failed" (isRight res)

testGetAuthedUser :: Aff Unit
testGetAuthedUser = do
  r1 <- registerUser user1
  r2 <- loginUser user1
  hs <- case r2 of
          Left e -> liftEffect $ assert' "User login failed" false $> []
          Right (Tuple hs _) -> pure hs
  let newhs = convertHeadersFilter (singleton "authorization") hs
  res <- getAuthedUser (convertHeadersFilter (singleton "authorization") hs)
  _ <- deleteUser user1.username
  now <- liftEffect nowDate
  case res of
    Right (userData) -> do 
      let joinDate = date $ unwrap userData.joinTime
      liftEffect $ assert' "Date wrong" ((year now) == (year joinDate) && (month now) == (month joinDate))
      liftEffect $ assert' "username not matching" (userData.username == user1.username)
    Left (e) -> do
      logShow e
      liftEffect $ assert' "User data not read" false

testGetUserNotExists :: Aff Unit
testGetUserNotExists = do 
  r1 <- userExists user1.username
  case r1 of 
    Right (exists) -> liftEffect $ assert' "Server says user exists when it shouldn't"  (not exists)
    Left (e) -> do 
      logShow e
      liftEffect $ assert' "User exist response wrong" false

testGetUserExists :: Aff Unit 
testGetUserExists = do
  _ <- registerUser user1
  r2 <- userExists user1.username
  _ <- deleteUser user1.username
  case r2 of 
    Right (exists) -> liftEffect $ assert' "Server says user doesn't exist when it should" exists
    Left (e) -> do 
      logShow e
      liftEffect $ assert' "User exist response wrong" false

tests :: List (Aff Unit)
tests = testIsTest : 
  testRegister : 
  testLogin : 
  testGetAuthedUser : 
  testGetUserNotExists : 
  testGetUserExists : 
  Nil
