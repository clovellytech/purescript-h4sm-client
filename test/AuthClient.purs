module Test.AuthClient where
  
import Data.List
import Prelude
import Test.Assert

import AuthClient (registerUser, UserRequest)
import Data.Either (Either(..), isRight)
import Data.List
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Type.Data.Boolean (kind Boolean)

user1 :: UserRequest
user1 = {username : "attempt1", "password" : "attempt2"}

testRegister :: Aff Unit
testRegister = do
    res <- registerUser user1
    liftEffect (assert' "Register failed" (isRight res))

testLogin :: Aff Unit
testLogin = do
    registerUser user1
    res <- loginUser user1
    liftEffect (assert' "Login failed" (isRight res))

testGetUser :: Aff Unit
testGetUser = do
    registerUser user1
    loginUser user1
    res <- getUser user1
    liftEffect (assert' "Get user data failed" (isRight res))

tests :: List (Aff Unit)
tests = testRegister : Nil
