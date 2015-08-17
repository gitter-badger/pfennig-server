{-# LANGUAGE QuasiQuotes #-}

module Authentication where

import           App
import           Control.Error.Safe  (justErr)
import           Control.Monad       (join)
import           Data.Bifunctor      (bimap)
import           Data.Text
import qualified Data.Text           as T
import           Data.Time.LocalTime (LocalTime)
import qualified Hasql               as H
import qualified Hasql.Postgres      as HP
import           Models
import qualified Modules             as M
import           Queries

makeModule :: AppConfig -> M.AuthModule
makeModule (AppConfig session) = M.Auth {
    M.registerUser = registerUser session
  , M.loginUser = loginUser session }

registerUser :: PostgresSession -> UserFields
             -> IO (Either T.Text User)
registerUser session userFields = do
  newUserRow <- session $ H.tx Nothing $ insertNewUser userFields
  return $ extractUser newUserRow
  where extractUser = join . bimap unpackSessionError (justErr "not found")

insertNewUser :: UserFields -> H.Tx HP.Postgres s (Maybe User)
insertNewUser (UserFields email password) = do
  newUser <- H.maybeEx $
             [H.stmt|insert into users (login, email, password, salt)
                     values (?, ?, ?, ?)
                     returning * |] email email password password
  return $ rowToUser <$> newUser

rowToUser :: (Int, LocalTime, LocalTime, Text, Text) -> User
rowToUser (i, created, updated, x, y) = User (UserId i) created updated x y

loginUser :: PostgresSession -> UserFields -> IO (Maybe User)
loginUser _ _ = return $ Just u
  where u = User uid' cat' uat' log' ema'
        uid' = UserId 1
        cat' = undefined
        uat' = undefined
        log' = "User"
        ema' = "user@this.me"
