{-# LANGUAGE DataKinds #-}
module Auth where

import           Control.Monad.IO.Class    (liftIO)
import           Crypto.Scrypt             (Pass (..), encryptPass',
                                            getEncryptedPass, newSalt)
import qualified Data.ByteString.Lazy      as BS
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Time.Calendar        (fromGregorian)
import           Data.Time.Clock           (NominalDiffTime, UTCTime (..),
                                            addUTCTime, diffUTCTime,
                                            getCurrentTime, secondsToDiffTime,
                                            secondsToDiffTime)
import           Debug.Trace
import           Models
import qualified Modules                   as M
import           Network.HTTP.Types.Status
import           Web.Cookie                (parseCookiesText)
import           Web.JWT
import           Web.Scotty                (ActionM, param, redirect, setHeader,
                                            status, text)

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

sessionDuration :: NominalDiffTime
sessionDuration = fromInteger $ 60 * 60 * 60

register :: M.AuthModule -> ActionM ()
register am = do
  email <- param "email" :: ActionM Text
  pw    <- param "pass" :: ActionM Text
  salt  <- liftIO newSalt
  let salted = encryptPass' salt (Pass $ encodeUtf8 pw)
  let userfields = UserFields email pw
  result <- liftIO $ M.registerUser am userfields
  status created201
  text $ traceShowId $ TL.decodeUtf8 $ BS.fromStrict $ getEncryptedPass salted
  return ()

login :: M.AuthModule -> ActionM ()
login am = do
  email <- param "email" :: ActionM Text
  pw    <- param "pass" :: ActionM Text
  r <- liftIO $ M.loginUser am $ UserFields email pw
  case r of
    Just _ -> authorize email
    Nothing -> status unauthorized401
  -- if pw == "passw0rd"
  --   then authorize email
  --   else status unauthorized401
  return ()

key :: Secret
key = secret "all your base are belong to us"

unauthorize :: ActionM ()
unauthorize =
  setHeader "Set-Cookie"
    "session=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT"

authorize :: Text -> ActionM ()
authorize username = do
  now <- liftIO getCurrentTime
  let jwtNbf = intDate 0
  let jwtExp = intDate $ diffUTCTime now epoch + sessionDuration
  let cs = def {
          sub = stringOrURI username
        , nbf = jwtNbf
        , Web.JWT.exp = jwtExp }
  let jwt = encodeSigned HS256 key cs
  setHeader "Set-Cookie" $ "session=" <>
    TL.fromStrict jwt <> "; path=/; HttpOnly"
  redirect "/main"

isCurrentlyValid :: JWT VerifiedJWT -> UTCTime -> Bool
isCurrentlyValid tkn now =
  let cl = claims tkn
      toUTC diff = addUTCTime (secondsSinceEpoch diff) epoch
      notBefore  = (now >=) . toUTC <$> nbf cl
      notExpired = (now <=) . toUTC <$> Web.JWT.exp cl
      valid = (&&) <$> notBefore <*> notExpired
  in Just True == valid

isAuthorized :: UTCTime -> TL.Text -> Bool
isAuthorized now cookie =
  let bs = BS.toStrict $ TL.encodeUtf8 cookie
      cs = parseCookiesText bs
      token = lookup "session" cs
  in case token of
      Nothing -> False
      Just val ->
        let mJwt = decodeAndVerifySignature key val
        in maybe False (`isCurrentlyValid` now) mJwt
