{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App
import           Auth
import qualified Authentication                as A
import           Control.Exception.Base        (bracket)
import           Control.Monad.IO.Class
import           Data.Maybe                    (fromMaybe)
import           Data.Time.Clock               (getCurrentTime)
import qualified Handlers
import qualified Hasql                         as H
import qualified Hasql.Postgres                as HP
import           Layout                        (readCSS)
import           Lucid
import           Migrations
import qualified Modules                       as M
import           Network.Wai.Middleware.Static (CacheContainer, CachingStrategy (PublicStaticCaching),
                                                hasPrefix, initCaching,
                                                staticPolicy')
import qualified Schema                        as S
import           View
import           Web.Scotty                    (ActionM, ScottyM, delete, get,
                                                header, middleware, post, raw,
                                                redirect, scotty, setHeader)

main :: IO ()
main = do
  let postgresSettings = HP.ParamSettings
                         "localhost"
                         5432
                         "pfennig"
                         "pfennig"
                         "pfennig"
  sessionSettings <- maybe (fail "Invalid settings") return $ H.poolSettings 6 3
  cache <- initCaching PublicStaticCaching

  bracket
    (H.acquirePool postgresSettings sessionSettings)
    H.releasePool
    (\pool -> do
        let session = H.session pool
        let cfg = AppConfig session
        runMigrations migrations pool
        scotty 3000 $ do
          setupMiddleware cache
          setupAssets
          setupViewRoutes
          setupAPIRoutes cfg)

migrations :: [H.Stmt HP.Postgres]
migrations = [ S.createUsers
             , S.createIntervals
             , S.createExpenditures
             , S.createTags
             , S.createExpendituresTags
             , S.createExpendituresIntervals]

lucid :: Html a -> ActionM ()
lucid h = do
  setHeader "Content-Type" "text/html"
  raw . renderBS  $ h

setupMiddleware :: CacheContainer -> ScottyM ()
setupMiddleware cache =
  middleware $ staticPolicy' cache $ hasPrefix "assets/"

setupAssets :: ScottyM ()
setupAssets =
  get "/assets/generated.css" $ Handlers.getCss readCSS

setupViewRoutes :: ScottyM ()
setupViewRoutes = do
  get "/" $ do
    now <- liftIO getCurrentTime
    cookie <- header "Cookie"
    if fromMaybe False $ isAuthorized now <$> cookie
      then redirect "/main"
      else lucid $ index View.login
  get "/register" $ lucid $ index View.register

setupAPIRoutes :: AppConfig -> ScottyM ()
setupAPIRoutes cfg = do
 -- expenditures
  get "/expenditure" $ Handlers.getExpenditures cfg
  get "/expenditure/:id" $ Handlers.getExpenditure cfg
  get "/expenditure/:start/:end" $ Handlers.getExpendituresBetween cfg
  post "/expenditure" $ Handlers.createExpenditure cfg
  -- post "/expenditure/:id" $ Handlers.updateExpenditure cfg
  delete "/expenditure/:id" $ Handlers.deleteExpenditure cfg

  -- auth
  post "/register" $ Auth.register authModule
  post "/login" $ Auth.login authModule
  get "/logout" $ do
    Auth.unauthorize
    redirect "/"

  get "/main" $ Handlers.main' cfg

  where authModule = A.makeModule cfg
