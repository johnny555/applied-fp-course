{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level05.Core
  ( runApp
  , app
  , prepareAppReqs
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy               as LBS

import           Data.Bifunctor                     (first)
import           Data.Either                        (either)
import           Data.Monoid                        ((<>))

import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A

import           Level05.AppM                       (AppM, liftEither, runAppM)
import qualified Level05.Conf                       as Conf
import qualified Level05.DB                         as DB
import           Level05.Types                      (ContentType (..),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  -- Load our configuration
  cfgE <- prepareAppReqs
  -- Loading the configuration can fail, so we have to take that into account now.
  case cfgE of
    Left err   -> print err
    Right cfg -> run 3000 (app cfg)

-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
--
-- Our application configuration is defined in Conf.hs
--
prepareAppReqs
  :: IO ( Either StartUpError DB.FirstAppDB )
prepareAppReqs =
  let
    filePath = Conf.dbFilePath Conf.firstAppConfig
  in
    first (DbInitErr) <$> DB.initDB filePath

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: ToJSON a
  => a
  -> Response
resp200Json =
  resp200 JSON . A.encode
-- |


-- How has this implementation changed, now that we have an AppM to handle the
-- errors for our application? Could it be simplified? Can it be changed at all?
app
  :: DB.FirstAppDB
  -> Application
app db rq cb = do
  let
    lastCheck = either mkErrorResponse id

  a <- runAppM $ do
             rq' <- mkRequest rq
             handleRequest db rq'
  resp <- pure $ lastCheck a
  cb resp
    {-
     <- handleRespErr <$> handleRErr rq'
    cb res    where
        handleRespErr :: Either Error Response -> Response
        handleRespErr = either mkErrorResponse id

        -- We want to pass the Database through to the handleRequest so it's
        -- aable to all of our handlers.
        handleRErr :: Either Error RqType -> IO (Either Error Response)
        handleRErr = either ( pure . Left ) ( handleRequest db )
-}


handleRequest
  :: DB.FirstAppDB
  -> RqType
  -> AppM Response
handleRequest db rqType = case rqType of
  -- Notice that we've been able to remove a layer of `fmap` because our `AppM`
  -- handles all of that for us. Such is the pleasant nature of these
  -- abstractions.
  AddRq t c -> resp200 PlainText "Success" <$ DB.addCommentToTopic db t c
  ViewRq t  -> resp200Json <$> DB.getComments db t
  ListRq    -> resp200Json <$> DB.getTopics db

mkRequest
  :: Request
  -> AppM RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO $ mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so build an Error response
    _                      -> pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _ ) =
  resp500 PlainText "Oh noes"
