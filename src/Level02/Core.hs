{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestBody, requestMethod,
                                           responseLBS, strictRequestBody)

import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType (PlainText), Error (EmptyCommentText, EmptyTopicText, HTTPError),
                                           RqType (AddRq, ListRq, ViewRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status ct =
  responseLBS status [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404


resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400


-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t bs =
  AddRq <$> topic <*> comment
  where
    topic = mkTopic t
    comment = mkCommentText $ lazyByteStringToStrictText bs
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t = ViewRq <$> mkTopic t


mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq


mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopicText = resp404 PlainText "Error: Empty Topic Text"
mkErrorResponse EmptyCommentText = resp404 PlainText "Error: Empty Comment Text"
mkErrorResponse HTTPError = resp404 PlainText "Error: HTTP ERROR"


-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  let
    method = requestMethod rq
    path = pathInfo rq

  in do
    body <- strictRequestBody rq
    pure $ case (method, path) of
             ("POST", [t, "add"]) ->  mkAddRequest t body
             ("GET", [t, "view"]) -> mkViewRequest t
             ("GET", ["list"])    ->  mkListRequest
             _                    -> Left HTTPError

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp404 PlainText "Add not implemnted"
handleRequest (ViewRq _)  = Right $ resp404 PlainText "View not implemented"
handleRequest (ListRq)    = Right $ resp404 PlainText "List not implemented"


-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app rq cb = do
  res <- mkRequest rq
  cb $ weirdExp res
  where
    weirdExp = handleRespErr . handleRErr
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id
    handleRErr :: Either Error RqType -> Either Error Response
    handleRErr = either Left handleRequest



runApp :: IO ()
runApp = run 3000 app
