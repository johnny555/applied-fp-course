{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first, second, bimap)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM, liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f ioa = do
  a <- liftIO ioa
  liftEither (f a)

parseSqlErrors :: SQLiteResponse -> Error
parseSqlErrors a = DBError a


getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments fadb t =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    con = dbConn fadb
    topic = getTopic t
    query = Sql.query con sql [topic]
    dbAction = Sql.runDBAction query
    trav = traverse fromDbComment
    pErrors x = first parseSqlErrors x
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
  in do
    res <- runDB (first parseSqlErrors) dbAction
    liftEither (trav res)
    --queryResult <- Sql.runDBAction $ Sql.query con sql [topic]
    --convertedErrors <- pure $ first parseSqlErrors queryResult
    --pure $ (traverse fromDbComment) =<< convertedErrors

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic fadb topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    con = dbConn fadb

  in do
    time <- liftIO  getCurrentTime
    let
      dbAction =  Sql.runDBAction $
        Sql.execute con sql (getTopic topic, getCommentText comment, time)
    runDB (first parseSqlErrors) dbAction


instance Sql.FromRow Text  where
  fromRow = Text.pack <$> Sql.field

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics fadb =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    con = dbConn fadb
    query = Sql.query_ con sql :: IO [Text.Text]
    dbAction = Sql.runDBAction query
  in do
    res <- runDB (first parseSqlErrors) dbAction
    liftEither $ (traverse mkTopic) res

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic fadb topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    con = dbConn fadb
    query = Sql.execute con sql [getTopic topic]
    dbAction = Sql.runDBAction query
  in do
    runDB (first parseSqlErrors) dbAction
