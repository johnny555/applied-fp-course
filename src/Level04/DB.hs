{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Bifunctor                     (first,second, bimap)

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)


import           Level04.Types                      (Comment, CommentText,
                                                     Error(..), Topic, fromDbComment,
                                                     getTopic, getCommentText, mkTopic)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB = Sql.close . dbConn


-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp =
  Sql.runDBAction$
  do
    con <- Sql.open fp
    Sql.execute_ con createTableQ
    pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
parseSqlErrors :: SQLiteResponse -> Error
parseSqlErrors _ = SqlDbError

getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments fadb topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"

  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
  in do
    queryResult <- Sql.runDBAction $ Sql.query (dbConn fadb) sql [getTopic topic]
    convertedErrors <- pure $ first parseSqlErrors queryResult
    pure $ (traverse fromDbComment) =<< convertedErrors



addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic fadb topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    con = dbConn fadb
  in do
    time <- getCurrentTime
    queryResult <- Sql.runDBAction $
      Sql.execute con sql (getTopic topic, getCommentText comment, time)
    pure $ first parseSqlErrors queryResult

instance Sql.FromRow Text  where
  fromRow = Text.pack <$> Sql.field

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics fadb =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    con = dbConn fadb
    query = Sql.query_ con sql :: IO [Text.Text]
  in do
    queryResult <- Sql.runDBAction $ query
    --pure $ bimap parseSqlErrors (_ (traverse mkTopic)) queryResult
    convertedErrors <- pure $ first parseSqlErrors queryResult
    pure $ (traverse mkTopic) =<< convertedErrors

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic fadb topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    con = dbConn fadb
    query = Sql.execute con sql [getTopic topic]
  in do
    qResult <- Sql.runDBAction query
    pure $ first parseSqlErrors qResult
