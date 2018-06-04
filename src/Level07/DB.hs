{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks, ask)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (AppM, Env (envDB), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn = do
  --env <-  ask
  --res <- pure $  dbConn . envDB  $ env
  --pure res
  asks (dbConn . envDB)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM b
runDB f g = do
  con <- getDBConn
  a <-liftIO $ g con
  liftEither $ f a


getComments
  :: Topic
  -> AppM [Comment]
getComments topic = do
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  runDB (traverse fromDbComment)
    $ \c -> Sql.query c q [getTopic topic]
--  getComments db t = do
  -- Write the query with an icky string and remember your placeholders!

  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  --runDB (traverse fromDbComment) $ Sql.query (dbConn db) q (Sql.Only . getTopic $ t)

--  error "Copy your completed 'getComments' and refactor to match the new type signature"

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic topic text = do
  -- Record the time this comment was created.
  nowish <- liftIO getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDB (const (Right ())) $ \con ->  Sql.execute con q (getTopic topic, getCommentText text, nowish)
  -- An alternative is to write a returning query to get the Id of the DbComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics
  :: AppM [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) $ \con -> Sql.query_ con q

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic topic =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB (const (Right ())) $ \con ->  Sql.execute con q (Sql.Only . getTopic $ topic)
