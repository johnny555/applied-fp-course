{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level07.AppM where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           Level07.Types          (Conf, FirstAppDB)
import           Level07.Types.Error    (Error)

-- First, let's clean up our (Conf,FirstAppDB) with an application Env type. We
-- will add a general purpose logging function as well. Remember that functions
-- are values, we're able to pass them around and place them on records like any
-- other type.
data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- It would be nice to remove the need to pass around our Env to every function
-- that needs it. Wouldn't it be great to have our functions run where we could
-- simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
--
-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
newtype AppM a = AppM ( Env -> IO (Either Error a) )
  -- Quite often, GHC is able to write the code for us. In this case we just
  -- tell GHC that we want a Functor instance for our newtype, and it is able to
  -- correctly derive what is needed.
  deriving Functor
  -- We could do this for the rest of these instances, but that would turn into
  -- "magic" what is otherwise straight-forward implementations. You are here to
  -- learn after all.

runAppM
  :: AppM a
  -> Env
  -> IO (Either Error a)
runAppM (AppM f) env = f env

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM $  \_ -> pure $ pure a

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  AppM f <*> AppM g = AppM $ \s -> do
    envf <- f s
    envg <- g s
    pure $ envf <*> envg

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  -- When it comes to running functions in AppM as a Monad, this will take care
  -- of passing the Env from one function to the next.
  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  AppM f >>= g = AppM $ \s -> do
    resA <-  f s
    resEA <-pure $ (runAppM . g) <$> resA
    case resEA of
      Left e -> pure $ Left e
      Right h -> h s

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError  = AppM . const . pure . Left

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM f) g = AppM $ \env -> do
    current <- f env
    case current of
             Left e -> runAppM (g e) env
             Right a -> runAppM (pure a) env


instance MonadReader Env AppM where
  -- Return the current Env from the AppM.
  ask :: AppM Env
  ask = AppM $ \env -> pure (Right env)

  -- Run a AppM inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM a -> AppM a
  local f (AppM g) = AppM (g . f )

  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM a
  reader f = AppM $ \env -> let
                      a = f env
                      in
                        pure $ Right a

instance MonadIO AppM where
  -- Take a type of 'IO a' and lift it into our AppM.
  liftIO :: IO a -> AppM a
  liftIO action = AppM $  \_ -> do
    a <- action
    pure $ Right a

-- Move on to ``src/Level07/DB.hs`` after this

liftEither
  :: Either Error a
  -> AppM a
liftEither (Left e) = throwError e
liftEither (Right a) = pure a
