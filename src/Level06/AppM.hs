{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level06.AppM where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Text              (Text)

import           Level06.Types          (Error)

import           Data.Bifunctor         (first)

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will create a newtype `AppM` that is a shorthand way of
-- describing the return type of a function that may contain an error.
--
-- This will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value.
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     alsoMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (IO (Either Error a))
runAppM
  :: AppM a
  -> IO (Either Error a)
runAppM (AppM m) =
  m

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  fmap f (AppM ma) = AppM $ ((<$>) f) <$> ma

instance Applicative AppM where
  pure :: a -> AppM a
  pure  = AppM . pure . pure

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  AppM mf <*> AppM ma = AppM $ ( (pure (<*>)) <*> mf ) <*> ma

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  --AppM aa >>= f = AppM $ aa >>= (\x -> pure ( x >>= (\y -> ( unsafePerformIO .runAppM ) (f y))))
  AppM aa >>= f = AppM $ do
    a <- aa
    runAppM $ either throwError f a


instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO ioa = AppM $ do
    a <- ioa
    (pure . pure) a

catchHelper :: (Error -> AppM a) -> Either Error a -> AppM a
catchHelper cb (Left e)  = cb e
catchHelper _ (Right a) = pure a

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError er = (AppM . pure . Left) er

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM a) cb = AppM $ a >>= runAppM . (catchHelper cb)



-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither (Left e) = throwError e
liftEither (Right a) = pure a
