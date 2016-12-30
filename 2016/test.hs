import Control.Monad.State.Strict
import Data.Functor.Identity

stuff :: Int -> State () [Int]
stuff n =
  (n :) <$> stuff (n + 1)

lazy :: Maybe Int -> ()
lazy ~(Just a) = ()

noReallyJustEvalState :: State s a -> s -> a
noReallyJustEvalState m s =
  case runIdentity (runStateT m s) of
    ~(a, s) -> a


main = do
  -- print (take 10 . evalState (stuff 0) $ ())
  print (take 10 . noReallyJustEvalState (stuff 0) $ ())
  -- print (lazy Nothing)

-- fmap f m = StateT $ \ s ->
--     fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s
