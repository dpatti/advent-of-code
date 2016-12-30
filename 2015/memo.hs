import Control.Monad.State
import qualified Data.Map as Map

type Memo = Map.Map

fibs :: Int -> Int
fibs 0 = 0
fibs 1 = 1
fibs n = fibs (n - 1) + fibs (n - 2)

memoize :: Ord a => (a -> b) -> a -> b
memoize f = flip evalState Map.empty . withMemo
  where
    withMemo :: Ord a => a -> State (Memo a b) b
    withMemo arg = do
      state <- get
      case Map.lookup arg state of
        (Just result) -> return result
        Nothing ->




main = print (memoize fibs 100)
