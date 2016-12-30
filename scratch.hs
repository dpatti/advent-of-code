import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

type Memo = Map.Map String Int

addState :: Int -> State Int ()
addState n = do
  i <- get
  put $ i + n
  return ()

memoize :: String -> Int -> State Memo Int
memoize key eval = do
  memo <- get
  case Map.lookup key memo of
    (Just cached) -> return cached
    Nothing -> do
      let cached = eval
      put $ Map.insert key cached memo
      return cached

main = do
  print . (`runState` Map.empty) $ do
    memoize "five" 5
    memoize "five" 6
