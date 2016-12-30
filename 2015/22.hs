import Control.Applicative
import Control.Monad
import Debug.Trace

type Hp = Int
type Mana = Int

data Character = Character
  { hp :: Int
  , damage :: Int
  , armor :: Int
  , mana :: Int
  } deriving Show

data Spell = Spell
  { name :: String
  , cost :: Int
  , event :: Event
  }
instance Show Spell where show = name

type Event = World -> World

data Effect = Effect EffectId Int Event
instance Show Effect where
  show (Effect id ticks _) = show (id, ticks)

data EffectId = Shield | Poison | Recharge deriving (Show, Eq)

type World = (Character, Character, [Effect])
type Strategy = [Spell]

noop :: Event
noop = id

updatePlayer :: (Character -> Character) -> Event
updatePlayer map (player, boss, effs) = (map player, boss, effs)

damagePlayer :: Int -> Event
damagePlayer amt = updatePlayer $ \player -> player { hp = hp player - amt }

spendMana :: Int -> Event
spendMana amount = updatePlayer $ \player -> player { mana = mana player - amount }

updateBoss :: (Character -> Character) -> Event
updateBoss map (player, boss, effs) = (player, map boss, effs)

damageBoss :: Int -> Event
damageBoss amt = updateBoss $ \boss -> boss { hp = hp boss - amt }

lossCondition :: Event
lossCondition = damagePlayer 200

addEffect :: EffectId -> Int -> Event -> Event
addEffect id ticks ev world@(player, boss, effs)
  | ticks <= 0 = world
  | activeEffect id effs = lossCondition world
  | otherwise = (player, boss, effect : effs)
  where effect = Effect id ticks ev

activeEffect :: EffectId -> [Effect] -> Bool
activeEffect search = any isMatch
  where
    isMatch (Effect id _ _)
      | id == search = True
      | otherwise = False

applyEffects :: Event
applyEffects world@(player, boss, effs) = foldl applyEffect (player, boss, []) effs
  where
    applyEffect :: World -> Effect -> World
    applyEffect world@(player, boss, effs) (Effect id ticks event) =
      addEffect id (ticks - 1) event . event $ world

bossAttack :: Event
bossAttack world@(player, boss, effs) = damagePlayer hit world
  where
    hit = damage boss - if shield then 7 else 0
    shield = activeEffect Shield effs

through :: [a -> a] -> a -> a
through fs x = foldl (flip ($)) x fs

magicMissile = Spell
  { name = "Magic Missile"
  , cost = 53
  , event = damageBoss 4
  }

drain = Spell
  { name = "Drain"
  , cost = 73
  , event = damageBoss 2 . damagePlayer (-2)
  }

shield = Spell
  { name = "Shield"
  , cost = 113
  , event = addEffect Shield 6 noop
  }

poison = Spell
  { name = "Poison"
  , cost = 173
  , event = addEffect Poison 6 (damageBoss 3)
  }

recharge = Spell
  { name = "Recharge"
  , cost = 229
  , event = addEffect Recharge 5 (spendMana (-101))
  }

spells = [magicMissile, drain, shield, poison, recharge]
  
player :: Character
player = Character { hp = 50, damage = 0, armor = 0, mana = 500 }

boss :: Character
boss = Character { hp = 71, damage = 10, armor = 0, mana = 0 }

world :: World
world = (player, boss, [])

evaluate :: Strategy -> World
evaluate = foldr turn world
  where
    turn :: Spell -> World -> World
    turn spell =
      through [
        hardModeAttrition,
        cast spell, 
        applyEffects,
        ifBossAlive bossAttack,
        applyEffects
      ]

    hardModeAttrition :: Event
    hardModeAttrition = damagePlayer 1

    cast :: Spell -> Event
    cast spell world@(player, boss, effs)
      | cost spell > mana player = lossCondition world
      | otherwise = spendMana (cost spell) . event spell $ world

    ifBossAlive :: Event -> Event
    ifBossAlive event world@(_, boss, _)
      | hp boss > 0 = event world
      | otherwise = world

endsInDefeat :: Strategy -> Bool
endsInDefeat st =
  case evaluate st of
    (player, boss, _)
      | hp player <= 0 -> True
      | otherwise -> False

endsInVictory :: Strategy -> Bool
endsInVictory st =
  case evaluate st of
    (player, boss, _)
      | hp player > 0 && hp boss <= 0 -> True
      | otherwise -> False

strategies :: [Strategy]
strategies = strategies' [empty]
  where
    strategies' :: [Strategy] -> [Strategy]
    strategies' [] = []
    strategies' sts = 
      let withNextMoves = nextMoves sts :: [Strategy]
       in withNextMoves ++ strategies' withNextMoves

    nextMoves :: [Strategy] -> [Strategy]
    nextMoves sts = do
      st <- sts
      s <- spells

      let st' = s : st
      guard (not (endsInDefeat st'))
      return st'

main :: IO ()
main = do
  -- print (evaluate (reverse [poison, recharge, magicMissile, poison, magicMissile, recharge, poison, magicMissile, magicMissile, magicMissile]))

  let bestStrategy = head (filter endsInVictory strategies)
  print bestStrategy
  print (evaluate bestStrategy)
  -- let bestStrategy = take 100 strategies
  -- mapM_ print bestStrategy
