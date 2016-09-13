import Control.Applicative
import Control.Monad

type Hp = Int
type Mana = Int

data Character = Character
  { hp :: Int
  , damage :: Int
  , armor :: Int
  , mana :: Int
  }

data Spell = Spell
  { name :: String
  , cost :: Int
  , event :: Event
  }

instance Show Spell where show = name

type Event = World -> World
data Effect = Effect EffectId Int Event
data EffectId = Shield | Poison | Recharge

type World = (Character, Character, [Effect])
type Strategy = [Spell]

noop :: Event
noop = id

updatePlayer :: (Character -> Character) -> Event
updatePlayer map (player, boss, effs) = (map player, boss, effs)

damagePlayer :: Int -> Event
damagePlayer amt = updatePlayer $ \player -> player { hp = hp player - amt }

updateBoss :: (Character -> Character) -> Event
updateBoss map (player, boss, effs) = (player, map boss, effs)

damageBoss :: Int -> Event
damageBoss amt = updateBoss $ \boss -> boss { hp = hp boss - amt }

lossCondition :: Event
lossCondition = damagePlayer 200

addEffect :: EffectId -> Int -> Event -> Event
addEffect id ticks ev (player, boss, effs) = (player, boss, effect : effs)
  where effect = Effect id ticks ev

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
    shield = any isShieldEffect effs

    isShieldEffect :: Effect -> Bool
    isShieldEffect _ = False
    -- isShieldEffect (Effect _ event)
    --   | event == id = True

through :: [a -> a] -> a -> a
through fs x = foldl (flip ($)) x fs

spells =
  [ Spell
    { name = "Magic Missile"
    , cost = 53
    , event = damageBoss 4
    }
  , Spell
    { name = "Drain"
    , cost = 73
    , event = damageBoss 2 . damagePlayer (-2)
    }
  , Spell
    { name = "Shield"
    , cost = 113
    , event = addEffect Shield 6 noop
    }
  , Spell
    { name = "Poison"
    , cost = 173
    , event = addEffect Poison 6 (damageBoss 3)
    }
  , Spell
    { name = "Recharge"
    , cost = 229
    , event = addEffect Recharge 5 (updatePlayer $ \player -> player { mana = mana player + 101 })
    }
  ]

player :: Character
player = Character { hp = 100, damage = 0, armor = 0, mana = 500 }

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
        cast spell, 
        applyEffects,
        -- maybe boss attack?
        bossAttack,
        applyEffects
      ]

    cast :: Spell -> Event
    cast spell world@(player, boss, effs)
      | cost spell > mana player = lossCondition world
      | otherwise = event spell world

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
  let bestStrategy = head (filter endsInVictory strategies)
  -- let bestStrategy = take 10 strategies
  print bestStrategy
