import Control.Monad
import Data.List

type Cost = Int
type Hp = Int
type Damage = Int
type Armor = Int

data Character = Character { hp :: Int, damage :: Int, armor :: Int }

data Item = Offensive Cost Damage 
          | Defensive Cost Armor
          deriving (Eq, Show)

cost :: Item -> Cost
cost (Offensive c _) = c
cost (Defensive c _) = c

weapons :: [Item]
weapons =
  [ Offensive 8 4
  , Offensive 10 5
  , Offensive 25 6
  , Offensive 40 7
  , Offensive 74 8
  ]

armors :: [Item]
armors =
  [ Defensive 0 0
  , Defensive 13 1
  , Defensive 31 2
  , Defensive 53 3
  , Defensive 75 4
  , Defensive 102 5
  ]

rings :: [Item]
rings =
  [ Offensive 0 0
  , Offensive 25 1
  , Offensive 50 2
  , Offensive 100 3

  , Defensive 0 0
  , Defensive 20 1
  , Defensive 40 2
  , Defensive 80 3
  ]

player :: Character
player = Character { hp = 100, damage = 0, armor = 0 }

boss :: Character
boss = Character { hp = 109, damage = 8, armor = 2 }

-- ceil(targetHp / max(1, attackerDamage - targetArmor))

turnsToKill :: Character -> Character -> Int
turnsToKill attacker target = (hp target - 1) `div` hitDamage + 1
  where hitDamage = max 1 (damage attacker - armor target)

canBeat :: Character -> Character -> Bool
canBeat player boss = (player `turnsToKill` boss) <= (boss `turnsToKill` player)

equip :: Character -> Item -> Character
equip (Character hp da ar) equipment = case equipment of
  (Offensive _ ida) -> equipped ida 0
  (Defensive _ iar) -> equipped 0 iar
  where equipped ida iar = Character { hp = hp, damage = da + ida, armor = ar + iar }

purchases :: [[Item]]
purchases = do
  weapon <- weapons
  armor <- armors
  lring <- rings
  rring <- rings

  guard (lring /= rring)

  return [weapon, armor, lring, rring]

main :: IO ()
main = do
  let isViable = (`canBeat` boss) . foldl' equip player
  let builds = filter isViable purchases

  print . minimum . map (sum . map cost) $ builds

  let isImpossible = not . isViable
  let badBuilds = filter isImpossible purchases

  print . maximum . map (sum . map cost) $ badBuilds

