module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

data World = World
  { ship1 :: Ship
  , ship2 :: Ship
  , bullets :: [Bullet]
  , keyStates :: KeyStates
  }

data Ship = Ship
  { shipPos :: Float -- ship angle around black hole
  , shipAngle :: Float -- ship angle (in radians)
  , coolDown :: Float
  , shieldOn :: Bool
  }

data Bullet = Bullet
  { bulletX :: Float
  , bulletY :: Float
  , bulletVX :: Float
  , bulletVY :: Float
  }

data KeyStates = KeyStates
  { keyW :: Bool -- True means key is down
  , keyA :: Bool
  , keyS :: Bool
  , keyD :: Bool
  , keyUp :: Bool
  , keyLeft :: Bool
  , keyDown :: Bool
  , keyRight :: Bool
  }

initialState :: World
initialState = World s1 s2 [] ks
  where
    s1 = Ship (-400) 0 0 False
    s2 = Ship 400 pi 0 False
    ks = KeyStates False False False False False False False False

posToXY :: Ship -> (Float, Float)
posToXY (Ship p _ _ _) = (300 * cos p, 300 * sin p)

renderShip :: Ship -> Picture
renderShip (Ship p a _ s) = pictures $ ship : [shield | s]
  where
    x = 300 * cos p
    y = 300 * sin p
    frontPoint = (x + 50 * cos a, y + 50 * sin a)
    leftPoint = (x + 25 * cos (a + pi / 2), y + 25 * sin (a + pi / 2))
    rightPoint = (x + 25 * cos (a - pi / 2), y + 25 * sin (a - pi / 2))
    ship = color green $ polygon [frontPoint, leftPoint, rightPoint]
    shield = translate x y $ circleSolid 75

renderBullet :: Bullet -> Picture
renderBullet (Bullet x y _ _) = translate x y $ circleSolid 10

render :: World -> Picture
render (World s1 s2 bs _) =
  pictures $ map renderBullet bs ++ [renderShip s1, renderShip s2, blackHole]
  where
    blackHole = circleSolid 100

handleKeys :: Event -> World -> World
handleKeys (EventKey key newState _ _) w = w {keyStates = newKeyStates}
  where
    currentKeyStates = keyStates w
    isDown = newState == Down
    newKeyStates =
      case key of
        Char 'w' -> currentKeyStates {keyW = isDown}
        Char 'a' -> currentKeyStates {keyA = isDown}
        Char 's' -> currentKeyStates {keyS = isDown}
        Char 'd' -> currentKeyStates {keyD = isDown}
        SpecialKey KeyUp -> currentKeyStates {keyUp = isDown}
        SpecialKey KeyLeft -> currentKeyStates {keyLeft = isDown}
        SpecialKey KeyDown -> currentKeyStates {keyDown = isDown}
        SpecialKey KeyRight -> currentKeyStates {keyRight = isDown}
        _ -> currentKeyStates
-- Do nothing for events that aren't keydowns/keyups
handleKeys _ w = w

turnLeft :: Float -> Ship -> Ship
turnLeft dt (Ship p a c s) = Ship p (a + 1 * pi * dt) c s

turnRight :: Float -> Ship -> Ship
turnRight dt (Ship p a c s) = Ship p (a - 1 * pi * dt) c s

shoot :: Ship -> [Bullet] -> [Bullet]
shoot ship@(Ship p a _ _) bs = newBullet : bs
  where
    (x, y) = posToXY ship
    vShip = 2 * pi * 300
    vX = 300 * cos a
    vY = 300 * sin a
    newBullet = Bullet x y vX vY

shield :: Ship -> Ship
shield s = s

applyIf :: Bool -> (a -> a) -> (a -> a)
applyIf True f = f
applyIf False _ = id

updateBulletPos :: Float -> Bullet -> Bullet
updateBulletPos dt (Bullet x y vX vY) =
  Bullet (x + vX' * dt) (y + vY' * dt) vX' vY'
  where
    disFromCenter = x ^ 2 + y ^ 2
    vX' = vX - 1000000 * signum x / disFromCenter
    vY' = vY - 1000000 * signum y / disFromCenter

bulletOnScreen :: Bullet -> Bool
bulletOnScreen (Bullet x y _ _) = distance > 100 && abs x < 400 && abs y < 400
  where
    distance = sqrt $ x ^ 2 + y ^ 2

update :: Float -> World -> World
update dt (World s1 s2 bs ks) = World s1'' s2'' bs' ks
  where
    updateShipPos (Ship p a c s) = Ship (p - 0.25 * pi * dt) a c s
    s1' =
      updateShipPos .
      applyIf (keyA ks) (turnLeft dt) . applyIf (keyD ks) (turnRight dt) $
      s1
    s2' =
      updateShipPos .
      applyIf (keyLeft ks) (turnLeft dt) . applyIf (keyRight ks) (turnRight dt) $
      s2
    bs' =
      map (updateBulletPos dt) .
      filter bulletOnScreen .
      applyIf (keyUp ks && (coolDown s2' < 0)) (shoot s2') .
      applyIf (keyW ks && (coolDown s1' < 0)) (shoot s1') $
      bs
    s1'' =
      if keyW ks && coolDown s1' < 0
        then s1' {coolDown = 1}
        else s1' {coolDown = coolDown s1' - 8 * dt}
    s2'' =
      if keyUp ks && coolDown s2' < 0
        then s2' {coolDown = 1}
        else s2' {coolDown = coolDown s2' - 8 * dt}
    --bs' = newBullet s1 : newBullet s2 : (map moveBullet $ take 50 bs)
    --newBullet ship = Bullet (shipX ship) (shipY ship) (shipAngle ship) ship
    --moveBullet (Bullet x y a o) = Bullet (x + ds * cos a) (y + ds * sin a) a o

-- Functions for the rendering library gloss
-- The Display type tells gloss how we want to display our Picture.
-- InWindow accepts a window title,
window :: Display
window = InWindow "Turn" (1200, 1200) (0, 0)

background :: Color
background = white

main :: IO ()
main = play window background 45 initialState render handleKeys update
