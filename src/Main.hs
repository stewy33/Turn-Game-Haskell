module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

data World = World
  { ship1 :: Ship
  , ship2 :: Ship
  , bullets :: [Bullet]
  , keyStates :: KeyStates
  , gravityStrength :: Float
  , bulletSpeed :: Float
  }

data Ship = Ship
  { shipPos :: Float -- ship angle around black hole
  , shipAngle :: Float -- ship angle (in radians)
  , coolDown :: Float
  , shieldOn :: Bool
  , isAlive :: Bool
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
initialState = World s1 s2 [] ks 500000 300
  where
    s1 = Ship 0 0 0 False True        -- Ship 1 at 0 degrees
    s2 = Ship pi pi 0 False True      -- Ship 2 at 180 degrees (pi radians)
    ks = KeyStates False False False False False False False False

posToXY :: Ship -> (Float, Float)
posToXY (Ship p _ _ _ _) = (300 * cos p, 300 * sin p)

renderShip :: Ship -> Picture
renderShip (Ship p a _ s alive) = pictures $ ship : [shield | s]
  where
    x = 300 * cos p
    y = 300 * sin p
    frontPoint = (x + 50 * cos a, y + 50 * sin a)
    leftPoint = (x + 25 * cos (a + pi / 2), y + 25 * sin (a + pi / 2))
    rightPoint = (x + 25 * cos (a - pi / 2), y + 25 * sin (a - pi / 2))
    shipColor = if alive then green else red
    ship = color shipColor $ polygon [frontPoint, leftPoint, rightPoint]
    shield = translate x y $ circleSolid 75

renderBullet :: Bullet -> Picture
renderBullet (Bullet x y _ _) = translate x y $ circleSolid 10

renderSlider :: Float -> Float -> String -> Float -> Float -> Float -> Picture
renderSlider x y label minVal maxVal currentVal = pictures [track, handle, labelText]
  where
    sliderWidth = 200
    normalizedVal = (currentVal - minVal) / (maxVal - minVal)
    handleX = x - sliderWidth/2 + normalizedVal * sliderWidth
    track = translate x y $ rectangleWire sliderWidth 10
    handle = translate handleX y $ circleSolid 8
    labelText = translate (x - sliderWidth/2 - 80) y $ scale 0.1 0.1 $ text label

render :: World -> Picture
render (World s1 s2 bs _ gravity bulletSpd) =
  pictures $ map renderBullet bs ++ [renderShip s1, renderShip s2, blackHole, gravitySlider, bulletSpeedSlider]
  where
    blackHole = circleSolid 100
    gravitySlider = renderSlider 0 500 "Gravity" 100000 1000000 gravity
    bulletSpeedSlider = renderSlider 0 450 "Bullet Speed" 100 500 bulletSpd

updateSlider :: Float -> Float -> Float -> Float -> Float -> Float
updateSlider mouseX sliderX minVal maxVal sliderWidth =
  let normalizedX = (mouseX - (sliderX - sliderWidth/2)) / sliderWidth
      clampedX = max 0 (min 1 normalizedX)
  in minVal + clampedX * (maxVal - minVal)

handleEvents :: Event -> World -> World
handleEvents (EventKey key newState _ _) w = w {keyStates = newKeyStates}
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
handleEvents (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) w =
  if mouseY >= 490 && mouseY <= 510
    then w { gravityStrength = updateSlider mouseX 0 100000 1000000 200 }
  else if mouseY >= 440 && mouseY <= 460
    then w { bulletSpeed = updateSlider mouseX 0 100 500 200 }
  else w
-- Do nothing for other events
handleEvents _ w = w

turnLeft :: Float -> Ship -> Ship
turnLeft dt (Ship p a c s alive) = Ship p (a + 1 * pi * dt) c s alive

turnRight :: Float -> Ship -> Ship
turnRight dt (Ship p a c s alive) = Ship p (a - 1 * pi * dt) c s alive

shoot :: Float -> Ship -> [Bullet] -> [Bullet]
shoot bulletSpd ship@(Ship p a _ _ _) bs = newBullet : bs
  where
    (shipX, shipY) = posToXY ship
    -- Spawn bullet at front of ship (50 units ahead)
    x = shipX + 50 * cos a
    y = shipY + 50 * sin a
    vShip = 2 * pi * 300
    vX = bulletSpd * cos a
    vY = bulletSpd * sin a
    newBullet = Bullet x y vX vY

shield :: Ship -> Ship
shield s = s

applyIf :: Bool -> (a -> a) -> (a -> a)
applyIf True f = f
applyIf False _ = id

updateBulletPos :: Float -> Float -> Bullet -> Bullet
updateBulletPos dt gravity (Bullet x y vX vY) =
  Bullet (x + vX' * dt) (y + vY' * dt) vX' vY'
  where
    disFromCenter = x ^ 2 + y ^ 2
    vX' = vX - gravity * signum x / disFromCenter
    vY' = vY - gravity * signum y / disFromCenter

bulletOnScreen :: Bullet -> Bool
bulletOnScreen (Bullet x y _ _) = distance > 100 && abs x < 400 && abs y < 400
  where
    distance = sqrt $ x ^ 2 + y ^ 2

shipBulletCollision :: Ship -> Bullet -> Bool
shipBulletCollision ship bullet = distance < 40
  where
    (shipX, shipY) = posToXY ship
    (bulletX', bulletY') = (bulletX bullet, bulletY bullet)
    distance = sqrt $ (shipX - bulletX') ^ 2 + (shipY - bulletY') ^ 2

checkCollisions :: Ship -> [Bullet] -> (Ship, [Bullet])
checkCollisions ship bullets
  | not (isAlive ship) = (ship, bullets)
  | otherwise =
      let collidingBullets = filter (shipBulletCollision ship) bullets
          remainingBullets = filter (not . shipBulletCollision ship) bullets
          newShip = if null collidingBullets then ship else ship { isAlive = False }
      in (newShip, remainingBullets)

update :: Float -> World -> World
update dt (World s1 s2 bs ks gravity bulletSpd) = World s1Final s2Final bsFinal ks gravity bulletSpd
  where
    updateShipPos (Ship p a c s alive) = Ship (p - 0.25 * pi * dt) a c s alive
    s1' =
      updateShipPos .
      applyIf (keyA ks && isAlive s1) (turnLeft dt) . applyIf (keyD ks && isAlive s1) (turnRight dt) $
      s1
    s2' =
      updateShipPos .
      applyIf (keyLeft ks && isAlive s2) (turnLeft dt) . applyIf (keyRight ks && isAlive s2) (turnRight dt) $
      s2
    bs' =
      map (updateBulletPos dt gravity) .
      filter bulletOnScreen .
      applyIf (keyUp ks && (coolDown s2' < 0) && isAlive s2') (shoot bulletSpd s2') .
      applyIf (keyW ks && (coolDown s1' < 0) && isAlive s1') (shoot bulletSpd s1') $
      bs
    s1'' =
      if keyW ks && coolDown s1' < 0 && isAlive s1'
        then s1' {coolDown = 1}
        else s1' {coolDown = coolDown s1' - 8 * dt}
    s2'' =
      if keyUp ks && coolDown s2' < 0 && isAlive s2'
        then s2' {coolDown = 1}
        else s2' {coolDown = coolDown s2' - 8 * dt}
    (s1Final, bs1) = checkCollisions s1'' bs'
    (s2Final, bsFinal) = checkCollisions s2'' bs1
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
main = play window background 45 initialState render handleEvents update
