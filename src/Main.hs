module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

data World = World { ship1 :: Ship
                   , ship2 :: Ship
                   , keyStates :: KeyStates }

data Ship = Ship { shipPX :: Float      -- x position (relative to origin at center of window)
                 , shipPY :: Float      -- y position
                 , shipAngle :: Float }  -- ship angle (in radians)

data KeyStates = KeyStates { keyA :: KeyState       -- defined in gloss, data KeyState = Down | Up
                           , keyLeft :: KeyState }


initialState :: World
initialState = World s1 s2 ks
    where s1 = Ship (-400) 0 0
          s2 = Ship 400 0 pi
          ks = KeyStates Up Up

renderShip :: Ship -> Picture
renderShip (Ship x y a) = translate x y $ rectangleSolid 50 50--color green $ polygon [frontPoint, leftPoint, rightPoint]
    where
        frontPoint = (x + 50 * cos a, y + 50 * sin a)
        leftPoint = (x + 25 * cos (a + pi / 2), y + 25 * cos (a + pi / 2))
        rightPoint = (x + 25 * cos (a - pi / 2), y + 25 * cos (a - pi / 2))

render :: World -> Picture
render (World s1 s2 _) = pictures [renderShip s1, renderShip s2]

handleKeys :: Event -> World -> World
handleKeys (EventKey key newState _ _) w = w { keyStates = newKeyStates }
    where
        currentKeyStates = keyStates w
        -- New keyStates value based on user input
        newKeyStates = case key of
                           -- Update state of keyA if a key was pressed
                           Char 'a' -> currentKeyStates { keyA = newState }
                           -- Update state of left arrow key if it was pressed
                           SpecialKey KeyLeft -> currentKeyStates { keyLeft = newState }
                           -- Don't change anything if another key was pressed
                           _ -> currentKeyStates
-- Do nothing for events that aren't keydowns/keyups
handleKeys _ w = w

update :: Float -> World -> World
update dt (World s1 s2 ks) = World (moveShip s1') (moveShip s2') ks
    where moveShip (Ship x y a) = Ship (x + ds * cos a) (y + ds * sin a) a
          -- Update shipAngle of ship if turn key is pressed down
          s1' = if keyA ks == Down
                    then s1 { shipAngle = shipAngle s1 + da}
                else s1
          s2' = if keyLeft ks == Down
                   then s2 { shipAngle = shipAngle s2 + da}
                else s2
          -- Distance traveled per frame is velocity * dt
          ds = 0--100 * dt
          -- angle turned per frame is pi radians left * dt
          da = 0---pi * dt


-- Functions for the rendering library gloss

-- The Display type tells gloss how we want to display our Picture.
-- InWindow accepts a window title,
window :: Display
window = InWindow "Turn" (1200, 1200) (0, 0)

background :: Color
background = white

main :: IO ()
main = play window background 30 initialState render handleKeys update
