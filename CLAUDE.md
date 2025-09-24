# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell-based multiplayer arcade game called "Turn" where two players control ships orbiting a black hole and try to avoid getting hit by bullets while shooting at each other. The game uses the Gloss graphics library for rendering and game loop management.

## Architecture

- **Main.hs**: Contains the entire game implementation in a single file
- **Game State**: Managed through a `World` data type containing two ships, bullets, and keyboard states
- **Graphics**: Uses Gloss library for 2D graphics rendering with a game loop at 45 FPS
- **Physics**: Ships orbit around a central black hole with gravitational effects on bullets
- **Controls**: Player 1 uses WASD keys, Player 2 uses arrow keys

## Key Data Structures

- `World`: Top-level game state containing ships, bullets, and input states
- `Ship`: Player ship with position (angle around black hole), facing angle, cooldown, and shield state
- `Bullet`: Projectiles with position and velocity, affected by black hole gravity
- `KeyStates`: Tracks pressed state of all control keys

## Build Commands

This project uses Stack for Haskell package management:

```bash
# Build the project
stack build

# Run the game
stack run

# Install dependencies (if needed)
stack setup
```

Note: The project requires the Gloss graphics library, which is specified in the .cabal file.

## Development Setup

The project uses:
- Stack resolver: lts-14.14
- Base library: >= 4.7 && < 5
- Graphics library: gloss
- Target executable: Turn-Game-Haskell

## Game Mechanics

- Ships automatically orbit the black hole at fixed radius (300 units)
- Players can turn their ships left/right and shoot bullets
- Bullets are affected by gravity from the central black hole
- Shooting has a cooldown system to prevent spam
- Game runs at 45 FPS with continuous updates