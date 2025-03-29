# Tic-Tac-Toe with AI using Decision Trees – COMP5450 Assessment 2

## Description

This repository contains my solution for **COMP5450 - Assessment 2**, which focuses on developing an interactive, terminal-based **Tic-Tac-Toe** (Noughts and Crosses) game that allows **human vs AI**, **AI vs AI**, or **human vs human** play. The AI component uses a **decision tree** (from Assessment 1) to choose its moves, learning and improving over time based on game outcomes.

This project is implemented entirely in **Haskell** and involves **concurrent programming** using channels for communication between the server and player processes. The AI uses **inductive decision tree learning** and continuously updates its dataset after each game.

## Features

- **Interactive Game Board**: Displays the board after each move using terminal output.
- **Human and AI Players**: Supports different player combinations.
- **Concurrent Game Server**: Manages turns, board state, and game results through IO and concurrency.
- **Decision Tree Learning AI**:
  - Uses information gain to select optimal moves.
  - Learns from past games using a training dataset.
  - Updates its decision tree after each round using `learnTree` and `addRow`.

## File Structure

- `se368.hs`: Main file containing my implementation of helper functions, server logic, human and AI players.
- `Base.hs`: Provided module defining game data types and helpers (must not be modified).
- `DecisionTree.hs`: Provided decision tree module containing `learnTree`, `infer`, `addRow`, and `bestGain`.
- `README.md`: This document.

## Installation & Usage

1. Download `Base.hs` and `DecisionTree.hs` from Moodle and place them in the same directory as `se368.hs`.

2. Open GHCi or use another Haskell interpreter and load the module:

   ```bash
   ghci se368.hs

3. Start the game by running the following in GHCi:

   ```bash
startGame humanPlayer aiPlayer

## Gameplay

- The game is played in the terminal.
- Players enter moves by typing row (1–3) and column (A–C).
- The server validates and updates the board after each move.
- When the game ends, scores are displayed and players can decide whether to continue.
- The AI updates its training data and decision tree between rounds.

## Notes

- **Do not modify** `Base.hs` or `DecisionTree.hs`.
- Code is structured according to assignment sections: helper functions, game server, players, and AI logic.
- Only `Prelude`, `Control.Concurrent`, and the provided modules are used (no extra imports).
