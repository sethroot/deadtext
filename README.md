![deadtext-logo](https://user-images.githubusercontent.com/307254/132119589-82edb919-a87a-4815-88be-546c353d7147.png)

deadtext is a text adventure game engine written in Haskell.

Games may be defined internally with native Haskell types or externally in JSON files that are then loaded into the engine at runtime.

## Installation

1) Install [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

2) Clone the repository
```
git clone https://github.com/sethroot/deadtext.git
```

3) Run the following in the deadtext directory
```bash
stack setup
stack build
stack run
```

## Usage
By default, deadtext will load the file `json/example.json` into the engine at runtime. To create a custom game based on this, modify the provided [example.json](https://github.com/sethroot/deadtext/blob/main/json/example.json) and then run without any arguments.
```bash
stack run
```

To load an arbitrary JSON file, provide the name of the file as argument
```bash
# Will attempt to load json/my-game.json
stack run my-game
```

To create a game defined by the internal Haskell types, modify the [Data.setState](https://github.com/sethroot/deadtext/blob/main/src/Data.hs#L15) implementation and run with the `noload` or `-n` argument
```bash
stack run noload
stack run -- -n
```
