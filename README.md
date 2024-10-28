# Haskell Rainbow Tables

This project demonstrates the concept of Rainbow Tables using Haskell. Rainbow tables allow for the efficient cracking of hashed passwords by striking a balance between time and space trade-offs. It shows how a password can be hashed, reduced, and stored in an optimized way to retrieve the original value based on its hash.

For more info: [How rainbow tables work](https://kestas.kuliukas.com/RainbowTables/) and [Rainbow tables in Wikipedia](https://en.wikipedia.org/wiki/Rainbow_table).

## Features

- Hashing and Reduction: Passwords are hashed to 32-bit integers and mapped back to possible passwords using a reduce function.
- Rainbow Table Generation: Chains of hashed and reduced passwords are computed and stored efficiently.
- Password Cracking: A hash value is reversed to the corresponding password using the precomputed table.
- Customizable Parameters: Control password length, character set, and table size to experiment with cracking efficiency.

## Usage

Install GHC. On macOS, use Homebrew:

```bash
brew install ghc cabal-install
```

Enter the GHC interactive environment and load the files with:

```bash
ghci
:l rainbow.hs
```

To create a rainbow table based on random initial passwords, use the `buildTable` function:

```Haskell
buildTable rainbowTable nLetters pwLength width height
```

To reverse the hash function, use the `findPassword` function which repeatedly apply `pwReduce` and `pwHash` to the hash value until we either find the corresponding password or reach the end of the chain:

```Haskell
buildTable rainbowTable nLetters pwLength width height
```

## Configuration

Modify the following constants in your code to adjust the complexity of the rainbow table:

```
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table
```