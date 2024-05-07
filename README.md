# Scrabble Project

## Created by:
- Karl Gustav Løhr (kagl@itu.dk)
- Sebastian Graakjær Blok (segb@itu.dk)
- Silas Wolff (siwo@itu.dk)

### Multiplayer - YES:
We can add several of our bots, Oxy or a combination.

### Dictionary - YES:
We have implemented a **Trie**. It is implemented in Dictionary.fs and and Dictionary.fsi

### Finish on all boards - YES:
We are able to finish a game on all boards. Some boards and seeds will give prettier games and/or better results than others.

### Parallelism - NO:
N/A

### Respect the timeout flag - NO:
N/A

## Running the program:

Make sure you are in ´ScrabbleTemplate´:

```bash
cd ScrabbleTemplate
```

Then run the program with the following command:

```bash
dotnet run
```

If you want to run with your IDE's thingy, make sure to change line 36 to:

```bash
let words = readLines "../../../Dictionaries/English.txt"
```