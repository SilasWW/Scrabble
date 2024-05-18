# Scrabble Project

## Created by:
- Karl Gustav Løhr (kagl@itu.dk)
- Sebastian Graakjær Blok (segb@itu.dk)
- Silas Wolff (siwo@itu.dk)

## Changes since last time:
Our code is now completely free of mutable variables, and our bot never gets any Gameplay errors, apart from occasionally getting a GPENotEnoughPieces (we will get back to this).

We consistently score high points, and are left with few pieces on hand. When playing alone, we generally score 300+ points, and are left with fewer than 4 pieces.
When playing 2 bots against each other, they each generally get 100-300 points, and are both left with fewer than 4 pieces.
When playing against Oxyphenbutazone, we generally get 100-300 points, and are left with fewer than 4 pieces.

This has been achieved by fixing some errors from earlier, and extending some functionality. Namely, our bot has an improved validater, that makes sure we never play a move that throws an error, and our bot is now able to play words that finish with a character on the board, and not only start with one. It can play from any letter on the board, and not just from one in the latest played letter.

The only error we rarely but occasionally get, is the 'GPENotEnoughPieces'. We tried to get the bot to keep state of how many pieces are left in the 'bag'. However, as it is not possible to know how many pieces the bag starts with, which is something we confirmed with Jesper through the 'Questions and Answers' channel in the Teams chat, this has not been possible to do perfectly. We started with the assumption, that there would be 100 pieces, since this is what we found was the normal amount when googling. However, this has proven to not always be true for our game. Therefore, we use a somewhat arbitrary, but easily adjustable, number when playing. This imperfection means that, a few seeds either end with a 'GPENotEnoughPieces' error or 7 pieces left on hand. This can be corrected by simply adjusting the expected number of tiles, which is the last argument in the starting-call. If there are 7 pieces left, try a higher number. If you get the error, try a smaller number.

### Playing against yourself on the infinite board - YES

### Playing against other people and implementing a Trie or a Gaddag - YES
We have implemented a *Trie*, and we can play admirable against both oxy and ourself.

### Parallelism - NO

### Respect the timeout flag - NO

## Playing the game:
Make sure you are in ´ScrabbleTemplate´:

```bash
cd ScrabbleTemplate
```

Then run the program with the following command:

```bash
dotnet run
```
