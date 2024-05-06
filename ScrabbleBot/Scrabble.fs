namespace Robert

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

//open FParsecLight.TextParser

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
   
    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        playedLetters : Map<coord, (char * int)>
        numberofplayers : uint32
        CBoard          : Map<coord, uint32>
    }

    let mkState b d pn N pt h L  CB = {board = b; dict = d;  playerNumber = pn; numberofplayers = N; hand = h; playerTurn = pt; playedLetters = L;  CBoard = CB;}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st      = st.playerTurn
    let playedLetters st = st.playedLetters
    let numberofplayers st = st.numberofplayers
    let CBoard st = st.CBoard

    let insertMovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
       List.fold (fun acc move ->
           let (coord, (_,(char, charPoints))) = move
            //debugPrint (sprintf "Inserting move %A %A\n" coord (char))
           let newPlayedLetters = acc.playedLetters |> Map.add coord (char, charPoints)
           mkState acc.board acc.dict acc.playerNumber acc.numberofplayers acc.playerTurn acc.hand newPlayedLetters acc.CBoard
        ) state moves
    
    
    let moves : (coord * (uint32 * (char * int))) list = [(0,0), (0u, ('a',0))]
    
    //let UpdateMoves ms=
        //moves <- ms
    let updateHand ms st newPieces =
                // get a multiset of the indexes (uint) of the tiles you played
                let playedIndexes = 
                    ms
                    |> Seq.map (fun move -> 
                        let (_, (charuint, (_, _))) = move
                        charuint
                        ) 
                    |> Seq.toList 
                    |> MultiSet.ofList
                    
                //printf "playedIndex: %A" playedIndexes

                // remove played tiles from your hand
                let subtractedHand = MultiSet.subtract (hand st) playedIndexes
                //printfn "subtractedHand: %A" subtractedHand

                // add the new tiles to your hand
                List.fold (fun acc (indexOfLetter, letterCount) -> 
                MultiSet.add indexOfLetter letterCount acc) subtractedHand newPieces
    
    let updateCustomBoard ms boardMap =
        List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) boardMap ms


    let charToUint char = 
        if (char = '?') then 0u
        else uint32(System.Char.ToUpper(char)) - 64u
    
    let charNumberToPoints (char: int) = 
            match char with
            | 17 | 26                                       -> 10
            | 10 | 24                                       -> 8
            | 11                                            -> 5
            | 6 | 8 | 22 | 23 | 25                          -> 4
            | 2 | 3 | 13 | 16                               -> 3
            | 4 | 7                                         -> 2
            | 1 | 5 | 9 | 12 | 14 | 15 | 18 | 19 | 20 | 21  -> 1
            | 0                                             -> 0
            | _                                             -> failwith "can't convert uint"

    // Returns list of starters. A starter is a word we can play that starts at the given coord. 
    // The form is (word, (coord, direction))
    let wordLookup playedLetters = 
        let directionalWordLookup playedLetters dir =
            let getExistingWord (coord:coord) (direction: coord) (playedLetters: Map<coord, char * int>) =                
                let rec recursivelyMoveInDir (coord:coord) (direction: coord) (playedLetters: Map<coord, char * int>) acc =
                    let (x, y) = coord
                    let (dx, dy) = direction

                    // Check next coord, if there is a letter
                    let newCoord = (x + dx, y + dy)
                    match Map.tryFind newCoord playedLetters with
                    | None -> 
                        // If we hit an empty square, return the word
                        acc
                    | Some (letter, _) -> 
                        // If we hit a letter, recursively move in the same direction
                        recursivelyMoveInDir newCoord direction playedLetters (acc + string letter)
                
                // Check if letter is in middle of word
                let (x, y) = coord
                let (dx, dy) = direction
                let checkPrevious = (x - dx, y - dy)
                match Map.tryFind checkPrevious playedLetters with
                | None ->
                    // If none, then we can move in the direction (because we are at the start of the word)) 
                    let letter = (fst(Map.find coord playedLetters))
                    Some (recursivelyMoveInDir coord direction playedLetters (string letter))
                | Some _ -> 
                    // If there is a letter, then we are in the middle of a word.
                    None

            // Get the existing word in the given direction.
            Map.fold (fun (acc:List<(string * (coord * coord))>) (coord:coord) ((letter, _): char * int) -> 
                let existingWord = getExistingWord coord dir playedLetters
                match existingWord with
                | Some word -> 
                    // If we found a word, add it to the list
                    (word, (coord, dir)) :: acc
                | None -> 
                    // If we didn't find a word, return the list
                    acc
                ) [] playedLetters

        // Get the existing words right and down direction.
        let downWords = directionalWordLookup playedLetters (0,1)
        let rightWords = directionalWordLookup playedLetters (1,0)
        
        // Merge the two lists
        downWords @ rightWords



module Scrabble =
    open System.Threading
    open MoveRobert

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) (myTurn: bool) (ms: (coord * (uint32 * (char * int))) list) =
            if (myTurn) then
                forcePrint "-------------------- Here is your hand ---------------------\n\n" 
                Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //let input =  System.Console.ReadLine() 
                if st.playedLetters.Count = 0 then
                    // First move
                    //sets starter values, when its the first turn
                    let StartingInfo = ((0, 0), (1, 0), [], 7u)
                    let letters = MultiSet.toList (State.hand st)
                    let input = MoveRobert.RobertsFirstMove(State.hand st) (State.board st) letters pieces st.dict st.playedLetters (State.board st).center (1,0) (StartingInfo)
                    //let input = System.Console.ReadLine()
                    let move = RegEx.parseMove (input)

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else
                    // Not first move
                    //sets starter values, when its the first turn
                    let StartingInfo = MoveRobert.getAllStarters (List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) st.CBoard ms) 
                    
                    let letters = MultiSet.toList (State.hand st)
                    let mutable listOfWords = List.Empty

                    // Convert allStartingPoints and uniqueStartingChars into a list of tuples
                    let startingInfoList = 
                        List.map (fun (coord, direction, chars, length) -> (coord, direction, chars, length)) StartingInfo

                    for startingInfo in startingInfoList do 
                        listOfWords <- MoveRobert.RobertsFirstMove (State.hand st) (State.board st) letters pieces st.dict st.playedLetters (State.board st).center (1,0) (startingInfo) :: listOfWords

                    //let stateWithInsertedMove = State.insertMovesIntoState ms st
                    //let everyWordOnTheBoardInStateWithInsertedMove = State.wordLookup stateWithInsertedMove.playedLetters

                    let stateValid (word: string) (boardWords: (string * (coord * coord)) list) =
                        // Extract the letters from the boardWords
                        let lettersOnBoard = [ for (existingWord, _) in boardWords -> existingWord |> Seq.toList ]
                                            |> List.concat

                        // Check if each letter in the word exists in the letters on the board
                        let lettersMatch = word |> Seq.forall (fun letter -> List.contains letter lettersOnBoard)

                        // Return true if all letters in the word can be found on the board
                        lettersMatch



                    //printf "LISTOFWORDS: %A" listOfWords

                    let validWordsList =
                        listOfWords
                        |> List.choose (fun word ->
                            let stateWithInsertedMove = State.insertMovesIntoState (RegEx.parseMove word) st
                            let everyWordOnTheBoardInStateWithInsertedMove = State.wordLookup stateWithInsertedMove.playedLetters
                            let stateValid =
                                List.fold (fun (stateValidity:bool) (key:string, _) ->
                                    if stateValidity then
                                        if key.Length = 1 then
                                            true
                                        elif Dictionary.lookup key st.dict then
                                            true
                                        else
                                            false
                                    else
                                        false
                                ) true everyWordOnTheBoardInStateWithInsertedMove
                            if stateValid then
                                Some word // Keep the word
                            else
                                None // Discard the word
                        )

                    
                    //printf "VALIDATEDWORDSLIST: %A" validWordsList



                    //let result =
                    // if stateValid then
                    //     word :: validWords
                    //     validWords
                    // else
                    //     validWords
                    
                    let spaceCount word = 
                        word |> Seq.filter (fun c -> c = ' ') |> Seq.length

                    let formattedWords = validWordsList 

                    //printf "LISTOFWORDS: %A" listOfWords

                    let longestWord = 
                        List.fold (fun longest word ->
                            let longestSpaces = spaceCount longest
                            let wordSpaces = spaceCount word
                            if wordSpaces > longestSpaces then
                                word
                            else
                                longest
                        ) "" (formattedWords)

                    
                     
                    //should loop through the startinginfo list instead of just setting .head
                    let input = longestWord
                    //let input = System.Console.ReadLine()
                    printfn "TEST -- In between input and move"
                    
                    match input with
                    | "pass" ->
                        printf "WORD PLAYED: %A" input
                        send cstream (SMPass)  // Send pass command
                        forcePrint "Passing this turn due to no possible moves.\n"
                    | "" ->
                        printf "WORD PLAYED: %A" input
                        send cstream (SMPass)  // Send pass command
                        forcePrint "Passing this turn due to no possible moves.\n"
                    | _ ->
                        printf "WORD PLAYED: %A" input
                        let move = RegEx.parseMove input
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
                        send cstream (SMPlay move)

            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
               
               //State.UpdateMoves ms
            
               let updateCustomBoard = State.updateCustomBoard ms st.CBoard


               // Update playedLetters with new moves
               forcePrint "-------------------- Successful play by you ---------------------\n"
               let updatedStateLetters = State.insertMovesIntoState ms st

               // Update hand
               let newHand = State.updateHand ms st newPieces

               // Update the state
               let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) newHand updatedStateLetters.playedLetters (updateCustomBoard)

               aux newState (st.playerNumber % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                printf "-------------------- Word Played by CMPlayed ---------------------\n"
                //let st' = st // This state needs to be updated
                // Update playedLetters with new moves
                let updatedStateLetters = State.insertMovesIntoState ms st

                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) (State.hand st) updatedStateLetters.playedLetters (State.CBoard st)

                aux newState (pid % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPassed (pid)) ->
                debugPrint (sprintf "-------------------- Word Passed byCMPlayed  ---------------------\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
                aux st (pid % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                //let st' = st // This state needs to be updated
                aux st (pid % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st false ms


        aux st (st.playerTurn = st.playerNumber) [(0,0), (0u, ('a',0))]

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet Map.empty Map.empty)
        