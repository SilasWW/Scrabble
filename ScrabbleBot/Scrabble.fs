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

    let MovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
       List.fold (fun acc move ->
           let (coord, (_,(character, characterPoints))) = move
           let newPlayedLetters = acc.playedLetters |> Map.add coord (character, characterPoints)
           mkState acc.board acc.dict acc.playerNumber acc.numberofplayers acc.playerTurn acc.hand newPlayedLetters acc.CBoard
        ) state moves
    
    let updateHand ms st newPieces =
                let playedIndexes = 
                    ms
                    |> Seq.map (fun move -> 
                        let (_, (charuint, (_, _))) = move
                        charuint
                        ) 
                    |> Seq.toList 
                    |> MultiSet.ofList

                let subtractedHand = MultiSet.subtract (hand st) playedIndexes

                List.fold (fun acc (indexOfLetter, letterCount) -> 
                MultiSet.add indexOfLetter letterCount acc) subtractedHand newPieces
    
    let newCBoard ms boardMap =
        List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) boardMap ms

    // Returns list of starters. A starter is a word we can play that starts at the given coord. 
    // The form is (word, (coord, direction))
    let validater letters = 
        let directedWord letters direction =
            let getExistingWord (coord:coord) (direction: coord) (letters: Map<coord, char * int>) =                
                let rec recMove (coord:coord) (direction: coord) (letters: Map<coord, char * int>) acc =
                    let (x, y) = coord
                    let (dx, dy) = direction

                    let coord2 = (x + dx, y + dy)
                    
                    //If None: return word, if Some: recursive
                    match Map.tryFind coord2 letters with
                    | None -> 
                        acc
                    | Some (letter, _) -> 
                        recMove coord2 direction letters (acc + string letter)
                
                let (x, y) = coord
                let (dx, dy) = direction
                let previous = (x - dx, y - dy)
                
                // if None we are at the start a word, if Some we are inside a word 
                match Map.tryFind previous letters with
                | None ->
                    let letter = (fst(Map.find coord letters))
                    Some (recMove coord direction letters (string letter))
                | Some _ -> 
                    None

            Map.fold (fun (acc:List<(string * (coord * coord))>) (coord:coord) ((letter, _): char * int) -> 
                let word = getExistingWord coord direction letters
                
                // if Some: add word to the list, if None: return list
                match word with
                | Some word2 -> 
                    (word2, (coord, direction)) :: acc
                | None -> 
                    acc
                ) [] letters

        let downWords = directedWord letters (0,1)
        let rightWords = directedWord letters (1,0)
        
        downWords @ rightWords



module Scrabble =
    open System.Threading
    open MoveRobert

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) (myTurn: bool) (ms: (coord * (uint32 * (char * int))) list) =
            if (myTurn) then
                forcePrint "-------------------- Here is your hand ---------------------\n\n" 
                Print.printHand pieces (State.hand st)

                if st.playedLetters.Count = 0 then
                    //Hardcoded starter values
                    let StartingInfo = ((0, 0), (1, 0), [], 7u)
                    let letters = MultiSet.toList (State.hand st)
                    let input = MoveRobert.RobertsFirstMove letters pieces st.dict  (StartingInfo)
                    let move = RegEx.parseMove (input)

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else
                    let StartingInfoNormal = MoveRobert.getAllStarters (List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) st.CBoard ms) 
                    
                    let reversedStartingInfo =
                        StartingInfoNormal
                        |> List.map (fun (coord, direction, list, id) ->
                            let reversedDirection = (abs (fst direction), abs (snd direction))  // Take the absolute value of the direction
                            (coord, reversedDirection, list, id))

                    let StartingInfo = StartingInfoNormal @ reversedStartingInfo
                    
                    let letters = MultiSet.toList (State.hand st)
                    let mutable listOfWords = List.Empty

                    // Convert allStartingPoints and uniqueStartingChars into a list of tuples
                    let startingInfoList = 
                        List.map (fun (coord, direction, chars, length) -> (coord, direction, chars, length)) StartingInfo

                    for startingInfo in startingInfoList do 
                        listOfWords <- MoveRobert.RobertsFirstMove letters pieces st.dict (startingInfo) :: listOfWords

                    let stateValid (word: string) (boardWords: (string * (coord * coord)) list) =
                        // Extract the letters from the boardWords
                        let lettersOnBoard = [ for (existingWord, _) in boardWords -> existingWord |> Seq.toList ]
                                            |> List.concat

                        // Check if each letter in the word exists in the letters on the board
                        let lettersMatch = word |> Seq.forall (fun letter -> List.contains letter lettersOnBoard)

                        // Return true if all letters in the word can be found on the board
                        lettersMatch


                    let validWordsList =
                        listOfWords
                        |> List.choose (fun word ->
                            let stateWithInsertedMove = State.MovesIntoState (RegEx.parseMove word) st
                            let everyWordOnTheBoardInStateWithInsertedMove = State.validater stateWithInsertedMove.playedLetters
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
                                Some word
                            else
                                None
                        )
                    
                    let spaceCount word = 
                        word |> Seq.filter (fun c -> c = ' ') |> Seq.length

                    let formattedWords = validWordsList 

                    let longestWord = 
                        List.fold (fun longest word ->
                            let longestSpaces = spaceCount longest
                            let wordSpaces = spaceCount word
                            if wordSpaces > longestSpaces then
                                word
                            else
                                longest
                        ) "" (formattedWords)

                    let input = longestWord
                    
                    match input with
                    | "pass" ->
                        printf "WORD PLAYED: %A" input
                        send cstream (SMPass)
                        forcePrint "Passing this turn due to no possible moves.\n"
                    | "" ->
                        printf "WORD PLAYED: %A" input
                        send cstream (SMPass)
                        forcePrint "Passing this turn due to no possible moves.\n"
                    | _ ->
                        printf "WORD PLAYED: %A" input
                        let move = RegEx.parseMove input
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
                        send cstream (SMPlay move)

            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            
               let updateCustomBoard = State.newCBoard ms st.CBoard
               
               forcePrint "-------------------- Successful play by you ---------------------\n"
               let updatedStateLetters = State.MovesIntoState ms st

               let newHand = State.updateHand ms st newPieces

               let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) newHand updatedStateLetters.playedLetters (updateCustomBoard)

               aux newState (st.playerNumber % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPlayed (pid, ms, points)) ->
                
                printf "-------------------- Word Played by CMPlayed ---------------------\n"
                
                let updatedStateLetters = State.MovesIntoState ms st

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