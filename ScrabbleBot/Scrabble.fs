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


    //sends moves into the state
    let MovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
       List.fold (fun acc move ->
           let (coord, (_,(character, characterPoints))) = move
           let newPlayedLetters = acc.playedLetters |> Map.add coord (character, characterPoints)
           mkState acc.board acc.dict acc.playerNumber acc.numberofplayers acc.playerTurn acc.hand newPlayedLetters acc.CBoard
        ) state moves
    
    //after each turn, we update out hand to get the new tiles needed to be a 7 tiles aways
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
    
    //updates our makeshift board to try our moves on
    let newCBoard ms boardMap =
        List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) boardMap ms
    
    //validates the letters we want to play
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

        let xAxieswords = directedWord letters (1,0)
        let yAxieswords = directedWord letters (0,1)        
        yAxieswords @ xAxieswords
    
    let filterStartingInfo (boardMap: Map<coord, uint32>) (startingInfo: (coord * coord * uint32 list * uint32) list) =
        let occupiedSet = boardMap.Keys |> Set.ofSeq

        //printf "OCCUPIED: %A" occupiedSet

        let rec hasOccupiedTiles (startCoord: coord) (direction: coord) (length: uint32) =
            let rec checkCoords (coord: coord) (remainingLength: uint32) =
                match remainingLength with
                | 0u -> false
                | _ ->
                    if Set.contains coord occupiedSet then
                        true
                    else
                        let nextCoord = (fst coord + fst direction, snd coord + snd direction)
                        checkCoords nextCoord (remainingLength - 1u)

            not (checkCoords startCoord length)

        List.filter (fun (startCoord, direction, _, length) -> hasOccupiedTiles startCoord direction length) startingInfo



module Scrabble =

    let playGame cstream pieces (st : State.state) =

        //our main recursive function , that runs throughout the game
        let rec aux (st : State.state) (myTurn: bool) (ms: (coord * (uint32 * (char * int))) list) =
            if (myTurn) then
                printf "-------------------- Here is Player %A hand ---------------------\n\n" st.playerNumber 
                
                //uncomment under if you want to see hand
                Print.printHand pieces (State.hand st)

                if st.playedLetters.Count = 0 then
                    //Hardcoded starter values on first turn
                    let StartingInfo = ((0, 0), (1, 0), [], 7u)
                    let letters = MultiSet.toList (State.hand st)
                    let input = MoveRobert.RobertsFirstMove letters pieces st.dict  (StartingInfo)
                    let move = RegEx.parseMove (input)

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else
                    //we have had some problems with parsing wrong directions, so we have tried to all normal directions
                    //and reversed, to cover all
                    
                    let StartingInfoNormal = MoveRobert.GetStartingInfo (List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) st.CBoard ms) 
                    let reversedStartingInfo =
                        StartingInfoNormal
                        |> List.map (fun (coord, direction, list, id) ->
                            let reversedDirection = (abs (fst direction), abs (snd direction))  // Take the absolute value of the direction
                            (coord, reversedDirection, list, id))

                    let StartingInfo = StartingInfoNormal @ reversedStartingInfo


                    //for startingInfoNormal in StartingInfoNormal do
                    //    printf "StartingInfoNormal: %A" startingInfoNormal
                    

                    //printfn "Starting Info Before Filtering: %A" StartingInfo

                    let FilteredStartingInfo = State.filterStartingInfo st.CBoard StartingInfo

                    //printfn "Starting Info After Filtering: %A" FilteredStartingInfo
                    
                    
                    //under here we sort in our listOfWords, to find the perfect one
                    let letters = MultiSet.toList (State.hand st)
                    let mutable listOfWords = List.Empty

                    let startingInfoList = 
                        List.map (fun (coord, direction, chars, length) -> (coord, direction, chars, length)) StartingInfo

                    for startingInfo in startingInfoList do 
                        listOfWords <- MoveRobert.RobertsFirstMove letters pieces st.dict (startingInfo) :: listOfWords
                    
                    let validWordsList =
                        listOfWords
                        |> Seq.map (fun word ->

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
                    
                    //here we count spaces in our already formatted strings,
                    //to get the longest word
                    let spaceCount word = 
                        word |> Seq.filter (fun c -> c = ' ') |> Seq.length

                    let formattedWords = validWordsList 

                    let formattedWordsList = formattedWords |> Seq.choose id |> Seq.toList

                    let longestWord = 
                        List.fold (fun longest word ->
                            let longestSpaces = spaceCount longest
                            let wordSpaces = spaceCount word
                            if wordSpaces > longestSpaces then
                                word
                            else
                                longest
                        ) "" (formattedWordsList)

                    let input = longestWord
                    
                    //we match input to see if we need to pass or actually have something
                    //to play
                    match input with
                    | "pass" ->
                        
                        send cstream (SMPass)
                        debugPrint (sprintf "Passing this turn due to no possible moves.\n")
                    | "" ->
                        send cstream (SMPass)
                        debugPrint (sprintf "Passing this turn due to no possible moves.\n")
                    | _ ->
                        let move = RegEx.parseMove input
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
                        send cstream (SMPlay move)

            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->

               let newCBoard = State.newCBoard ms st.CBoard
               
               debugPrint (sprintf "-------------------- Successful play by you ---------------------\n")
               let newLetters = State.MovesIntoState ms st

               let newHand = State.updateHand ms st newPieces

               let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) newHand newLetters.playedLetters (newCBoard)

               aux newState (st.playerNumber % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPlayed (pid, ms, points)) ->
                
                debugPrint (sprintf "-------------------- Word Played by CMPlayed ---------------------\n")

                let newCBoard = State.newCBoard ms st.CBoard
                
                let newLetters = State.MovesIntoState ms st

                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) (State.hand st) newLetters.playedLetters (newCBoard)

                aux newState (pid % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPassed (pid)) ->
                debugPrint (sprintf "-------------------- Word Passed byCMPlayed  ---------------------\n")
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