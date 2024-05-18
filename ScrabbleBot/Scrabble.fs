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
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        playedLetters : Map<coord, char * int>
        numberofplayers : uint32
        CBoard          : Map<coord, uint32>
        numberOfTilesLeft : uint32
    }

    let mkState b d pn N pt h L  CB notl = {board = b; dict = d;  playerNumber = pn; numberofplayers = N; hand = h; playerTurn = pt; playedLetters = L;  CBoard = CB; numberOfTilesLeft = notl}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st      = st.playerTurn
    let playedLetters st = st.playedLetters
    let numberofplayers st = st.numberofplayers
    let CBoard st = st.CBoard
    let numberOfTilesLeft st = st.numberOfTilesLeft


    //sends moves into the state
    let MovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
       List.fold (fun acc move ->
           let coord, (_,(character, characterPoints)) = move
           let newPlayedLetters = acc.playedLetters |> Map.add coord (character, characterPoints)
           mkState acc.board acc.dict acc.playerNumber acc.numberofplayers acc.playerTurn acc.hand newPlayedLetters acc.CBoard acc.numberOfTilesLeft
        ) state moves
    
    //after each turn, we update out hand to get the new tiles needed to be a 7 tiles aways
    let updateHand ms st newPieces =
                let playedIndexes = 
                    ms
                    |> Seq.map (fun move -> 
                        let _, (charuint, (_, _)) = move
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
                    let x, y = coord
                    let dx, dy = direction

                    let coord2 = (x + dx, y + dy)
                    
                    //If None: return word, if Some: recursive
                    match Map.tryFind coord2 letters with
                    | None -> 
                        acc
                    | Some (letter, _) -> 
                        recMove coord2 direction letters (acc + string letter)
                
                let x, y = coord
                let dx, dy = direction
                let previous = (x - dx, y - dy)
                
                // if None we are at the start a word, if Some we are inside a word 
                match Map.tryFind previous letters with
                | None ->
                    let letter = fst(Map.find coord letters)
                    Some (recMove coord direction letters (string letter))
                | Some _ -> 
                    None
            Map.fold (fun (acc:List<string * (coord * coord)>) (coord:coord) ((_, _): char * int) -> 
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
    
    let longestWord (filteredWords: list<list<uint32>>) =
        let findLongest (accLength, accList) lst =
            let length = List.length lst
            if length > accLength then
                (length, lst)
            else
                (accLength, accList)

        let _, maxList = List.fold findLongest (0, []) filteredWords
        if List.isEmpty maxList then
            ([], false)
        else
            (maxList, true)

    let findcharacterpoints (char: int) = 
        match char with
        | 0 -> 0
        | 1 | 5 | 9 | 12 | 14 | 15 | 18 | 19 | 20 | 21 -> 1
        | 4 | 7 -> 2
        | 2 | 3 | 13 | 16 -> 3
        | 6 | 8 | 22 | 23 | 25-> 4
        | 11-> 5
        | 10 | 24-> 8
        | 17 | 26 -> 10
        | _-> failwith "error"
    
    // input follow a specific format, that we make here
    let longestWordFormat (startCoord: coord) (direction: coord) (startingChars: uint32 list) (wordListWithCount: List<uint32> * int) : string =
        let alphabet = "0ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let dirX, dirY = direction
        let startX, startY = startCoord

        let (wordList: List<uint32>), _ = wordListWithCount                    

        let rec formatter acc (x, y) isFirstLetter = function
            | [] -> String.concat " " (List.rev acc)
            | hd::tl ->
                let number = int hd 
                let letterIndex = number  // Adjust index because 'A' = 1 in your system
                if letterIndex < 0 || letterIndex >= alphabet.Length then
                    failwith "Character index out of range"

                let letter = if letterIndex = 0 then alphabet[1] else alphabet[letterIndex]
                let points = findcharacterpoints number  // Get corresponding points for the number
                let formatted = sprintf "%d %d %d%c%d" x y number letter points  // Concatenate number and points after the letter
                
                let removeFirstLetter = 
                    isFirstLetter && 
                    (List.isEmpty startingChars || 
                     List.head startingChars = uint32 (int letter - int 'A' + 1))

                if removeFirstLetter then
                    formatter acc (x + dirX, y + dirY) false tl
                else
                    formatter (formatted::acc) (x + dirX, y + dirY) false tl

        let isFirstLetter = 
            not (List.isEmpty startingChars) && 
            List.head startingChars = List.head wordList
        
        let changedX, changedY = if isFirstLetter then
                                     match direction with
                                     | 1, 0 -> startX, startY + dirY 
                                     | 0, 1 -> startX + dirX, startY
                                     | _ -> startX, startY 
                                 else startX, startY

        formatter [] (changedX, changedY) isFirstLetter wordList
    
    //
    let reversedLongestWordFormat (endCoord: coord) (direction: coord) (startingChars: uint32 list) (wordListWithCount: List<uint32> * int) : string =
        let alphabet = "0ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let dirX, dirY = direction
        let endX, endY = endCoord

        let wordList, count = wordListWithCount

        // Calculate the starting coordinate based on the end coordinate and direction
        let startX = endX - dirX * (count - 1)
        let startY = endY - dirY * (count - 1)

        // Function to format the word
        let rec formatter acc (x, y) _ = function
            | [] -> String.concat " " (List.rev acc)
            | hd::tl ->
                let number = int hd 
                let letterIndex = number  // Adjust index because 'A' = 1 in your system
                if letterIndex < 0 || letterIndex >= alphabet.Length then
                    failwith "Character index out of range"
                
                let letter = if letterIndex = 0 then alphabet[1] else alphabet[letterIndex]
                let points = findcharacterpoints number  // Get corresponding points for the number
                let formatted = sprintf "%d %d %d%c%d" x y number letter points  // Concatenate number and points after the letter

                // Recursively format the rest of the word list
                formatter (formatted::acc) (x + dirX, y + dirY) false tl

        // Check if the first letter should be removed based on startingChars
        let isFirstLetter = 
            not (List.isEmpty startingChars) &&
            List.head startingChars = List.head wordList

        // Start formatting from the calculated starting coordinates
        formatter [] (startX, startY) isFirstLetter wordList

module Scrabble =

    let playGame cstream pieces (st : State.state) =

        //our main recursive function , that runs throughout the game
        let rec aux (st : State.state) (myTurn: bool) (ms: (coord * (uint32 * (char * int))) list) =
            if myTurn then
                debugPrint "-------------------- Here is Player hand ---------------------\n\n" 
                
                //uncomment under if you want to see hand
                //Print.printHand pieces (State.hand st)

                if st.playedLetters.Count = 0 then
                    //Hardcoded starter values on first turn
                    let StartingInfo = ((0, 0), (1, 0), [], 7u)
                    let letters = MultiSet.toList (State.hand st)
                    let filteredWords = MoveRobert.RobertsFirstMove letters pieces st.dict  StartingInfo
                    let downWords, startingInfo = filteredWords
                    let longestWordDown = State.longestWord downWords 

                    let startCoord, direction, startingChars, _ = startingInfo

                     // Call longestWordFormat with needed inputs
                    let longestWord =
                        match longestWordDown with
                            | wordList, true -> 
                                State.longestWordFormat startCoord direction startingChars (wordList, List.length wordList)
                            | _, false -> 
                                "pass"

                    let move = RegEx.parseMove longestWord

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else
                    //we have had some problems with parsing wrong directions, so we have tried to all normal directions
                    //and reversed, to cover all
                    
                    let StartingInfo = MoveRobert.GetStartingInfo (List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) st.CBoard ms) 
                                        
                    let startingInfoList = 
                        List.map (fun (coord, direction, chars, length) -> (coord, direction, chars, length)) StartingInfo
                    
                    let startingInfoListFiltered = 
                        List.filter (fun (_, _, _, length) -> length <> 0u) startingInfoList
                    
                    let letters = MultiSet.toList (State.hand st)
                    let listOfWords =
                        [
                            for startingInfo in startingInfoListFiltered do
                                yield MoveRobert.RobertsFirstMove letters pieces st.dict startingInfo
                        ]

                    let formattedWords =
                        [
                            for longestWordDown, startingInfo in listOfWords do
                                let startCoord, direction, startingChars, _ = startingInfo
                                for wordList in longestWordDown do
                                    if direction = (1, 0) || direction = (0, 1) then
                                        yield State.longestWordFormat startCoord direction startingChars (wordList, List.length wordList)
                                    else
                                        let reversedWordList = List.rev wordList
                                        yield State.longestWordFormat startCoord direction startingChars (reversedWordList, List.length wordList)
                        ]

                    //
                    let validWordsList =
                        formattedWords
                        |> Seq.choose (fun word ->
                            let stateWithInsertedMove = State.MovesIntoState (RegEx.parseMove word) st
                            if State.validater stateWithInsertedMove.playedLetters |> List.forall (fun (key, _) -> key.Length = 1 || Dictionary.lookup key st.dict) then
                                Some word
                            else
                                None)
                        
                    
                    let longestWord =
                        if Seq.isEmpty validWordsList then
                            "pass"
                        else
                            validWordsList
                            |> Seq.maxBy (fun word -> word |> Seq.filter (fun c -> c = ' ') |> Seq.length)

                    let input = longestWord
                    
                    //we match input to see if we need to pass or actually have something
                    //to play
                    match input with
                    | "pass" | "" ->
                        if st.numberOfTilesLeft < MultiSet.size st.hand then
                            send cstream SMPass
                        else 
                            send cstream (SMChange (MultiSet.toList st.hand))

                    | _ ->
                        let move = RegEx.parseMove input
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
                        send cstream (SMPlay move)

            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, _, newPieces)) ->

               let newCBoard = State.newCBoard ms st.CBoard
               
               debugPrint "-------------------- Successful play by you ---------------------\n"
               let newLetters = State.MovesIntoState ms st

               let newHand = State.updateHand ms st newPieces
                
               let piecesLeft = st.numberOfTilesLeft - uint32 newPieces.Length

               let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) newHand newLetters.playedLetters newCBoard (if piecesLeft > 1000u then 0u else piecesLeft)

               aux newState (st.playerNumber % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPlayed (pid, ms, _)) ->
                
                debugPrint "-------------------- Word Played by CMPlayed ---------------------\n"

                let newCBoard = State.newCBoard ms st.CBoard

                let piecesLeft = st.numberOfTilesLeft - uint32 ms.Length
                
                let newLetters = State.MovesIntoState ms st

                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) (State.hand st) newLetters.playedLetters newCBoard (if piecesLeft > 1000u then 0u else piecesLeft)

                aux newState (pid % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPassed pid) ->
                debugPrint "-------------------- Word Passed byCMPlayed  ---------------------\n"
                aux st (pid % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                //let st' = st // This state needs to be updated
                aux st (pid % st.numberofplayers + 1u = st.playerNumber) ms
            |RCM (CMChangeSuccess tiles) ->
                debugPrint "-------------------- CMPlayed Exhanged Tiles ---------------------\n"
                let hand = List.fold (fun acc (indexOfLetter, letterCount) -> MultiSet.add indexOfLetter letterCount acc) MultiSet.empty tiles
                let _State :State.state= State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) hand (State.playedLetters st) (State.CBoard st) (st.numberOfTilesLeft - uint32 tiles.Length)
                aux _State (st.playerNumber % st.numberofplayers + 1u = st.playerNumber) ms
            | RCM (CMChange(pid, numberOfTiles)) ->

                let piecesLeft = st.numberOfTilesLeft - uint32 numberOfTiles
                let _State :State.state= State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) (State.hand st) (State.playedLetters st) (State.CBoard st) piecesLeft

                debugPrint "-------------------- CMPlayed Changed Tiles ---------------------\n"
                aux _State (pid % st.numberofplayers + 1u = st.playerNumber) ms
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
        
        //The last argument in the call is the amount of pieces in the bag. We tried to make sure they wouldnt consistently
        //try to switch pieces, even when there no longer any left in the bag, causing GPENotEnoughPieces errors.
        
        //However, since there are no way to accurately tell how many pieces there is in the bag (Jesper confirmed this in
        //the Teams 'Questions and Answers' chat), we have picked a number that is somewhat arbitrary.
        //We started at 100, since this is what we could google was the normal amount. But then we have further reduced
        //it by 5, simply to have smoother games. Many games would be possible to run without the last -5u, but some don't.
        
        //If you have a game where there still is a GPENotEnoughPieces error, try making the value even lower. If you have a
        //seed with many pieces left on the hand, try making the value higher. 
        
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet Map.empty Map.empty (100u-(numPlayers*7u)-5u))