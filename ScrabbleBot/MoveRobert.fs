module internal MoveRobert

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    
    let RobertsFirstMove (initCharactersOnHand : uint32 list) pieces (dict : Dict) (initStartingInfo : coord * coord * uint32 list * uint32) =
        // Let's keep the unchanged part of the function as it is
        //let charactersOnHand = initCharactersOnHand |> List.filter (fun x -> x <> 0u)
        let charactersOnHand = initCharactersOnHand |> List.filter (fun x -> x <> 0u)
        
        let StartingInfo = initStartingInfo
        
        // takes out the length in our startingInfo and sets it as value maxLengthOfWord
        let maxLengthOfWord = StartingInfo |> fun (_, _, _, len) -> len

        // extracts the pair from the set
        let GetTuple (set:Set<char*int>) : char*int = 
            match (Set.toList set) with 
            | x::_ -> x 
            | [] -> failwith "error" 
        
        // This function removes the letter that was just used
        let RemoveUsedLetterFromCurrentHand letterToBeRemoved charactersOnHand = 
            let foundIndex = List.tryFindIndex (fun x -> x = letterToBeRemoved) charactersOnHand
            match foundIndex with
            | Some(i) -> List.removeAt i charactersOnHand
            | None -> charactersOnHand
        

        // converts our list of uint32 to the word as a string, using the alphabet
        let ConvertIntListToString (wordList: uint32 list) =
            let alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            List.fold (fun acc letterAsUint ->
                if letterAsUint >= 0u && letterAsUint <= 26u then
                    acc + string alphabet[int letterAsUint]
                else
                    "pass" // Ignore invalid letter codes
            ) "" wordList
        
        // steps into our dictionary with the starting chars, 
        // to form words that always start with what is on the board
        // This is where we could improve by also allowing to have them inside words,
        // or at the end, this limits us a lot, but we couldn't find a
        // sufficient solution
        let StartingDictWithStartingChars (dict: Dict) (listOfCharsAsUint: list<uint32>) =
            List.fold (fun (accDict, accWord, _) char ->
                match step (fst (GetTuple (Map.find char pieces))) accDict with
                | Some (_, nextDict) ->
                    (nextDict, accWord @ [char], true)
                | None -> 
                    (accDict, accWord, false)
            ) (dict, [], true) listOfCharsAsUint
        
        // Modified GetAllPossibleWords to use initialized state
        let rec GetAllPossibleWords (dict: Dictionary.Dict) (remainingLetters: uint32 list) (currentWord: uint32 list) =
            if (uint32 currentWord.Length) >= maxLengthOfWord then ([currentWord], true) else
                let (words, valid) =
                    List.fold (fun (acc, valid) letter ->
                        let IsWordFoundAndDict = step (fst (GetTuple (Map.find letter pieces))) dict
                        match IsWordFoundAndDict with
                        | Some (isWord, nextDict) ->
                            let newWord = currentWord @ [letter]
                            let newAcc = if isWord then newWord::acc else acc
                            (newAcc @ (fst (GetAllPossibleWords nextDict (RemoveUsedLetterFromCurrentHand letter remainingLetters) newWord)), true)
                        | None ->
                            (acc, false)
                    ) ([], true) remainingLetters
                (words, valid)
        
        let addToLists (inputList: list<list<uint32>> * bool) (additionalList: list<uint32>) =
            let (lists, boolValue) = inputList
            let newList = List.map (fun subList -> subList @ additionalList) lists
            (newList, boolValue)

        // setting starting chars in starting info, 
        // which follows starting coord, direction in the form of coord, startingChars, and length as previously shown
        let _, direction, startingChars, _ = StartingInfo
        

        let checkCharactersOnHand (word: uint32 list) (charactersOnHand: uint32 list) (startingChars: uint32 list) =
            // Function to remove a character from the available characters lists
            let removeFromCharsList (c: uint32) (charsList: uint32 list) =
                match List.tryFindIndex (fun x -> x = c) charsList with
                | Some idx -> List.take idx charsList @ List.skip (idx + 1) charsList
                | None -> charsList

            // Function to check if each character in the word can be matched with available characters
            let rec checkWordChars (word: uint32 list) (charsList: uint32 list) =
                match word with
                | [] -> true // All characters matched
                | c::rest ->
                    match List.tryFindIndex (fun x -> x = c) charsList with
                    | Some idx ->
                        // Remove the matched character from the available characters list
                        let updatedCharsList = removeFromCharsList c charsList
                        checkWordChars rest updatedCharsList
                    | None ->
                        false // Character not found in available characters

            // Check if the word is longer than the starting characters
            if List.length word <= List.length startingChars then
                false
            else
                checkWordChars word (List.append charactersOnHand startingChars)

        // here we already check if we should pass
       
        // if it's valid, we get all possible words from this starting info
        // getting our dict and word prepared 
        let possibleWords =
            if direction = (1, 0) || direction = (0, 1) then
                let (preparedDict: Dict), (preparedWord: uint32 list), isValidInit = StartingDictWithStartingChars dict startingChars
                GetAllPossibleWords dict charactersOnHand preparedWord
            else
                addToLists (GetAllPossibleWords dict charactersOnHand []) startingChars

        // Filter the words to find valid words to play

        // First set of filtered words
        let rec filterValidWords1 (dict: Dictionary.Dict) (possibleWords: list<list<uint32>> * bool) =
            match possibleWords with
            | [], _ -> ([], false)
            | word::rest, valid ->
                let wordString = ConvertIntListToString word
                let isValidWord = lookup wordString dict
                let CanMakeWord = checkCharactersOnHand word charactersOnHand startingChars
                if isValidWord && CanMakeWord then
                    let (validWords, allValid) = filterValidWords1 dict (rest, valid)
                    (word :: validWords, allValid)
                else
                    let (validWords, allValid) = filterValidWords1 dict (rest, valid)
                    (validWords, false)

        // Call the filtering functions for both sets of possible words
        let (filteredWords1, allValid1) = filterValidWords1 dict possibleWords

        
        // Return both sets of filtered words along with the StartingInfo tuple
        (filteredWords1, StartingInfo)

    let GetStartingInfo (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
        let occupied = boardMap.Keys |> Seq.cast |> List.ofSeq
        let occupiedSet = Set.ofList occupied

        let aboveHelper (coord: coord) : bool =
            let checkAbove = (fst coord, snd coord - 1): coord
            not (Set.contains checkAbove occupiedSet)

        let belowHelper (coord: coord) : bool =
            let checkBelow = (fst coord, snd coord + 1): coord
            not (Set.contains checkBelow occupiedSet)

        let leftHelper (coord: coord) : bool =
            let checkLeft = (fst coord - 1, snd coord): coord
            not (Set.contains checkLeft occupiedSet)

        let rightHelper (coord: coord) : bool =
            let checkRight = (fst coord + 1, snd coord): coord
            not (Set.contains checkRight occupiedSet)

        let rec lettersAbove (coord: coord) (acc: uint32 list) : uint32 list =
            let coord2: coord = (fst coord, snd coord - 1)
            if Set.contains coord2 occupiedSet then
                lettersAbove coord2 (Map.find coord2 boardMap :: acc)
            else
                acc

        let rec lettersBelow (coord: coord) (acc: uint32 list) : uint32 list =
            let coord2: coord = (fst coord, snd coord + 1)
            if Set.contains coord2 occupiedSet then
                lettersBelow coord2 (Map.find coord2 boardMap :: acc)
            else
                acc
        
        let rec lettersLeft (coord: coord) (acc: uint32 list) : uint32 list =
            let coord2: coord = (fst coord - 1, snd coord)
            if Set.contains coord2 occupiedSet then
                lettersLeft coord2 (Map.find coord2 boardMap :: acc)
            else
                acc

        let rec lettersRight (coord: coord) (acc: uint32 list) : uint32 list =
            let coord2: coord = (fst coord + 1, snd coord)
            if Set.contains coord2 occupiedSet then
                lettersRight coord2 (Map.find coord2 boardMap :: acc)
            else
                acc        

        let rec verticalLen (coord: coord) (acc: uint32) : uint32 =
            let coord2: coord = (fst coord, snd coord + 1)
            if belowHelper coord2 && acc < 7u then
                verticalLen coord2 (acc + 1u)
            else
                acc

        let rec horizontalLen (coord: coord) (acc: uint32) : uint32 =
            let coord2: coord = (fst coord + 1, snd coord)
            if rightHelper coord2 && acc < 7u then
                horizontalLen coord2 (acc + 1u)
            else
                acc
        
        let rec verticalLenUp (coord: coord) (acc: uint32) : uint32 =
            let coord2: coord = (fst coord, snd coord - 1)
            if aboveHelper coord2 && acc < 7u then
                verticalLenUp coord2 (acc + 1u)
            else
                acc

        let rec horizontalLenLeft (coord: coord) (acc: uint32) : uint32 =
            let coord2: coord = (fst coord - 1, snd coord)
            if leftHelper coord2 && acc < 7u then
                horizontalLenLeft coord2 (acc + 1u)
            else
                acc

        let rec verticalHelper (coord: coord) =
            let allDirections = [(0, 1); (0, -1); (1, 0); (-1, 0)]
            List.collect (fun (dx, dy) ->
                if dy > 0 && belowHelper coord then
                    [ (coord, ((dx, dy): coord), [ Map.find coord boardMap ], verticalLen coord 0u) ]
                elif dy < 0 && aboveHelper coord then
                    [ (coord, ((dx, dy): coord), lettersAbove coord [ Map.find coord boardMap ], verticalLenUp coord 0u) ]
                else []
            ) allDirections

        let rec horizontalHelper (coord: coord) =
            let allDirections = [(1, 0); (-1, 0); (0, 1); (0, -1)]
            List.collect (fun (dx, dy) ->
                if dx > 0 && rightHelper coord then
                    [ (coord, ((dx, dy): coord), [ Map.find coord boardMap ], horizontalLen coord 0u) ]
                elif dx < 0 && leftHelper coord then
                    [ (coord, ((dx, dy): coord), lettersLeft coord [ Map.find coord boardMap ], horizontalLenLeft coord 0u) ]
                else []
            ) allDirections


        let verticalStarters = List.collect verticalHelper occupied
        let horizontalStarters = List.collect horizontalHelper occupied
        
        verticalStarters @ horizontalStarters
