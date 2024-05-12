module internal MoveRobert

    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    
    let RobertsFirstMove (initCharactersOnHand : uint32 list) pieces (dict : Dictionary.Dict) (initStartingInfo : (coord * coord * uint32 list * uint32))=

        //let charactersOnHand = initCharactersOnHand |> List.filter (fun x -> x <> 0u)
        let charactersOnHand = initCharactersOnHand |> List.filter (fun x -> x <> 0u)

        let StartingInfo = initStartingInfo
        
        // takes out the lenth in our startingInfo, and sets it as value maxLengthOfWord
        let maxLengthOfWord = StartingInfo |> fun (_, _, _, len) -> len

        //extracts the pair from from the set
        let GetTuple (set:Set<char*int>) : char*int = 
            match (Set.toList set) with 
            | x::xs -> x 
            | [] -> failwith "error" 
        
        //This function removes the letter that were just used
        let RemoveUsedLetterFromCurrentHand letterToBeRemoved charactersOnHand = 
            let foundIndex = List.tryFindIndex (fun x -> x = letterToBeRemoved) charactersOnHand
            match foundIndex with
            | Some(i) -> List.removeAt i charactersOnHand
            | None -> charactersOnHand
        

        // converts out list of uint32 to the word as a string, using the alfabet
        let ConvertIntListToString (wordList: uint32 list) =
            let alphabet = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            List.fold (fun acc letterAsUint ->
                if letterAsUint >= 0u && letterAsUint <= 26u then
                    acc + string alphabet.[int letterAsUint]
                else
                    "pass" // Ignore invalid letter codes
            ) "" wordList
        
        //steps into our dictionary with the starting chars, 
        //to form words that always start with what is on the board
        // This is where we could improve by also allowing to have them inside words,
        // or at the end, this limits us a lot, but we couldn't find a
        // sufficient solution
        let StartingDictWithStartingChars (dict: Dictionary.Dict) (listOfCharsAsUint: list<uint32>) =
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
                (words, valid && List.length words > 0)

        //setting startingchars in startinginfo, 
        //which follows startingcoord, direction in form of coord, startingChars, and length as prev shown
        let _, _, startingChars, _ = StartingInfo

        //getting our dict and word prepared 
        let (preparedDict: Dict), (preparedWord: uint32 list), isValidInit = StartingDictWithStartingChars dict startingChars

        let checkCharactersOnHand (word: uint32 list) (charactersOnHand: uint32 list) (startingChars: uint32 list) =
            // Function to remove a character from the available characters lists
            let removeFromCharsList (c: uint32) (charsList: uint32 list) =
                match List.tryFindIndex (fun x -> x = c) charsList with
                | Some idx -> List.take idx charsList @ List.skip (idx + 1) charsList
                | None -> charsList

            // Make copies of charactersOnHand and startingChars to avoid modifying the original lists
            let mutable availableChars = List.append charactersOnHand startingChars

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
                checkWordChars word availableChars

        
        let Letters = charactersOnHand @ startingChars

        //here we already check if we should pass
        if not isValidInit then "pass"
        else 
            //if its valid, wer get all possible words from this startinginfo
            let possibleWords = GetAllPossibleWords dict (charactersOnHand) preparedWord
            
            //we filter the words to find valid words to play, 
            //this it so that we dont play Autisti because the stepping
            //doesnt stop it because autistic is a word. this prevents that
            match possibleWords with
            | _, false -> "pass"
            | _, true -> (
                let rec filterValidWords (dict: Dictionary.Dict) (possibleWords: list<list<uint32>> * bool) =
                    match possibleWords with
                    | [], _ -> ([], false)
                    | word::rest, valid ->
                        let wordString = ConvertIntListToString word
                        let isValidWord = lookup wordString dict
                        let CanMakeWord = checkCharactersOnHand word charactersOnHand startingChars
                        if isValidWord && CanMakeWord then
                            let (validWords, allValid) = filterValidWords dict (rest, valid)
                            (word :: validWords, allValid)
                        else
                            let (validWords, allValid) = filterValidWords dict (rest, valid)
                            (validWords, false)

                //here we call the function above
                let (filteredWords, allValid) = filterValidWords dict possibleWords

                let longestWord = 
                    let mutable maxLength = 0
                    let mutable maxList = []
                    for lst in filteredWords do
                        let length = List.length lst
                        if length > maxLength then
                            maxLength <- length
                            maxList <- lst
                    if maxList.IsEmpty then
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
                    
                    // we get the wordlist out
                    let (wordList: List<uint32>), _ = wordListWithCount                    

                    //this formats out word
                    let rec formatter acc (x, y) isFirstLetter = function
                        | [] -> String.concat " " (List.rev acc)
                        | hd::tl ->
                            let number = int hd 
                            let letterIndex = number  // Adjust index because 'A' = 1 in your system
                            if letterIndex < 0 || letterIndex >= alphabet.Length then
                                failwith "Character index out of range"
                            let mutable letter = alphabet.[letterIndex]
                            match letterIndex with
                            | 0 -> letter = alphabet.[1]
                            | _ -> letter = alphabet.[letterIndex]
                            let points = findcharacterpoints number  // Get corresponding points for the number
                            let formatted = sprintf "%d %d %d%c%d" x y number letter points  // Concatenate number and points after the letter
                            
                            // Check if the first letter should be removed
                            let removeFirstLetter = 
                                isFirstLetter && 
                                (List.isEmpty startingChars || 
                                List.head startingChars = uint32 (int letter - int 'A' + 1))
                            
                            // Recursively format the rest of the word list
                            if removeFirstLetter then
                                formatter acc (x + dirX, y + dirY) false tl
                            else
                                formatter (formatted::acc) (x + dirX, y + dirY) false tl

                    let isFirstLetter = not (List.isEmpty startingChars) && List.head startingChars = List.head wordList
                    
                    //adjusts the direction after
                    let changedX, changedY =
                        if isFirstLetter then
                            match direction with
                            | 1, 0 ->  startX, startY + dirY 
                            | 0, 1 -> startX + dirX, startY
                            | _ -> startX, startY 
                        else
                            startX, startY // Maintain the current coordinates if the first letter is not removed
                

                    formatter [] (changedX, changedY) isFirstLetter wordList

                let startCoord, direction, startingChars, _ = StartingInfo


                // Call longestWordFormat with needed inputs
                let formattedWord = 
                    match longestWord with
                    | wordList, true -> 
                        longestWordFormat startCoord direction startingChars (wordList, List.length wordList)
                    | _, false -> 
                        "pass"

                formattedWord)

    let GetStartingInfo (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
        let occupied = boardMap.Keys |> Seq.cast |> List.ofSeq

        let occupiedSet = Set.ofList occupied

        //for OccupiedSet in occupiedSet do
        //    printf "occupiedSet: %A" OccupiedSet
        
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
            if
                belowHelper coord2
                && leftHelper coord2
                && rightHelper coord2
                && acc < 7u
            then
                verticalLen coord2 (acc + 1u)
            else
                acc 
        
        let rec horizontalLen (coord: coord) (acc: uint32) : uint32 =
            let coord2: coord = (fst coord + 1, snd coord)
            if
                rightHelper coord2
                && aboveHelper coord2
                && belowHelper coord2
                && acc < 7u
            then
                horizontalLen coord2 (acc + 1u)
            else
                acc
        
        let rec verticalHelper (coord: coord) =
            if belowHelper coord && aboveHelper coord then
                [ (coord, ((0, 1): coord), [ Map.find coord boardMap ], verticalLen coord 0u) ]
            elif belowHelper coord && not (aboveHelper coord) then
                [ (coord, ((0, 1): coord), lettersAbove coord [ Map.find coord boardMap ], verticalLen coord 0u) ]
            else
                []
        
        let rec horizontalHelper (coord: coord) =
            if rightHelper coord && leftHelper coord then
                [ (coord, ((1, 0): coord), [ Map.find coord boardMap ], horizontalLen coord 0u) ]
            elif rightHelper coord && not (leftHelper coord) then
                [ (coord, ((1, 0): coord), lettersLeft coord [ Map.find coord boardMap ], horizontalLen coord 0u) ]
            else
                []
        
        let verticalStarters = List.collect verticalHelper occupied
        let horizontalStarters = List.collect horizontalHelper occupied
        
        verticalStarters @ horizontalStarters
