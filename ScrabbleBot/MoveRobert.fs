module internal MoveRobert

    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open System
    open ScrabbleUtil.ServerCommunication
    
    open System.IO
    open ScrabbleUtil.DebugPrint
    
    let RobertsFirstMove (InitCharactersOnHand : uint32 list) pieces (dict : Dictionary.Dict) (initStartingInfo : (coord * coord * uint32 list * uint32))=
            
        let charactersOnHand = InitCharactersOnHand |> List.filter (fun x -> x <> 0u)

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
        // or at the end, this limits us alot, but we couldn't find a
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

        //here we already check if we should pass
        if not isValidInit then "pass"
        else 
            //if its valid, wer get all possible words from this startinginfo
            let possibleWords = GetAllPossibleWords dict (charactersOnHand |> List.filter (fun char -> not (List.contains char startingChars))) preparedWord
            
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
                        if isValidWord then
                            let (validWords, allValid) = filterValidWords dict (rest, valid)
                            (word :: validWords, allValid)
                        else
                            let (validWords, allValid) = filterValidWords dict (rest, valid)
                            (validWords, false)

                //here we call the function above
                let (filteredWords, allValid) = filterValidWords dict possibleWords
 
                let longestWord = 
                    let mutable (_, isValid) = GetAllPossibleWords preparedDict (charactersOnHand |> List.filter (fun char -> not (List.contains char startingChars))) preparedWord
                    if not isValid then 
                        ([], false)
                    else
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
                    let mutable deltaX, deltaY = direction
                    deltaY <- (deltaY)
                    deltaX <- (deltaX)
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
                            // prinft "\n2.5 \n"
                            
                            // Check if the first letter should be removed
                            let removeFirstLetter = 
                                isFirstLetter && 
                                (List.isEmpty startingChars || 
                                List.head startingChars = uint32 (int letter - int 'A' + 1))
                            // prinft "\n2.6 %A \n" (uint32 (int letter - int 'A' + 1))
                            
                            // Recursively format the rest of the word list
                            if removeFirstLetter then
                                // prinft "\n2.7 \n"
                                formatter acc (x + deltaX, y + deltaY) false tl
                            else
                                // prinft "\n2.8 \n"
                                formatter (formatted::acc) (x + deltaX, y + deltaY) false tl

                    let isFirstLetter = not (List.isEmpty startingChars) && List.head startingChars = List.head wordList
                    
                    //adjusts the direction after
                    let changedX, changedY =
                        if isFirstLetter then
                            match direction with
                            | 1, 0 ->  startX, startY + deltaY 
                            | 0, 1 -> startX + deltaX, startY
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

  
    //Takes the boardMap, finds all vertical starters, and returns them as a list of truples: (coord:StartingPointOfStarter, coord:Direction, list<uint32>:ListOfTilesBeforeStarter)

    let getAllStarters (boardMap: Map<coord, uint32>) : (coord * coord * (uint32) list * uint32) list =
        let keys = (boardMap.Keys |> Seq.cast |> List.ofSeq)
        //If tile above c is clear return true, else return false
        let abovePredicate (c: coord) : bool =
            let cAbove = (fst c, snd c - 1): coord
            let tileAbove = Map.tryFind (cAbove) boardMap
            if tileAbove = None then true else false
        //If tile below c is clear return true, else return false
        let belowPredicate (c: coord) : bool =
            let cBelow = (fst c, snd c + 1): coord
            let tileBelow = Map.tryFind (cBelow) boardMap
            if tileBelow = None then true else false
        //If tile to the left of c is clear return true, else return false
        let leftPredicate (c: coord) : bool =
            let cLeft = (fst c - 1, snd c): coord
            let tileLeft = Map.tryFind (cLeft) boardMap
            if tileLeft = None then true else false
        //If tile to the right of c is clear return true, else return false
        let rightPredicate (c: coord) : bool =
            let cRight = (fst c + 1, snd c): coord
            let tileRight = Map.tryFind (cRight) boardMap
            if tileRight = None then true else false
        //Recursively get letters above tile
        let rec getLettersAboveCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c, snd c - 1)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersAboveCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc

        let rec getLettersBelowCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c, snd c + 1)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersBelowCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc
        
        let rec getLettersLeftOfCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c-1, snd c)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersLeftOfCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc

        let rec getLettersRightOfCoord (c: coord) (acc: (uint32) list) : (uint32) list =
            let coordToInvestigate: coord = (fst c + 1, snd c)

            if List.contains (coordToInvestigate: coord) keys then
                getLettersRightOfCoord coordToInvestigate ((Map.find coordToInvestigate boardMap) :: acc)
            else
                acc        

        let rec getVerticalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c, snd c + 1)

            if
                belowPredicate coordToInvestigate
                && leftPredicate coordToInvestigate
                && rightPredicate coordToInvestigate
                && acc < 7u
            then
                getVerticalLength coordToInvestigate (acc + 1u)
            else
                acc
        
        let rec getRevVerticalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c, snd c - 1)

            if
                abovePredicate coordToInvestigate
                && leftPredicate coordToInvestigate
                && rightPredicate coordToInvestigate
                && acc < 7u
            then
                getRevVerticalLength coordToInvestigate (acc + 1u)
            else
                acc
        
        let rec getHorizontalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c + 1, snd c)
            if
                rightPredicate coordToInvestigate
                && abovePredicate coordToInvestigate
                && belowPredicate coordToInvestigate
                && acc < 7u
            then
                getHorizontalLength coordToInvestigate (acc + 1u)
            else
                acc
        
        let rec getRevHorizontalLength (c: coord) (acc: uint32) : uint32 =
            let coordToInvestigate: coord = (fst c - 1, snd c)
            if
                leftPredicate coordToInvestigate
                && abovePredicate coordToInvestigate
                && belowPredicate coordToInvestigate
                && acc < 7u
            then
                getRevHorizontalLength coordToInvestigate (acc + 1u)
            else
                acc
        
        let rec verticalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if belowPredicate c && abovePredicate c then
                [ (c, ((0, 1): coord), [ Map.find c boardMap ], getVerticalLength c 0u) ]
            else if belowPredicate c && not (abovePredicate c) then
                [ (c, ((0, 1): coord), getLettersAboveCoord c [ Map.find c boardMap ], getVerticalLength c 0u) ]
            else
                []
        
        let rec reverseVerticalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if belowPredicate c && abovePredicate c then
                [ (c, ((0, -1): coord), [ Map.find c boardMap ], getRevVerticalLength c 0u) ]
            else if belowPredicate c && not (abovePredicate c) then
                //if (List.contains (fst c, snd c - 1) keys)
                //printf "HERE 1: %A" (getLettersBelowCoord c [ Map.find c boardMap ])
                [ (c, ((0, -1): coord), getLettersBelowCoord c [ Map.find c boardMap ], getRevVerticalLength c 0u) ]
            else
                []
        let rec horizontalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if rightPredicate c && leftPredicate c then
                //printfn "1 - coord: %A" c
                //printfn "mapFind: %A" (Map.find c boardMap)
                //printf "HERE 3: %A" (getLettersBelowCoord c [ Map.find c boardMap ])
                [ (c, ((1, 0): coord), [ Map.find c boardMap ], getHorizontalLength c 0u) ]
            else if rightPredicate c && not (leftPredicate c) then
                // "HERE 2: %A" (getLettersBelowCoord c [ Map.find c boardMap ])
                [ (c, ((1, 0): coord), getLettersLeftOfCoord c [ Map.find c boardMap ], getHorizontalLength c 0u) ]
            else
                []
        
        let rec reverseHorizontalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if rightPredicate c && leftPredicate c then
                //printfn "2 - coord: %A" c
                //printfn "mapFind: %A" (Map.find c boardMap)
                //printf "HERE 4: %A" (getLettersBelowCoord c [ Map.find c boardMap ])
                [ (c, ((-1, 0): coord), [ Map.find c boardMap ], getRevHorizontalLength c 0u) ]
            else if rightPredicate c && not (leftPredicate c) then
                [ (c, ((-1, 0): coord), getLettersRightOfCoord c [ Map.find c boardMap ], getRevHorizontalLength c 0u) ]
            else
                []
        
        let verticalStarters = List.fold (fun acc c -> List.append acc (verticalPredicateHandler c)) [] keys
        let horizontalStarters = List.fold (fun acc c -> List.append acc (horizontalPredicateHandler c)) [] keys
        let reverseVerticalStarters = List.fold (fun acc c -> List.append acc (reverseVerticalPredicateHandler c)) [] keys
        let reverseHorizontalStarters = List.fold (fun acc c -> List.append acc (reverseHorizontalPredicateHandler c)) [] keys
        verticalStarters @ horizontalStarters @ reverseVerticalStarters @ reverseHorizontalStarters