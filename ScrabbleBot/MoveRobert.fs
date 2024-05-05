module internal MoveRobert

    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open System
    open ScrabbleUtil.ServerCommunication
    
    open System.IO
    open ScrabbleUtil.DebugPrint
    
    let RobertsFirstMove (hand : MultiSet<uint32>) (board : board) (charactersOnHand : uint32 list) pieces (dict : Dictionary.Dict) (playedLetters : Map<coord, (char * int)>) (coord : coord) (direction : (int * int)) (initStartingInfo : (coord * coord * uint32 list * uint32) list)=
            
        //print the how many letters have been played
        printf "PlayedLetters.Count = %A words" playedLetters.Count

        let rand = new System.Random()
        let randomIndex = rand.Next(initStartingInfo.Length)
        let StartingInfo = initStartingInfo.[randomIndex]

        printf "\n1.1 \n"
        //states how long a word can be, which should be set by StartingInfo
        let maxLengthOfWord = StartingInfo |> fun (_, _, _, len) -> len

        //extracts the pair from from the set
        let GetTuple (set:Set<char*int>) : char*int = 
            match (Set.toList set) with
            | x::xs -> x
            | [] -> failwith "error"
        
        //removes the letter as it is used to try to form a word
        let RemoveUsedLetterFromHand letterToBeRemoved charactersOnHand = 
            let foundIndex = List.tryFindIndex (fun x -> x = letterToBeRemoved) charactersOnHand
            match foundIndex with
            | Some(i) -> List.removeAt i charactersOnHand
            | None -> charactersOnHand
        
        // Helper function to process starting characters into a valid initial dictionary state
        let InitializeWithStartingChars (dict: Dictionary.Dict) (chars: list<uint32>) =
            List.fold (fun (accDict, accWord, success) char ->
                match Dictionary.step (fst (GetTuple (Map.find char pieces))) accDict with
                | Some (_, nextDict) -> 
                    printfn "Step successful for character %A" char
                    (nextDict, accWord @ [char], true)
                | None -> 
                    printfn "Step failed for character %A" char
                    (accDict, accWord, false)
            ) (dict, [], true) chars


        // Modified GetAllPossibleWords to use initialized state
        let rec GetAllPossibleWords (dict: Dictionary.Dict) (remainingLetters: uint32 list) (currentWord: list<uint32>) =
            if (uint32 currentWord.Length) >= maxLengthOfWord then ([currentWord], true) else
            let (words, valid) =
                List.fold (fun (acc, valid) letter ->
                    let IsWordFoundAndDict = Dictionary.step (fst (GetTuple (Map.find letter pieces))) dict
                    match IsWordFoundAndDict with
                    | Some (isWord, nextDict) ->
                        let newWord = currentWord @ [letter]
                        let newAcc = if isWord then newWord::acc else acc
                        (newAcc @ (fst (GetAllPossibleWords nextDict (RemoveUsedLetterFromHand letter remainingLetters) newWord)), true)
                    | None ->
                        (acc, false)
                ) ([], true) remainingLetters
            (words, valid && List.length words > 0) 

        // Adjust the starting point of possible words
        printf "\n1 \n"
        let _, _, startingChars, _ = StartingInfo

        printf "\n1.1.1 %A\n" startingChars

        let initializedDict, initialWord, isValidInit = InitializeWithStartingChars dict startingChars
        printf "\n1.1.2 \n"
        if not isValidInit then "pass"
        else 
            let possibleWords = GetAllPossibleWords initializedDict (charactersOnHand |> List.filter (fun char -> not (List.contains char startingChars))) initialWord
            printf "\n1.1.3 \n"
                    
            match possibleWords with
            | _, false -> "pass"
            | words, true -> (
                
                // Adjust the possible words to remove the startingChars from each word
                let adjustedWords = 
                    let (possibleWords, isValid) = GetAllPossibleWords initializedDict (charactersOnHand |> List.filter (fun char -> not (List.contains char startingChars))) initialWord
                    if not isValid then 
                        ([], false)
                    else 
                        (possibleWords |> List.map (fun word -> List.skip (List.length startingChars) word), true)

                
                printf "\n1.2 \n"

                let findLongestList (lists : List<List<uint32>>) =
                    let mutable maxLength = 0
                    let mutable maxList = []
                    printf "\n1.3 \n"

                    printf "\nList count: %d\n" (List.length lists)
                    printf "\n1.4 \n"
                    for lst in lists do
                        let length = List.length lst
                        if length > maxLength then
                            printf "\n1.5 \n"
                            maxLength <- length
                            maxList <- lst

                    if maxList.IsEmpty then failwith "Empty list of lists"
                    else (maxList, maxLength)
                    
                printf "\n1.8 \n"
                
              
                let longestWord = 
                    let (possibleWords, isValid) = GetAllPossibleWords initializedDict (charactersOnHand |> List.filter (fun char -> not (List.contains char startingChars))) initialWord
                    if not isValid then 
                        ([], false)  // If no valid words are found, return empty and false
                    else
                        let mutable maxLength = 0
                        let mutable maxList = []
                        for lst in possibleWords do
                            let length = List.length lst
                            if length > maxLength then
                                maxLength <- length
                                maxList <- lst
                        if maxList.IsEmpty then 
                            ([], false)  // No words found, return false
                        else 
                            (maxList, true)  // Return the longest word found and true
                
                printf "\n2 \n"

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

                // Add debugging output to trace function execution
                let longestWordFormat (startCoord: coord) (direction: coord) (startingChars: uint32 list) (wordListWithCount: List<uint32> * int) : string =
                    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                    printf "\n2.1 \n"
                    let deltaX, deltaY = direction
                    let startX, startY = startCoord
                    printf "\n2.2 \n"
                    // Adjust the starting position based on the length of startingChars
                    let initialX = startX + deltaX * List.length startingChars
                    let initialY = startY + deltaY * List.length startingChars
                    printf "\n2.3 \n"
                    let wordList, count = wordListWithCount  // Decompose the tuple into wordList and count
                    
                    printf "WORDLIST %A\n" wordList  // Debugging output for word list
                    
                    printf "\n2.4 \n"

                    let rec formatHelper acc (x, y) isFirstLetter = function
                        | [] -> String.concat " " (List.rev acc)
                        | hd::tl ->
                            let number = int hd 
                            let letterIndex = number - 1  // Adjust index because 'A' = 1 in your system
                            if letterIndex < 0 || letterIndex >= alphabet.Length then
                                printfn "this is just before a failwith. letterIndex is: %A" letterIndex
                                failwith "Character index out of range"
                            let mutable letter = alphabet.[letterIndex]
                            match letterIndex with
                             | 0 -> letter <- alphabet.[1]
                             | _ -> letter <- alphabet.[letterIndex]
                            let points = charNumberToPoints number  // Get corresponding points for the number
                            let formatted = sprintf "%d %d %d%c%d" x y number letter points  // Concatenate number and points after the letter
                            printf "\n2.5 \n"
                            
                            // Check if the first letter should be removed
                            let removeFirstLetter = 
                                isFirstLetter && 
                                (List.isEmpty startingChars || 
                                List.head startingChars = uint32 (int letter - int 'A' + 1))
                            printf "\n2.6 %A \n" (uint32 (int letter - int 'A' + 1))
                            
                            // Recursively format the rest of the word list
                            if removeFirstLetter then
                                printf "\n2.7 \n"
                                formatHelper acc (x + deltaX, y + deltaY) false tl
                            else
                                printf "\n2.8 \n"
                                formatHelper (formatted::acc) (x + deltaX, y + deltaY) false tl

                    let initialIsFirstLetter = not (List.isEmpty startingChars) && List.head startingChars = List.head wordList
                    let adjustedInitialX, adjustedInitialY =
                        if initialIsFirstLetter then
                            match direction with
                            | 1, 0 -> initialX - 1, initialY // If direction is (1, 0), shift in x direction
                            | 0, 1 -> initialX, initialY - 1 // If direction is (0, 1), shift in y direction
                            | _ -> initialX, initialY // No shift otherwise
                        else
                            initialX, initialY // Maintain the current coordinates if the first letter is not removed

                    formatHelper [] (adjustedInitialX, adjustedInitialY) initialIsFirstLetter wordList


                // Debugging output for function completion
                printf "\n2.2 \n"

                let startCoord, direction, startingChars, _ = StartingInfo

                printf "STARTING CHARS: %A" startingChars
                

                // Call longestWordFormat with provided inputs
                let formattedWord = 
                    match longestWord with
                    | wordList, true -> 
                        // If the operation was successful, format the word list for output
                        longestWordFormat startCoord direction startingChars (wordList, List.length wordList)
                    | _, false -> 
                        // If the operation failed, return an error message directly
                        "pass"

                
                
                printf "\n3 \n"
                //for word in possiblewords do
                    //printfn "word: %A" formattedWord
                
                printfn "WORD PLAYED: %A" formattedWord

                let accWord = ""

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
                [ (c, ((0, -1): coord), getLettersBelowCoord c [ Map.find c boardMap ], getRevVerticalLength c 0u) ]
            else
                []
        
        let rec horizontalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if rightPredicate c && leftPredicate c then
                [ (c, ((1, 0): coord), [ Map.find c boardMap ], getHorizontalLength c 0u) ]
            else if rightPredicate c && not (leftPredicate c) then
                [ (c, ((1, 0): coord), getLettersLeftOfCoord c [ Map.find c boardMap ], getHorizontalLength c 0u) ]
            else
                []
        
        let rec reverseHorizontalPredicateHandler (c: coord) =
            //If both above and below is clear, return list with starter for down direction (Can be extended to both up and down direction if we want to look for both).
            if rightPredicate c && leftPredicate c then
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