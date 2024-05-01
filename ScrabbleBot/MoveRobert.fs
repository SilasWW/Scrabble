module internal MoveRobert

    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open System
    
    let RobertsFirstMove (hand : MultiSet<uint32>) (board : board) (charactersOnHand : uint32 list) pieces (dict : Dictionary.Dict) (playedLetters : Map<coord, (char * int)>) (coord : coord) (direction : (int * int)) =

        //shouldn't be used, just for setting an initial value to StartingInfo
        let mutable (StartingInfo : coord * coord * list<uint32> * uint32) = ((-99, 99), (99, 99), [], 7u)
        
        //print the how many letters have been played
        printf "PlayedLetters.Count = %A words" playedLetters.Count

        //sets starter values, when its the first turn
        if (playedLetters.Count = 0) then
            StartingInfo <- ((-1, 0), (1, 0), [], 7u)
            ()
        else 
            //this should be changed to handle when its not the first turn
            StartingInfo <- ((-1, 0), (1, 0), [], 7u)
            ()

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

        //helperfunction to find all possible words that can be played
        let rec GetAllPossibleWords (remainingLetters : Dict) (charactersOnHand : uint32 list) (word : list<uint32>) =
            //this checks if our word has reached the max length
            if ((uint32) word.Length) >= maxLengthOfWord then [] else
            //our fold call over letters, we patternmatch if a if the next letter is found, if it is not and if IsWordFoundAndDict is empty
            List.fold (fun acc letter ->
                let IsWordFoundAndDict = step (fst (GetTuple (Map.find letter pieces))) remainingLetters
                match IsWordFoundAndDict with
                | Some result when (fst result) -> 
                    let newWord = List.append word [letter]
                    (newWord::acc) @ (GetAllPossibleWords (snd result) (RemoveUsedLetterFromHand letter charactersOnHand) newWord)
                | Some result when not (fst result) ->
                    let newWord = List.append word [letter]
                    acc @ (GetAllPossibleWords (snd result) (RemoveUsedLetterFromHand letter charactersOnHand) newWord)
                | None -> acc
            ) [] charactersOnHand
        
        //collects all possible words
        let possiblewords = GetAllPossibleWords dict charactersOnHand []

        for word in possiblewords do
            printfn "word: %A" word

        let accWord = ""

        accWord
