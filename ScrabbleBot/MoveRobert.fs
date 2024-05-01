module internal MoveRobert

    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open System
    
    let RobertsFirstMove (hand : MultiSet<uint32>) (board:board) (letters:uint32 list) pieces (dict : Dictionary.Dict) (playedLetters: Map<coord, (char * int)>) (coord:coord) (direction:(int * int)) =

        let starter:coord * coord * list<uint32> * uint32 = ((-1, 0), (1, 0), [], 7u)
        let _, _, _, possibleLength = starter

        let getPairFromSet (set:Set<char*int>) : char*int = 
            match (Set.toList set) with
            | x::xs -> x
            | [] -> failwith "Should not happen!"
        
        let removeLetter letter letters = 
            let index = List.tryFindIndex (fun x -> x = letter) letters
            match index with
            | Some(i) -> List.removeAt i letters
            | None -> letters

        //1. Define helper function that uses recursion to find all continuations
        let rec aux (word:list<uint32>) letters auxDict =
            //Make sure that the length of the current word is not longer than the possible length from the starter
            if ((uint32) word.Length) >= possibleLength then [] else
            //2. Fold over all letters available
            List.fold (fun acc letter ->
                //3. Use step function to check if a word can be made with the given letter
                let result = Dictionary.step (fst (getPairFromSet (Map.find letter pieces))) auxDict
                match result with
                //3.1 If a word is found, append the the new word to the accumulator and continue the search with the new word. 
                | Some r when (fst r) -> 
                    let newWord = List.append word [letter]
                    (newWord::acc) @ (aux newWord (removeLetter letter letters) (snd r))
                //3.2 If a word is not found, but the letter is legal, continue the search with the new word.
                | Some r when not (fst r) ->
                    let newWord = List.append word [letter]
                    acc @ (aux newWord (removeLetter letter letters) (snd r))
                //3.3 If letter is illegal, return empty list.
                | None -> acc
            ) [] letters
        
        let possiblewords = aux [] letters dict

        for word in possiblewords do
            printfn "word: %A" word

        let accWord = ""

        accWord
