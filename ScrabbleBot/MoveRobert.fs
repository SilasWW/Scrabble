module internal MoveRobert

    open MultiSet
    open Parser
    open ScrabbleUtil
    open ScrabbleUtil.Dictionary
    open System
    
    let RobertsFirstMove (hand : MultiSet<uint32>) (board:board) (dict : Dict) (playedLetters: Map<coord, (char * int)>) (coord:coord) (direction:(int * int)) =
        
        let uintToChar id = char(id + 64u)

        let multisetToChar = MultiSet.map (fun (i:uint) ->
            if i = 0u then
                "ABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray() |> Array.toList
            else
                [uintToChar i]
            ) 
        
        // Convert the multisetToChar to a list of lists of chars
        let wordBuildingBlock = [multisetToChar hand]

        // Find all valid words using wordBuildingBlock and the dictionary
        let findMatches (wordBuildingBlock: list<list<char>>) (dictionary: Dict) =
            let rec buildWords (prefix: string) (blocks: list<list<char>>) =
                match blocks with
                | [] -> if lookup prefix dictionary then [prefix] else []
                | block :: rest ->
                    List.collect (fun c -> buildWords (prefix + string c) rest) block

            List.collect (fun block -> buildWords "" block) wordBuildingBlock

        let possibleWords = findMatches wordBuildingBlock dict

        for word in possibleWords do
            printfn "word: %A" word

        let accWord = ""

        accWord
