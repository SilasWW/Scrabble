module internal Dictionary

    type Trie =
        | Leaf of bool
        | Node of bool * System.Collections.Generic.Dictionary<char, Trie>

    type TrieDictionary = System.Collections.Generic.Dictionary<char, Trie>

    let emptyTrie () = Leaf false

    let rec insertTrie (word: string) =
        function
        | Leaf _ when word.Length = 0 -> Leaf true
        | Node (_, trieDict) when word.Length = 0 -> Node(true, trieDict)
        | Leaf isLeaf  -> 
            let tmpDict = TrieDictionary ()
            let charToAdd = word.[0]
            tmpDict.[charToAdd] <- insertTrie word.[1..] (emptyTrie ())
            Node(isLeaf, tmpDict)
        | Node (isNode, dict) ->
            let charToAdd = word.[0]

            match dict.TryGetValue charToAdd with
            | (true, value) ->
                dict.[charToAdd] <- insertTrie word.[1..] value
                Node(isNode, dict)
            | (false, _)    ->
                dict.[charToAdd] <- insertTrie word.[1..] (emptyTrie())
                Node(isNode, dict)
    

    let rec lookupTrie (word: string) =
        function
        | Leaf isLeaf when word.Length = 0 -> isLeaf
        | Leaf _ -> false

        | Node (isNode, _) when word.Length = 0 -> isNode
     
        | Node (isNode, dict) ->
            match dict.TryGetValue word.[0] with
            | (true, value) -> lookupTrie word.[1..] value
            | (false, _) -> false

    let stepintoTrie (c: char) =
        function
        | Node (_, dict) ->
            match dict.TryGetValue c with
            | (true, value) -> 
                match value with
                | Leaf b -> Some (b, value)
                | Node (b, _) -> Some (b, value)
            | (false, _) -> None
        | Leaf _ -> None