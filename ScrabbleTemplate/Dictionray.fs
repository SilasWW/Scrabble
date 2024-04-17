module internal Dictionary

    type Dict =
        | Leaf of bool //(* empty csDict)
        | Node of bool * System.Collections.Generic.Dictionary<char, Dict> 

    type csDict = System.Collections.Generic.Dictionary<char, Dict> 

    let empty () = Leaf false

    let rec insert (word: string) =
        function
        | Leaf _ when word.Length = 0 -> Leaf true
        | Node (_, csDict) when word.Length = 0 -> Node(true, csDict)
        | Leaf b  -> 
            let tmp = csDict ()
            let c = word.[0]
            tmp.[c] <- insert word.[1..] (empty ())
            Node(b, tmp)
        | Node (b, dic) ->
            let c = word.[0]

            match dic.TryGetValue c with
            | (true, value) ->
                dic.[c] <- insert word.[1..] value
                Node(b, dic)
            | (false, _)    ->
                dic.[c] <- insert word.[1..] (empty())
                Node(b, dic)
    

    let rec lookup (word: string) =
        function
       
        | Leaf b when word.Length = 0 -> b
        | Leaf _ -> false

       
        | Node (b, _) when word.Length = 0 -> b
     
        | Node (b, dic) ->
            match dic.TryGetValue word.[0] with
            | (true, value) -> lookup word.[1..] value
            | (false, _) -> false

    let step (c: char) =
        function
        | Node (_, dic) ->
            match dic.TryGetValue c with
            | (true, value) -> 
                match value with
                | Leaf b -> Some (b, value)
                | Node (b, _) -> Some (b, value)
            | (false, _) -> None
        | Leaf _ -> None