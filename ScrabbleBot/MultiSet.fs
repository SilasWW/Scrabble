module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32>

    let empty : MultiSet<'a> = R Map.empty

    let isEmpty (s: MultiSet<'a>) : bool =
        match s with
        |R map -> Map.isEmpty map

    let size (R(s)) = Map.fold (fun acc _ count -> acc + count) 0u s

    let contains (a : 'a) (R(s) : MultiSet<'a>) : bool = Map.containsKey a s

    let numItems (a: 'a) (R(s) : MultiSet<'a>) = Map.tryFind a s |> Option.defaultValue 0u

    let add (key : 'a) (num : uint32) (R map : MultiSet<'a>) : MultiSet<'a> =
        let updatedMap =
            match Map.tryFind key map with
            | Some count -> Map.add key (count + num) map
            | None -> Map.add key num map
        R updatedMap

    let addSingle (key : 'a) (R map : MultiSet<'a>) : MultiSet<'a> =
        let updatedMap =
            match Map.tryFind key map with
            | Some count -> Map.add key (count + 1u) map
            | None -> Map.add key 1u map
        R updatedMap

    let remove (a : 'a) (n: uint32) (R(s) : MultiSet<'a>) =
        let element = (Map.tryFind a s) |> Option.defaultValue (0u)
        let updated_count =
            if element < n then 0u
            else element - n
        if updated_count = 0u then
            R(Map.remove a s)
        else
            R(Map.add a updated_count s)

    let removeSingle (a : 'a) (R(s) : MultiSet<'a>) : MultiSet<'a>  = remove a 1u (R(s))

    let fold (f : 'b -> 'a -> uint32 -> 'b) (x : 'b) (R(s): MultiSet<'a>) = Map.fold f x s 
    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (R(s) : MultiSet<'a>) (x : 'b) = Map.foldBack f s x

    let ofList (lst : 'a list) : MultiSet<'a> = List.fold(fun acc element -> addSingle element acc) empty lst

    let toList s = foldBack (fun elem num acc -> List.init (int32 num) (fun _ -> elem) @ acc) s []

    let map f s = ofList (List.map f (toList s))
    
    let subtract s1 s2 =
      fold (fun acc elem _ ->
          remove elem (numItems elem s2) acc) s1 s2