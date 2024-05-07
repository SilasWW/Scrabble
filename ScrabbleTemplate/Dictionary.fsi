module internal Dictionary

    type Trie

    val emptyTrie : unit -> Trie

    val insertTrie : string -> Trie -> Trie

    val lookupTrie : string -> Trie -> bool

    val stepintoTrie : char -> Trie -> (bool * Trie) option