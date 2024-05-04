﻿namespace Robert

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

//open FParsecLight.TextParser

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
   
    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        playedLetters : Map<coord, (char * int)>
        numberofplayers : uint32
        CBoard          : Map<coord, uint32>
    }

    let mkState b d pn N pt h L  CB = {board = b; dict = d;  playerNumber = pn; numberofplayers = N; hand = h; playerTurn = pt; playedLetters = L;  CBoard = CB;}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st      = st.playerTurn
    let playedLetters st = st.playedLetters
    let numberofplayers st = st.numberofplayers
    let CBoard st = st.CBoard

    let insertMovesIntoState (moves:list<coord * (uint32 * (char * int))>) (state:state) =
       List.fold (fun acc move ->
           let (coord, (_,(char, charPoints))) = move
            //debugPrint (sprintf "Inserting move %A %A\n" coord (char))
           let newPlayedLetters = acc.playedLetters |> Map.add coord (char, charPoints)
           mkState acc.board acc.dict acc.playerNumber acc.numberofplayers acc.playerTurn acc.hand newPlayedLetters acc.CBoard
        ) state moves
    
    
    let mutable moves : (coord * (uint32 * (char * int))) list = [(0,0), (0u, ('a',0))]
    
    let UpdateMoves ms=
        moves <- ms
    let updateHand ms st newPieces =
                // get a multiset of the indexes (uint) of the tiles you played
                let playedIndexes = 
                    ms
                    |> Seq.map (fun move -> 
                        let (_, (charuint, (_, _))) = move
                        charuint
                        ) 
                    |> Seq.toList 
                    |> MultiSet.ofList

                // remove played tiles from your hand
                let subtractedHand = MultiSet.subtract (hand st) playedIndexes

                // add the new tiles to your hand
                List.fold (fun acc (indexOfLetter, letterCount) -> 
                MultiSet.add indexOfLetter letterCount acc) subtractedHand newPieces

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) (myTurn: bool) =
            if (myTurn) then
                forcePrint "-------------------- Here is your hand ---------------------\n\n" 
                Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //let input =  System.Console.ReadLine() 
                if st.playedLetters.Count = 0 then
                    // First move
                    //sets starter values, when its the first turn
                    let StartingInfo = [((-1, 0), (1, 0), [], 7u)]
                    let letters = MultiSet.toList (State.hand st)
                    let input = MoveRobert.RobertsFirstMove (State.hand st) (State.board st) letters pieces st.dict st.playedLetters (State.board st).center (1,0) (StartingInfo)
                    //let input = System.Console.ReadLine()
                    let move = RegEx.parseMove input

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else
                    // Not first move
                    //sets starter values, when its the first turn
                    let StartingInfo = MoveRobert.getAllStarters (List.fold (fun acc (coord, (id, (_, _))) -> Map.add coord id acc) st.CBoard State.moves) //To Chat-gpt: What do i put here?
                    let letters = MultiSet.toList (State.hand st)

                    //should loop through the startinginfo list instead of just setting .head
                    let input = MoveRobert.RobertsFirstMove (State.hand st) (State.board st) letters pieces st.dict st.playedLetters (State.board st).center (1,0) (StartingInfo)
                    //let input = System.Console.ReadLine()
                    let move = RegEx.parseMove input

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)

            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
               
               State.UpdateMoves ms
               
               // Update playedLetters with new moves
               forcePrint "-------------------- Successful play by you ---------------------\n"
               let updatedStateLetters = State.insertMovesIntoState ms st

               // Update hand
               let newHand = State.updateHand ms st newPieces

               // Update the state
               let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) newHand updatedStateLetters.playedLetters (State.CBoard st)

               aux newState (st.playerNumber % st.numberofplayers + 1u = st.playerNumber)
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                printf "-------------------- Word Played by CMPlayed ---------------------\n"
                //let st' = st // This state needs to be updated
                // Update playedLetters with new moves
                let updatedStateLetters = State.insertMovesIntoState ms st

                let newState = State.mkState (State.board st) (State.dict st) (State.playerNumber st) (State.numberofplayers st) (State.playerTurn st) (State.hand st) updatedStateLetters.playedLetters (State.CBoard st)

                aux newState (pid % st.numberofplayers + 1u = st.playerNumber)
            | RCM (CMPassed (pid)) ->
                debugPrint (sprintf "-------------------- Word Passed byCMPlayed  ---------------------\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")
                aux st (pid % st.numberofplayers + 1u = st.playerNumber)
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                //let st' = st // This state needs to be updated
                aux st (pid % st.numberofplayers + 1u = st.playerNumber)
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st false


        aux st (st.playerTurn = st.playerNumber)

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet Map.empty Map.empty)
        