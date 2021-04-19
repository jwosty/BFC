module BF.Optimizer
open System
open BF.Parser

#nowarn "0049"

/// Level-1 optimizations - collect instruction sequences together and remove beginning comments -- for example, `+++` becomes AddCell(3)
let optimize1 instructions =
    let rec optimize1 instructions =
        match instructions with
        | [] -> []
        | AddPtr a :: AddPtr b :: rest -> AddPtr(a+b) :: rest |> optimize1
        | AddCell a :: AddCell b :: rest -> AddCell(a+b) :: rest |> optimize1
        | WhileNonzero(instructions) :: rest -> WhileNonzero(optimize1 instructions) :: optimize1 rest
        | this :: rest -> this :: optimize1 rest
    match instructions with
    | WhileNonzero _ :: rest -> optimize1 rest
    | _ -> optimize1 instructions

//type MulLoopBodyParserState =
//    | Init
//    | 

type MulLoopBodyParserState =
    | Init
    | ExpectingAddPtr of decrWasFirst:bool
    | ExpectingAddCell of decrWasFirst:bool
    | Error

let (|MoveMulLoopBody|_|) loopBody =
    // Move-mul loops can either have the iterator decrement instruction at the beginning (e.g. [->+<]) or  the end
    // (e.g. [>+<-]) of the loop -- both work perfectly fine.
    // We also account here for loops with multiple destinations.

    //let rec innerCollect decrWasFirst instructions =
    //    match instructions with
    //    // we should have already caught the [-] scenario in an earlier optimization level, so this branch actually does
    //    // NOT correspond to that situation
    //    | [] when decrWasFirst -> None
    //    | [] -> Some []
    //    | AddPtr relDst :: AddCell relFactor :: rest

    //let blah =
    //    match loopBody with
    //    | [] -> None
    //    | AddCell -1 :: rest ->
    //        // essentially a little finite state machine here
    //        let xyz = loopBody |> List.fold (fun state item ->
    //            match state with
    //            | None -> 
    //        )
    //        []
    //    | rest -> []

    //let blah = 
    //    loopBody |> List.fold (fun state item ->
    //        match state, item with
    //        | MulLoopBodyParserState.Init, AddCell -1 ->
    //            MulLoopBodyParserState.ExpectingAddPtr true
    //        | MulLoopBodyParserState.ExpectingAddCell, AddPtr p ->
    //            MulLoopBodyParserState.ExpectingAddPtr
    //    )

    let rec f1 ΔpAcc list =
        match list with
        | [AddPtr p] when p = -ΔpAcc -> Some []
        | AddPtr p :: AddCell c :: xs ->
            let ΔpAcc' = ΔpAcc+p
            match f1 ΔpAcc' xs with
            | Some xs -> Some ((ΔpAcc',c)::xs)
            | None -> None
        | _ -> None

    let rec f2 ΔpAcc list =
        match list with
        | [AddPtr p; AddCell -1] when p = -ΔpAcc -> Some []
        | AddPtr p :: AddCell c :: xs ->
            let ΔpAcc' = ΔpAcc+p
            match f2 ΔpAcc' xs with
            | Some xs -> Some ((ΔpAcc',c)::xs)
            | None -> None
        | _ -> None

    match loopBody with
    | AddCell -1 :: rest ->
        match f1 0 rest with
        | Some result -> Some result
        | None -> None
    | rest -> f2 0 rest

    //match loopBody with
    //| [] -> None
    //| AddCell -1 :: rest ->
    //    ()

    //match loopBody with
    //| [AddCell -1; AddPtr dst; AddCell factor; AddPtr ndst]
    //| [AddPtr dst; AddCell factor; AddPtr ndst; AddCell -1] when dst = -ndst ->
    //    Some [dst, factor]
    //| _ -> None

/// Level-2-only optimizations
let rec optimize2 instructions =
    match instructions with
    | [] -> []
    // optimize clear-loops (`[-] and [+]` become ClearCell)
    | WhileNonzero([AddCell -1 | AddCell 1]) :: rest -> ClearCell :: optimize2 rest
    
    // move loops
    | WhileNonzero(MoveMulLoopBody targets) :: rest -> MoveMulCell targets :: optimize2 rest

    | WhileNonzero(instructions) :: rest -> WhileNonzero(optimize2 instructions) :: optimize2 rest
    | this :: rest -> this :: optimize2 rest

/// All optimizations
let rec optimize instructions =
    instructions
    |> optimize1
    |> optimize2
