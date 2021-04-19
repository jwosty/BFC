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
        | AddCell (0, a) :: AddCell (0, b) :: rest -> AddCell(0, a+b) :: rest |> optimize1
        | WhileNonzero(instructions) :: rest -> WhileNonzero(optimize1 instructions) :: optimize1 rest
        | this :: rest -> this :: optimize1 rest
    match instructions with
    | WhileNonzero _ :: rest -> optimize1 rest
    | _ -> optimize1 instructions

let (|MoveMulLoopBody|_|) loopBody =
    let rec f decrIsAtBeginning ΔpAcc list =
        // either we saw the decrementer instruction at the beginning, in which case we shouldn't see it again at the end,
        // or we didn't see it at the beginning, in which case we need to see it at the end.
        match decrIsAtBeginning, list with
        | true, [AddPtr p] when p = -ΔpAcc -> Some []
        | false, [AddPtr p; AddCell (0,-1)] when p = -ΔpAcc -> Some []
        | _, AddPtr p :: AddCell (0,c) :: xs ->
            let ΔpAcc' = ΔpAcc+p
            match f decrIsAtBeginning ΔpAcc' xs with
            | Some xs -> Some ((ΔpAcc',c)::xs)
            | None -> None
        | _ -> None

    match loopBody with
    | [] -> None
    | AddCell (0,-1) :: rest -> f true 0 rest
    | rest -> f false 0 rest

/// Level-2-only optimizations
let rec optimize2 instructions =
    match instructions with
    | [] -> []
    // optimize clear-loops (`[-] and [+]` become ClearCell)
    | WhileNonzero([AddCell(0,-1) | AddCell (0, 1)]) :: rest -> ClearCell 0 :: optimize2 rest
    
    // move loops
    | WhileNonzero(MoveMulLoopBody targets) :: rest -> MoveMulCell targets :: optimize2 rest

    | WhileNonzero(instructions) :: rest -> WhileNonzero (optimize2 instructions) :: optimize2 rest
    | this :: rest -> this :: optimize2 rest
    
/// An active pattern that matches any IR instruction that has an offset associated with it. First parameter returned
/// is said offset, and the second parameter is a function that recreates the matched instruction with the given value
/// added to the offset
let (|Offsettable|_|) instruction =
    match instruction with
    | AddCell (offset, n) -> Some (fun x -> AddCell (offset+x, n))
    | ClearCell offset -> Some (fun x -> ClearCell (offset+x))
    | MoveMulCell destinations ->
        Some (fun x ->
            destinations
            |> List.map (fun (offset, n) ->
                (offset+x, n)
            )
            |> MoveMulCell
        )

    | AddPtr _ | Read | Write | WhileNonzero _ -> None

/// Level-3-only optimizations
let rec optimize3 instructions =
    match instructions with
    | [] -> []
    
    | AddPtr offsetA :: Offsettable addToInstOffset :: AddPtr offsetC :: rest ->
        let offset = offsetA+offsetC
        [   yield addToInstOffset offsetA
            if offset <> 0 then yield AddPtr offset
            yield! rest]
        |> optimize3
    | AddPtr offsetA :: Offsettable addToInstOffset :: rest ->
        [   yield addToInstOffset offsetA
            if offsetA <> 0 then yield AddPtr offsetA
            yield! rest]
        |> optimize3

    | WhileNonzero instructions :: rest ->
        let blah = optimize3 instructions
        let orest = optimize3 rest
        WhileNonzero blah :: orest
    | this :: rest -> this :: optimize3 rest

let optimizeUpto1 instructions = optimize1 instructions
let optimizeUpto2 instructions = instructions |> optimizeUpto1 |> optimize2
let optimizeUpto3 instructions = instructions |> optimizeUpto2 |> optimize3

/// All optimizations
let rec optimize instructions = optimizeUpto3 instructions
