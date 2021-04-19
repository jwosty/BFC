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

let (|MoveMulLoopBody|_|) loopBody =
    let rec f decrIsAtBeginning ΔpAcc list =
        // either we saw the decrementer instruction at the beginning, in which case we shouldn't see it again at the end,
        // or we didn't see it at the beginning, in which case we need to see it at the end.
        match decrIsAtBeginning, list with
        | true, [AddPtr p] when p = -ΔpAcc -> Some []
        | false, [AddPtr p; AddCell -1] when p = -ΔpAcc -> Some []
        | _, AddPtr p :: AddCell c :: xs ->
            let ΔpAcc' = ΔpAcc+p
            match f decrIsAtBeginning ΔpAcc' xs with
            | Some xs -> Some ((ΔpAcc',c)::xs)
            | None -> None
        | _ -> None

    match loopBody with
    | [] -> None
    | AddCell -1 :: rest -> f true 0 rest
    | rest -> f false 0 rest

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
