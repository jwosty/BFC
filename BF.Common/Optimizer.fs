module BF.Optimizer
open System
open BF.Parser

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
    // move-mul loops can either have the iterator decrement instruction at the beginning (e.g. [->+<]) or  the end
    // (e.g. [>+<-]) of the loop -- both work perfectly fine
    match loopBody with
    | [AddCell -1; AddPtr dst; AddCell factor; AddPtr ndst]
    | [AddPtr dst; AddCell factor; AddPtr ndst; AddCell -1] when dst = -ndst ->
        Some [dst, factor]
    | _ -> None

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
