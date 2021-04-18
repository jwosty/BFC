module BF.Optimizer
open System
open BF.Parser

/// Level-1 optimizations - collect instruction sequences together -- for example, `+++` becomes AddCell(3)
let rec optimize1 instructions =
    match instructions with
    | [] -> []
    | AddPtr a :: AddPtr b :: rest -> AddPtr(a+b) :: rest |> optimize1
    | AddCell a :: AddCell b :: rest -> AddCell(a+b) :: rest |> optimize1
    | WhileNonzero(instructions) :: rest -> WhileNonzero(optimize1 instructions) :: optimize1 rest
    | this :: rest -> this :: optimize1 rest

/// Level-2-only optimizations
let rec optimize2 instructions =
    match instructions with
    | [] -> []
    // optimize clear-loops (`[-] and [+]` become ClearCell)
    | WhileNonzero([AddCell -1 | AddCell 1]) :: rest -> ClearCell :: optimize2 rest
    
    // move loops
    //| WhileNonzero([AddCell -1; AddPtr dst; AddCell 1; AddPtr ndst]) :: rest when dst = -ndst ->
    //    MoveMulCell (0,dst,1) :: optimize2 rest
    | WhileNonzero([AddCell -1; AddPtr dst; AddCell factor; AddPtr ndst]) :: rest when dst = -ndst ->
        MoveMulCell (0, dst, factor) :: optimize2 rest

    | WhileNonzero(instructions) :: rest -> WhileNonzero(optimize2 instructions) :: optimize2 rest
    | this :: rest -> this :: optimize2 rest

/// All optimizations
let rec optimize instructions =
    instructions
    |> optimize1
    |> optimize2
