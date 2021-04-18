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
    | WhileNonzero([AddCell(-1)]) :: rest -> ClearCell :: optimize2 rest
    | WhileNonzero(instructions) :: rest -> WhileNonzero(optimize2 instructions) :: optimize2 rest
    | this :: rest -> this :: optimize2 rest

/// All optimizations
let rec optimize instructions =
    instructions
    |> optimize1
    |> optimize2
