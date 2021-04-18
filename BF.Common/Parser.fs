module BF.Parser
open System

type Instruction = | IncPtr | DecPtr | IncCell | DecCell | WhileNonzero of Instruction list | Read | Write

let rec recurse baseCase f (state: 'State) =
    if baseCase state
    then recurse baseCase f (f state)
    else state

let parse code =
    let rec parse start expectingRBracket (code: string) =
        (start, [])
        |> recurse
            (fun (i, chr) ->
                if expectingRBracket then
                    if i >= code.Length then failwith "Unmatched right bracket"
                    code.[i] <> ']'
                else i < code.Length)
            (fun (i, tokens) ->
                match code.[i] with
                    | '>' -> i+1, IncPtr::tokens | '<' -> i+1, DecPtr::tokens | '+' -> i+1, IncCell::tokens
                    | '-' -> i+1, DecCell::tokens | ',' -> i+1, Instruction.Read::tokens | '.' -> i+1, Instruction.Write::tokens
                    | '[' ->
                        let afterLoop, loopTokens = parse (i+1) true code
                        afterLoop, (WhileNonzero loopTokens)::tokens
                    | ']' -> failwith "Unexpected right bracket"
                    | _ -> i+1, tokens)
        |> (function (i, tokens) -> i+1, List.rev tokens)
    parse 0 false code |> snd

/// Optimizeable intermediate BF code (IR = intermediate representation)
type IRInstruction =
    | AddPtr of int
    | AddCell of int
    | ClearCell
    | Read
    | Write
    | WhileNonzero of IRInstruction list
    | MoveMulCell of relSrc: int * relDst: int * multiplicationFactor: int

let rec toIR instructions =
    instructions |> List.map (function
        | IncPtr -> AddPtr 1 | DecPtr -> AddPtr -1 | IncCell -> AddCell 1
        | DecCell -> AddCell -1 | Instruction.Read -> Read | Instruction.Write -> Write
        | Instruction.WhileNonzero instructions -> WhileNonzero(toIR instructions))

