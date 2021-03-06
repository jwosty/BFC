﻿module BF.Interpreter.Main
open System
open System.IO
open BF.Optimizer
open BF.Parser

/// Interprets a BF program
let rec execute (cells: char[]) currentCell program =
    program |> Seq.fold (fun currentCell instruction ->
        match instruction with
        | AddPtr(n) -> currentCell + n
        | AddCell(pOffset,n) -> cells.[currentCell+pOffset] <- cells.[currentCell] + char n; currentCell
        | ClearCell pOffset -> cells.[currentCell+pOffset] <- '\000'; currentCell
        | Read -> cells.[currentCell] <- (System.Console.ReadKey ()).KeyChar; currentCell
        | Write -> System.Console.Write cells.[currentCell]; currentCell
        | WhileNonzero(loopProgram) ->
            recurse
                (fun currentCell -> cells.[currentCell] <> '\000')          // base case
                (fun currentCell -> execute cells currentCell loopProgram)  // body
                currentCell)
      currentCell

[<EntryPoint>]
let main args =
    if args.Length > 0 then
        args.[0] |> File.ReadAllText
        |> parse |> toIR |> optimize
        |> execute (Array.zeroCreate 1024) 0
        |> ignore
        0
    else
        printfn "No input file specified."
        1