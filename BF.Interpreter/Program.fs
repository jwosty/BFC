module BF.Interpreter.Main
open BF.Common
open System
open System.IO

let rec recurse baseCase f (state: 'State) =
  if baseCase state
  then recurse baseCase f (f state)
  else state

/// Interprets a BF program
let rec execute (cells: char[]) currentCell (program: Instruction seq) =
  program |> Seq.fold (fun currentCell instruction ->
    match instruction with
    | AddPtr(n) -> currentCell + n
    | AddCell(n) -> cells.[currentCell] <- cells.[currentCell] + char n; currentCell
    | Read -> cells.[currentCell] <- (System.Console.ReadKey ()).KeyChar; currentCell
    | Write -> System.Console.Write cells.[currentCell]; currentCell
    | Loop(loopProgram) ->
      recurse
        (fun currentCell -> cells.[currentCell] <> '\000')          // base case
        (fun currentCell -> execute cells currentCell loopProgram)  // body
        currentCell)
    currentCell

[<EntryPoint>]
let main args =
  if args.Length > 0 then
    args.[0] |> File.ReadAllBytes
    |> Array.map char |> List.ofArray
    |> parse
    |> execute (Array.zeroCreate 1024) 0
    |> ignore
    0
  else
    printfn "No input file specified."
    1