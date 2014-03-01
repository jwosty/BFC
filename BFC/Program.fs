module BFC.Main
open System
open System.IO
open System.Text.RegularExpressions
open BFC.Compilers

[<EntryPoint>]
let main args = 
  let template = File.ReadAllText("template.c")
  let bDump =
    args.[0]
    |> File.ReadAllBytes
    |> Array.map char
    |> List.ofArray
    |> Parser.parse
    |> C.sourceFromBF 1
  let cSource = Regex.Replace(template, "  /// --- BF CODE --- ///\n", bDump)
  ignore (C.compile cSource args.[1])
  
  0