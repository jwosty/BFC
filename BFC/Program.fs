module BFC.Main
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main args = 
  let template = File.ReadAllText("template.c")
  let bDump =
    args.[0]
    |> File.ReadAllBytes
    |> Array.map char
    |> List.ofArray
    |> Parser.parse
    |> BFC.To.CSource 1
  let cSource = Regex.Replace(template, "  /// --- BF CODE --- ///\n", bDump)
  ignore (BFC.To.C cSource args.[1])
  
  0