module BF.Compiler.Main
open System
open System.IO
open System.Text.RegularExpressions
open NDesk.Options

type Platform = | CSource

type Options =
  { platform: Platform
    inFile: string
    outFile: string }

let platformFromName (name: string) =
  match name.ToLowerInvariant () with
  | "c" -> CSource
  | _ ->
    eprintfn "Unknown platform: `%s'" name
    exit 1

let printUsage () =
  printfn "Usage: bfc <source file> <options>"

let printHelp (optionSet: OptionSet) =
  optionSet.WriteOptionDescriptions Console.Out

let parseArgs (args: string []) =
  let optionSet = new OptionSet()
  
  let platform = ref CSource
  let outFile = ref "a.out"
  let inFile = ref ""
  
  optionSet.Add("i=|infile=", "Source file to read.",
    (fun f -> inFile := f)) |> ignore
  optionSet.Add("p=|platform=", "Compile targeting this platform. Currently, the only platform is C source which is then compiled using GCC.",
    (fun p -> platform := (platformFromName p))) |> ignore
  optionSet.Add("o=|outfile=", "Write output to this file.",
    (fun f -> outFile := f)) |> ignore
  let inFile =
    if args.Length = 0 || (args.[0].[0] = '-')
      then
        printUsage()
        printHelp optionSet
        exit 1
      else ref args.[0]
  let args = optionSet.Parse args
  
  { platform = !platform
    inFile = !inFile
    outFile = !outFile }

[<EntryPoint>]
let main args =
  let options = parseArgs args
  
  let template = File.ReadAllText("template.c")
  let bDump =
    options.inFile
    |> File.ReadAllBytes
    |> Array.map char
    |> List.ofArray
    |> Parser.parse
    |> BFC.To.CSource 1
  let cSource = Regex.Replace(template, "  /// --- BF CODE --- ///\n", bDump)
  ignore (BFC.To.C cSource args.[1])
  
  0