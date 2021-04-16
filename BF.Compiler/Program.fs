module BF.Compiler.Main
open BF.Common
open System
open System.IO
open System.Text.RegularExpressions
open Argu

type Target = | Native = 0

type CliArgs =
    | [<MainCommand>] InFile of string
    | [<AltCommandLine "-o">] OutFile of string
    | Target of Target

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Target _ -> "Compile targetting the specified runtime. Currently, only 'native' is supported, which compiles to native code using GCC."
            | InFile _ -> "Specify an input source file"
            | OutFile _ -> "Specify an output source file"

//type Platform = | CSource

//type Options =
//  { platform: Platform
//    inFile: string
//    outFile: string }

//let platformFromName (name: string) =
//  match name.ToLowerInvariant () with
//  | "c" -> CSource
//  | _ ->
//    eprintfn "Unknown platform: `%s'" name
//    exit 1

//let printUsage () =
//  printfn "Usage: bfc <source file> <options>"

//let printHelp (optionSet: OptionSet) =
//  optionSet.WriteOptionDescriptions Console.Out

//let parseArgs (args: string []) =
//  let optionSet = new OptionSet()
  
//  let platform = ref CSource
//  let outFile = ref "a.out"
//  let inFile = ref ""
  
//  optionSet.Add("i=|infile=", "Source file to read.",
//    (fun f -> inFile := f)) |> ignore
//  optionSet.Add("p=|platform=", "Compile targeting this platform. Currently, the only platform is C source which is then compiled using GCC.",
//    (fun p -> platform := (platformFromName p))) |> ignore
//  optionSet.Add("o=|outfile=", "Write output to this file.",
//    (fun f -> outFile := f)) |> ignore
//  let inFile =
//    if args.Length = 0 || (args.[0].[0] = '-')
//      then
//        printUsage()
//        printHelp optionSet
//        exit 1
//      else ref args.[0]
//  let args = optionSet.Parse args
  
//  { platform = !platform
//    inFile = !inFile
//    outFile = !outFile }

let argParser = ArgumentParser.Create<CliArgs>()

[<EntryPoint>]
let main args =
    try
        let cliArgs = argParser.Parse args

        let target = cliArgs.GetResult ((<@ Target @>), defaultValue = Target.Native)
        let inFile = cliArgs.GetResult <@ InFile @>
        let outFile = cliArgs.GetResult <@ OutFile @>

        let template = File.ReadAllText("template.c")
        let bDump =
            inFile |> File.ReadAllText
            |> parse |> toIR |> optimize
            |> To.CSource 1
        let cSource = Regex.Replace(template, "  /// --- BF CODE --- ///", bDump)
        printfn "cSrc:\n%s" cSource
        To.C cSource outFile
        0
    with
        | :? ArguParseException ->
            System.Console.Error.WriteLine (argParser.PrintUsage ())
            1