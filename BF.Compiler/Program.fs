module BF.Compiler.Main
open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Argu
open BF.Optimizer
open BF.Parser

type Target = | Native = 0

type CliArgs =
    | [<MainCommand>] InFile of string
    | [<AltCommandLine "-o">] OutFile of string
    | [<AltCommandLine "-NO">] NoOptimize
    | [<AltCommandLine "-c">] NCells of int
    | Target of Target

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Target _ -> "Compile targetting the specified runtime. Currently, only 'native' is supported, which compiles to native code using GCC."
            | InFile _ -> "Specify an input source file."
            | OutFile _ -> "Specify an output source file."
            | NoOptimize _ -> "Skip optimization."
            | NCells _ -> "The number of cells to allocate in the compiled program. Default is 1024."

let argParser = ArgumentParser.Create<CliArgs>()

let mainCompilation inFile outFile shouldOptimize (nCells: int option) = async {
    let assembly = typeof<CliArgs>.Assembly
    //printfn "resources: %A" (assembly.GetManifestResourceNames ())

    let templateName = $"{assembly.GetName().Name}.template.c"
    //printfn "searching for %s" name
    use templateReader = new StreamReader(assembly.GetManifestResourceStream templateName)
    let! template = Async.AwaitTask (templateReader.ReadToEndAsync ())
    //printfn "%A" template
    let ir =
        inFile |> File.ReadAllText
        |> parse |> toIR
    let optIr =
        if shouldOptimize then
            optimize ir
        else ir
    let bDump = optIr |> To.CSource 1
    
    let template' =
        match nCells with
        | Some nCells ->
            Regex.Replace(template, "N_CELLS = \\d+", "N_CELLS = " + string nCells)
        | None -> template
    let cSource = Regex.Replace(template', "  /// --- BF CODE --- ///", bDump)
    //printfn "cSrc:\n%s" cSource
    To.C cSource outFile
}
    

[<EntryPoint>]
let main args =
    try
        let cliArgs = argParser.Parse args

        let target = cliArgs.GetResult ((<@ Target @>), defaultValue = Target.Native)
        let inFile = cliArgs.GetResult <@ InFile @>

        let outFile = cliArgs.GetResult (<@ OutFile @>, defaultValue = Path.GetFileNameWithoutExtension inFile)
        let noOptimize = cliArgs.Contains (<@ NoOptimize @>)

        let nCells = cliArgs.TryGetResult <@ NCells @>

        Async.RunSynchronously (mainCompilation inFile outFile (not noOptimize) nCells)
        0
    with
        | :? ArguParseException ->
            System.Console.Error.WriteLine (argParser.PrintUsage ())
            1