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
    | Target of Target

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Target _ -> "Compile targetting the specified runtime. Currently, only 'native' is supported, which compiles to native code using GCC."
            | InFile _ -> "Specify an input source file"
            | OutFile _ -> "Specify an output source file"
            | NoOptimize _ -> "Skip optimization"

let argParser = ArgumentParser.Create<CliArgs>()

let mainCompilation inFile outFile shouldOptimize = async {
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
    let cSource = Regex.Replace(template, "  /// --- BF CODE --- ///", bDump)
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

        Async.RunSynchronously (mainCompilation inFile outFile (not noOptimize))
        0
    with
        | :? ArguParseException ->
            System.Console.Error.WriteLine (argParser.PrintUsage ())
            1