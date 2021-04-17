module BF.Compiler.Main
open BF.Common
open System
open System.IO
open System.Reflection
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

let argParser = ArgumentParser.Create<CliArgs>()

let mainCompilation inFile outFile = async {
    let assembly = typeof<CliArgs>.Assembly
    //printfn "resources: %A" (assembly.GetManifestResourceNames ())

    let templateName = $"{assembly.GetName().Name}.template.c"
    //printfn "searching for %s" name
    use templateReader = new StreamReader(assembly.GetManifestResourceStream templateName)
    let! template = Async.AwaitTask (templateReader.ReadToEndAsync ())
    //printfn "%A" template
    let bDump =
        inFile |> File.ReadAllText
        |> parse |> toIR |> optimize
        |> To.CSource 1
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

        Async.RunSynchronously (mainCompilation inFile outFile)
        0
    with
        | :? ArguParseException ->
            System.Console.Error.WriteLine (argParser.PrintUsage ())
            1