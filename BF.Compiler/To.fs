module BF.Compiler.To
open System
open System.Diagnostics
open System.IO
open System.Text
open BF.Optimizer
open BF.Parser

let inline private append (x: 'a) (sb: ^T) =
    ((^T) : (member Append : 'a -> ^T) sb, x)
    |> ignore

let withSignAsOp (x: int) =
    if x = 0 then ""
    else if x > 0 then "+" + string x
    else string x

let appendWithSignAsOp (x: int) (sb: StringBuilder) =
    if x = 0 then ()
    else if x > 0 then
        sb |> append "+"
        sb |> append x
    else
        sb |> append x

/// Converts a list of BF instructions into C statements
let rec CSource indentLevel bf =
    bf
    |> List.map (fun instruction ->
        let indent = new String(' ', indentLevel * 2)
        match instruction with
        | AddPtr n when n > 0 -> indent + "p += " + string n + ";\n"
        | AddPtr n when n < 0 -> indent + "p -= " + string -n + ";\n"
        | AddPtr _ -> ""
    
        | AddCell (ptrOffset, n) when n > 0 -> indent + "data[p" + withSignAsOp ptrOffset + "] += " + string n + ";\n"
        | AddCell (ptrOffset, n) when n < 0 -> indent + "data[p" + withSignAsOp ptrOffset + "] -= " + string -n + ";\n"
        | AddCell _ -> ""
    
        | Read -> indent + "data[p] = getchar();\n"
    
        | Write -> indent + "putchar(data[p]);\n"
    
        | ClearCell ptrOffset -> indent + "data[p" + withSignAsOp ptrOffset + "] = 0;\n"
    
        | WhileNonzero code ->
            indent + "while (data[p]) {\n"
            +           CSource (indentLevel + 1) code
            + indent + "}\n"

        | MoveMulCell (relSrc, destinations) ->
            let sb = new StringBuilder()
            for (relDst, factor) in destinations do
                sb |> append indent
                sb |> append "data[p"
                sb |> appendWithSignAsOp relDst
                sb |> append "] += data[p"
                sb |> appendWithSignAsOp relSrc
                sb |> append "]"
                if factor <> 1 then
                    sb |> append " * "
                    sb |> append factor
                sb |> append ";\n"
            sb |> append indent
            sb |> append "data[p"
            sb |> appendWithSignAsOp relSrc
            sb |> append "] = 0;\n"
            sb.ToString ()

    )
    |> List.reduce (+)

/// Compile C source code by invoking gcc
let C source out =
    let sourceFileName = out + ".c"
    File.WriteAllText(sourceFileName, source)
    let compiler, args = "gcc", sourceFileName + " -o " + out
    printfn "Compiling: `%s %s`" compiler args

    // TODO: redirect stdout and stderr to log files
    let cproc = new Process()
    let info = cproc.StartInfo
    info.UseShellExecute <- false
    info.FileName <- compiler
    info.Arguments <- args
    ignore (cproc.Start())
    cproc.WaitForExit()