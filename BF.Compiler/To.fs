module BF.Compiler.To
open System
open System.IO
open System.Diagnostics
open BF.Optimizer
open BF.Parser

/// Converts a list of BF instructions into C statements
let rec CSource indentLevel bf =
    bf
    |> List.map (fun instruction ->
        let indent = new String(' ', indentLevel * 2)
        match instruction with
        | AddPtr n when n > 0 -> indent + "loc += " + string n + ";\n"
        | AddPtr n when n < 0 -> indent + "loc -= " + string -n + ";\n"
        | AddPtr _ -> ""
    
        | AddCell n when n > 0 -> indent + "cells[loc] += " + string n + ";\n"
        | AddCell n when n < 0 -> indent + "cells[loc] -= " + string -n + ";\n"
        | AddCell _ -> ""
    
        | Read -> indent + "cells[loc] = getchar();\n"
    
        | Write -> indent + "putchar(cells[loc]);\n"
    
        | ClearCell -> indent + "cells[loc] = 0;\n"
    
        | WhileNonzero code ->
            indent + "while (cells[loc]) {\n"
            +           CSource (indentLevel + 1) code
            + indent + "}\n"

        | MoveMulCell (relSrc,relDst,factor) ->
            indent + $"cells[loc + {relDst}] += cells[loc + {relSrc}] * {factor};\n"
            + indent + $"cells[loc + {relSrc}] = 0;\n"
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