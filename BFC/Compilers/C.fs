module BFC.Compilers.C
open System
open System.IO
open System.Diagnostics
open BFC.Parser

let rec sourceFromBF indentLevel bf =
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

    // Optimization for "[-]" which sets the cell to 0
    | Loop [AddCell -1] -> indent + "cells[loc] = 0;\n"
    | Loop code ->
      indent + "while (cells[loc]) {\n"
      +           sourceFromBF (indentLevel + 1) code
      + indent + "}")
  |> List.reduce (+)

let compile source out =
  let sourceFileName = out + ".c"
  File.WriteAllText(sourceFileName, source)
  let compiler, args = "gcc", sourceFileName + " -o " + out
  printfn "Compiling program via: `%s %s`" compiler args

  // TODO: redirect stdout and stderr to log files
  let cproc = new Process()
  let info = cproc.StartInfo
  info.UseShellExecute <- false
  info.FileName <- compiler
  info.Arguments <- args
  ignore (cproc.Start())
  cproc.WaitForExit()