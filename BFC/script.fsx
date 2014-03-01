#load "Parser.fs"
#load "Compilers/C.fs"
open BFC
open BFC.Parser
open BFC.Compilers

let strToCharArr (s: string) = s.ToCharArray()
let parse =
  strToCharArr
  >> List.ofArray
  >> Parser.parse
let compileCBody = parse >> C.sourceFromBF 0