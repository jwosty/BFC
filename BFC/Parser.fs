module BFC.Parser
open System

type Instruction =
  | AddPtr of int
  | AddCell of int
  | Read
  | Write
  | Loop of Instruction list

let stl (s: string) = List.ofArray (s.ToCharArray())

let accResult n instructionConversion rest =
  if n = 0 then rest else instructionConversion n :: rest

let rec dump instructions =
  match instructions with
  | [] -> ""
  | instruction :: rest ->
    match instruction with
    | AddPtr n when n < 0 ->  new String('<', -n)
    | AddPtr n ->             new String('>',  n)
    | AddCell n when n < 0 -> new String('-', -n)
    | AddCell n ->            new String('+',  n)
    | Loop contents ->    "[" + dump contents + "]"
    | Read -> ","
    | Write -> "."
    + dump rest

let parse code =
  let rec parse code =
    match code with
    | [] -> [], []
    | ch :: restCode ->
      let restInstructions, nextCode = parse restCode
      match restInstructions, ch with
      | AddPtr n :: rest, '>' -> accResult (n + 1) AddPtr rest, nextCode
      | _, '>' -> AddPtr 1 :: restInstructions, nextCode
      
      | AddPtr n :: rest, '<' -> accResult (n - 1) AddPtr rest, nextCode
      | _, '<' -> AddPtr -1 :: restInstructions, nextCode
      
      | AddCell n :: rest, '+' -> accResult (n + 1) AddCell rest, nextCode
      | _, '+' -> AddCell 1 :: restInstructions, nextCode
      
      | AddCell n :: rest, '-' -> accResult (n - 1) AddCell rest, nextCode
      | _, '-' -> AddCell -1 :: restInstructions, nextCode
      
      | _, ',' -> Read :: restInstructions, nextCode
      
      | _, '.' -> Write :: restInstructions, nextCode
      
      | _, '[' ->
        let restInstructionsAfterLoop, nextCode = parse nextCode
        Loop restInstructions :: restInstructionsAfterLoop, nextCode
      | _, ']' -> [], restCode
      
      | _ -> restInstructions, nextCode
  fst (parse code)