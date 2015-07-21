#load "Parser.fs"
#load "To.fs"
open System
open BFC
open BFC.Parser

let strToCharArr (s: string) = s.ToCharArray()
let parse =
  strToCharArr
  >> List.ofArray
  >> Parser.parse
let compileCBody = parse >> BFC.To.CSource 0

let s = """>>+++[<+++>-]<+
[
  > +++            30  (cell 2)
  >>+++[<+++>-]<+  100 (cell 3)
  [<]> -           move back to start
]
<+++[>+++<-]>+
[>++++<-]          30 (cell 2) plus (4 * 10) (cell 1)

>++.               H   (cell 2)
>+.                e   (cell 3)
>+++[<++>-]<
+..                ll  (cell 3)
+++.               o   (cell 3)

<<<+++[>+++<-]>+
[
  >----            72 (cell 2) minus (10 * 4) (cell 1) = 32 (cell 2)
  <-
]
>.                 SPACE (cell 2)

|Address|Value|Pointer|
|   ^   | 000 |  ===  |
|   1   | 032 |       |
|   2   | 111 |       |
|   v   | 000 |       |

>
[>+>+<<-]          Move cell 2 into 3 and 4
++++[>++<-]        Set cell 3 to 119 (w)

>.                 w (cell 3)
>.                 o (cell 4)
+++.               r (cell 4)
>++[<--->-]
<.                 l (cell 4)
>++[<---->-]
<.                 d (cell 4)

<<<+.              ! (cell 1)"""

let myString = """>>+++[<+++>-]<+[>+++>>+++[<+++>-]<+[<]>-]<+++[>+++
<-]>+[>++++<-]>++.>+.>+++[<++>-]<+..+++.<<<+++[>++
+<-]>+[>----<-]>.>[>+>+<<-]++++[>++<-]>.>.+++.>++[
<--->-]<.>++[<---->-]<.<<<+."""

s
|> parse
|> Parser.dump
|> strToCharArr
|> Seq.windowed 59
|> Seq.take 5
|> Array.ofSeq
|> Array.fold (fun str chars -> str + "\n" + (new String(chars))) ""

//s |> strToCharArr |> Array.filter (fun c -> c = '>' || c = '<' || c = '+' || c = '[' || c = ']' || c = ',' || c = '.')