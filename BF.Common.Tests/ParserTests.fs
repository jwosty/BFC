module BF.Parser.Tests
open System
open BF
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Given a simple program`` () =
    "++>+.,-<-[-]"
    |> parse
    |> should equal [IncCell;IncCell;IncPtr;IncCell;Instruction.Write;Instruction.Read;DecCell;DecPtr;DecCell;Instruction.WhileNonzero [DecCell]]

[<Fact>]
let ``Given a simple program with comments`` () =
    "Hello++ world,."
    |> parse
    |> should equal [IncCell;IncCell;Instruction.Read;Instruction.Write]