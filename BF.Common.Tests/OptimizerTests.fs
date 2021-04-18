module BF.Optimizer.Tests
open BF.Optimizer
open BF.Parser
open System
open BF
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Given a sequence of +`` () =
    "+++++"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5]

[<Fact>]
let ``Given a sequence of >>`` () =
    ">>>"
    |> parse |> toIR |> optimize
    |> should equal [AddPtr 3]

[<Fact>]
let ``Given a long sequence of +`` () =
    "++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
     ++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
     ++++++++++ ++++++++++ ++++++++++ ++++++++++"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 140]

[<Fact>]
let ``Given a combination of atom sequences`` () =
    "++++>>+++<<<-->[--]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 4; AddPtr 2; AddCell 3; AddPtr -3; AddCell -2; AddPtr 1; WhileNonzero [AddCell -2]]


