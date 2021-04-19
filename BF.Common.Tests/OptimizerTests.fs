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

[<Fact>]
let ``Given a comment loop at the beginning of a program`` () =
    "[Hello, world. This loop body can never be executed, so we have ourselves a makeshift comment here. We can write
      whatever we want in here! </comment>] +++++[-]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; ClearCell]

[<Fact>]
let ``Given a clear loop`` () =
    "+[-]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 1; ClearCell]

[<Fact>]
let ``Given a clear loop inside a more complex loop`` () =
    "++>+++>++[>[-]<<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 2; AddPtr 1; AddCell 3; AddPtr 1; AddCell 2; WhileNonzero [AddPtr 1; ClearCell; AddPtr -2]]

[<Fact>]
let ``Given a + clear loop`` () =
    "+[+]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 1; ClearCell]

[<Fact>]
let ``Given a + clear loop inside a more complex loop`` () =
    "++>+++>++[>[+]<<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 2; AddPtr 1; AddCell 3; AddPtr 1; AddCell 2; WhileNonzero [AddPtr 1; ClearCell; AddPtr -2]]

[<Fact>]
let ``Given a simple move loop`` () =
    "+++++[->+<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; MoveMulCell(1,1)]

[<Fact>]
let ``Given a move loop with a non-adjacent cell`` () =
    "+++++[->>+<<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; MoveMulCell(2,1)]

[<Fact>]
let ``Given a move loop with a distant cell`` () =
    "+++++[->>>>>+<<<<<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; MoveMulCell(5,1)]

[<Fact>]
let ``Given a move-and-multiply loop with a factor of 3`` () =
    "+++++[->+++<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; MoveMulCell(1,3)]

[<Fact>]
let ``Given a move-and-multiply loop with a factor of 6 and non-adjacent cells`` () =
    "+++++[->>>++++++<<<]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; MoveMulCell(3,6)]

[<Fact>]
let ``Given a move-and-multiply loop with iterator decrement at end of loop `` () =
    "+++++[>>++++<<-]"
    |> parse |> toIR |> optimize
    |> should equal [AddCell 5; MoveMulCell(2,4)]
