module BF.Optimizer.Tests
open BF.Optimizer
open BF.Parser
open System
open BF
open FsUnit.Xunit
open Xunit

module InstructionSequences =
    [<Fact>]
    let ``Given a sequence of +`` () =
        "+++++"
        |> parse |> toIR |> optimizeUpto1
        |> should equal [AddCell (0, 5)]

    [<Fact>]
    let ``Given a sequence of >>`` () =
        ">>>"
        |> parse |> toIR |> optimizeUpto1
        |> should equal [AddPtr 3]

    [<Fact>]
    let ``Given a long sequence of +`` () =
        "++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
         ++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
         ++++++++++ ++++++++++ ++++++++++ ++++++++++"
        |> parse |> toIR |> optimizeUpto1
        |> should equal [AddCell (0, 140)]

    [<Fact>]
    let ``Given a combination of atom sequences`` () =
        "++++>>+++<<<-->[--]"
        |> parse |> toIR |> optimizeUpto1
        |> should equal [AddCell (0, 4); AddPtr 2; AddCell (0, 3); AddPtr -3; AddCell (0, -2); AddPtr 1; WhileNonzero [AddCell (0, -2)]]

module SimpleLoops =
    [<Fact>]
    let ``Given a comment loop at the beginning of a program`` () =
        "[Hello, world. This loop body can never be executed, so we have ourselves a makeshift comment here. We can write
          whatever we want in here! </comment>] +++++[-]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); ClearCell]

    [<Fact>]
    let ``Given a clear loop`` () =
        "+[-]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 1); ClearCell]

    [<Fact>]
    let ``Given a clear loop inside a more complex loop`` () =
        "++>+++>++[>[-]<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 2); AddPtr 1; AddCell (0, 3); AddPtr 1; AddCell (0, 2); WhileNonzero [AddPtr 1; ClearCell; AddPtr -2]]

    [<Fact>]
    let ``Given a + clear loop`` () =
        "+[+]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 1); ClearCell]

    [<Fact>]
    let ``Given a + clear loop inside a more complex loop`` () =
        "++>+++>++[>[+]<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 2); AddPtr 1; AddCell (0, 3); AddPtr 1; AddCell (0, 2); WhileNonzero [AddPtr 1; ClearCell; AddPtr -2]]

module MoveMulLoops =
    [<Fact>]
    let ``Given a simple move loop`` () =
        "+++++[->+<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [1,1]]

    [<Fact>]
    let ``Given a move loop with a non-adjacent cell`` () =
        "+++++[->>+<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [2,1]]

    [<Fact>]
    let ``Given a move loop with a distant cell`` () =
        "+++++[->>>>>+<<<<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [5,1]]

    [<Fact>]
    let ``Given a move-and-multiply loop with a factor of 3`` () =
        "+++++[->+++<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [1,3]]

    [<Fact>]
    let ``Given a move-and-multiply loop with a factor of 6 and non-adjacent cells`` () =
        "+++++[->>>++++++<<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [3,6]]

    [<Fact>]
    let ``Given a move-and-multiply loop with iterator decrement at end of loop `` () =
        "+++++[>>++++<<-]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [2,4]]

    [<Fact>]
    let ``Given a move-and-multiply loop with two destinations`` () =
        "+++++[->+++++>+++<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [1,5;2,3]]

    [<Fact>]
    let ``Given a move-and-multiply loop with three destinations`` () =
        "+++++[->+++++>+++>++<<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [1,5;2,3;3,2]]

    [<Fact>]
    let ``Given a move-and-multiply loop with three non-adjacent destinations`` () =
        "+++++[->+++++>>+++>>++<<<<<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [1,5;3,3;5,2]]

    [<Fact>]
    let ``Given a move-and-multiply loop with two non-adjacent destinations, using cells both to the left and to the right`` () =
        "+++++[-<<+++++>>>+++<]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [-2,5;1,3]]

    [<Fact>]
    let ``Given a move-and-multiply loop with two non-adjacent destinations, using cells both to the left and to the right, and with the decrementer at the end`` () =
        "+++++[<<+++++>>>+++<-]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 5); MoveMulCell [-2,5;1,3]]

    [<Fact>]
    let ``Given a move-and-multiply loop with a very distant destination cell`` () =
        "+[>>>>>>>[-<<<<<<<<<<<+++++>>>>>>>>>>>]>>>]"
        |> parse |> toIR |> optimizeUpto2
        |> should equal [AddCell (0, 1); WhileNonzero [AddPtr 7; MoveMulCell [-11,5]; AddPtr 3]]

module OffsetOperationSequence =
    [<Fact>]
    let ``Given an offset operation sequence`` () =
        ">+++++"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [AddCell (1,5); AddPtr 1]

    [<Fact>]
    let ``Given an offset operation sequence with 2 operations`` () =
        ">+++++>>+++"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [AddCell (1,5); AddCell (3,3); AddPtr 3]

    [<Fact>]
    let ``Given an offset operation sequence with 3 operations`` () =
        ">+++++>>----<++++"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [AddCell (1,5); AddCell (3,-4); AddCell (2,4); AddPtr 2]

    [<Fact>]
    let ``Given an offset operation sequence beginning with a +`` () =
        "+++<<<++>>------"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [AddCell (0,3); AddCell (-3,2); AddCell (-1,-6); AddPtr -1]

    [<Fact>]
    let ``Given an offset operation sequence ending with a ptr change`` () =
        ">>++++>+<"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [AddCell (2,4); AddCell (3,1); AddPtr 2]

    [<Fact>]
    let ``Given an offset operation sequence that brings the pointer back to where it started`` () =
        ">>>>++<<-----<<"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [AddCell (4,2); AddCell (2,-5)]

    [<Fact>]
    let ``Given an offset operation sequence inside a loop`` () =
        "+>+<[+++>>+>>>++>>-]>>+<<"
        |> parse |> toIR |> optimizeUpto3
        |> should equal [
            AddCell (0,1)
            AddCell (1,1)
            WhileNonzero [AddCell (0,3); AddCell (2,1); AddCell (5,2); AddCell (7,-1); AddPtr 7]
            AddCell (2,1)]

    // TODO: optimize things like >+>[-]

        //"+++++>>>++>>----->+++"
        //|> parse |> toIR |> optimizeUpto3
        //|> should equal [AddCell (0,5); AddCell (3,2); AddCell (5,-5); AddCell (6,3)]
