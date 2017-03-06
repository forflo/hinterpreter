# A parser and interpreter for a tiny, educational, Pascal-like language

The sole purpose of this project is an educational one.
I wanted to implement the semantics of a programming
language by using the denotational semantics toolset.
The semantic specification was largely taken from a
book that I'm currently reading, but adjustments and
additions have been made. For instance the `if`, and
`if else` constructs have been added. I've also added
the semantics for the `while` loop using denotational 
semantics.

Furthermore, the interpreter can properly handle scopes and
is statically typed. However, the only usable data type is Integer.
The program uses Parsec to generate an AST.

_A few notes on the source code_
The function `interpreter` takes a string of program code
and returns a `Poststore`, which is a mapping from
locations (integers) to values (also integers).

The rest of this readme provides a few code samples
and their evaluation.

## Syntax correkt, semantic incorrekt
    begin
        const FOO = 12;
        const BAR = 3;
        var IDX
    is
        IDX := 0;
        FOO := FOO + 1;
        while FOO do
           IDX := IDX + 2
        end
    end

    ghci>> let s2 = interpret test
    ghci>> (\(Ok store) -> map store [0..10]) s2
    =>*** Exception: <interactive>:194:2-23: Non-exhaustive patterns in lambda
    ghci>> (\(Error store) -> map store [0..10]) s2
    =>[0,0,0,0,0,0,0,0,0,0,0]

# Syntax correkt, Semantic correct

    ghci>> let s2 = interpret "begin var I; var J is I:=10; J:=1; while I do I:=I-1; J:=J+2 end end."
    ghci>> (\(Ok store) -> map store [0..10]) s2
    =>[0,21,0,0,0,0,0,0,0,0,0]
       I J

Thus, correct result on memory position for variable J (position #2).

# Scoping
    begin
        var I;
        var J;
        var AFTER
    is
        J:=2;
        I:=3;
        begin 
            var I
        is
            I:=5;
            while I do
                I := I - 1;
                J := J * 2
            end
        end;
        AFTER := I
    end.

    ghci>> let s2 = interpret "beginvarI;varJ;varAFTERisI:=3;J:=2;beginvarIisI:=5;whileIdoI:=I-1;J:=J*2endend;AFTER:=Iend."
    ghci>> (\(Ok store) -> map store [0..10]) s2
    =>[3,64,3,0,0,0,0,0,0,0,0]
       I  J AFTER

# Usage

For trying out:

1. Start ghci
1. Load the interpreter laden (in ghci type `:l parser.hs)
1. Run function `parse_program` with the argument `test`
