# A parser + interpreter for a tiny educational language
The interpreter can properly handle scopes and
is statically typed. The only data type is Integer.
It uses Parsec to generate an AST.

The sole purpose of this project is an educational one.
I wanted to implement the semantics of a programming
language by using the denotational semantics toolset.
The semantic specification was largely taken from a
book that I'm currently reading, but adjustments and
additions have been made. For instance the if, and
if else constructs have been added. I've also written
the semantics fo the while loop.

The function interpreter takes a string of program code
and returns a Poststore, which is a mapping from
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

Somit korrektes Ergebnis auf Speicherstelle 
für Variable J (Speicherstelle 2).

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

# Notes

Um Auszuprobieren:
(1) ghci starten
(2) Parser laden => in GHCI ":l parser.hs" eingeben
(3) run parse_program test

