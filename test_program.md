# Syntax korrekt, semantik inkorrekt
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

# Syntax korrekt, Semantik korrekt

    ghci>> let s2 = interpret "begin var I; var J is I:=10; J:=1; while I do I:=I-1; J:=J+2 end end."
    ghci>> (\(Ok store) -> map store [0..10]) s2
    =>[0,21,0,0,0,0,0,0,0,0,0]

Somit korrektes Ergebnis auf Speicherstelle 
für Variable J (Speicherstelle 2).

# Mit Scoping
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

# Anmerkungen

Um Auszuprobieren:
(1) ghci starten
(2) Parser laden => in GHCI ":l parser.hs" eingeben
(3) run parse_program test

Anmerkung test ist in parse.hs wie folgt definiert:

test = "begin const FOO = 12; const BAR = 3; var IDX is IDX := 0; FOO := FOO + 1; while FOO do IDX := IDX + 2 end end."

