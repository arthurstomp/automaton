automaton
==========

Compute words from a regular expression

To compile: "ghc lib/automaton.hs"

Then "mv /lib/\*.o /lib/\*.hi /lib/automaton bin/"

The binary file is ./bin/automaton . Example of user ./bin/automaton < test/afn1.txt

I'm usind the Data.Matrix module. Please install the matrix hackage "cabal install matrix"
<a href="https://hackage.haskell.org/package/matrix">https://hackage.haskell.org/package/matrix</a>
