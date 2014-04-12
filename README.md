Botworld is a cellular automaton developed at [MIRI](http://intelligence.org)
as a concrete environment in which to study self-modifying agents embedded in
their environment. (Contrast this with "classical" models of artificial
intelligence where agents interact with the environment only via I/O channels.)

An extensive introduction to Botworld can be found in the
[Botworld technical report](http://intelligence.org/files/Botworld.pdf).
The source code for Botworld can be found in the corresponding
[literate Haskell file](Botworld.lhs).

Some examples may be found in the [examples](examples/) directory.

### Setup

If you don't have Haskell (namely, `cabal`), the
[Haskell platform](http://www.haskell.org/platform/) is a good way to get it.

Once you have `cabal` installed, simply run `cabal install` in this directory
to install Botworld. You may then import the `Botworld` module in any Haskell
file.

To play around with Botworld, you can start by playing around with the
[rudimentary example](examples/Rudimentary.hs). Make sure you have `ghci`
installed, then load up the file with

    ghci examples/Rudimentary.hs

Then simply run

    main

to see a visualization of the example game.
