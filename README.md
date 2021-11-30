# Advent of Code 2021

This repo contains my solutions to the [Advent of Code 2021](https://adventofcode.com/2021) using primarily F#, with C# and Python as fallbacks for when I'm stuck.

The code will likely be bad.

## Setup

I'm using .Net 6.0 and Python 3.10.

Python requirements (if there are any) can be installed with `pip install -r requirements.txt`.

Any C# scripts will require [dotnet-script](https://github.com/filipw/dotnet-script).

## Running solutions/tests

Any tests I write will likely just be asserts in each solution file.

How to run the solution file for each day depends on the language.

For F# solutions `dotnet fsi dayXX/solution.fsx`

For C# solutions `dotnet script dayXX/solution.csx`

For Python solutions `python dayXX/solution.py`
