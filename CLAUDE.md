# CLAUDE.md

Rules for agents that work in this repository.

## This repository is public

- Use made-up data only, in tests, examples, documentation and commit messages.
- Do not write the name of a study, a coauthor, a data file or a variable from a real project.

## Behaviour of an exported function does not change

Frozen versions of analysis projects call this package. A change that would give a different result for the same call gets a new function name or a new argument with the old default. Record every change in `NEWS.md`.

## Every check is proved in both directions

Each rule of `check_project()` and `check_results()` has one test with an input that must fail and one with an input that must pass. Tests that start a new R session need the package installed: run them with the package in a throwaway library.

## Licensing

This package is `MIT + file LICENSE`. `LICENSE` holds exactly two lines, `YEAR:` and `COPYRIGHT HOLDER:`. `DESCRIPTION` `Authors@R` names the same holder with `role = "cph"`. The copyright holder is Richard Aubrey White.
