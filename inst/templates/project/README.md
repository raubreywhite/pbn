# {{name}}

What the project is: the question, the population, the exposure and the outcome.

## How to run

Open R in this folder, and run `pbn::verify()` with the newest version, for example `pbn::verify("v01")`. It runs the run script of the version and checks the project with `pbn::check_project()`. The verdict is PASS or FAIL. Each finding of a rule names its fix.

The run script runs the fake scenarios only, because `pbn::scenarios()` gets `only = setdiff(CFG$scenarios$scenario, "real")`. The verdict then says that the scenario `real` did not run, and its PASS covers the fake scenarios only. When `data_raw()` reads the real data, remove `only`, so that the run includes the scenario `real`. From then on, `pbn::verify()` needs the real data on the computer.

## The fake scenarios

- `known` has 10 rows. Group a has 5 rows: 1 with the outcome, 3 without, and 1 with a missing outcome, which the step cohort excludes. Group b has 5 rows: 3 with the outcome and 2 without. The difference of the risks, b minus a, is 3/5 - 1/4 = 0.35.
