# pbn

# pbn

**An R analysis framework built for AI agents.**

pbn (“paint by numbers”) is for analysis projects where an AI agent
writes code. The agent checks its own code with
[`pbn::verify()`](https://www.rwhite.no/pbn/reference/verify.md). Each
finding of a rule names its fix. pbn gives each project one layout and
rules that code enforces. The fake scenarios have expected values. They
run through the same code as the real data. A person confirms the
expected values.

## What pbn does

- **One way to do each thing.** One run script, up to four data steps,
  and [`pbn::folder()`](https://www.rwhite.no/pbn/reference/folder.md)
  for every path to the data.
- **Checks that name the fix.**
  [`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  checks the project folder against the rules in `pbn.yml`. Checks
  between the data steps stop a run that breaks their contract, for
  example a step clean that drops rows. Each finding names the rule, the
  file and the fix.
- **Verification on fake data.** The real data is one scenario among
  fake ones with expected values. `expect_` functions check each fake
  scenario, and
  [`pbn::read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  does not read the results of a fake scenario with a failed assertion.
  It also does not read the real results of a run that had a fake
  scenario with a failed assertion.
- **Records to review.** Each run writes `check.csv` with every
  assertion, a run log `run.md`, and a manifest with the md5 of the
  code. A person reviews these files as well as the changed lines.
- **An agent needs no real data.** It works on the code and the fake
  scenarios.
  [`pbn::to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  writes the code for the run of the real data into one script, for a
  server without a network.

pbn is the only package of its family that sets rules for a project.
`org` (paths), `cohort` (exclusions) and `plnr` (plans) set no rules.

## For an AI agent

Read <https://www.rwhite.no/pbn/articles/pbn.md> before you write or
change the run script of an analysis project. It is the manual of pbn as
one worked example, in plain Markdown, written for an agent to read as
one file. <https://www.rwhite.no/pbn/llms.txt> lists every other page.
[`pbn::create_project()`](https://www.rwhite.no/pbn/reference/create_project.md)
writes `AGENTS.md` from the template `inst/templates/AGENTS.md`. It
lists what to run before a report, and what a person decides.

## Installation

``` r
pak::pak("raubreywhite/pbn")
```

## Quick start

In an empty folder:

``` r
pbn::create_project(folders = c(main = "../data"))
pbn::verify("v01")
```

[`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md)
writes a project at version v01 that passes
[`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).
Its run script runs the fake scenarios only, so `pbn::verify("v01")`
gives a PASS before code reads the real data. It stops, and writes
nothing, when one of its files exists. `pbn::new_version("v01")` freezes
v01 and starts v02.

## Start a new project with an agent

Copy this prompt to the agent. Fill in the folder and the study:

``` text
Set up a new R analysis project with the R package pbn, in this folder.
1. Read https://www.rwhite.no/pbn/articles/pbn.md, the manual of pbn.
2. If pbn and cohort are not installed, install them with pak::pak(c("raubreywhite/pbn", "raubreywhite/cohort")).
3. Run pbn::create_project(folders = c(main = "<folder of the real data>")).
4. Do not open, copy or print the real data.
5. The study: <question, population, exposure, outcome>.
6. Replace the scenario `known`, `expect_known()` and `fit_risk_difference()` of the project with ones for this study. Write the data steps and the analyses. Write fake scenarios with expected values: one small scenario whose values a person can compute by hand, and for each estimate one fake scenario with many simulations (`sims`).
7. Keep `only` in the call `pbn::scenarios()` of v01_run.R until a person runs the real data.
8. Run pbn::verify("v01") until the verdict is PASS.
9. Then stop. List each fake scenario and its expected values, and wait for me to confirm them.
```

## Read next

- [Get started](https://www.rwhite.no/pbn/articles/pbn.html): the
  manual. It builds a small fake project file by file, and runs it.
- [pbn with AI agents](https://www.rwhite.no/pbn/articles/agents.html):
  the four layers of pbn, and three mistakes of an agent that pbn stops.
- [Reference](https://www.rwhite.no/pbn/reference/index.html): every
  function. The help page of each function gives its rules.
