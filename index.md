# pbn

# pbn

**An R analysis framework built for AI agents.**

pbn (“paint by numbers”) is for analysis projects where an AI agent
writes code. The agent checks its own code with
[`pbn::verify()`](https://www.rwhite.no/pbn/reference/verify.md), and
each problem comes with its fix. pbn gives each project one layout and
rules that code enforces. It adds fake data with known answers, which
runs through the same code as the real data. A person confirms the
expected answers.

## What pbn does

- **One way to do each thing.** One run script, up to four data steps,
  and [`pbn::folder()`](https://www.rwhite.no/pbn/reference/folder.md)
  for every path to the data. An agent has fewer choices to get wrong.
- **Checks that name the fix.**
  [`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  checks the project folder against the rules in `pbn.yml`. Checks
  between the data steps stop a run that breaks a rule. Each finding
  names the rule, the file and the fix.
- **Verification on fake data.** The real data is one scenario among
  fake ones with known answers. `expect_` functions check each fake
  scenario, and
  [`pbn::read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  does not read the results of a fake scenario with a failed assertion.
  It also does not read the real results of a run that had a fake
  scenario with a failed assertion.
- **Records to review.** Each run writes `check.csv` with every
  assertion, a run log `run.md`, and a manifest with the md5 of the
  code. A person reviews these files as well as the changed lines.
- **The data stays on its server.** An agent works on the code and the
  fake scenarios, and needs no real data.
  [`pbn::to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  writes the code for the run of the real data into one script, for a
  server without a network.

pbn is the only package of its family that sets rules for a project.
`org` (paths), `cohort` (exclusions) and `plnr` (plans) set no rules.

## For an AI agent

Read <https://www.rwhite.no/pbn/articles/pbn.md> before you write or
change the run script of an analysis project. It is the manual of pbn as
one worked example, in plain Markdown, built for an agent to read in one
piece. <https://www.rwhite.no/pbn/llms.txt> lists every other page.

## Installation

``` r
pak::pak("raubreywhite/pbn")
```

## Quick start

A project version has a run script `v01_run.R` and a folder
`v01_functions/`. `v01_functions/_cfg.R` names the scenarios in
`CFG$scenarios`: the real data, and fake data that the project makes
itself. The run script is one `plnr` plan over every scenario and
simulation. Its data steps read the data, clean it and build a `cohort`:

``` r
pbn::initialize_project(version = "v01", home = ".", folders = c(main = "../data"))
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d", raw = data_raw, clean = data_clean, cohort = data_cohort)
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, outcome = CFG$outcomes)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::make_outputs(make_output, scenarios)
```

The file `pbn.yml` in the project folder names the rules of
[`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md),
as one line: `extends: house_2026_11_19`. From the project folder, run
the script with `Rscript v01_run.R`, and check the layout with
[`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).
`pbn::verify("v01")` does both, and prints a short verdict.

## Read next

- [Get started](https://www.rwhite.no/pbn/articles/pbn.html): the
  manual. It builds a small fake project file by file, and runs it.
- [pbn with AI agents](https://www.rwhite.no/pbn/articles/agents.html):
  the layers of pbn, and three mistakes of an agent that pbn stops.
- [Reference](https://www.rwhite.no/pbn/reference/index.html): every
  function. The help page of each function holds its rules.
