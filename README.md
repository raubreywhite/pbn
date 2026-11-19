# pbn <a href="https://www.rwhite.no/pbn/"><img src="man/figures/logo.png" align="right" width="120" /></a>

**An R analysis framework built for AI agents.**

An AI agent can write most of the code of an analysis. pbn ("paint by numbers") makes that code checkable. It gives each project one layout, rules that code enforces, and fake data with known answers that runs through the same code as the real data.

This is a trial version. It holds the smallest set of functions that one real project can use.

## What pbn does

- **One way to do each thing.** One run script, four data steps, and `pbn::folder()` for every path to the data. An agent has fewer choices to get wrong.
- **Checks that name the fix.** `pbn::check_project()` checks the project folder against the rules in `pbn.yml`. Checks between the data steps stop a run that breaks a rule. Each error names the rule and the file.
- **Verification on fake data.** The real data is one scenario among fake ones with known answers. `expect_` functions check each fake scenario, and `pbn::read_results()` does not read the results of a scenario with a failed assertion.
- **Records to review.** Each run writes `check.csv` with every assertion, a run log `run.md`, and a manifest with the md5 of the code. A person reviews these, not only the changed lines.
- **The data stays on its server.** An agent works on the code and the fake scenarios, and needs no real data. `pbn::to_one_file()` writes the run of the real data as one script for a server without a network.

pbn is the one policy package of its family. `org` (paths), `cohort` (exclusions) and `plnr` (plans) are mechanism packages without opinions.

## For an AI agent

Read <https://www.rwhite.no/pbn/articles/pbn.md> before you write or change the run script of an analysis project. It is the manual of pbn as one worked example, in plain Markdown, built for an agent to read in one piece. <https://www.rwhite.no/pbn/llms.txt> lists every other page.

## Installation

```r
pak::pak("raubreywhite/pbn")
```

## Quick start

A project version has a run script `v01_run.R` and a folder `v01_functions/`. `v01_functions/_cfg.R` names the scenarios in `CFG$scenarios`: the real data, and fake data that the project makes itself. The run script is one `plnr` plan over every scenario and simulation. Its data steps read the data, clean it and build a `cohort`:

```r
pbn::initialize_project(version = "v01", home = ".", folders = c(main = "../data"))
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d", raw = data_raw, clean = data_clean, cohort = data_cohort)
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, outcome = CFG$outcomes)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::make_outputs(make_output, scenarios)
```

The file `pbn.yml` in the project folder names the rules of `pbn::check_project()`, as one line: `extends: house_2026_11_17`. From the project folder, run the script with `Rscript v01_run.R`, and check the layout with `pbn::check_project()`.

## Read next

- [Get started](https://www.rwhite.no/pbn/articles/pbn.html): the manual. It builds a small fake project file by file, and runs it.
- [pbn with AI agents](https://www.rwhite.no/pbn/articles/agents.html): the layers of pbn, and three mistakes of an agent that pbn stops.
- [Reference](https://www.rwhite.no/pbn/reference/index.html): every function. The help page of each function holds its rules.
