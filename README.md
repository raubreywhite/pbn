# pbn <a href="https://www.rwhite.no/pbn/"><img src="man/figures/logo.png" align="right" width="120" /></a>

pbn ("paint by numbers") holds the house layout of an analysis project, the
checks of that layout, and a few shared helpers. It is the one policy package
of the family. `org` (paths), `cohort` (exclusions) and `plnr` (plans) are
mechanism packages without opinions.

This is a trial version. It holds the smallest set of functions that one real
project can use.

## For an AI agent

Read <https://www.rwhite.no/pbn/articles/pbn.md> before you write or change
the run script of an analysis project. It is the manual of pbn as one worked
example, in plain Markdown, built for an agent to read in one piece.
<https://www.rwhite.no/pbn/llms.txt> lists every other page.

## Installation

```r
pak::pak("raubreywhite/pbn")
```

## Quick start

A project version has a run script `v01_run.R` and a folder
`v01_functions/`. `v01_functions/_cfg.R` names the scenarios in
`CFG$scenarios`: the real data, and fake data that the project makes itself.
The run script is one `plnr` plan over every scenario and simulation. Its
data steps read the data, clean it and build a `cohort`:

```r
pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d", raw = data_raw, clean = data_clean, cohort = data_cohort)
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, outcome = CFG$outcomes)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::make_outputs(make_output, scenarios)
```

The file `pbn.yml` in the project folder names the rules of
`pbn::check_project()`, as one line: `extends: house_2026_11_17`. From the
project folder, run the script with `Rscript v01_run.R`, and check the layout
with `pbn::check_project()`.

The article [Get started](https://www.rwhite.no/pbn/articles/pbn.html) is the
manual. It builds a small fake project file by file, and runs it.

The [reference](https://www.rwhite.no/pbn/reference/index.html) lists every
function, and the help page of each function holds its rules.
