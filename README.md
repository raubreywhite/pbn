# pbn

pbn ("paint by numbers") holds the house layout of an analysis project, the
checks of that layout, and a few shared helpers. It is the one policy package
of the family. `org` (paths), `cohort` (exclusions) and `plnr` (plans) are
mechanism packages without opinions.

This is a trial version. It holds the smallest set of functions that one real
project can use.

## Installation

```r
pak::pak("raubreywhite/pbn")
```

## The house layout

All names are lower case. The version is a prefix.

```
v01_run.R                 the run script; first call is pbn::initialize_project(version = "v01", ...)
v01_functions/            sourced folder; holds _cfg.R (top-level `CFG <- list(...)`) and project functions
v01_results/results.qs2   the results object
v01_results/scenario/<name>/  the results of a run with data_raw=<path>; <name> is the basename of the normalised path
v01_test/                 optional testthat files for project functions
v01_check_functions.R  v01_check_scenarios.R   optional checks of the functions and of scenario runs
check_project.R           one line: pbn::check_project()
pbn_allow.txt             optional exceptions to check_project()
manuscript/manual/vNN_<initials>.docx   hand-edited Word files (initials upper case, e.g. v16_SA.docx)
manuscript/built/vNN/     pipeline output
background/  adhoc/  data_describe/
README.md  CHANGELOG.md  AGENTS.md  .gitignore
```

## Quick start

`v01_run.R`:

```r
p <- pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")
d <- data.table::fread(file.path(p$data_raw, "d.csv"))
pbn::save_results(
  exclusions = data.frame(step = "all", n_before = nrow(d), n_excluded = 0, n_after = nrow(d)),
  estimates = data.frame(term = "n", estimate = nrow(d)),
  tables = list(table1 = pbn::table1(d, by = "group", vars = CFG$vars)),
  d = d
)
```

Run it in a new R session, once with a different data folder:

```r
pbn::run_version("v01")
pbn::run_version("v01", data_raw = "/data/test")
```

The second run writes to `v01_results/scenario/test/`. It does not change `v01_results/results.qs2`.
A scenario run stops when its data folder is the default data folder, or is or holds `v01_results/`.
It also stops when the scenario folder holds the results of another data folder with the same basename.
`?initialize_project` lists every stop.

## The seven functions

| Function | What it does |
|---|---|
| `initialize_project()` | Starts one version with the house defaults of `org`, and sources `vNN_functions/`. |
| `run_version()` | Runs `vNN_run.R` in a new R session, optionally with `data_raw=<path>`. |
| `save_results()` | Writes `vNN_results/results.qs2` with a manifest, and stops when the object fails the contract. |
| `read_results()` | Reads `vNN_results/results.qs2`. |
| `check_results()` | Checks a results object against the contract. |
| `check_project()` | Checks a project folder against the house layout, and quits with status 1 on a failure when R is not interactive. |
| `table1()` | Computes the numbers of a descriptive table, with chi-square or Fisher's exact p-values. |

The help page of each function holds its rules. Start with
`?check_project` and `?save_results`.
