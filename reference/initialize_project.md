# Start one scenario of an analysis project version

`initialize_project()` sets up one simulation of one scenario of a
project version, and sources the functions folder of that version. It
MUST be the first call in every `vNN_run.R`.

## Usage

``` r
initialize_project(
  version,
  home,
  data_raw,
  scenario = "real",
  env = .GlobalEnv
)
```

## Arguments

- version:

  The version, written as `"v"` and two digits, for example `"v01"`.

- home:

  The project folder. It holds `<version>_run.R` and
  `<version>_functions/`.

- data_raw:

  The folder of the real raw data. It MUST exist when the scenario is
  `"real"`. pbn does not read it for any other scenario.

- scenario:

  The scenario, one name in `CFG$scenarios$scenario`. The default
  `"real"` is the real data.

- env:

  The environment that the functions folder is sourced into.

## Value

Invisibly, a list with `version`, `home`, `scenario`, `sim`, `sims`,
`results` and `data_raw`. `results` is the results folder and `data_raw`
is the data folder of the run.

## Details

`_cfg.R` in `<version>_functions/` MUST set `CFG$scenarios`, a
data.frame with these columns:

- `scenario`: unique names that match `^[a-z0-9_]+$`. One name MUST be
  `"real"`.

- `sims`: the number of simulations of the scenario, a whole number of 1
  or more.

Two trailing command-line arguments replace the defaults. pbn reads no
environment variables.

- `scenario=<name>` replaces `scenario`.

- `sim=<k>` gives the simulation, a whole number of 1 or more. Without
  it, the simulation is 1.

`initialize_project()` does these steps in this order:

1.  It calls
    [`org::initialize_project()`](https://www.rwhite.no/org/reference/initialize_project.html)
    with `results` set to `<home>/<version>_results`,
    `results_subfolder` set to the scenario, `folders_to_be_sourced` set
    to `<version>_functions`, and `max_loc_per_file` set to 1000.

2.  It reads `CFG$scenarios`. The results folder is
    `<version>_results/<scenario>/` when the scenario has 1 simulation,
    and `<version>_results/<scenario>/sim_<kkkk>/` otherwise. `<kkkk>`
    is the simulation with four digits. `org::project$results_today` is
    set to this folder.

3.  It calls `set.seed(sim)`. It also calls `set.seed(sim)` before step
    1, so random work while org sources the functions folder depends on
    `sim` only.

4.  For `"real"`, the data folder is `data_raw`. For any other scenario,
    the data folder is `data/` in the results folder.
    `initialize_project()` deletes it, creates it empty, and calls
    `fake_<scenario>(dir)` with its path. The project MUST define
    `fake_<scenario>()` in `<version>_functions/`.

5.  It computes the md5 of each file directly in the data folder.
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    stores these values as `manifest$data_md5`, so a file that the run
    writes later is not in the list.

`initialize_project()` stops in these cases:

- `scenario=` or `sim=` is given more than once.

- `sim=` is not a whole number of 1 or more.

- The scenario is not in `CFG$scenarios`, or `sim` is more than its
  `sims`.

- `fake_<scenario>()` is missing.

When it stops, it deletes each empty results folder that the call
created. It never creates a folder with today's date.

## See also

Other project setup:
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_initialize_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
writeLines(
  c(
    "CFG <- list(",
    "  scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 3))",
    ")",
    "fake_null <- function(dir) {",
    "  utils::write.csv(data.frame(x = rnorm(5)), file.path(dir, 'd.csv'))",
    "}"
  ),
  file.path(home, "v01_functions", "_cfg.R")
)

env <- new.env()
p <- pbn::initialize_project(
  version = "v01",
  home = home,
  data_raw = file.path(home, "data"),
  scenario = "null",
  env = env
)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/Rtmpcg63Uf/pbn_initialize_example/v01_functions into 
p$results
#> [1] "/tmp/Rtmpcg63Uf/pbn_initialize_example/v01_results/null/sim_0001"
list.files(p$data_raw)
#> [1] "d.csv"

unlink(home, recursive = TRUE)
```
