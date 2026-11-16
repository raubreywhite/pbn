# Start one scenario of an analysis project version

`initialize_project()` sets up one simulation of one scenario of a
project version, and sources the functions folder of that version. It
MUST be the first call in every `vNN_run.R`.

## Usage

``` r
initialize_project(version, home, data_raw, env = .GlobalEnv)
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

The scenario and the simulation come from the first of these that gives
them. pbn reads no environment variables.

1.  The trailing command-line arguments `scenario=<name>` and `sim=<k>`.
    [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
    gives both. When only one of them is there, the other one is
    `"real"` or 1.

2.  The choice of
    [`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md),
    for a person who steps through the script by hand.

3.  The scenario `"real"` and the simulation 1.

The choice of
[`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md)
lasts for the R session.

In an interactive R session, `initialize_project()` prints one line, for
example `pbn: scenario null, simulation 2 of 3 (fake)`. `(fake)` is
there for each scenario other than `"real"`.

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

5.  It computes an md5 that identifies the data, and
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    stores it in the manifest:

    - For `"real"`, the md5 of each file directly in the data folder, as
      `manifest$data_md5`. A file that the run writes later is not in
      the list.

    - For a fake scenario, the md5 of the code of `fake_<scenario>()`,
      as `manifest$fake_md5`. A writer such as `haven::write_sav()`
      stamps the time into the file. The md5 of the files would then
      change on every run, and the data would not.

`initialize_project()` stops in these cases:

- `scenario=` or `sim=` is given more than once.

- `sim=` is not a whole number of 1 or more.

- The scenario is not in `CFG$scenarios`, or `sim` is more than its
  `sims`.

- `fake_<scenario>()` is missing.

When it stops, it deletes each empty results folder that the call
created. The state of the run is then empty, so
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
and
[`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
stop until a call succeeds. The choice of
[`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md)
stays. It never creates a folder with today's date.

## See also

Other project setup:
[`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md),
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md),
[`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md)

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

utils::write.csv(data.frame(x = 1:5), file.path(home, "data", "d.csv"))

env <- new.env()
p <- pbn::initialize_project(
  version = "v01",
  home = home,
  data_raw = file.path(home, "data"),
  env = env
)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/Rtmpx6CwFY/pbn_initialize_example/v01_functions into 
p$scenario
#> [1] "real"
p$results
#> [1] "/tmp/Rtmpx6CwFY/pbn_initialize_example/v01_results/real"
list.files(p$data_raw)
#> [1] "d.csv"

unlink(home, recursive = TRUE)
```
