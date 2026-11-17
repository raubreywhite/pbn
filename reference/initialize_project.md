# Start a project version

`initialize_project()` sets the paths of a project version and sources
its functions folder. It MUST be the first call in every `vNN_run.R`.

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

  The folder of the real raw data, one string. The folder MAY be absent,
  for example on a computer without the real data. Only
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  for the scenario `"real"` then stops.

- env:

  The environment that the functions folder is sourced into.

## Value

Invisibly, a list with `version`, `home`, `results` and `data_raw`.
`results` is the results folder.

## Details

`initialize_project()` does these steps in this order:

1.  It computes the md5 of `<version>_run.R` and of each file in
    `<version>_functions/`, before it sources them.
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    stores them in `manifest$source_md5`.

2.  It computes the md5 of each file directly in `data_raw`, when the
    folder exists.
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    stores them in `manifest$data_md5` of the scenario `"real"`. A file
    that the run writes later is not in the list.

3.  It calls
    [`org::initialize_project()`](https://www.rwhite.no/org/reference/initialize_project.html)
    with `folders_to_be_sourced` set to `<version>_functions`,
    `max_loc_per_file` set to 1000 and `data_raw` set to `data_raw`. A
    data function finds the folder at `org::project$data_raw`.

The results folder is `<home>/<version>_results`.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
creates it. `initialize_project()` creates no folder, also no folder
with today's date. It stops when `<version>_functions/` does not exist.

`initialize_project()` reads no scenario, no simulation, no command-line
argument and no environment variable, and it sets no seed.
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)
chooses the scenarios, and
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) and
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
set the seeds.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other project setup:
[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md),
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_initialize_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines(
  "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 3)))",
  file.path(home, "v01_functions", "_cfg.R")
)

env <- new.env()
p <- pbn::initialize_project(
  version = "v01",
  home = home,
  data_raw = file.path(home, "data"),
  env = env
)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/Rtmp05XhSl/pbn_initialize_example/v01_functions into 
p$results
#> [1] "/tmp/Rtmp05XhSl/pbn_initialize_example/v01_results"
env$CFG$scenarios
#>   scenario sims
#> 1     real    1
#> 2     null    3

unlink(home, recursive = TRUE)
```
