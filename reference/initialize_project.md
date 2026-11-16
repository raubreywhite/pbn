# Start one version of an analysis project

`initialize_project()` sets up one version of a project with the house
defaults, and sources the functions folder of that version. It MUST be
the first call in every `vNN_run.R`.

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

  The default folder of the raw data. It MUST exist.

- env:

  The environment that the functions folder is sourced into.

## Value

Invisibly, a list with `version`, `home`, `results`, `data_raw` and
`data_raw_source`. `data_raw_source` is `"default"` or `"argument"`.

## Details

`initialize_project()` calls
[`org::initialize_project()`](https://www.rwhite.no/org/reference/initialize_project.html)
with these arguments:

- `results` is `<home>/<version>_results`. In a scenario run with
  `data_raw=<path>`, it is `<home>/<version>_results/scenario/<name>`.
  `<name>` is the basename of the normalised path, so `~`, `..` and a
  trailing slash are resolved first.

- `folders_to_be_sourced` is `<version>_functions`.

- `max_loc_per_file` is 1000.

A trailing command-line argument `data_raw=<path>` replaces `data_raw`.
[`run_version()`](https://www.rwhite.no/pbn/reference/run_version.md)
gives this argument. pbn reads no environment variables. A run with this
argument is a scenario run. A scenario run never writes to the results
folder of the real run.

A scenario run stops before it writes anything in these cases:

- The scenario results folder, normalised, is the real results folder.

- The data folder is the default `data_raw`. That is the real run.

- The data folder is the real results folder, or holds it.

- The scenario folder holds a `results.qs2` with a different
  `manifest$data_raw`. Two data folders with the same basename share one
  scenario folder.

The data folder MAY be the scenario results folder itself.
`initialize_project()` computes the md5 of each file directly in the
data folder.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stores these values as `manifest$data_md5`, so a file that the run
writes later is not in the list.

org creates a dated subfolder in the results folder. pbn writes to the
results folder itself. Thus `initialize_project()` deletes each dated
subfolder that the call created and that is empty.

## See also

Other project setup:
[`run_version()`](https://www.rwhite.no/pbn/reference/run_version.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_initialize_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
writeLines("CFG <- list(alpha = 0.05)", file.path(home, "v01_functions", "_cfg.R"))

env <- new.env()
p <- pbn::initialize_project(
  version = "v01",
  home = home,
  data_raw = file.path(home, "data"),
  env = env
)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/RtmpzsUfWf/pbn_initialize_example/v01_functions into 
p$results
#> [1] "/tmp/RtmpzsUfWf/pbn_initialize_example/v01_results"
env$CFG$alpha
#> [1] 0.05

unlink(home, recursive = TRUE)
```
