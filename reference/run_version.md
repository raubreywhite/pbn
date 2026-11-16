# Run one version of a project in a new R session

`run_version()` runs `<home>/<version>_run.R` with
[`callr::rscript()`](https://callr.r-lib.org/reference/rscript.html) in
a new R session, with `home` as the working directory. It stops, and
shows the output of the script, when the script fails.

## Usage

``` r
run_version(version, data_raw = NULL, home = ".")
```

## Arguments

- version:

  The version, for example `"v01"`.

- data_raw:

  `NULL`, or the path of a raw data folder. A path goes to the script as
  the trailing command-line argument `data_raw=<path>`, which
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  reads.

- home:

  The project folder.

## Value

The path of the results file. It is
`<home>/<version>_results/results.qs2` when `data_raw` is `NULL`, and
`<home>/<version>_results/scenario/<name>/results.qs2` otherwise.
`<name>` is the basename of the normalised `data_raw`.

## See also

Other project setup:
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)

## Examples

``` r
# \donttest{
home <- file.path(tempdir(), "pbn_run_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
writeLines(
  c(
    'pbn::initialize_project(version = "v01", home = ".", data_raw = "data")',
    "pbn::save_results(",
    "  exclusions = data.frame(",
    "    step = character(0), n_before = numeric(0),",
    "    n_excluded = numeric(0), n_after = numeric(0)",
    "  ),",
    "  estimates = data.frame(term = 'x', estimate = 1),",
    "  tables = list()",
    ")"
  ),
  file.path(home, "v01_run.R")
)
pbn::run_version("v01", home = home)
#> [1] "/tmp/RtmpzsUfWf/pbn_run_example/v01_results/results.qs2"

unlink(home, recursive = TRUE)
# }
```
