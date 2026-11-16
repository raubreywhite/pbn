# Save the results object of a project version

`save_results()` writes `<results>/results.qs2`, the one results object
of the project version. It stops before it writes when
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
finds a failure.

## Usage

``` r
save_results(exclusions, estimates, tables, extra = list(), d = NULL)
```

## Arguments

- exclusions:

  A data.frame with the columns `step`, `n_before`, `n_excluded` and
  `n_after`, one row per exclusion step. Zero rows are allowed.

- estimates:

  A data.frame of estimates.

- tables:

  A named list of data.frames.

- extra:

  A list of other results.

- d:

  The analysis data, or `NULL`. pbn stores only its number of rows.

## Value

Invisibly, the path of the results file.

## Details

Call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
first. The results folder is the `results` element that
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
returns. It is `<home>/<version>_results/<scenario>/` for a scenario
with 1 simulation, and `<home>/<version>_results/<scenario>/sim_<kkkk>/`
otherwise.

The object is a named list with exactly these top-level names:
`manifest`, `exclusions`, `estimates`, `tables` and `extra`. pbn builds
the `manifest`, a list with these elements:

- `pbn_version` and `r_version`.

- `packages`, a named character vector. The names are the attached and
  loaded packages that are not base packages, and the values are their
  versions. `qs2` is always in the list, because it writes the file.

- `run_date`, the date of the call as `YYYY-MM-DD`. It has no time, so
  two runs on one day with the same inputs and packages write the same
  bytes.

- `version`, `scenario`, `sim`, `sims` and `data_raw`, from
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  `data_raw` is the data folder of the run.

- `data_md5`, a named character vector with the md5 of each file
  directly in `data_raw`.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  computes it, so a file that the run writes after that call is not in
  the list.

- `n_rows_data`, the number of rows of `d`, or `NA` when `d` is `NULL`.

## See also

Other results:
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_save_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
writeLines(
  "CFG <- list(scenarios = data.frame(scenario = 'real', sims = 1))",
  file.path(home, "v01_functions", "_cfg.R")
)
pbn::initialize_project("v01", home, file.path(home, "data"), env = new.env())
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/Rtmpcg63Uf/pbn_save_example/v01_functions into 

pbn::save_results(
  exclusions = data.frame(
    step = "age 18 or more",
    n_before = 100,
    n_excluded = 4,
    n_after = 96
  ),
  estimates = data.frame(term = "x", estimate = 1.2),
  tables = list(counts = data.frame(grp = c("a", "b"), n = c(50, 46)))
)
names(pbn::read_results("v01", home = home))
#> [1] "manifest"   "exclusions" "estimates"  "tables"     "extra"     

unlink(home, recursive = TRUE)
```
