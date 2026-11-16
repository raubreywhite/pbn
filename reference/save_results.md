# Save the results object of a project version

`save_results()` writes `<results>/results.qs2`, the one results object
of the project version. It stops before it writes when
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
finds a failure.

## Usage

``` r
save_results(..., d)
```

## Arguments

- ...:

  The results, each as a named argument, for example `estimates = e` or
  `p_interaction = 0.04`. Give at least one. Each name MUST be syntactic
  and MUST be there once. A result named `manifest` is an error, because
  pbn writes that element. An element MAY be a data.frame, an atomic
  vector, a factor, a date, `NULL`, or a list of these to any depth.
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  lists every allowed object. An element named `exclusions` MUST be the
  table of the exclusion steps, with the columns `step`, `n_before`,
  `n_excluded` and `n_after`. It MAY have zero rows.

- d:

  The analysis data, or `NULL`. You MUST give `d`. pbn stores only its
  number of rows. It then refuses a data.frame with that number of rows
  at any depth of the results. It also refuses a vector or a matrix with
  that number of values in one dimension. Give `d = NULL` when the run
  has no one table of the data.

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

The object is a named list. Its first element is `manifest`, and the
results follow in the order of the call. pbn builds the `manifest`, a
list with these elements:

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

- `settings`, the named list of the settings of the scenario from the
  setting columns of `CFG$scenarios`. It is an empty list when there are
  none.

- `source_md5`, a named character vector for every scenario. It holds
  the md5 of `<version>_run.R` and of each file in
  `<version>_functions/`, named by the path relative to `home` and
  sorted.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  computes it before it sources the files. The
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  rule `manifest_source` needs the run script in it, so `save_results()`
  stops when `<version>_run.R` does not exist.

- For the scenario `"real"` only, `data_md5`, a named character vector
  with the md5 of each file directly in `data_raw`.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  computes it, so a file that the run writes after that call is not in
  the list.

- For a fake scenario only, `fake_md5`, one string: the md5 of the code
  of `fake_<scenario>()`, without its source references. Two runs of one
  simulation on one day with the same code and packages then write the
  same bytes. This holds also when `fake_<scenario>()` stamps the time
  into a file. A change in a function that `fake_<scenario>()` calls
  does not change `fake_md5`.

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
writeLines("# The run script.", file.path(home, "v01_run.R"))
writeLines(
  "CFG <- list(scenarios = data.frame(scenario = 'real', sims = 1))",
  file.path(home, "v01_functions", "_cfg.R")
)
pbn::initialize_project("v01", home, file.path(home, "data"), env = new.env())
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/RtmpOG2SVD/pbn_save_example/v01_functions into 

pbn::save_results(
  exclusions = data.frame(
    step = "age 18 or more",
    n_before = 100,
    n_excluded = 4,
    n_after = 96
  ),
  estimates = data.frame(term = "x", estimate = 1.2),
  counts = list(by_group = data.frame(grp = c("a", "b"), n = c(50, 46))),
  p_interaction = 0.04,
  d = NULL
)
names(pbn::read_results("v01", home = home))
#> [1] "manifest"      "exclusions"    "estimates"     "counts"       
#> [5] "p_interaction"

unlink(home, recursive = TRUE)
```
