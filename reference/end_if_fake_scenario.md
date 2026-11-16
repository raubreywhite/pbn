# End the run script here when the scenario is fake

`end_if_fake_scenario()` ends a run script after
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
when the scenario is not `"real"`. The part of the script after it is
for the real data only, for example the figures.

## Usage

``` r
end_if_fake_scenario()
```

## Value

Invisibly, `NULL`, when the scenario is `"real"`.

## Details

Call it directly after
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
in `vNN_run.R`. It reads the scenario that the last
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
call set:

- For `"real"`, it does nothing, and the script continues.

- For a fake scenario in a non-interactive R session, for example under
  `Rscript` or
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md),
  it quits R with status 0. No line after it runs.

- For a fake scenario in an interactive R session, it stops with an
  error of class `pbn_end_if_fake_scenario`. The R session stays open.

It stops with an error when no
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
call set a scenario in this R session.

## See also

Other project setup:
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md),
[`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_end_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
dir.create(file.path(home, "data"))
writeLines(
  "CFG <- list(scenarios = data.frame(scenario = 'real', sims = 1))",
  file.path(home, "v01_functions", "_cfg.R")
)
pbn::initialize_project("v01", home, file.path(home, "data"), env = new.env())
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/RtmpoLbHrA/pbn_end_example/v01_functions into 
pbn::end_if_fake_scenario()

unlink(home, recursive = TRUE)
```
