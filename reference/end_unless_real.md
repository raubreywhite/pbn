# End the run script here unless the real data ran

`end_unless_real()` ends a run script after
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
when the scenario `"real"` is not among the scenarios of the run. The
part of the script after it is for the real results only, for example
the figures.

## Usage

``` r
end_unless_real(scenarios)
```

## Arguments

- scenarios:

  The scenarios of the run, from
  [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md).

## Value

Invisibly, `NULL`, when `"real"` is in `scenarios`.

## Details

- When `"real"` is in `scenarios`, `end_unless_real()` does nothing, and
  the script continues.

- Otherwise, in a non-interactive R session, for example under
  `Rscript`, it prints one line, flushes the output and quits R with
  status 0. No line after it runs.

- Otherwise, in an interactive R session, it stops with an error of
  class `pbn_end_unless_real`. The R session stays open.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other project setup:
[`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md),
[`folder()`](https://www.rwhite.no/pbn/reference/folder.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`new_version()`](https://www.rwhite.no/pbn/reference/new_version.md),
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)

## Examples

``` r
CFG <- list(scenarios = data.frame(scenario = c("real", "null"), sims = 1))
pbn::end_unless_real(pbn::scenarios(CFG))
```
