# Run a version and check the project

`verify()` runs `<version>_run.R` in a new R session, and then checks
the project with the rules of
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).
It prints a short verdict: the exit status of the run, each failed
assertion of `check.csv`, and each FAIL and WARN finding.

## Usage

``` r
verify(version, home = ".")
```

## Arguments

- version:

  The version, `"v"` and two digits, for example `"v01"`.

- home:

  The project folder. The run script runs with it as the working
  directory, as `Rscript v01_run.R` does when you start it there.

## Value

Invisibly, a list with three elements. `status` is the exit status of
the run. `assertions` is a data.frame of the failed assertions, with the
columns `file`, `assertion`, `expected` and `observed`. `findings` holds
the findings, as
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
returns them.

## Details

The new R session is that of
[`callr::rscript()`](https://callr.r-lib.org/reference/rscript.html),
with the library paths of this session. The output of the run shows as
it comes.

`verify()` reads only the `check.csv` files of this run. These are the
files `<version>_results/<scenario>/check.csv` that changed at or after
the start of the run. Before the run starts, `verify()` waits for the
next whole second, up to 1 second, and that second is the start. So a
`check.csv` of an earlier run is older than the start, also on a file
system that keeps the time of a change in whole seconds. The failed
assertions are the rows of these files with a value of `pass` that is
not `TRUE`.

The verdict is FAIL when one of these is true:

- the run ends with a status other than 0.

- the run wrote no `check.csv`. Then it checked no fake scenario, for
  example because it stopped before
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).

- a `check.csv` of the run has a failed assertion. This holds at each
  level of the rule `scenario_checks` in `pbn.yml`, also `off`.

- a rule of
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  fails.

Else the verdict is PASS. The last line names each reason of a FAIL. A
PASS of a run that did not write `<version>_results/real/results.qs2`
has the last line
`` Verdict: PASS on the fake scenarios only. The scenario `real` did not run. ``
It covers the fake data only. Its verdict is still PASS. In a
non-interactive R session, `verify()` quits R with status 1 after a
verdict FAIL, as
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
does.

## See also

Other checks:
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md),
[`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)

## Examples

``` r
if (FALSE) { # interactive()
pbn::verify("v01")
}
```
