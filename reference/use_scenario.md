# Choose the scenario for stepping through a run script by hand

`use_scenario()` chooses the scenario and the simulation that every
later
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
call in this R session uses. It is for a person who steps through a run
script by hand.

## Usage

``` r
use_scenario(scenario, sim = 1)
```

## Arguments

- scenario:

  The scenario, one name of lower-case letters, digits and underscores.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  checks that it is in `CFG$scenarios`.

- sim:

  The simulation, a whole number of 1 or more.

## Value

Invisibly, a list with `scenario` and `sim`.

## Details

Write one commented line for each scenario directly above the
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
call of `vNN_run.R`, in the order of `CFG$scenarios`:

    # Run one of these lines to step through a scenario by hand. The choice lasts for the R session.
    # pbn::use_scenario("real", sim = 1)
    # pbn::use_scenario("null", sim = 1)
    p <- pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")

To step through a scenario, run one of these lines without the `#`, then
run the script line by line. The lines MUST stay commented in the file.
`use_scenario()` stops in a non-interactive R session, for example under
`Rscript`, and
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
fails an active call.

The choice lasts for the R session. Every later
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
call uses it, also when you run that call again, until the next
`use_scenario()` call. `use_scenario("real", sim = 1)` goes back to the
real data. The command-line arguments `scenario=<name>` and `sim=<k>`
replace the choice as a whole.

## See also

Other project setup:
[`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)

## Examples

``` r
if (interactive()) {
  pbn::use_scenario("null", sim = 2)
}
```
