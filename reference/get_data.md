# Get the data or the argset of one analysis at the console

`get_data()` returns the data that the analyses of one scenario and
simulation of the plan `p` get as `data`. `get_argset()` returns the
argset of one analysis, so you do not type the name of the analysis.

## Usage

``` r
get_data(p, scenario = "real", sim = 1)

get_argset(p, scenario = "real", sim = 1, name, i = 1)
```

## Arguments

- p:

  A plan from `plnr::Plan$new()`, with data from
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md).
  `get_argset()` also needs analyses from
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).

- scenario:

  The scenario. The default is `"real"`.

- sim:

  The simulation, a whole number from 1 to the number of simulations of
  the scenario.

- name:

  The name of the analysis kind, for example `"models"`.

- i:

  The counter of the extra dimensions of the kind, see
  [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).
  A kind without extra dimensions has 1 only.

## Value

`get_data()`: a named list with one element for each data element, and
`hash`. `get_argset()`: the argset, a named list.

## Details

`get_data(p, "null", 3)` calls `p$get_data()` for the analysis
`null__sim_0003__pbn_n_rows`.
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
adds this analysis for each scenario and simulation, and it gets the
same data as each other analysis of that scenario and simulation. So
`get_data()` takes the path of `p$get_data()`: the seed `sim`, the steps
of [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md), and
the cache with its one line. The value is the named list of the data
elements, with the element `hash` that plnr adds. `pbn::get_data(p)$d`
is the data `d` of `"real"`.

`get_argset(p, "null", 3, "models", 2)` is
`p$get_argset("null__sim_0003__models__2")`.

Call `get_argset()` after
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).
Call `get_data()` after
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md), before
or after
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md). Before
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
`get_data()` takes the scenarios of the last call of
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md) before
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md).
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
forgets them. It then calls each data function with the scenario, the
simulation and the settings, in the same way as plnr. The value then has
no element `hash`. So the line `d <- pbn::get_data(p, "real")$d` works
directly after
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) in a run
script.

Both stop on a scenario or a simulation that the plan does not hold, and
the error names the values that it holds. `get_argset()` also stops on a
kind or a counter that the plan does not hold for that scenario. An
example is a kind with `control = TRUE` for `"real"`.

To step through an action function by hand, put these lines first in it:

    if (plnr::is_run_directly()) {
      argset <- pbn::get_argset(p, "null", 3, "models")
      data <- pbn::get_data(p, "null", 3)
    }

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
steps through one analysis by hand.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
[`progress()`](https://www.rwhite.no/pbn/reference/progress.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
CFG <- list(scenarios = data.frame(scenario = c("real", "null"), sims = 2))
p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) {
  return(data.frame(x = stats::rnorm(3)))
})
pbn::add_analyses(
  p,
  pbn::scenarios(CFG),
  name = "means",
  fn = function(data, argset) return(data.frame(mean = mean(data$d$x))),
  outcome = c("y1", "y2")
)
pbn::get_data(p, "null", 2)$d
#>            x
#> 1 -0.8969145
#> 2  0.1848492
#> 3  1.5878453
pbn::get_argset(p, "null", 2, "means", 2)$outcome
#> [1] "y2"
```
