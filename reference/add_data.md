# Add a data function to a plan

`add_data()` adds the data function `fn` to the plnr plan `p` with
`p$add_data()`. plnr calls it once for each scenario and simulation, and
gives the data to each analysis of that scenario and simulation.

## Usage

``` r
add_data(p, name, fn)
```

## Arguments

- p:

  A plan from `plnr::Plan$new()`.

- name:

  The name of the data. Each action function gets it as `data[[name]]`.

- fn:

  The data function, normally
  `function(scenario, sim) get_data(scenario, sim)`.

## Value

Invisibly, the name of the data.

## Details

plnr keys a data function on its arguments without a default. These are
the key fields. Each key field of `fn` MUST be `scenario`, `sim` or a
setting column of `CFG$scenarios`. Otherwise
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) stops
before it runs anything. plnr calls `fn` once for each set of values of
the key fields, and gives each analysis with those values the same data.

`add_data()` stops on these arguments of `fn`:

- `scenario` or `sim` with a default. plnr does not key on it, so every
  scenario or simulation would get the same data.

- `sim` without `scenario`. Every scenario would get the same data.

A data function without `scenario` and `sim`, for example one that reads
a lookup table, runs once for the whole run and sets no seed.

pbn adds `fn` in a wrapper. The wrapper has the key fields of `fn` as
its arguments, so plnr keys it in the same way. It calls `fn` with them
by name, so another argument of `fn` with a default keeps its default.
When `sim` is an argument, the wrapper calls `set.seed(sim)` before
`fn`. The data of simulation `k` is thus the same in each run, and does
not depend on the other scenarios of the run. A function that is called
with no `sim` sets no seed.

The wrapper needs no object of the R session that adds it.
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) gives
each worker the objects that the functions folder defines.

`name` MUST be one non-empty string, and it MUST NOT be `"hash"`,
because plnr adds `data$hash` to the data of each analysis. Two data
functions of one plan MUST NOT have the same name. Add data with
`add_data()`, not with `p$add_data()`.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
reads the data functions of `add_data()` only.

While [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
runs, an error in `fn` fails each analysis that gets the data. When you
call `p$get_data()` by hand, the error stops the call.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other plan:
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) {
  return(data.frame(x = stats::rnorm(3)))
})
```
