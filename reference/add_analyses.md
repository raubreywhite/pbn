# Add one analysis for each scenario and simulation to a plan

`add_analyses()` adds the analysis kind `name` to the plnr plan `p`: one
analysis for each scenario and simulation of `scenarios`, and for each
combination of the extra dimensions in `...`.

## Usage

``` r
add_analyses(p, scenarios, name, fn, ..., control = FALSE)
```

## Arguments

- p:

  A plan from `plnr::Plan$new()`.

- scenarios:

  The scenarios of the run, from
  [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md).

- name:

  The name of the analysis kind, for example `"models"`. It is the name
  of the element of the results object. It MUST be a syntactic name that
  starts with a letter. It MUST NOT be `"manifest"` or `"exclusions"`,
  and it MUST NOT start with `"pbn_"`. Each kind of one plan MUST have
  its own name.
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  names the exclusion table of a cohort `exclusions`.

- fn:

  The action function, `function(data, argset)`.

- ...:

  The extra dimensions, each named, for example
  `outcome = CFG$outcomes`. Each MUST be an atomic vector with at least
  one value, with no `NA` and no value twice. A vector of length 1 is a
  constant of the kind.

- control:

  `TRUE` adds the kind to every fake scenario of `scenarios`, and never
  to `"real"`. Use it for a check that is not a result, for example a
  model without adjustment that a fake scenario tests.

## Value

Invisibly, the names of the analyses of the kind, in the order of the
plan.

## Details

Each analysis gets the name `<scenario>__sim_<kkkk>__<name>__<i>`, for
example `null__sim_0003__models__2`. `<kkkk>` is the simulation with
four digits, also for a scenario with 1 simulation. `<i>` counts the
combinations of the extra dimensions from 1, with the last dimension of
`...` changing fastest. A kind without extra dimensions has `<i>` 1. To
step through one analysis by hand, use
[`get_argset()`](https://www.rwhite.no/pbn/reference/get_data.md) and
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md). They
take the scenario, the simulation, the kind and `<i>`, so you do not
type the name.

The argset of each analysis holds these fields:

- `scenario`, `sim`, and `analysis`, the kind `name`.

- `seed`, the seed of the analysis.

- the value of each extra dimension.

- the value of each setting column of its scenario.

The name of an extra dimension MUST follow these rules:

- It MUST be syntactic.

- It MUST NOT be the name of a setting column.

- It MUST NOT be one of `scenario`, `sim`, `analysis`, `seed`,
  `index_analysis`, `name`, `fn`, `fn_name`, `data` and `argset`.

pbn adds `fn` in a wrapper. The wrapper calls `set.seed(argset$seed)`
and then `fn(data, argset)`. `fn` runs inside the wrapper, so
[`plnr::is_run_directly()`](https://www.rwhite.no/plnr/reference/is_run_directly.html)
is `FALSE` in it when the plan runs.

The seed depends on the kind `name`, the counter `<i>` and `sim` only:

- It does not depend on the order of the analyses, the worker count or
  the R session.

- Two kinds of one simulation get different seeds, so they draw
  different numbers.

- One kind gets the same seed in each scenario, so its draws in `null`
  and in `real` of one simulation are the same.

- It is 10000 or more, so it differs from the seed `sim` of the data of
  simulations 1 to 9999.

The seed is a polynomial hash with base 257, modulo the prime
2147483629, of the code points of `"<name>__<i>__<sim>"`, plus 10000.
Two analyses of one simulation with the same seed stop `add_analyses()`
with an error that names both kinds.

While [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
runs, an analysis fails when it changes the shared data in place. plnr
gives every analysis of one scenario and simulation the same object. pbn
compares the names, the number of rows, the data.table key and the
address of each column of each data.frame before and after the analysis.
That finds an added, removed, renamed or replaced column, for example
`data$d[, x := 1]`, and a new key from
[`data.table::setkey()`](https://rdrr.io/pkg/data.table/man/setkey.html).
It does not find a change of values in place: `data$d[1, x := 0]`,
`data$d[g == "a", y := 0]`,
[`data.table::set()`](https://rdrr.io/pkg/data.table/man/assign.html) on
some rows,
[`data.table::setorder()`](https://rdrr.io/pkg/data.table/man/setorder.html)
and
[`data.table::setattr()`](https://rdrr.io/pkg/data.table/man/setattr.html)
on a column. Copy the data first with
[`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html)
when the analysis changes it.

While [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
runs, an analysis also fails when it changes an object of the functions
folder, for example `CFG`. plnr runs many analyses in one R session, so
the change would reach each later analysis of that session. A result
would then depend on the analyses that ran before it there. pbn checks
each object that the functions folder made or changed when
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
sourced it, other than a function, a plnr plan and `.Random.seed`. It
compares the bytes of
[`serialize()`](https://rdrr.io/r/base/serialize.html) of each object
before and after the analysis. That finds a new value, a new element and
a new attribute. It compares them before and after each data function or
data step too. A data function that changes such an object fails each
analysis that gets its data. The error names the first top-level element
that changed, for example `CFG$table`. It also says how: a new attribute
such as the index of a data.table, a new or a gone element, or another
value. The output function of
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
and each `expect_` function of
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
get the same check in this R session.

While [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
runs, two more parts of the state of an R session cannot pass from one
analysis to the next:

- An analysis fails when it leaves a new object in the global
  environment, for example with `<<-` or
  [`assign()`](https://rdrr.io/r/base/assign.html). `.Random.seed` does
  not count.

- pbn takes the kind of random number generator of the run from
  [`RNGkind()`](https://rdrr.io/r/base/Random.html) at the start of
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md). It
  sets that kind again before each seed of a data function and of an
  analysis. An analysis that calls
  [`RNGkind()`](https://rdrr.io/r/base/Random.html) then changes no draw
  of a later analysis.

A data.table subset with a logical, for example
`CFG$table[group == "a"]`, adds an index to the table in place. Copy the
table first with
[`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html), or
set `options(datatable.auto.index = FALSE)` in the project. pbn sets
neither.

The check has these limits:

- It does not see a function inside an object, because R compiles a
  function after some calls.

- It does not see the contents of an environment inside an object, for
  example that of a formula. It does not call an active binding.

- It does not see a change of an object that already was in the global
  environment, other than an object of the functions folder.

- The analysis that made the change fails, and the change stays in that
  R session. The run stops at that failure, see
  [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md).

- Without
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  pbn checks no object.

The check costs time in proportion to the size of the objects. One
computer took about 45 microseconds for each analysis with a `CFG` of 49
elements. These held 5 data.tables of 100 rows, 20 KB in
[`serialize()`](https://rdrr.io/r/base/serialize.html).

For each scenario and simulation, pbn also adds one analysis named
`<scenario>__sim_<kkkk>__pbn_n_rows`. It counts the rows of each data
element.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stores the counts in `manifest$n_rows_data`, and leaves the analysis out
of the results.

`add_analyses()` stops before it adds anything when an analysis of that
name is in the plan already.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other plan:
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md),
[`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md),
[`progress()`](https://www.rwhite.no/pbn/reference/progress.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)

## Examples

``` r
CFG <- list(
  scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 2)),
  outcomes = c("y1", "y2")
)
p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) {
  return(data.frame(y1 = stats::rnorm(5), y2 = stats::rnorm(5)))
})
pbn::add_analyses(
  p,
  pbn::scenarios(CFG),
  name = "means",
  fn = function(data, argset) {
    return(data.frame(mean = mean(data$d[[argset$outcome]])))
  },
  outcome = CFG$outcomes
)
pbn::get_argset(p, "null", 2, "means", 2)
#> $scenario
#> [1] "null"
#> 
#> $sim
#> [1] 2
#> 
#> $analysis
#> [1] "means"
#> 
#> $seed
#> [1] 1380106697
#> 
#> $outcome
#> [1] "y2"
#> 
```
