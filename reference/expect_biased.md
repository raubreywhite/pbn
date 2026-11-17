# Check that a control analysis shows its bias

`expect_biased()` checks that the estimate of a control analysis is
biased over the simulations of a fake scenario. Use it for an analysis
that MUST be wrong, for example a crude model in a scenario with a
confounder.

## Usage

``` r
expect_biased(x, truth, estimate = "estimate", by = NULL)
```

## Arguments

- x:

  The stacked results of one analysis kind, for example
  `results$models`. It is a data.frame with the column `sim` that
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  adds. It has one row for each simulation, or for each simulation and
  group of `by`.

- truth:

  The true value of the estimate. One number for every group, or a
  data.frame with the columns of `by` and a column `truth`, with one row
  for each group.

- estimate:

  The name of the column of `x` that holds the estimate.

- by:

  `NULL`, or the names of the columns of `x` that split it into groups,
  for example `"term"`. Each group gets its own three assertions.

## Value

A data.frame with the columns `assertion`, `expected`, `observed` and
`pass`, and one row for each group.

## Details

A control analysis shows that the fake scenario can find the error it
was built for. When a crude model is not biased, the confounder of the
scenario is too weak, and the recovery of the adjusted model shows
nothing.

For each group, `n` is the number of simulations with a finite
`estimate`. The bias is `mean(estimate) - truth`, and its Monte Carlo SE
is `sd(estimate) / sqrt(n)`. The assertion passes when
`|bias| > z * MCSE`, with the `z` of
[`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md),
3.89. Otherwise it fails, and the row says that the control did not show
the bias it was built to show. The row shows the "can detect" value of
[`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md):
the smallest bias that the assertion finds with a probability of 80%.

Below 100 simulations, the assertion fails with "too few simulations to
check". It also fails when more than 5% of the simulations have a
missing or infinite estimate, and when all estimates are equal.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
checks a crude model in a scenario with a confounder.

Other checks of fake scenarios:
[`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md)

## Examples

``` r
set.seed(1)
x <- data.frame(sim = 1:300, estimate = rnorm(300, 0.9, 0.2))
pbn::expect_biased(x, truth = 0.5)
#>                        assertion
#> 1 x: bias of estimate, a control
#>                                                   expected
#> 1 |bias| > 0.0433 (3.89 MCSE); can detect |bias| of 0.0527
#>                        observed pass
#> 1 0.407; 300 of 300 simulations TRUE
```
