# Check that an analysis recovers the truth of a fake scenario

`expect_recovers()` checks the bias, the standard error and the coverage
of an estimate over the simulations of a fake scenario. It returns the
assertions in the format of an `expect_<scenario>()` function.

## Usage

``` r
expect_recovers(
  x,
  truth,
  estimate = "estimate",
  se = "se",
  lower = "lower",
  upper = "upper",
  by = NULL,
  level = 0.95
)
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

- estimate, se, lower, upper:

  The names of the columns of `x` that hold the estimate, its standard
  error and the limits of its confidence interval.

- by:

  `NULL`, or the names of the columns of `x` that split it into groups,
  for example `"term"`. Each group gets its own three assertions.

- level:

  The level of the confidence interval of `lower` and `upper`.

## Value

A data.frame with the columns `assertion`, `expected`, `observed` and
`pass`, and three rows for each group. Give it to
[`rbind()`](https://rdrr.io/r/base/cbind.html) with the other assertions
of the `expect_<scenario>()` function.

## Details

The checks work on the scale of the columns. Give a log odds ratio, its
standard error and its limits on the log scale, with a truth on the log
scale. Do not give odds ratios. On the scale of the odds ratio, the
estimate is biased, and the standard error is that of the log.

For each group and assertion, `n` is the number of simulations with the
values that the assertion needs. The bias needs a finite `estimate`, the
standard error a finite `estimate` and `se`, and the coverage a finite
`lower` and `upper`. The empirical SE is `sd(estimate)` over these `n`
simulations. `z` is 3.89, the two-sided normal quantile of 0.0001. The
three assertions are:

1.  Bias: `mean(estimate) - truth`. Its Monte Carlo SE is
    `sd(estimate) / sqrt(n)`. The assertion passes when
    `|bias| <= z * MCSE`.

2.  Standard error: the relative error `ModSE / EmpSE - 1` of the model
    SE `ModSE = sqrt(mean(se^2))` against the empirical SE `EmpSE`. Its
    Monte Carlo SE is
    `(ModSE / EmpSE) * sqrt(var(se^2) / (4 * n * ModSE^4) + 1 / (2 * (n - 1)))`.
    The assertion passes when `|ModSE / EmpSE - 1| <= z * MCSE`.

3.  Coverage: the share of simulations where `lower <= truth <= upper`.
    The assertion passes when the share is within
    `z * sqrt(level * (1 - level) / n)` of `level`.

Morris, White and Crowther (2019), Table 6, define these three
performance measures and give a Monte Carlo SE for each. The SE of the
relative error is a delta-method approximation. It assumes that the
estimate is normal, and that `se` is independent of the estimate. The SE
of the coverage uses `level`, not the observed share. Under a correct
analysis the true share is `level`, and the observed share is 1 in many
small studies, which gives an SE of 0.

Each row shows the observed value, the limit and the number of
simulations. It also shows "can detect": the smallest error that the
assertion finds with a probability of 80% at this number of simulations.
For the bias, this is `(z + 0.84) * sd(estimate) / sqrt(n)`. For the
standard error, it is the ratio `ModSE / EmpSE` below and above 1. For
the coverage, it is the true coverage below `level`. A large "can
detect" value means that the check passes almost anything.

## The fewest simulations

An assertion fails with "too few simulations to check" when `n` is
below:

- 100 for the bias and the standard error. The bias uses `sd(estimate)`,
  so its false-alarm probability is that of a t distribution. At 100
  simulations it is 0.00018. At 30 it is 0.00054.

- for the coverage, the smallest `n` that puts both limits inside 0 and
  1, and at least 100. The limits are
  `level +/- z * sqrt(level * (1 - level) / n)`. This is 288 for the
  level 0.95, and 137 for 0.9.

An assertion also fails when more than 5% of the simulations of the
group miss a value that it needs. Each row shows its own number of
missing simulations, also when it passes. Simulations where the model
failed are not missing at random, so a pass with missing simulations is
weaker.

An assertion fails on a value that cannot be right, and its row gives
the count:

- the standard error, on an `se` of 0 or less.

- the coverage, on a `lower` above its `upper`.

- the bias and the standard error, when all estimates are equal. The
  Monte Carlo SE is then 0.

## The false alarms

A false alarm is a failed assertion for a correct analysis. It stops the
run, and the same seeds give it again in the next run. So `z` is large.
For each assertion the probability is about 0.0001. For one call with
`g` groups and 3 assertions in each group, it is about
`1 - (1 - 0.0001)^(3 * g)`:

|        |            |                         |
|--------|------------|-------------------------|
| Groups | Assertions | False alarm of the call |
| 1      | 3          | 0.0003                  |
| 6      | 18         | 0.0018                  |
| 20     | 60         | 0.0060                  |

The real probabilities are a little larger, because the bias is a t
statistic and the coverage is a binomial count. At 300 simulations, the
bias has 0.00012 and the coverage at the level 0.95 has 0.00028. There
is no option to change `z`.

## References

Morris TP, White IR, Crowther MJ. Using simulation studies to evaluate
statistical methods. Statistics in Medicine. 2019;38:2074-2102.
[doi:10.1002/sim.8086](https://doi.org/10.1002/sim.8086)

## See also

[`expect_biased()`](https://www.rwhite.no/pbn/reference/expect_biased.md)
for a control analysis, and
[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md).

Other checks of fake scenarios:
[`expect_biased()`](https://www.rwhite.no/pbn/reference/expect_biased.md)

## Examples

``` r
set.seed(1)
x <- data.frame(sim = 1:300, estimate = rnorm(300, 0.5, 0.2), se = 0.2)
x$lower <- x$estimate - 1.96 * x$se
x$upper <- x$estimate + 1.96 * x$se
pbn::expect_recovers(x, truth = 0.5)
#>                                assertion
#> 1                    x: bias of estimate
#> 2 x: model SE of estimate against its SD
#> 3        x: coverage of the 95% interval
#>                                                                         expected
#> 1                      |bias| <= 0.0433 (3.89 MCSE); can detect |bias| of 0.0527
#> 2 |relative error| <= 16.5% (3.89 MCSE); can detect ModSE/EmpSE of 0.834 or 1.23
#> 3                       0.901 to 0.999 (3.89 MCSE); can detect coverage of 0.886
#>                                                 observed pass
#> 1                        0.00672; 300 of 300 simulations TRUE
#> 2 +3.8% (ModSE 0.2, EmpSE 0.193); 300 of 300 simulations TRUE
#> 3                          0.947; 300 of 300 simulations TRUE
```
