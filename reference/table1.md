# Compute the numbers of a descriptive table

`table1()` computes the numbers of a descriptive table, per group and
for all rows. It does no formatting.

## Usage

``` r
table1(d, by, vars)
```

## Arguments

- d:

  A data.table, or a data.frame.

- by:

  The name of the grouping column in `d`.

- vars:

  A data.table with the columns `var` and `type`. `var` names a column
  in `d`. `type` is `"bin"` for a 0/1 variable, or `"cont"` for a
  numeric variable.

## Value

A data.table with one row per variable and group, in the order of
`vars`, and `"All"` first. The columns are `var`, `grp`, `n`, `missing`,
`mean`, `sd`, `events`, `pct`, `p` and `test`. `n` counts the
non-missing values.

## Details

The groups are the levels of `by`, plus `"All"`. The levels are the
factor levels when `by` is a factor, and the sorted unique values
otherwise. A row with a missing `by` counts in `"All"` only. A level of
`by` that is `"All"` is an error. A level with no rows has a row in the
output with `n = 0`.

For a `"bin"` variable:

- `events` is the count of 1, and `pct` is `100 * events / n`.

- `p` compares the levels of `by` that have a non-missing value. It
  comes from the Pearson chi-square test without continuity correction.
  When any expected count is below 5, it comes from Fisher's exact test.

- `test` is `"chisq"` or `"fisher"`, and it is the same on every row of
  the variable. When fewer than two levels have a non-missing value, `p`
  and `test` are `NA`.

- A value other than 0, 1 or `NA` is an error.

For a `"cont"` variable, `mean` and `sd` hold the numbers. `p` and
`test` are `NA` in this version.

A column that does not apply to the type holds `NA`.

## Examples

``` r
d <- data.table::data.table(
  g = rep(c("a", "b"), each = 10),
  smoker = rep(c(1, 0, 0, 1), 5),
  age = 21:40
)
vars <- data.table::data.table(
  var = c("smoker", "age"),
  type = c("bin", "cont")
)
pbn::table1(d, by = "g", vars = vars)
#>       var    grp     n missing  mean      sd events   pct     p   test
#>    <char> <char> <int>   <int> <num>   <num>  <int> <num> <num> <char>
#> 1: smoker    All    20       0    NA      NA     10    50     1  chisq
#> 2: smoker      a    10       0    NA      NA      5    50     1  chisq
#> 3: smoker      b    10       0    NA      NA      5    50     1  chisq
#> 4:    age    All    20       0  30.5 5.91608     NA    NA    NA   <NA>
#> 5:    age      a    10       0  25.5 3.02765     NA    NA    NA   <NA>
#> 6:    age      b    10       0  35.5 3.02765     NA    NA    NA   <NA>
```
