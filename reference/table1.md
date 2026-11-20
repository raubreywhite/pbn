# Compute the numbers of a descriptive table

`table1()` computes the numbers of a descriptive table, per group and
for all rows. With a style, it also formats these numbers as text.

## Usage

``` r
table1(d, by, vars, style = NULL)
```

## Arguments

- d:

  A data.table, or a data.frame.

- by:

  The name of the grouping column in `d`.

- vars:

  A data.table with the columns `var` and `type`. `var` names a column
  in `d`. `type` is `"bin"` for a 0/1 variable, `"cont"` for a numeric
  variable, or `"cat"` for a categorical variable.

- style:

  `NULL`, the name of an installed style, or the path to a style file
  that ends in `.yml`.
  [`table1_styles()`](https://www.rwhite.no/pbn/reference/table1_styles.md)
  gives the names of the installed styles. With `NULL`, `table1()` does
  no formatting. The section Styles below describes a style file.

## Value

A data.table with one row per variable and group, in the order of
`vars`, and `"All"` first. A `"cat"` variable has one row per level and
group. The columns are `var`, `grp`, `n`, `missing`, `mean`, `sd`,
`events`, `pct`, `p`, `test`, `level`, `median`, `q1` and `q3`. `n`
counts the non-missing values. pbn 2026.12.3 added the last four
columns. With a style, the columns `row`, `value_pretty` (the cell text)
and `p_pretty` (the text of `p`) follow. The attribute `footnotes` then
holds the footnotes of the style as a character vector. A style can add
rows, as the section Styles says. The other columns of each row of the
output without a style stay the same.

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

For a `"cont"` variable, `mean`, `sd`, `median`, `q1` and `q3` hold the
numbers of the non-missing values. `q1` and `q3` are the first and the
third quartile. The median and the quartiles are
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) with
`type = 7`, the default of R. `p` and `test` are `NA` in this version.

A `"cat"` variable has one row for each level and group. The rows of a
level are together, in the order of the groups.

- The levels are the factor levels when the variable is a factor.
  Otherwise they are the sorted unique values that are not `NA`, so a
  number sorts as a number. `level` holds each level as text.

- `events` is the count of the level, and `n` is the count of the
  non-missing values of the group. So `pct` is `100 * events / n`, and
  the percentages of a group with `n` above 0 add up to 100.

- `p` and `test` follow the rule of a `"bin"` variable, with the levels
  in place of 0 and 1. A level with a count of 0 in each group is not
  part of the test. When fewer than two levels or fewer than two groups
  remain, `p` and `test` are `NA`.

- A variable that has no level, because each value is `NA` and it is not
  a factor, has one row per group. Its `level`, `events` and `pct` are
  `NA`. With a style it gets no level row, only its heading row and the
  Missing row that `missing.show` asks for.

A column that does not apply to the type holds `NA`.

## Styles

A style formats the numbers of `table1()` for a journal or a report. A
style can only format what `table1()` computes. It cannot add a test of
a `"cont"` variable.

An installed style is the file `inst/table1/<name>_<YYYY_MM_DD>.yml` of
pbn. Its name is the file name without `.yml`. pbn never edits a
released style, so a changed style gets a new date.

A style file is a YAML map of sections. Each field of a section is a map
with the keys `value` and `source`. `source` is one string that says
where the value comes from. Examples are the guide and its count of
papers (`k/N papers`), the guideline and its page, and
`editorial choice`. `table1()` checks every field when it reads the
file. An unknown field, a missing field, a missing `source` or a value
that the field does not allow stops it. The error names the field and
the file. The fields that pbn 2026.12.3 added are optional, so a style
file of an earlier pbn stays valid. They are `cont.median_digits`, the
section `cat`, and the fields `missing.row_template` and
`missing.row_pct_digits`. An optional field that is present gets the
same checks as the other fields.

A template is text with placeholders in braces, such as `{events}`. A
template can hold only the placeholders of its field, in any order, and
it can hold none. These are the fields:

- `bin.template`: the cell of a `"bin"` row, over `{events}`, `{n}` and
  `{pct}`. An example is `"{events} ({pct})"`.

- `bin.pct_digits`: the decimals of `pct`, a whole number from 0 to 10.

- `cont.template`: the cell of a `"cont"` row, over `{mean}`, `{sd}`,
  `{median}`, `{q1}` and `{q3}`. An example is `"{median} ({q1}-{q3})"`.
  A template that holds `{median}`, `{q1}` or `{q3}` needs
  `cont.median_digits`.

- `cont.mean_digits` and `cont.sd_digits`: the decimals of `mean` and of
  `sd`, each a whole number from 0 to 10.

- `cont.median_digits` (optional): the decimals of `median`, `q1` and
  `q3`, a whole number from 0 to 10.

- `cat` (an optional section): it MUST be present when `vars` has a
  `"cat"` variable. When it is present, it MUST hold all three fields
  below.

- `cat.template`: the cell of a level row of a `"cat"` variable, over
  `{events}`, `{n}` and `{pct}`, as `bin.template`.

- `cat.pct_digits`: the decimals of `pct` in a level row, a whole number
  from 0 to 10.

- `cat.heading_template`: the cell of the heading row of a `"cat"`
  variable. It holds no placeholder. `""` gives an empty cell.

- `p.digits`: the decimals of `p`, a whole number from 0 to 10.

- `p.template`: the text of `p`, over `{p}`. `"P ={p}"` gives
  `P =0.005`, and `"{p}"` gives the number only.

- `p.lower`: `null`, or a number above 0 and below 1. A `p` below it
  gets `p.below_template` in place of `p.template`.

- `p.below_template`: the text of such a `p`, over `{lower}`. With
  `p.lower` 0.001, `"P <{lower}"` gives `P <0.001`.

- `p.upper`: `null`, or a number above 0 and below 1. A `p` above it
  gets `p.above_template`.

- `p.above_template`: the text of such a `p`, over `{upper}`. With
  `p.upper` 0.999, `"P >{upper}"` gives `P >0.999`.

- `thousands.mark`: the thousands separator, such as `","`. The empty
  string `""` gives no separator.

- `thousands.from`: a number above 0. A number gets the separator when
  its rounded absolute value is `thousands.from` or more. With 10000,
  12347 prints as `12,347` and 9999 prints as `9999`.

- `missing.show`: `never`, `if_any`, `always`, `row_if_any` or
  `row_always`. With `if_any`, a cell gets `missing.template` when
  `missing` is above 0. With `always`, each cell that is not `NA` gets
  it. With `row_if_any`, a variable gets a Missing row when its `"All"`
  group has a missing value. With `row_always`, each variable gets a
  Missing row. The two row choices need `missing.row_template` and
  `missing.row_pct_digits`.

- `missing.template`: the text that `table1()` adds to the end of a
  cell, over `{missing}`. An example is `"; {missing} missing"`. Only
  `if_any` and `always` use it.

- `missing.row_template` (optional): the cell of a Missing row, over
  `{missing}` and `{pct}`. An example is `"{missing} ({pct}%)"`.

- `missing.row_pct_digits` (optional): the decimals of `pct` in a
  Missing row, a whole number from 0 to 10.

- `footnotes`: a list, which can be empty. Each item is a map with the
  keys `value` and `source`, and `value` is the text of one footnote.

`p.lower` and `p.upper` print with `p.digits` decimals, so each MUST
have no more decimals than `p.digits`. `p.lower` MUST be below
`p.upper`. `table1()` compares `p` with the thresholds before it rounds
`p`, so 0.9994 is above 0.999. A `p` equal to a threshold gets
`p.template`.

A style can add rows, and the column `row` names the kind of each row:

- `"value"`: a row of the output without a style. A level row of a
  `"cat"` variable has `p_pretty` `NA`.

- `"heading"`: one row for each group before the level rows of a `"cat"`
  variable. Its cell is `cat.heading_template`, and its `p_pretty` is
  the P of the variable.

- `"missing"`: one row for each group after the other rows of a
  variable, when `missing.show` asks for it. Its cell gives `missing`
  and its percent, and its column `pct` holds that percent,
  `100 * missing / (n + missing)`. The denominator counts every row of
  the group, missing ones included. Its `p_pretty` is `NA`.

A heading row and a Missing row keep `var`, `grp`, `n`, `missing`, `p`
and `test` of the group. Their `level` and their other numbers are `NA`,
except `pct` of a Missing row. A table with no `"cat"` variable and no
Missing row keeps the rows of the output without a style.

`table1()` rounds each number with
[`formatC()`](https://rdrr.io/r/base/formatc.html), which rounds as the
C function [`sprintf()`](https://rdrr.io/r/base/sprintf.html) does.
`events`, `n` and `missing` print with no decimals. The thousands
separator applies to every number of a cell. A cell is `NA` when a
number of its template is `NA`, as for a group with `n = 0`. `p_pretty`
is `NA` where `p` is `NA`.

## See also

Other shared helpers:
[`table1_styles()`](https://www.rwhite.no/pbn/reference/table1_styles.md)

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
#>       var    grp     n missing  mean      sd events   pct     p   test  level
#>    <char> <char> <int>   <int> <num>   <num>  <int> <num> <num> <char> <char>
#> 1: smoker    All    20       0    NA      NA     10    50     1  chisq   <NA>
#> 2: smoker      a    10       0    NA      NA      5    50     1  chisq   <NA>
#> 3: smoker      b    10       0    NA      NA      5    50     1  chisq   <NA>
#> 4:    age    All    20       0  30.5 5.91608     NA    NA    NA   <NA>   <NA>
#> 5:    age      a    10       0  25.5 3.02765     NA    NA    NA   <NA>   <NA>
#> 6:    age      b    10       0  35.5 3.02765     NA    NA    NA   <NA>   <NA>
#>    median    q1    q3
#>     <num> <num> <num>
#> 1:     NA    NA    NA
#> 2:     NA    NA    NA
#> 3:     NA    NA    NA
#> 4:   30.5 25.75 35.25
#> 5:   25.5 23.25 27.75
#> 6:   35.5 33.25 37.75

# A style file, written to tempdir()
style <- file.path(tempdir(), "example_2026_12_01.yml")
writeLines(c(
  "bin:",
  "  template: {value: '{events} ({pct})', source: editorial choice}",
  "  pct_digits: {value: 1, source: editorial choice}",
  "cont:",
  "  template: {value: '{mean} ({sd})', source: editorial choice}",
  "  mean_digits: {value: 1, source: editorial choice}",
  "  sd_digits: {value: 1, source: editorial choice}",
  "p:",
  "  digits: {value: 3, source: editorial choice}",
  "  template: {value: 'P ={p}', source: editorial choice}",
  "  lower: {value: 0.001, source: editorial choice}",
  "  below_template: {value: 'P <{lower}', source: editorial choice}",
  "  upper: {value: 0.999, source: editorial choice}",
  "  above_template: {value: 'P >{upper}', source: editorial choice}",
  "thousands:",
  "  mark: {value: ',', source: editorial choice}",
  "  from: {value: 10000, source: editorial choice}",
  "missing:",
  "  show: {value: if_any, source: editorial choice}",
  "  template: {value: '; {missing} missing', source: editorial choice}",
  "footnotes:",
  "  - {value: 'Values are n (percent) or mean (SD).', source: editorial choice}"
), style)
t1 <- pbn::table1(d, by = "g", vars = vars, style = style)
t1[, c("var", "grp", "value_pretty", "p_pretty")]
#>       var    grp value_pretty p_pretty
#>    <char> <char>       <char>   <char>
#> 1: smoker    All    10 (50.0) P >0.999
#> 2: smoker      a     5 (50.0) P >0.999
#> 3: smoker      b     5 (50.0) P >0.999
#> 4:    age    All   30.5 (5.9)     <NA>
#> 5:    age      a   25.5 (3.0)     <NA>
#> 6:    age      b   35.5 (3.0)     <NA>
attr(t1, "footnotes")
#> [1] "Values are n (percent) or mean (SD)."
```
