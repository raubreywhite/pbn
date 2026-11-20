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
  variable, or `"cat"` for a categorical variable. Two text columns are
  optional. `label` is the printed label of the variable, and `NA` gives
  its name. `section` names the section that the variable belongs to,
  and `NA` or `""` gives none.

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
columns. The column `label` follows, which pbn 2026.12.4 added: the
label of the variable, or the level of a level row. With a style, the
columns `row`, `value_pretty` (the cell text) and `p_pretty` (the text
of `p`) follow. The attribute `footnotes` then holds the footnotes of
the style as a character vector. A style can add rows, as the section
Styles says. The other columns of each row of the output without a style
stay the same.

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

- Fisher's exact test first uses the default workspace of
  [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).
  When the test stops with that workspace, it runs again with a
  workspace of `2e7`. When it also stops then, `p` is `NA`, `test` is
  `"fisher_failed"`, and a warning names the variable. A p of Fisher's
  exact test is exact, never simulated.

- `test` is `"chisq"`, `"fisher"` or `"fisher_failed"`, and it is the
  same on every row of the variable. When fewer than two levels have a
  non-missing value, `p` and `test` are `NA`.

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
where the value comes from. `table1()` takes any `source` that is not
empty. The source of an installed style MUST cite a reference table or a
guideline page, as the subsection on the proof of a style says.
`table1()` checks every field when it reads the file. An unknown field,
a missing field, a missing `source` or a value that the field does not
allow stops it. The error names the field and the file. The fields that
pbn 2026.12.3 added are optional, so a style file of an earlier pbn
stays valid. They are `cont.median_digits`, the section `cat`, and the
fields `missing.row_template` and `missing.row_pct_digits`. pbn
2026.12.4 added the optional fields `cat.level_prefix` and
`missing.row_label`. An optional field that is present gets the same
checks as the other fields.

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
  `"cat"` variable. When it is present, it MUST hold the three fields
  below that are not optional.

- `cat.template`: the cell of a level row of a `"cat"` variable, over
  `{events}`, `{n}` and `{pct}`, as `bin.template`.

- `cat.pct_digits`: the decimals of `pct` in a level row, a whole number
  from 0 to 10.

- `cat.heading_template`: the cell of the heading row of a `"cat"`
  variable. It holds no placeholder. `""` gives an empty cell.

- `cat.level_prefix` (optional): the text before the level in the column
  `label` of a level row, such as `"- "`.

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

- `missing.row_label` (optional): the column `label` of a Missing row,
  such as `"Missing"`. Without it, the label is `NA`.

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

- `"section"`: one row for each group before the rows of the first
  variable of each section, when `vars` has the column `section`. Its
  `grp` is the group and its `label` is the section. Each other column
  is `NA`.

The column `label` of a heading row is the label of the variable. A
level row has `cat.level_prefix` and the level, and a Missing row has
`missing.row_label`.

A heading row and a Missing row keep `var`, `grp`, `n`, `missing`, `p`
and `test` of the group. Their `level` and their other numbers are `NA`,
except `pct` of a Missing row. A table with no `"cat"` variable, no
Missing row and no section keeps the rows of the output without a style.

`table1()` rounds each number with
[`formatC()`](https://rdrr.io/r/base/formatc.html), which rounds as the
C function [`sprintf()`](https://rdrr.io/r/base/sprintf.html) does.
`events`, `n` and `missing` print with no decimals. The thousands
separator applies to every number of a cell. A cell is `NA` when a
number of its template is `NA`, as for a group with `n = 0`. `p_pretty`
is `NA` where `p` is `NA`.

### The proof of a style

pbn installs a style only with the real published Table 1s that prove
it. Beside the file `inst/table1/<name>.yml` is the folder
`inst/table1/<name>/`. For each reference table k, it holds five files:

- `reference_<k>.csv`: the table as printed. It has one row per printed
  row and one column per printed column. The header holds the printed
  column headers, and the first column holds the printed row labels.
  Each cell is the printed string, and an empty cell is `""`.

- `source_<k>.yml`: the fields `authors`, `title`, `journal`, `year`,
  `volume`, `pages`, `doi`, `table` and `transcribed`. `authors` is a
  list of maps with the keys `family` and `given`. `pages` is the
  article number or the pages, and `table` is the number of the table.
  `transcribed` is a map with the keys `file`, `by` and `date`.

- `fake_<k>.csv`: the fake data, one row per fake person. An empty cell
  or `NA` is a missing value.

- `vars_<k>.csv`: the columns `var`, `type`, `label`, `levels` and
  `section`. The one row of type `by` names the column of the groups.
  The other rows, with `var`, `type`, `label` and `section`, are the
  argument `vars`. `levels` gives the order of the groups, or of the
  levels of a `"cat"` variable, separated by `|`. `levels` and `section`
  can be empty.

- `layout_<k>.yml`: the map of the output onto the printed table. Each
  item of `columns` names a printed column, except the first, in order.
  It has `cell: value` and a `grp`, or `cell: p`. Each item of `rows`
  names a printed row, in order. It has the key `row` and, where it
  applies, `var` and `level`, and no other key. `row` is `value`,
  `heading` or `missing` as in the column `row` of the output. It is
  `section` for a section heading, and `none` for a printed row with no
  computed counterpart.

The test of pbn runs `table1(fake, by, vars, style)` for each reference
table. It compares each printed cell with the cell of the style, the row
labels of the first column included. The headers are not cells. The cell
of the style is `value_pretty` of the group, or `p_pretty` for
`cell: p`, and `NA` is `""`. The label of the style is the column
`label` of the same row of the output. No label comes from the layout or
the reference. A printed row of `section` takes the label of the section
row of the output. That row MUST come right before the first row of the
next variable of the layout, and the label is `""` without it. A row of
`section` has `""` in each cell after the label. A row of `none` has
`""` in each cell. So each printed cell of it that is not empty MUST be
an exception with the rule `not_computable`.

A cell passes when the two strings are identical. A cell that differs
passes only when the file `exceptions.yml` of the folder lists it with
the same two strings.

`exceptions.yml` is a map with the one key `exceptions`, a list. Each
item has these fields:

- `reference`, `row` and `column`: the cell, as `reference_1`, the
  number of the printed row after the header, and the printed header.

- `printed` and `style`: the printed string and the string of the style.

- `rule`, `source` and `justification`: why the two differ. `source` and
  `justification` MUST NOT be empty.

`rule` is one of these:

- `guideline`: the reference departs from the author guidelines, and the
  style follows them. `source` names the guideline and its page.

- `other_reference`: two references disagree, and the style follows the
  other one. `source` cites a cell of the other reference.

- `not_computable`: `table1()` cannot compute the cell, such as a P of a
  `"cont"` variable.

- `published_error`: the printed cell is wrong. `source` gives the
  computation that shows it.

The test fails a style in each of these cases:

- A cell differs, and `exceptions.yml` does not list it.

- An exception has strings other than those of its cell. A cell that
  matches MUST NOT have an exception.

- An exception names no cell, or a cell twice.

- The style has no folder. The folder lacks a file, or it holds a file
  of another name.

- The `source` of a field holds "editorial", in any case.

- The `source` of a field cites none of the three forms of a citation:

  1.  a cell, as `reference_1, row 2, column "P"`.

  2.  the family name of the first author of a reference, with
      `Table <n>` of its source.

  3.  a guide or a guideline with its page, as `p. 4` or `page 4`.

A style that pbn cannot prove is withdrawn. pbn removes its file and
keeps its name, so `table1()` stops with the version that withdrew it
and the reason.
[`table1_styles()`](https://www.rwhite.no/pbn/reference/table1_styles.md)
does not list it. pbn 2026.12.4 withdrew `ijoa_brief_report_2026_11_20`,
because it was not proven against real published tables.

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
#>    median    q1    q3  label
#>     <num> <num> <num> <char>
#> 1:     NA    NA    NA smoker
#> 2:     NA    NA    NA smoker
#> 3:     NA    NA    NA smoker
#> 4:   30.5 25.75 35.25    age
#> 5:   25.5 23.25 27.75    age
#> 6:   35.5 33.25 37.75    age

# A style file, written to tempdir()
style <- file.path(tempdir(), "example_2026_12_01.yml")
writeLines(c(
  "bin:",
  "  template: {value: '{events} ({pct})', source: example only}",
  "  pct_digits: {value: 1, source: example only}",
  "cont:",
  "  template: {value: '{mean} ({sd})', source: example only}",
  "  mean_digits: {value: 1, source: example only}",
  "  sd_digits: {value: 1, source: example only}",
  "p:",
  "  digits: {value: 3, source: example only}",
  "  template: {value: 'P ={p}', source: example only}",
  "  lower: {value: 0.001, source: example only}",
  "  below_template: {value: 'P <{lower}', source: example only}",
  "  upper: {value: 0.999, source: example only}",
  "  above_template: {value: 'P >{upper}', source: example only}",
  "thousands:",
  "  mark: {value: ',', source: example only}",
  "  from: {value: 10000, source: example only}",
  "missing:",
  "  show: {value: if_any, source: example only}",
  "  template: {value: '; {missing} missing', source: example only}",
  "footnotes:",
  "  - {value: 'Values are n (percent) or mean (SD).', source: example only}"
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
