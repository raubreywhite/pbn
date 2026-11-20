# Table 1 styles

[`pbn::table1()`](https://www.rwhite.no/pbn/reference/table1.md)
computes the numbers of a descriptive table. A style formats these
numbers as the text that a journal or a report needs. With a style, the
output gets the columns `row`, `value_pretty` and `p_pretty`. A style
can also add a heading row and a Missing row to a variable.

## What a style is

A style is a YAML file of pbn, `inst/table1/<name>_<YYYY_MM_DD>.yml`.
Give its name, the file name without `.yml`, to the argument `style` of
[`table1()`](https://www.rwhite.no/pbn/reference/table1.md). pbn never
edits a released style. A changed style gets a new file with a new date,
so a project that names a style gets the same text at each run.

Each field of a style holds a `value` and a `source`. The source says
where the value comes from. In an installed style, it cites a cell of a
real published table or a page of the author guidelines. The section
below says how pbn checks this.

## The schema

This style file has every field. The section Styles of
[`?table1`](https://www.rwhite.no/pbn/reference/table1.md) gives the
type and the check of each field. These are optional: the section `cat`,
and the fields `cont.median_digits`, `cat.level_prefix`,
`missing.row_template`, `missing.row_pct_digits` and
`missing.row_label`.

``` yaml
bin:
  template: {value: "{events} ({pct})", source: example only}
  pct_digits: {value: 1, source: example only}
cont:
  template: {value: "{median} ({q1}-{q3})", source: example only}
  mean_digits: {value: 1, source: example only}
  sd_digits: {value: 1, source: example only}
  median_digits: {value: 1, source: example only}
cat:
  template: {value: "{events} ({pct})", source: example only}
  pct_digits: {value: 1, source: example only}
  heading_template: {value: "", source: example only}
  level_prefix: {value: "- ", source: example only}
p:
  digits: {value: 3, source: example only}
  template: {value: "P ={p}", source: example only}
  lower: {value: 0.001, source: example only}
  below_template: {value: "P <{lower}", source: example only}
  upper: {value: 0.999, source: example only}
  above_template: {value: "P >{upper}", source: example only}
thousands:
  mark: {value: ",", source: example only}
  from: {value: 10000, source: example only}
missing:
  show: {value: row_if_any, source: example only}
  template: {value: "; {missing} missing", source: example only}
  row_template: {value: "{missing} ({pct})", source: example only}
  row_pct_digits: {value: 1, source: example only}
  row_label: {value: "Missing", source: example only}
footnotes:
  - {value: "Values are n (percent) or median (quartiles).", source: example only}
```

The fake data below have 12,000 rows in two groups. `education` is a
`"cat"` variable with three levels and 60 missing values. `wide()` puts
one printed row on each row, with the column `label` of the output, and
one group in each column.

``` r
set.seed(1)
n <- 12000
d <- data.table::data.table(
  group = rep(c("Control", "Exposed"), times = c(9000, 3000)),
  smoker = stats::rbinom(n, 1, 0.15),
  age = round(stats::rnorm(n, mean = 31, sd = 5), 1),
  education = factor(
    sample(c("Primary", "Secondary", "Tertiary"), n, replace = TRUE),
    levels = c("Primary", "Secondary", "Tertiary")
  )
)
d$smoker[sample(n, 40)] <- NA
d$education[sample(n, 60)] <- NA
vars <- data.table::data.table(
  var = c("smoker", "age"),
  type = c("bin", "cont")
)
vars_cat <- rbind(vars, data.table::data.table(var = "education", type = "cat"))

# One printed row for each "value", "heading" and "missing" row of the "All"
# group, and the groups in the order of t1.
wide <- function(t1) {
  all <- t1$grp == "All"
  out <- data.frame(row = t1$label[all])
  for (g in unique(t1$grp)) {
    out[[g]] <- t1$value_pretty[t1$grp == g]
  }
  out$p <- t1$p_pretty[all]
  return(out)
}

t1 <- pbn::table1(
  d,
  by = "group",
  vars = vars_cat,
  style = file.path(tempdir(), "example_2026_12_01.yml")
)
knitr::kable(wide(t1))
```

| row          | All              | Control          | Exposed          | p        |
|:-------------|:-----------------|:-----------------|:-----------------|:---------|
| smoker       | 1874 (15.7)      | 1409 (15.7)      | 465 (15.6)       | P =0.839 |
| Missing      | 40 (0.3)         | 30 (0.3)         | 10 (0.3)         |          |
| age          | 30.9 (27.6-34.3) | 30.9 (27.6-34.3) | 31.1 (27.7-34.5) |          |
| education    |                  |                  |                  | P =0.576 |
| \- Primary   | 3965 (33.2)      | 2982 (33.3)      | 983 (32.9)       |          |
| \- Secondary | 3963 (33.2)      | 2948 (32.9)      | 1015 (34.0)      |          |
| \- Tertiary  | 4012 (33.6)      | 3022 (33.8)      | 990 (33.1)       |          |
| Missing      | 60 (0.5)         | 48 (0.5)         | 12 (0.4)         |          |

``` r
attr(t1, "footnotes")
#> [1] "Values are n (percent) or median (quartiles)."
```

The level rows of `education` divide by the non-missing count of the
group. Its Missing row divides by every row of the group.

## How a style is proven

pbn installs a style only with the real published Table 1s whose format
it reproduces. Beside each style file `inst/table1/<name>.yml` is the
folder `inst/table1/<name>/`. For each reference table k, the folder
holds five files:

- `reference_<k>.csv`: the real table as printed, one string per cell.
- `source_<k>.yml`: the authors, the journal, the DOI, the table number,
  and who transcribed the table from which file.
- `fake_<k>.csv`: fake data, one row per fake person. They MUST give
  every printed row and column, and need not give the printed numbers.
- `vars_<k>.csv`: the arguments `by` and `vars` of
  [`table1()`](https://www.rwhite.no/pbn/reference/table1.md), with the
  columns `label` and `section` of `vars`.
- `layout_<k>.yml`: the printed row and column of each cell of the
  output. It holds no label.

A test of pbn runs `table1(fake, by, vars, style)` for each reference
table. It compares every printed cell with the cell of the style, the
row labels included. A label MUST be the same string. A value cell MUST
have the same shape, which is the string with each number masked:

- The digits before a decimal point collapse to one `N`. A thousands
  separator stays, so `12,347` gives `N,N`.
- Each digit after a decimal point becomes its own `N`, so `0.272` gives
  `N.NNN`.
- Every other character stays as printed.

So `397 (27.0%)` and `5 (2.8%)` share the shape `N (N.N%)`, but
`57 (76%)` gives `N (N%)`. A style controls the format, and the numbers
come from the data.

Each label is the column `label` of the output of
[`table1()`](https://www.rwhite.no/pbn/reference/table1.md), and never
comes from the layout or the reference. A printed row with no computed
counterpart MUST be an exception with the rule `not_computable`. A cell
that differs MUST have an entry in the file `exceptions.yml` of the
folder. The entry gives the two strings, a rule, a source and a
justification. The rule is one of these:

- `guideline`: the reference departs from the author guidelines, and the
  style follows them. The source gives the page.
- `other_reference`: two references disagree, and the style follows the
  other one. The source cites its cell.
- `not_computable`:
  [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) cannot
  compute the cell.
- `published_error`: the format of the printed cell is wrong, such as a
  median printed without its value. A wrong number in the right format
  has the shape of the style, so it needs no exception.

The test fails a difference that `exceptions.yml` does not list. It
fails an exception whose strings no longer match those of its cell by
the same rule. It fails an exception of a cell that now passes, such as
one whose two shapes are now equal. It also fails a style without its
folder or with a missing file. A field whose `source` says “editorial”,
or cites neither a reference table nor a guideline page, fails too. The
section Styles of
[`?table1`](https://www.rwhite.no/pbn/reference/table1.md) gives every
field of every file.

### Withdrawn styles

pbn withdraws a style that it cannot prove. pbn removes its file, and
[`table1()`](https://www.rwhite.no/pbn/reference/table1.md) stops on its
name with the version that withdrew it and the reason.
[`table1_styles()`](https://www.rwhite.no/pbn/reference/table1_styles.md)
does not list it. These are the withdrawn styles:

| style | withdrawn_in | reason |
|:---|:---|:---|
| ijoa_brief_report_2026_11_20 | 2026.12.4 | it has no reference tables, and 7 of its 18 field sources are an editorial choice |

## The installed styles

The tables below show `smoker` and `age` only. A style without the
section `cat` cannot format a `"cat"` variable.

pbn 2026.12.7 installs no style.
