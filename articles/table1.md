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
where the value comes from. It is the guide of a journal and its count
of papers, the guideline and its page, or `editorial choice`.

## The schema

This style file has every field. The section Styles of
[`?table1`](https://www.rwhite.no/pbn/reference/table1.md) gives the
type and the check of each field. The fields `cont.median_digits`,
`missing.row_template` and `missing.row_pct_digits`, and the section
`cat`, are optional.

``` yaml
bin:
  template: {value: "{events} ({pct})", source: editorial choice}
  pct_digits: {value: 1, source: editorial choice}
cont:
  template: {value: "{median} ({q1}-{q3})", source: editorial choice}
  mean_digits: {value: 1, source: editorial choice}
  sd_digits: {value: 1, source: editorial choice}
  median_digits: {value: 1, source: editorial choice}
cat:
  template: {value: "{events} ({pct})", source: editorial choice}
  pct_digits: {value: 1, source: editorial choice}
  heading_template: {value: "", source: editorial choice}
p:
  digits: {value: 3, source: editorial choice}
  template: {value: "P ={p}", source: editorial choice}
  lower: {value: 0.001, source: editorial choice}
  below_template: {value: "P <{lower}", source: editorial choice}
  upper: {value: 0.999, source: editorial choice}
  above_template: {value: "P >{upper}", source: editorial choice}
thousands:
  mark: {value: ",", source: editorial choice}
  from: {value: 10000, source: editorial choice}
missing:
  show: {value: row_if_any, source: editorial choice}
  template: {value: "; {missing} missing", source: editorial choice}
  row_template: {value: "{missing} ({pct})", source: editorial choice}
  row_pct_digits: {value: 1, source: editorial choice}
footnotes:
  - {value: "Values are n (percent) or median (quartiles).", source: editorial choice}
```

The fake data below have 12,000 rows in two groups. `education` is a
`"cat"` variable with three levels and 60 missing values. `wide()` puts
one printed row on each row and one group in each column.

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
  label <- t1$var
  label[!is.na(t1$level)] <- paste("-", t1$level[!is.na(t1$level)])
  label[t1$row == "missing"] <- "- Missing"
  out <- data.frame(row = label[all])
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
| \- Missing   | 40 (0.3)         | 30 (0.3)         | 10 (0.3)         |          |
| age          | 30.9 (27.6-34.3) | 30.9 (27.6-34.3) | 31.1 (27.7-34.5) |          |
| education    |                  |                  |                  | P =0.576 |
| \- Primary   | 3965 (33.2)      | 2982 (33.3)      | 983 (32.9)       |          |
| \- Secondary | 3963 (33.2)      | 2948 (32.9)      | 1015 (34.0)      |          |
| \- Tertiary  | 4012 (33.6)      | 3022 (33.8)      | 990 (33.1)       |          |
| \- Missing   | 60 (0.5)         | 48 (0.5)         | 12 (0.4)         |          |

``` r
attr(t1, "footnotes")
#> [1] "Values are n (percent) or median (quartiles)."
```

The level rows of `education` divide by the non-missing count of the
group. Its Missing row divides by every row of the group.

## The installed styles

The tables below show `smoker` and `age` only. A style without the
section `cat` cannot format a `"cat"` variable.

### ijoa_brief_report_2026_11_20

| row | All | Control | Exposed | p |
|:---|:---|:---|:---|:---|
| smoker | 1874 (15.7%); 40 missing | 1409 (15.7%); 30 missing | 465 (15.6%); 10 missing | P =0.839 |
| age | 31.0 (5.0) | 30.9 (5.0) | 31.1 (5.0) |  |

Values are n (%) or mean (SD).

SD, standard deviation.

P values compare the groups with Pearson’s chi-square test, or with
Fisher’s exact test when an expected count is below 5.
