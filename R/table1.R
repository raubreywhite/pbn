#' Compute the numbers of a descriptive table
#'
#' `table1()` computes the numbers of a descriptive table, per group and for
#' all rows. With a style, it also formats these numbers as text.
#'
#' @param d A data.table, or a data.frame.
#' @param by The name of the grouping column in `d`.
#' @param vars A data.table with the columns `var` and `type`. `var` names a
#'   column in `d`. `type` is `"bin"` for a 0/1 variable, `"cont"` for a
#'   numeric variable, or `"cat"` for a categorical variable. Two text
#'   columns are optional. `label` is the printed label of the variable, and
#'   `NA` gives its name. `section` names the section that the variable
#'   belongs to, and `NA` or `""` gives none.
#' @param style `NULL`, the name of an installed style, or the path to a style
#'   file that ends in `.yml`. [table1_styles()] gives the names of the
#'   installed styles. With `NULL`, `table1()` does no formatting. The section
#'   Styles below describes a style file.
#' @details
#' The groups are the levels of `by`, plus `"All"`. The levels are the factor
#' levels when `by` is a factor, and the sorted unique values otherwise. A row
#' with a missing `by` counts in `"All"` only. A level of `by` that is `"All"`
#' is an error. A level with no rows has a row in the output with `n = 0`.
#'
#' For a `"bin"` variable:
#' - `events` is the count of 1, and `pct` is `100 * events / n`.
#' - `p` compares the levels of `by` that have a non-missing value. It comes
#'   from the Pearson chi-square test without continuity correction. When
#'   any expected count is below 5, it comes from Fisher's exact test.
#' - `test` is `"chisq"` or `"fisher"`, and it is the same on every row of the
#'   variable. When fewer than two levels have a non-missing value, `p` and
#'   `test` are `NA`.
#' - A value other than 0, 1 or `NA` is an error.
#'
#' For a `"cont"` variable, `mean`, `sd`, `median`, `q1` and `q3` hold the
#' numbers of the non-missing values. `q1` and `q3` are the first and the
#' third quartile. The median and the quartiles are `stats::quantile()` with
#' `type = 7`, the default of R. `p` and `test` are `NA` in this version.
#'
#' A `"cat"` variable has one row for each level and group. The rows of a
#' level are together, in the order of the groups.
#' - The levels are the factor levels when the variable is a factor.
#'   Otherwise they are the sorted unique values that are not `NA`, so a
#'   number sorts as a number. `level` holds each level as text.
#' - `events` is the count of the level, and `n` is the count of the
#'   non-missing values of the group. So `pct` is `100 * events / n`, and the
#'   percentages of a group with `n` above 0 add up to 100.
#' - `p` and `test` follow the rule of a `"bin"` variable, with the levels in
#'   place of 0 and 1. A level with a count of 0 in each group is not part of
#'   the test. When fewer than two levels or fewer than two groups remain,
#'   `p` and `test` are `NA`.
#' - A variable that has no level, because each value is `NA` and it is not
#'   a factor, has one row per group. Its `level`, `events` and `pct` are
#'   `NA`. With a style it gets no level row, only its heading row and the
#'   Missing row that `missing.show` asks for.
#'
#' A column that does not apply to the type holds `NA`.
#' @section Styles:
#' A style formats the numbers of `table1()` for a journal or a report. A
#' style can only format what `table1()` computes. It cannot add a test of a
#' `"cont"` variable.
#'
#' An installed style is the file `inst/table1/<name>_<YYYY_MM_DD>.yml` of
#' pbn. Its name is the file name without `.yml`. pbn never edits a released
#' style, so a changed style gets a new date.
#'
#' A style file is a YAML map of sections. Each field of a section is a map
#' with the keys `value` and `source`. `source` is one string that says where
#' the value comes from. `table1()` takes any `source` that is not empty. The
#' source of an installed style MUST cite a reference table or a guideline
#' page, as the subsection on the proof of a style says.
#' `table1()` checks every field when it reads the file. An unknown field, a
#' missing field, a missing `source` or a value that the field does not allow
#' stops it. The error names the field and the file. The fields that pbn
#' 2026.12.3 added are optional, so a style file of an earlier pbn stays
#' valid. They are `cont.median_digits`, the section `cat`, and the fields
#' `missing.row_template` and `missing.row_pct_digits`. pbn 2026.12.4 added
#' the optional fields `cat.level_prefix` and `missing.row_label`. An
#' optional field that is present gets the same checks as the other fields.
#'
#' A template is text with placeholders in braces, such as `{events}`. A
#' template can hold only the placeholders of its field, in any order, and
#' it can hold none. These are the fields:
#'
#' - `bin.template`: the cell of a `"bin"` row, over `{events}`, `{n}` and
#'   `{pct}`. An example is `"{events} ({pct})"`.
#' - `bin.pct_digits`: the decimals of `pct`, a whole number from 0 to 10.
#' - `cont.template`: the cell of a `"cont"` row, over `{mean}`, `{sd}`,
#'   `{median}`, `{q1}` and `{q3}`. An example is `"{median} ({q1}-{q3})"`.
#'   A template that holds `{median}`, `{q1}` or `{q3}` needs
#'   `cont.median_digits`.
#' - `cont.mean_digits` and `cont.sd_digits`: the decimals of `mean` and of
#'   `sd`, each a whole number from 0 to 10.
#' - `cont.median_digits` (optional): the decimals of `median`, `q1` and
#'   `q3`, a whole number from 0 to 10.
#' - `cat` (an optional section): it MUST be present when `vars` has a
#'   `"cat"` variable. When it is present, it MUST hold the three fields
#'   below that are not optional.
#' - `cat.template`: the cell of a level row of a `"cat"` variable, over
#'   `{events}`, `{n}` and `{pct}`, as `bin.template`.
#' - `cat.pct_digits`: the decimals of `pct` in a level row, a whole number
#'   from 0 to 10.
#' - `cat.heading_template`: the cell of the heading row of a `"cat"`
#'   variable. It holds no placeholder. `""` gives an empty cell.
#' - `cat.level_prefix` (optional): the text before the level in the column
#'   `label` of a level row, such as `"- "`.
#' - `p.digits`: the decimals of `p`, a whole number from 0 to 10.
#' - `p.template`: the text of `p`, over `{p}`. `"P ={p}"` gives `P =0.005`,
#'   and `"{p}"` gives the number only.
#' - `p.lower`: `null`, or a number above 0 and below 1. A `p` below it gets
#'   `p.below_template` in place of `p.template`.
#' - `p.below_template`: the text of such a `p`, over `{lower}`. With
#'   `p.lower` 0.001, `"P <{lower}"` gives `P <0.001`.
#' - `p.upper`: `null`, or a number above 0 and below 1. A `p` above it gets
#'   `p.above_template`.
#' - `p.above_template`: the text of such a `p`, over `{upper}`. With
#'   `p.upper` 0.999, `"P >{upper}"` gives `P >0.999`.
#' - `thousands.mark`: the thousands separator, such as `","`. The empty
#'   string `""` gives no separator.
#' - `thousands.from`: a number above 0. A number gets the separator when its
#'   rounded absolute value is `thousands.from` or more. With 10000, 12347
#'   prints as `12,347` and 9999 prints as `9999`.
#' - `missing.show`: `never`, `if_any`, `always`, `row_if_any` or
#'   `row_always`. With `if_any`, a cell gets `missing.template` when
#'   `missing` is above 0. With `always`, each cell that is not `NA` gets
#'   it. With `row_if_any`, a variable gets a Missing row when its `"All"`
#'   group has a missing value. With `row_always`, each variable gets a
#'   Missing row. The two row choices need `missing.row_template` and
#'   `missing.row_pct_digits`.
#' - `missing.template`: the text that `table1()` adds to the end of a cell,
#'   over `{missing}`. An example is `"; {missing} missing"`. Only `if_any`
#'   and `always` use it.
#' - `missing.row_template` (optional): the cell of a Missing row, over
#'   `{missing}` and `{pct}`. An example is `"{missing} ({pct}%)"`.
#' - `missing.row_pct_digits` (optional): the decimals of `pct` in a Missing
#'   row, a whole number from 0 to 10.
#' - `missing.row_label` (optional): the column `label` of a Missing row,
#'   such as `"Missing"`. Without it, the label is `NA`.
#' - `footnotes`: a list, which can be empty. Each item is a map with the keys
#'   `value` and `source`, and `value` is the text of one footnote.
#'
#' `p.lower` and `p.upper` print with `p.digits` decimals, so each MUST have
#' no more decimals than `p.digits`. `p.lower` MUST be below `p.upper`.
#' `table1()` compares `p` with the thresholds before it rounds `p`, so 0.9994
#' is above 0.999. A `p` equal to a threshold gets `p.template`.
#'
#' A style can add rows, and the column `row` names the kind of each row:
#' - `"value"`: a row of the output without a style. A level row of a
#'   `"cat"` variable has `p_pretty` `NA`.
#' - `"heading"`: one row for each group before the level rows of a `"cat"`
#'   variable. Its cell is `cat.heading_template`, and its `p_pretty` is the
#'   P of the variable.
#' - `"missing"`: one row for each group after the other rows of a variable,
#'   when `missing.show` asks for it. Its cell gives `missing` and its
#'   percent, and its column `pct` holds that percent,
#'   `100 * missing / (n + missing)`. The denominator counts every row of the
#'   group, missing ones included. Its `p_pretty` is `NA`.
#' - `"section"`: one row for each group before the rows of the first
#'   variable of each section, when `vars` has the column `section`. Its
#'   `grp` is the group and its `label` is the section. Each other column is
#'   `NA`.
#'
#' The column `label` of a heading row is the label of the variable. A level
#' row has `cat.level_prefix` and the level, and a Missing row has
#' `missing.row_label`.
#'
#' A heading row and a Missing row keep `var`, `grp`, `n`, `missing`, `p` and
#' `test` of the group. Their `level` and their other numbers are `NA`, except
#' `pct` of a Missing row. A table with no `"cat"` variable, no Missing row
#' and no section keeps the rows of the output without a style.
#'
#' `table1()` rounds each number with `formatC()`, which rounds as the C
#' function `sprintf()` does. `events`, `n` and `missing` print with no
#' decimals. The thousands separator applies to every number of a cell. A
#' cell is `NA` when a number of its template is `NA`, as for a group with
#' `n = 0`. `p_pretty` is `NA` where `p` is `NA`.
#'
#' ## The proof of a style
#'
#' pbn installs a style only with the real published Table 1s that prove it.
#' Beside the file `inst/table1/<name>.yml` is the folder
#' `inst/table1/<name>/`. For each reference table k, it holds five files:
#' - `reference_<k>.csv`: the table as printed. It has one row per printed
#'   row and one column per printed column. The header holds the printed
#'   column headers, and the first column holds the printed row labels. Each
#'   cell is the printed string, and an empty cell is `""`.
#' - `source_<k>.yml`: the fields `authors`, `title`, `journal`, `year`,
#'   `volume`, `pages`, `doi`, `table` and `transcribed`. `authors` is a list
#'   of maps with the keys `family` and `given`. `pages` is the article
#'   number or the pages, and `table` is the number of the table.
#'   `transcribed` is a map with the keys `file`, `by` and `date`.
#' - `fake_<k>.csv`: the fake data, one row per fake person. An empty cell or
#'   `NA` is a missing value.
#' - `vars_<k>.csv`: the columns `var`, `type`, `label`, `levels` and
#'   `section`. The one row of type `by` names the column of the groups. The
#'   other rows, with `var`, `type`, `label` and `section`, are the argument
#'   `vars`. `levels` gives the order of the groups, or of the levels of a
#'   `"cat"` variable, separated by `|`. `levels` and `section` can be
#'   empty.
#' - `layout_<k>.yml`: the map of the output onto the printed table. Each
#'   item of `columns` names a printed column, except the first, in order. It
#'   has `cell: value` and a `grp`, or `cell: p`. Each item of `rows` names a
#'   printed row, in order. It has the key `row` and, where it applies, `var`
#'   and `level`, and no other key. `row` is `value`, `heading` or `missing`
#'   as in the column `row` of the output. It is `section` for a section
#'   heading, and `none` for a printed row with no computed counterpart.
#'
#' The test of pbn runs `table1(fake, by, vars, style)` for each reference
#' table. It compares each printed cell with the cell of the style, the row
#' labels of the first column included. The headers are not cells. The cell
#' of the style is `value_pretty` of the group, or `p_pretty` for `cell: p`,
#' and `NA` is `""`. The label of the style is the column `label` of the
#' same row of the output. No label comes from the layout or the reference.
#' A printed row of `section` takes the label of the section row of the
#' output. That row MUST come right before the first row of the next
#' variable of the layout, and the label is `""` without it. A row of
#' `section` has `""` in each cell after the label. A row of `none` has `""`
#' in each cell. So each printed cell of it that is not empty MUST be an
#' exception with the rule `not_computable`.
#'
#' A cell passes when the two strings are identical. A cell that
#' differs passes only when the file `exceptions.yml` of the folder lists it
#' with the same two strings.
#'
#' `exceptions.yml` is a map with the one key `exceptions`, a list. Each item
#' has these fields:
#' - `reference`, `row` and `column`: the cell, as `reference_1`, the number
#'   of the printed row after the header, and the printed header.
#' - `printed` and `style`: the printed string and the string of the style.
#' - `rule`, `source` and `justification`: why the two differ. `source` and
#'   `justification` MUST NOT be empty.
#'
#' `rule` is one of these:
#' - `guideline`: the reference departs from the author guidelines, and the
#'   style follows them. `source` names the guideline and its page.
#' - `other_reference`: two references disagree, and the style follows the
#'   other one. `source` cites a cell of the other reference.
#' - `not_computable`: `table1()` cannot compute the cell, such as a P of a
#'   `"cont"` variable.
#' - `published_error`: the printed cell is wrong. `source` gives the
#'   computation that shows it.
#'
#' The test fails a style in each of these cases:
#' - A cell differs, and `exceptions.yml` does not list it.
#' - An exception has strings other than those of its cell. A cell that
#'   matches MUST NOT have an exception.
#' - An exception names no cell, or a cell twice.
#' - The style has no folder. The folder lacks a file, or it holds a file of
#'   another name.
#' - The `source` of a field holds "editorial", in any case.
#' - The `source` of a field cites none of the three forms of a citation:
#'   1. a cell, as `reference_1, row 2, column "P"`.
#'   2. the family name of the first author of a reference, with
#'      `Table <n>` of its source.
#'   3. a guide or a guideline with its page, as `p. 4` or `page 4`.
#'
#' A style that pbn cannot prove is withdrawn. pbn removes its file and keeps
#' its name, so `table1()` stops with the version that withdrew it and the
#' reason. `table1_styles()` does not list it. pbn 2026.12.4 withdrew
#' `ijoa_brief_report_2026_11_20`, because it was not proven against real
#' published tables.
#' @return A data.table with one row per variable and group, in the order of
#'   `vars`, and `"All"` first. A `"cat"` variable has one row per level and
#'   group. The columns are `var`, `grp`, `n`, `missing`, `mean`, `sd`,
#'   `events`, `pct`, `p`, `test`, `level`, `median`, `q1` and `q3`. `n`
#'   counts the non-missing values. pbn 2026.12.3 added the last four
#'   columns. The column `label` follows, which pbn 2026.12.4 added: the
#'   label of the variable, or the level of a level row. With a style, the columns `row`, `value_pretty` (the cell text)
#'   and `p_pretty` (the text of `p`) follow. The attribute `footnotes` then
#'   holds the footnotes of the style as a character vector. A style can add
#'   rows, as the section Styles says. The other columns of each row of the
#'   output without a style stay the same.
#' @examples
#' d <- data.table::data.table(
#'   g = rep(c("a", "b"), each = 10),
#'   smoker = rep(c(1, 0, 0, 1), 5),
#'   age = 21:40
#' )
#' vars <- data.table::data.table(
#'   var = c("smoker", "age"),
#'   type = c("bin", "cont")
#' )
#' pbn::table1(d, by = "g", vars = vars)
#'
#' # A style file, written to tempdir()
#' style <- file.path(tempdir(), "example_2026_12_01.yml")
#' writeLines(c(
#'   "bin:",
#'   "  template: {value: '{events} ({pct})', source: example only}",
#'   "  pct_digits: {value: 1, source: example only}",
#'   "cont:",
#'   "  template: {value: '{mean} ({sd})', source: example only}",
#'   "  mean_digits: {value: 1, source: example only}",
#'   "  sd_digits: {value: 1, source: example only}",
#'   "p:",
#'   "  digits: {value: 3, source: example only}",
#'   "  template: {value: 'P ={p}', source: example only}",
#'   "  lower: {value: 0.001, source: example only}",
#'   "  below_template: {value: 'P <{lower}', source: example only}",
#'   "  upper: {value: 0.999, source: example only}",
#'   "  above_template: {value: 'P >{upper}', source: example only}",
#'   "thousands:",
#'   "  mark: {value: ',', source: example only}",
#'   "  from: {value: 10000, source: example only}",
#'   "missing:",
#'   "  show: {value: if_any, source: example only}",
#'   "  template: {value: '; {missing} missing', source: example only}",
#'   "footnotes:",
#'   "  - {value: 'Values are n (percent) or mean (SD).', source: example only}"
#' ), style)
#' t1 <- pbn::table1(d, by = "g", vars = vars, style = style)
#' t1[, c("var", "grp", "value_pretty", "p_pretty")]
#' attr(t1, "footnotes")
#' @family shared helpers
#' @export
table1 <- function(d, by, vars, style = NULL) {
  check_table1_args(d, by, vars)
  if (!is.null(style)) {
    style <- read_table1_style(style)
    check_style_types(style, vars)
  }
  grp <- d[[by]]
  levels_by <- if (is.factor(grp)) {
    levels(grp)
  } else {
    sort(unique(as.character(grp[!is.na(grp)])))
  }
  if ("All" %in% levels_by) {
    stop(
      "A level of `by` is \"All\". table1() uses \"All\" for every row, ",
      "so rename that level.",
      call. = FALSE
    )
  }
  grp <- as.character(grp)
  labels <- var_labels(vars)
  rows <- lapply(seq_len(nrow(vars)), function(i) {
    out <- table1_one_var(
      x = d[[vars$var[i]]],
      var = vars$var[i],
      type = vars$type[i],
      grp = grp,
      levels_by = levels_by
    )
    label <- rep(labels[i], nrow(out))
    label[!is.na(out$level)] <- out$level[!is.na(out$level)]
    data.table::set(out, j = "label", value = label)
    return(out)
  })
  out <- data.table::rbindlist(rows)
  if (is.null(style)) {
    return(out)
  }
  n_rows <- vapply(rows, nrow, integer(1))
  return(format_table1(
    out,
    type = rep(vars$type, n_rows),
    index = rep(seq_along(rows), n_rows),
    n_groups = length(levels_by) + 1L,
    style = style,
    labels = labels,
    sections = var_sections(vars)
  ))
}

# The label of each variable of `vars`: its `label`, or its name where vars
# has no column label or the label is NA.
var_labels <- function(vars) {
  out <- if (is.null(vars[["label"]])) vars$var else vars$label
  out[is.na(out)] <- vars$var[is.na(out)]
  return(as.character(out))
}

# The section of each variable of `vars`, and "" for no section.
var_sections <- function(vars) {
  out <- if (is.null(vars[["section"]])) rep("", nrow(vars)) else vars$section
  out[is.na(out)] <- ""
  return(as.character(out))
}

# The columns of table1(), in their order. pbn 2026.12.3 added the last four.
table1_columns <- c(
  "var",
  "grp",
  "n",
  "missing",
  "mean",
  "sd",
  "events",
  "pct",
  "p",
  "test",
  "level",
  "median",
  "q1",
  "q3"
)

check_table1_args <- function(d, by, vars) {
  if (!is.data.frame(d)) {
    stop("`d` must be a data.table or a data.frame.", call. = FALSE)
  }
  if (!is.character(by) || length(by) != 1 || !by %in% names(d)) {
    stop("`by` must be the name of one column in `d`.", call. = FALSE)
  }
  if (!is.data.frame(vars) || !all(c("var", "type") %in% names(vars))) {
    stop(
      "`vars` must be a data.table with the columns var and type.",
      call. = FALSE
    )
  }
  absent <- setdiff(vars$var, names(d))
  if (length(absent) > 0) {
    stop(
      "These variables in `vars` are not columns in `d`: ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  bad_type <- vars$var[!vars$type %in% c("bin", "cont", "cat")]
  if (length(bad_type) > 0) {
    stop(
      "The type must be \"bin\", \"cont\" or \"cat\". It is not for: ",
      paste(bad_type, collapse = ", "),
      call. = FALSE
    )
  }
  check_vars_text(vars)
  return(invisible(NULL))
}

# The optional columns label and section of `vars` are text.
check_vars_text <- function(vars) {
  for (column in intersect(c("label", "section"), names(vars))) {
    if (!is.character(vars[[column]])) {
      stop(
        "The column ",
        column,
        " of `vars` must be text, with NA where it does not apply.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

table1_one_var <- function(x, var, type, grp, levels_by) {
  if (type == "cat") {
    return(table1_cat_var(x, var, grp, levels_by))
  }
  if (!is.numeric(x)) {
    stop("Variable '", var, "' is not numeric.", call. = FALSE)
  }
  if (type == "bin") {
    bad <- !is.na(x) & !x %in% c(0, 1)
    if (any(bad)) {
      stop(
        "Variable '",
        var,
        "' has type \"bin\" but holds values other than 0, 1 and NA: ",
        paste(utils::head(unique(x[bad]), 5), collapse = ", "),
        call. = FALSE
      )
    }
    tested <- bin_test(x, grp, levels_by)
  } else {
    tested <- list(p = NA_real_, test = NA_character_)
  }
  groups <- c("All", levels_by)
  parts <- lapply(groups, function(g) {
    return(summarise_one(group_values(x, grp, g), type))
  })
  out <- data.table::rbindlist(parts)
  out <- cbind(
    data.table::data.table(var = var, grp = groups),
    out,
    data.table::data.table(
      p = tested$p,
      test = tested$test,
      level = NA_character_
    )
  )
  data.table::setcolorder(out, table1_columns)
  return(out)
}

# The values of `x` in the group `g` of `grp`. "All" is every row.
group_values <- function(x, grp, g) {
  if (g == "All") {
    return(x)
  }
  return(x[!is.na(grp) & grp == g])
}

# The levels of a "cat" variable: the factor levels, or the sorted unique
# values that are not NA.
cat_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  }
  return(as.character(sort(unique(x[!is.na(x)]))))
}

# One row per level and group of a "cat" variable, the levels in their order
# and the groups in their order within each level.
table1_cat_var <- function(x, var, grp, levels_by) {
  levels_x <- cat_levels(x)
  x <- as.character(x)
  tested <- cat_test(x, grp, levels_by, levels_x)
  groups <- c("All", levels_by)
  counts <- lapply(groups, function(g) {
    xi <- group_values(x, grp, g)
    return(list(
      n = sum(!is.na(xi)),
      missing = sum(is.na(xi)),
      events = vapply(
        levels_x,
        function(l) return(sum(xi %in% l)),
        integer(1),
        USE.NAMES = FALSE
      )
    ))
  })
  # A variable with no level gets one row per group, with level NA, so that
  # a style can still give it a Missing row.
  shown <- if (length(levels_x) == 0) NA_character_ else levels_x
  size <- length(shown)
  n <- rep(vapply(counts, function(k) return(k$n), integer(1)), size)
  events <- unlist(lapply(seq_len(size), function(i) {
    return(vapply(counts, function(k) return(k$events[i]), integer(1)))
  }))
  pct <- 100 * events / n
  pct[n == 0] <- NA_real_
  m <- length(n)
  return(data.table::data.table(
    var = rep(var, m),
    grp = rep(groups, size),
    n = n,
    missing = rep(
      vapply(counts, function(k) return(k$missing), integer(1)),
      size
    ),
    mean = rep(NA_real_, m),
    sd = rep(NA_real_, m),
    events = as.integer(events),
    pct = pct,
    p = rep(tested$p, m),
    test = rep(tested$test, m),
    level = rep(shown, each = length(groups)),
    median = rep(NA_real_, m),
    q1 = rep(NA_real_, m),
    q3 = rep(NA_real_, m)
  ))
}

summarise_one <- function(x, type) {
  n <- sum(!is.na(x))
  out <- list(
    n = n,
    missing = sum(is.na(x)),
    mean = NA_real_,
    sd = NA_real_,
    events = NA_integer_,
    pct = NA_real_,
    median = NA_real_,
    q1 = NA_real_,
    q3 = NA_real_
  )
  if (type == "bin") {
    out$events <- sum(x == 1, na.rm = TRUE)
    out$pct <- if (n > 0) 100 * out$events / n else NA_real_
  } else if (n > 0) {
    out$mean <- mean(x, na.rm = TRUE)
    out$sd <- stats::sd(x, na.rm = TRUE)
    q <- stats::quantile(
      x,
      c(0.25, 0.5, 0.75),
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
    out$q1 <- q[1]
    out$median <- q[2]
    out$q3 <- q[3]
  }
  return(out)
}

# Pearson chi-square without continuity correction, or Fisher's exact test
# when any expected count is below 5. This is the rule SPSS uses. A level
# with no row in the table is not a group of the test, so an unused factor
# level does not change the test.
bin_test <- function(x, grp, levels_by) {
  keep <- !is.na(grp)
  tab <- table(
    factor(grp[keep], levels = levels_by),
    factor(x[keep], levels = c(0, 1))
  )
  return(contingency_test(tab[rowSums(tab) > 0, , drop = FALSE]))
}

# The test of a "cat" variable over its levels by the groups. A level with no
# row is not a column of the test, as a group with no row is not a row of it.
cat_test <- function(x, grp, levels_by, levels_x) {
  keep <- !is.na(grp)
  tab <- table(
    factor(grp[keep], levels = levels_by),
    factor(x[keep], levels = levels_x)
  )
  return(contingency_test(tab[
    rowSums(tab) > 0,
    colSums(tab) > 0,
    drop = FALSE
  ]))
}

# The rule of bin_test() for a table of groups by values. With fewer than two
# rows or two columns there is no test.
contingency_test <- function(tab) {
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(list(p = NA_real_, test = NA_character_))
  }
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  if (any(expected < 5)) {
    return(list(p = stats::fisher.test(tab)$p.value, test = "fisher"))
  }
  return(list(
    p = stats::chisq.test(tab, correct = FALSE)$p.value,
    test = "chisq"
  ))
}

# styles ----------------------------------------------------------------------

#' List the installed Table 1 styles
#'
#' `table1_styles()` gives the names of the Table 1 styles that pbn installs.
#' Give one of these names to the argument `style` of [table1()].
#'
#' @return A character vector of style names, such as `"name_2026_12_01"`,
#'   sorted in the C locale. It is `character(0)` when pbn installs no style.
#'   It never holds a withdrawn style.
#' @examples
#' pbn::table1_styles()
#' @family shared helpers
#' @export
table1_styles <- function() {
  return(setdiff(style_names(table1_style_dir()), names(withdrawn_styles)))
}

# The withdrawn styles: the pbn version that withdrew each one, and why. A
# withdrawn name stays here, so that a call that names it stops with the
# reason. table1_styles() never lists it.
withdrawn_styles <- list(
  ijoa_brief_report_2026_11_20 = list(
    version = "2026.12.4",
    reason = paste(
      "it has no reference tables, and 7 of its 18 field sources are an",
      "editorial choice"
    )
  )
)

# The error for the withdrawn style `style`.
stop_withdrawn <- function(style) {
  w <- withdrawn_styles[[style]]
  stop(
    "pbn ",
    w$version,
    " withdrew the Table 1 style '",
    style,
    "', because it was not proven against real published tables: ",
    w$reason,
    ". table1_styles() gives the installed styles.",
    call. = FALSE
  )
}

# The folder of the installed styles, or "" when pbn installs none.
table1_style_dir <- function() {
  return(system.file("table1", package = "pbn"))
}

style_file_pattern <- "^[a-z][a-z0-9_]*_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.yml$"

# The names of the style files in `dir`, without .yml.
style_names <- function(dir) {
  if (!nzchar(dir)) {
    return(character(0))
  }
  files <- list.files(dir, pattern = style_file_pattern)
  return(sort(sub("\\.yml$", "", files), method = "radix"))
}

# The one loader of a Table 1 style. `style` is the name of an installed
# style, or the path to a style file that ends in .yml. It returns the fields
# of the file, after check_style(), with the attribute `file`.
read_table1_style <- function(style) {
  if (!is.character(style) || length(style) != 1 || is.na(style)) {
    stop(
      "`style` must be one string: the name of a style or the path to a ",
      "style file.",
      call. = FALSE
    )
  }
  file <- style_file(style)
  fields <- tryCatch(
    read_yaml_file(file),
    error = function(e) {
      stop(
        "pbn cannot read the style file ",
        file,
        " as YAML: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  check_style(fields, file)
  return(structure(fields, file = file))
}

# The path of the style `style`, or an error that names it.
style_file <- function(style) {
  if (grepl("\\.yml$", style)) {
    if (!file.exists(style)) {
      stop("The style file ", style, " does not exist.", call. = FALSE)
    }
    return(style)
  }
  if (style %in% names(withdrawn_styles)) {
    stop_withdrawn(style)
  }
  installed <- table1_styles()
  if (!style %in% installed) {
    known <- if (length(installed) == 0) {
      "pbn installs no style."
    } else {
      paste0("The installed styles are: ", paste(installed, collapse = ", "))
    }
    stop(
      "pbn has no Table 1 style named '",
      style,
      "'. ",
      known,
      call. = FALSE
    )
  }
  return(file.path(table1_style_dir(), paste0(style, ".yml")))
}

# The fields of a style file, by section. Each field is a map with the keys
# value and source. `type` names the check of the value in style_checks.
# `keys` are the placeholders that a template can hold. A field with
# `optional = TRUE`, and a section of optional_sections, can be absent. These
# came in pbn 2026.12.3, so a style file of an earlier pbn stays valid.
style_schema <- list(
  bin = list(
    template = list(type = "template", keys = c("events", "n", "pct")),
    pct_digits = list(type = "digits")
  ),
  cont = list(
    template = list(
      type = "template",
      keys = c("mean", "sd", "median", "q1", "q3")
    ),
    mean_digits = list(type = "digits"),
    sd_digits = list(type = "digits"),
    median_digits = list(type = "digits", optional = TRUE)
  ),
  cat = list(
    template = list(type = "template", keys = c("events", "n", "pct")),
    pct_digits = list(type = "digits"),
    heading_template = list(type = "template", keys = character(0)),
    level_prefix = list(type = "string", optional = TRUE)
  ),
  p = list(
    digits = list(type = "digits"),
    template = list(type = "template", keys = "p"),
    lower = list(type = "threshold"),
    below_template = list(type = "template", keys = "lower"),
    upper = list(type = "threshold"),
    above_template = list(type = "template", keys = "upper")
  ),
  thousands = list(
    mark = list(type = "string"),
    from = list(type = "positive")
  ),
  missing = list(
    show = list(
      type = "choice",
      choices = c("never", "if_any", "always", "row_if_any", "row_always")
    ),
    template = list(type = "template", keys = "missing"),
    row_template = list(
      type = "template",
      keys = c("missing", "pct"),
      optional = TRUE
    ),
    row_pct_digits = list(type = "digits", optional = TRUE),
    row_label = list(type = "string", optional = TRUE)
  )
)

optional_sections <- "cat"

# The names of the placeholders in the template `x`, without the braces.
template_keys <- function(x) {
  used <- regmatches(x, gregexpr("\\{[^{}]*\\}", x))[[1]]
  return(gsub("[{}]", "", used))
}

is_style_string <- function(x) {
  return(is.character(x) && length(x) == 1 && !is.na(x) && !is_yaml_bool(x))
}

is_style_number <- function(x) {
  return(is.numeric(x) && length(x) == 1 && is.finite(x))
}

# The checks of a value, one for each type of style_schema. Each check
# returns NULL for a good value, and otherwise the end of the error message.
check_template_value <- function(x, spec) {
  if (!is_style_string(x)) {
    return("must be one quoted string.")
  }
  bad <- setdiff(template_keys(x), spec$keys)
  if (length(bad) == 0) {
    return(NULL)
  }
  can <- if (length(spec$keys) == 0) {
    "It can hold no placeholder."
  } else {
    paste0(
      "It can hold only ",
      paste0("{", spec$keys, "}", collapse = ", "),
      "."
    )
  }
  return(paste0("holds {", bad[1], "}. ", can))
}

check_digits_value <- function(x, spec) {
  if (is_style_number(x) && x == round(x) && x >= 0 && x <= 10) {
    return(NULL)
  }
  return("must be a whole number from 0 to 10.")
}

check_threshold_value <- function(x, spec) {
  if (is.null(x) || (is_style_number(x) && x > 0 && x < 1)) {
    return(NULL)
  }
  return("must be null, or a number above 0 and below 1.")
}

check_string_value <- function(x, spec) {
  if (is_style_string(x)) {
    return(NULL)
  }
  return("must be one quoted string.")
}

check_positive_value <- function(x, spec) {
  if (is_style_number(x) && x > 0) {
    return(NULL)
  }
  return("must be a number above 0.")
}

check_choice_value <- function(x, spec) {
  if (is_style_string(x) && x %in% spec$choices) {
    return(NULL)
  }
  return(paste0(
    "must be one of: ",
    paste(spec$choices, collapse = ", "),
    "."
  ))
}

style_checks <- list(
  template = check_template_value,
  digits = check_digits_value,
  threshold = check_threshold_value,
  string = check_string_value,
  positive = check_positive_value,
  choice = check_choice_value
)

field_error <- function(field, file, what) {
  stop(
    "The field ",
    field,
    " of the style file ",
    file,
    " ",
    what,
    call. = FALSE
  )
}

# An error for the first key of `x` that `known` does not hold, and for the
# first key of `required` that `x` lacks. `prefix` makes the full field name.
check_keys <- function(x, known, prefix, file, required = known) {
  if (!is.list(x) || (length(x) > 0 && is.null(names(x)))) {
    field_error(sub("\\.$", "", prefix), file, "must be a YAML map.")
  }
  unknown <- setdiff(names(x), known)
  if (length(unknown) > 0) {
    stop(
      "The style file ",
      file,
      " has an unknown field: ",
      prefix,
      unknown[1],
      ".",
      call. = FALSE
    )
  }
  absent <- setdiff(required, names(x))
  if (length(absent) > 0) {
    stop(
      "The style file ",
      file,
      " lacks the field ",
      prefix,
      absent[1],
      ".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# A field is a map with the keys value and source, and source is one string.
check_field <- function(x, field, file) {
  if (!is.list(x) || is.null(names(x))) {
    field_error(field, file, "must be a map with the keys value and source.")
  }
  check_keys(x, c("value", "source"), paste0(field, "."), file)
  if (!is_style_string(x$source) || !nzchar(trimws(x$source))) {
    field_error(field, file, "needs a source: one string that is not empty.")
  }
  return(invisible(NULL))
}

check_style <- function(fields, file) {
  if (!is.list(fields) || is.null(names(fields))) {
    stop("The style file ", file, " must be a YAML map.", call. = FALSE)
  }
  known <- c(names(style_schema), "footnotes")
  check_keys(fields, known, "", file, setdiff(known, optional_sections))
  for (section in intersect(names(style_schema), names(fields))) {
    check_section(fields[[section]], section, file)
  }
  check_footnotes(fields$footnotes, file)
  check_thresholds(fields$p, file)
  check_needs(fields, file)
  return(invisible(NULL))
}

# The keys of one section, then each field of it that the file holds.
check_section <- function(x, section, file) {
  spec <- style_schema[[section]]
  optional <- vapply(spec, function(f) return(isTRUE(f$optional)), logical(1))
  prefix <- paste0(section, ".")
  check_keys(x, names(spec), prefix, file, names(spec)[!optional])
  for (name in intersect(names(spec), names(x))) {
    field <- paste0(prefix, name)
    check_field(x[[name]], field, file)
    what <- style_checks[[spec[[name]]$type]](x[[name]]$value, spec[[name]])
    if (!is.null(what)) {
      field_error(field, file, what)
    }
  }
  return(invisible(NULL))
}

# An optional field is needed where another field uses it: cont.median_digits
# by a template that holds {median}, {q1} or {q3}, and missing.row_template
# and missing.row_pct_digits by a missing.show that adds a Missing row.
check_needs <- function(fields, file) {
  used <- intersect(
    template_keys(fields$cont$template$value),
    c("median", "q1", "q3")
  )
  if (length(used) > 0 && is.null(fields$cont[["median_digits"]])) {
    field_error(
      "cont.template",
      file,
      paste0(
        "holds {",
        used[1],
        "}, so the style needs the field cont.median_digits."
      )
    )
  }
  show <- fields$missing$show$value
  if (!show %in% c("row_if_any", "row_always")) {
    return(invisible(NULL))
  }
  for (name in c("row_template", "row_pct_digits")) {
    if (is.null(fields$missing[[name]])) {
      field_error(
        "missing.show",
        file,
        paste0(
          "is ",
          show,
          ", so the style needs the field missing.",
          name,
          "."
        )
      )
    }
  }
  return(invisible(NULL))
}

# A style without the section cat cannot format a "cat" variable.
check_style_types <- function(style, vars) {
  cat_vars <- vars$var[vars$type == "cat"]
  if (length(cat_vars) > 0 && is.null(style[["cat"]])) {
    stop(
      "The style file ",
      attr(style, "file"),
      " has no section cat, so it cannot format the \"cat\" variable ",
      paste(cat_vars, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# footnotes is a list. Each item is a field whose value is one string.
check_footnotes <- function(x, file) {
  if (!is.list(x) || !is.null(names(x))) {
    field_error("footnotes", file, "must be a YAML list, which can be empty.")
  }
  for (i in seq_along(x)) {
    field <- paste0("footnotes[", i, "]")
    check_field(x[[i]], field, file)
    if (!is_style_string(x[[i]]$value)) {
      field_error(field, file, "must have a value that is one quoted string.")
    }
  }
  return(invisible(NULL))
}

# A threshold prints with p.digits decimals, so it MUST have no more. The
# lower threshold MUST be below the upper one.
check_thresholds <- function(p, file) {
  digits <- p$digits$value
  for (name in c("lower", "upper")) {
    x <- p[[name]]$value
    if (!is.null(x) && abs(round(x, digits) - x) > 1e-12) {
      field_error(
        paste0("p.", name),
        file,
        "must have no more decimals than p.digits."
      )
    }
  }
  both <- c(p$lower$value, p$upper$value)
  if (length(both) == 2 && both[1] >= both[2]) {
    field_error("p.lower", file, "must be below p.upper.")
  }
  return(invisible(NULL))
}

# formatting ------------------------------------------------------------------

# `template` with each {key} replaced by the element of values[[key]]. The
# result is NA where a value of a placeholder in the template is NA.
fill_template <- function(template, values) {
  out <- rep(template, length(values[[1]]))
  for (key in names(values)) {
    out <- vapply(
      seq_along(out),
      function(i) {
        return(gsub(
          paste0("{", key, "}"),
          values[[key]][i],
          out[i],
          fixed = TRUE
        ))
      },
      character(1)
    )
  }
  return(out)
}

# `x` with `digits` decimals. A number whose rounded absolute value is
# thousands.from or more gets the thousands mark. NA stays NA.
format_number <- function(x, digits, thousands) {
  out <- formatC(x, format = "f", digits = digits)
  big <- !is.na(x)
  big[big] <- abs(as.numeric(out[big])) >= thousands$from$value
  out[big] <- formatC(
    x[big],
    format = "f",
    digits = digits,
    big.mark = thousands$mark$value
  )
  out[is.na(x)] <- NA_character_
  return(out)
}

# The cell of a "bin" or a "cat" row. `fields` is the section bin or cat.
format_count <- function(rows, fields, thousands) {
  return(fill_template(
    fields$template$value,
    list(
      events = format_number(rows$events, 0, thousands),
      n = format_number(rows$n, 0, thousands),
      pct = format_number(rows$pct, fields$pct_digits$value, thousands)
    )
  ))
}

# The cell of a "cont" row. The median and the quartiles are placeholders
# only when the style has cont.median_digits.
format_cont <- function(rows, style) {
  th <- style$thousands
  s <- style$cont
  values <- list(
    mean = format_number(rows$mean, s$mean_digits$value, th),
    sd = format_number(rows$sd, s$sd_digits$value, th)
  )
  digits <- s[["median_digits"]]$value
  if (!is.null(digits)) {
    for (key in c("median", "q1", "q3")) {
      values[[key]] <- format_number(rows[[key]], digits, th)
    }
  }
  return(fill_template(s$template$value, values))
}

# The percent of all rows of the group that are missing, missing ones
# included. NA for a group with no row.
missing_pct <- function(missing, n) {
  total <- missing + n
  pct <- 100 * missing / total
  pct[total == 0] <- NA_real_
  return(pct)
}

# The cell of a Missing row: the missing count, and its percent of all rows
# of the group, missing ones included.
format_missing_row <- function(missing, n, style) {
  pct <- missing_pct(missing, n)
  return(fill_template(
    style$missing$row_template$value,
    list(
      missing = format_number(missing, 0, style$thousands),
      pct = format_number(
        pct,
        style$missing$row_pct_digits$value,
        style$thousands
      )
    )
  ))
}

# The rows of one variable with the rows that the style adds: for a "cat"
# variable a heading row first, which takes the P of the variable, and the
# Missing row last when missing.show asks for it. The first `n_groups` rows
# hold one row per group.
add_style_rows <- function(rows, type, n_groups, style, label) {
  first <- data.table::as.data.table(rows_of(rows, seq_len(n_groups)))
  heading <- NULL
  if (type == "cat") {
    heading <- extra_rows(
      first,
      "heading",
      rep(style$cat$heading_template$value, n_groups),
      first$p_pretty,
      label
    )
    data.table::set(rows, j = "p_pretty", value = NA_character_)
    rows <- data.table::as.data.table(rows_of(rows, !is.na(rows$level)))
    prefix <- style$cat[["level_prefix"]]$value
    if (!is.null(prefix)) {
      data.table::set(rows, j = "label", value = paste0(prefix, rows$level))
    }
  }
  missing <- NULL
  show <- style$missing$show$value
  if (
    show == "row_always" || (show == "row_if_any" && any(first$missing > 0))
  ) {
    row_label <- style$missing[["row_label"]]$value
    missing <- extra_rows(
      first,
      "missing",
      format_missing_row(first$missing, first$n, style),
      NA_character_,
      if (is.null(row_label)) NA_character_ else row_label
    )
    data.table::set(
      missing,
      j = "pct",
      value = missing_pct(first$missing, first$n)
    )
  }
  return(data.table::rbindlist(list(heading, rows, missing)))
}

# A heading row or a Missing row for each group of `first`. It keeps var,
# grp, n, missing, p and test, and the other numbers are NA.
extra_rows <- function(first, row, value, p_pretty, label) {
  out <- data.table::copy(first)
  for (j in c("mean", "sd", "pct", "median", "q1", "q3")) {
    data.table::set(out, j = j, value = NA_real_)
  }
  data.table::set(out, j = "events", value = NA_integer_)
  data.table::set(out, j = "level", value = NA_character_)
  data.table::set(out, j = "label", value = label)
  data.table::set(out, j = "row", value = row)
  data.table::set(out, j = "value_pretty", value = value)
  data.table::set(out, j = "p_pretty", value = p_pretty)
  return(out)
}

# One section row for each group of `first`: `row` "section", `grp` the
# group, `label` the section, and NA in every other column.
section_rows <- function(first, section) {
  out <- data.table::copy(first)
  for (j in setdiff(names(out), c("grp", "label", "row"))) {
    data.table::set(out, j = j, value = out[[j]][NA_integer_])
  }
  data.table::set(out, j = "label", value = section)
  data.table::set(out, j = "row", value = "section")
  return(out)
}

# Adds missing.template to the end of each cell that missing.show selects.
add_missing <- function(value, missing, style) {
  show <- style$missing$show$value
  add <- !is.na(value) &
    (show == "always" | (show == "if_any" & missing > 0))
  value[add] <- paste0(
    value[add],
    fill_template(
      style$missing$template$value,
      list(missing = format_number(missing[add], 0, style$thousands))
    )
  )
  return(value)
}

format_p <- function(p, style) {
  s <- style$p
  digits <- s$digits$value
  out <- fill_template(
    s$template$value,
    list(p = formatC(p, format = "f", digits = digits))
  )
  out[is.na(p)] <- NA_character_
  lower <- s$lower$value
  if (!is.null(lower)) {
    out[!is.na(p) & p < lower] <- fill_template(
      s$below_template$value,
      list(lower = formatC(lower, format = "f", digits = digits))
    )
  }
  upper <- s$upper$value
  if (!is.null(upper)) {
    out[!is.na(p) & p > upper] <- fill_template(
      s$above_template$value,
      list(upper = formatC(upper, format = "f", digits = digits))
    )
  }
  return(out)
}

# The columns of `out` at the rows `keep`, as a list. pbn does not import
# data.table, so `out[keep]` would select columns.
rows_of <- function(out, keep) {
  return(lapply(as.list(out), function(v) return(v[keep])))
}

# The output of table1() with the columns row, value_pretty and p_pretty, the
# rows that the style adds, and the attribute footnotes. `type` is the type
# of each row, `index` the number of its variable in `vars`, and `n_groups`
# the number of groups, "All" included.
format_table1 <- function(out, type, index, n_groups, style, labels, sections) {
  value <- rep(NA_character_, nrow(out))
  for (t in intersect(c("bin", "cat"), type)) {
    keep <- type == t
    value[keep] <- format_count(rows_of(out, keep), style[[t]], style$thousands)
  }
  cont <- type == "cont"
  value[cont] <- format_cont(rows_of(out, cont), style)
  value <- add_missing(value, out$missing, style)
  data.table::set(out, j = "row", value = rep("value", nrow(out)))
  data.table::set(out, j = "value_pretty", value = value)
  data.table::set(out, j = "p_pretty", value = format_p(out$p, style))
  show <- style$missing$show$value
  starts <- nzchar(sections) & !duplicated(sections)
  if (
    any(type == "cat") || show %in% c("row_if_any", "row_always") || any(starts)
  ) {
    pieces <- split(seq_len(nrow(out)), index)
    out <- data.table::rbindlist(lapply(seq_along(pieces), function(k) {
      i <- pieces[[k]]
      rows <- data.table::as.data.table(rows_of(out, i))
      first <- data.table::as.data.table(rows_of(rows, seq_len(n_groups)))
      rows <- add_style_rows(rows, type[i[1]], n_groups, style, labels[k])
      if (!starts[k]) {
        return(rows)
      }
      return(data.table::rbindlist(list(
        section_rows(first, sections[k]),
        rows
      )))
    }))
  }
  notes <- vapply(
    style$footnotes,
    function(f) return(f$value),
    character(1)
  )
  data.table::setattr(out, "footnotes", notes)
  return(out)
}
