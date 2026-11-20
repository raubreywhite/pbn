#' Compute the numbers of a descriptive table
#'
#' `table1()` computes the numbers of a descriptive table, per group and for
#' all rows. With a style, it also formats these numbers as text.
#'
#' @param d A data.table, or a data.frame.
#' @param by The name of the grouping column in `d`.
#' @param vars A data.table with the columns `var` and `type`. `var` names a
#'   column in `d`. `type` is `"bin"` for a 0/1 variable, or `"cont"` for a
#'   numeric variable.
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
#' For a `"cont"` variable, `mean` and `sd` hold the numbers. `p` and `test`
#' are `NA` in this version.
#'
#' A column that does not apply to the type holds `NA`.
#' @section Styles:
#' A style formats the numbers of `table1()` for a journal or a report. A
#' style can only format what `table1()` computes. It cannot add a median, an
#' IQR or a test of a `"cont"` variable.
#'
#' An installed style is the file `inst/table1/<name>_<YYYY_MM_DD>.yml` of
#' pbn. Its name is the file name without `.yml`. pbn never edits a released
#' style, so a changed style gets a new date.
#'
#' A style file is a YAML map of sections. Each field of a section is a map
#' with the keys `value` and `source`. `source` is one string that says where
#' the value comes from. Examples are the guide and its count of papers
#' (`k/N papers`), the guideline and its page, and `editorial choice`.
#' `table1()` checks every field when it reads the file. An unknown field, a
#' missing field, a missing `source` or a value that the field does not allow
#' stops it. The error names the field and the file.
#'
#' A template is text with placeholders in braces, such as `{events}`. A
#' template can hold only the placeholders of its field, in any order, and
#' it can hold none. These are the fields:
#'
#' - `bin.template`: the cell of a `"bin"` row, over `{events}`, `{n}` and
#'   `{pct}`. An example is `"{events} ({pct})"`.
#' - `bin.pct_digits`: the decimals of `pct`, a whole number from 0 to 10.
#' - `cont.template`: the cell of a `"cont"` row, over `{mean}` and `{sd}`.
#' - `cont.mean_digits` and `cont.sd_digits`: the decimals of `mean` and of
#'   `sd`, each a whole number from 0 to 10.
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
#' - `missing.show`: `never`, `if_any` or `always`. With `if_any`, a cell gets
#'   `missing.template` when `missing` is above 0.
#' - `missing.template`: the text that `table1()` adds to the end of a cell,
#'   over `{missing}`. An example is `"; {missing} missing"`.
#' - `footnotes`: a list, which can be empty. Each item is a map with the keys
#'   `value` and `source`, and `value` is the text of one footnote.
#'
#' `p.lower` and `p.upper` print with `p.digits` decimals, so each MUST have
#' no more decimals than `p.digits`. `p.lower` MUST be below `p.upper`.
#' `table1()` compares `p` with the thresholds before it rounds `p`, so 0.9994
#' is above 0.999. A `p` equal to a threshold gets `p.template`.
#'
#' `table1()` rounds each number with `formatC()`, which rounds as the C
#' function `sprintf()` does. `events`, `n` and `missing` print with no
#' decimals. The thousands separator applies to every number of a cell. A
#' cell is `NA` when a number of its template is `NA`, as for a group with
#' `n = 0`. `p_pretty` is `NA` where `p` is `NA`.
#' @return A data.table with one row per variable and group, in the order of
#'   `vars`, and `"All"` first. The columns are `var`, `grp`, `n`, `missing`,
#'   `mean`, `sd`, `events`, `pct`, `p` and `test`. `n` counts the non-missing
#'   values. With a style, the columns `value_pretty` (the cell text) and
#'   `p_pretty` (the text of `p`) follow. The attribute `footnotes` then
#'   holds the footnotes of the style as a character vector. The other
#'   columns are the same as with `style = NULL`.
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
#'   "  template: {value: '{events} ({pct})', source: editorial choice}",
#'   "  pct_digits: {value: 1, source: editorial choice}",
#'   "cont:",
#'   "  template: {value: '{mean} ({sd})', source: editorial choice}",
#'   "  mean_digits: {value: 1, source: editorial choice}",
#'   "  sd_digits: {value: 1, source: editorial choice}",
#'   "p:",
#'   "  digits: {value: 3, source: editorial choice}",
#'   "  template: {value: 'P ={p}', source: editorial choice}",
#'   "  lower: {value: 0.001, source: editorial choice}",
#'   "  below_template: {value: 'P <{lower}', source: editorial choice}",
#'   "  upper: {value: 0.999, source: editorial choice}",
#'   "  above_template: {value: 'P >{upper}', source: editorial choice}",
#'   "thousands:",
#'   "  mark: {value: ',', source: editorial choice}",
#'   "  from: {value: 10000, source: editorial choice}",
#'   "missing:",
#'   "  show: {value: if_any, source: editorial choice}",
#'   "  template: {value: '; {missing} missing', source: editorial choice}",
#'   "footnotes:",
#'   "  - {value: 'Values are n (percent) or mean (SD).', source: editorial choice}"
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
  rows <- lapply(seq_len(nrow(vars)), function(i) {
    return(table1_one_var(
      x = d[[vars$var[i]]],
      var = vars$var[i],
      type = vars$type[i],
      grp = grp,
      levels_by = levels_by
    ))
  })
  out <- data.table::rbindlist(rows)
  if (is.null(style)) {
    return(out)
  }
  type <- rep(vars$type, vapply(rows, nrow, integer(1)))
  return(format_table1(out, type, style))
}

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
  bad_type <- vars$var[!vars$type %in% c("bin", "cont")]
  if (length(bad_type) > 0) {
    stop(
      "The type must be \"bin\" or \"cont\". It is not for: ",
      paste(bad_type, collapse = ", "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

table1_one_var <- function(x, var, type, grp, levels_by) {
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
    xi <- if (g == "All") x else x[!is.na(grp) & grp == g]
    return(summarise_one(xi, type))
  })
  out <- data.table::rbindlist(parts)
  out <- cbind(
    data.table::data.table(var = var, grp = groups),
    out,
    data.table::data.table(p = tested$p, test = tested$test)
  )
  return(out)
}

summarise_one <- function(x, type) {
  n <- sum(!is.na(x))
  out <- list(
    n = n,
    missing = sum(is.na(x)),
    mean = NA_real_,
    sd = NA_real_,
    events = NA_integer_,
    pct = NA_real_
  )
  if (type == "bin") {
    out$events <- sum(x == 1, na.rm = TRUE)
    out$pct <- if (n > 0) 100 * out$events / n else NA_real_
  } else if (n > 0) {
    out$mean <- mean(x, na.rm = TRUE)
    out$sd <- stats::sd(x, na.rm = TRUE)
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
  tab <- tab[rowSums(tab) > 0, , drop = FALSE]
  if (nrow(tab) < 2) {
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
#' @examples
#' pbn::table1_styles()
#' @family shared helpers
#' @export
table1_styles <- function() {
  return(style_names(table1_style_dir()))
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
# `keys` are the placeholders that a template can hold.
style_schema <- list(
  bin = list(
    template = list(type = "template", keys = c("events", "n", "pct")),
    pct_digits = list(type = "digits")
  ),
  cont = list(
    template = list(type = "template", keys = c("mean", "sd")),
    mean_digits = list(type = "digits"),
    sd_digits = list(type = "digits")
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
    show = list(type = "choice", choices = c("never", "if_any", "always")),
    template = list(type = "template", keys = "missing")
  )
)

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
  used <- regmatches(x, gregexpr("\\{[^{}]*\\}", x))[[1]]
  bad <- setdiff(gsub("[{}]", "", used), spec$keys)
  if (length(bad) > 0) {
    return(paste0(
      "holds {",
      bad[1],
      "}. It can hold only ",
      paste0("{", spec$keys, "}", collapse = ", "),
      "."
    ))
  }
  return(NULL)
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
# first key of `known` that `x` lacks. `prefix` makes the full field name.
check_keys <- function(x, known, prefix, file) {
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
  absent <- setdiff(known, names(x))
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
  check_keys(fields, c(names(style_schema), "footnotes"), "", file)
  for (section in names(style_schema)) {
    spec <- style_schema[[section]]
    check_keys(fields[[section]], names(spec), paste0(section, "."), file)
    for (name in names(spec)) {
      field <- paste0(section, ".", name)
      check_field(fields[[section]][[name]], field, file)
      what <- style_checks[[spec[[name]]$type]](
        fields[[section]][[name]]$value,
        spec[[name]]
      )
      if (!is.null(what)) {
        field_error(field, file, what)
      }
    }
  }
  check_footnotes(fields$footnotes, file)
  check_thresholds(fields$p, file)
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

format_bin <- function(events, n, pct, style) {
  th <- style$thousands
  return(fill_template(
    style$bin$template$value,
    list(
      events = format_number(events, 0, th),
      n = format_number(n, 0, th),
      pct = format_number(pct, style$bin$pct_digits$value, th)
    )
  ))
}

format_cont <- function(mean, sd, style) {
  th <- style$thousands
  return(fill_template(
    style$cont$template$value,
    list(
      mean = format_number(mean, style$cont$mean_digits$value, th),
      sd = format_number(sd, style$cont$sd_digits$value, th)
    )
  ))
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

# The output of table1() with the columns value_pretty and p_pretty, and the
# attribute footnotes. `type` is the type of each row.
format_table1 <- function(out, type, style) {
  bin <- type == "bin"
  value <- rep(NA_character_, nrow(out))
  value[bin] <- format_bin(out$events[bin], out$n[bin], out$pct[bin], style)
  value[!bin] <- format_cont(out$mean[!bin], out$sd[!bin], style)
  value <- add_missing(value, out$missing, style)
  data.table::set(out, j = "value_pretty", value = value)
  data.table::set(out, j = "p_pretty", value = format_p(out$p, style))
  notes <- vapply(
    style$footnotes,
    function(f) return(f$value),
    character(1)
  )
  data.table::setattr(out, "footnotes", notes)
  return(out)
}
