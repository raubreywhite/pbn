#' Compute the numbers of a descriptive table
#'
#' `table1()` computes the numbers of a descriptive table, per group and for
#' all rows. It does no formatting.
#'
#' @param d A data.table, or a data.frame.
#' @param by The name of the grouping column in `d`.
#' @param vars A data.table with the columns `var` and `type`. `var` names a
#'   column in `d`. `type` is `"bin"` for a 0/1 variable, or `"cont"` for a
#'   numeric variable.
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
#' @return A data.table with one row per variable and group, in the order of
#'   `vars`, and `"All"` first. The columns are `var`, `grp`, `n`, `missing`,
#'   `mean`, `sd`, `events`, `pct`, `p` and `test`. `n` counts the non-missing
#'   values.
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
#' @family shared helpers
#' @export
table1 <- function(d, by, vars) {
  check_table1_args(d, by, vars)
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
  return(data.table::rbindlist(rows))
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
