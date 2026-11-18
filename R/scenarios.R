# interactive(), in one place, so that the tests can mock it.
is_interactive <- function() {
  return(interactive())
}

# The argset fields that pbn or plnr sets, and the arguments of
# plnr::Plan$add_analysis(). No setting and no extra dimension MAY have one of
# these names.
reserved_fields <- c(
  "scenario",
  "sim",
  "analysis",
  "seed",
  "index_analysis",
  "name",
  "fn",
  "fn_name",
  "data",
  "argset"
)

#' Choose the scenarios of a run
#'
#' `scenarios()` checks the scenario table `cfg$scenarios` and returns it, or
#' the rows of the scenarios in `only`. The real data is the scenario
#' `"real"`, and each other scenario is fake.
#'
#' @param cfg The config list of the project, normally `CFG` from
#'   `<version>_functions/_cfg.R`. It MUST have the element `scenarios`.
#' @param only `NULL` for every scenario, or a character vector of scenario
#'   names.
#' @details
#' `cfg$scenarios` MUST be a data.frame with these columns:
#' - `scenario`: unique names that match `^[a-z0-9_]+$`. One name MUST be
#'   `"real"`.
#' - `sims`: the number of simulations of the scenario, a whole number of 1 or
#'   more.
#'
#' - `output`: optional. `TRUE` or `FALSE` on each row, and `FALSE` for
#'   `"real"`. [make_outputs()] writes the output of a fake scenario with
#'   `TRUE`. This column is not a setting. It is not in the argsets, in the
#'   key of the cache or in `manifest$settings`.
#'
#' Each other column is a setting of the scenario, for example `n_boot` for a
#' number of bootstrap replicates that is smaller in the fake scenarios. A
#' setting column MUST be an atomic vector, and its name MUST be syntactic.
#' It MUST have a value for each fake scenario. It MAY have `NA` for
#' `"real"`, when only the fake scenarios use the setting. The argsets of
#' `"real"`, its cache key and its `manifest$settings` then do not hold the
#' setting. A data function MUST NOT take such a setting as an argument:
#' plnr finds no value of it for `"real"`, and [run_plan()] stops. Its name MUST NOT be one of `scenario`, `sim`, `analysis`,
#' `seed`, `index_analysis`, `name`, `fn`, `fn_name`, `data` and `argset`.
#' [add_analyses()] puts the setting of the scenario into each argset.
#'
#' `scenarios()` stops on a table that breaks a rule, and on a name in
#' `only` that is not in the table. The error names the problem.
#'
#' pbn keeps the value of the last call of `scenarios()` in this R session,
#' and [initialize_project()] removes it. [add_data()] gives it to the plan,
#' and [get_data()] reads it there before [add_analyses()].
#' @return A data.frame with the columns `scenario`, `sims`, `output` when
#'   `cfg$scenarios` has it, and each setting column. It has one row for each
#'   chosen scenario in the order of `cfg$scenarios`. `sims` is an integer.
#' @examples
#' CFG <- list(
#'   scenarios = data.frame(
#'     scenario = c("real", "null"),
#'     sims = c(1, 3),
#'     n_boot = c(200, 20)
#'   )
#' )
#' pbn::scenarios(CFG)
#' pbn::scenarios(CFG, only = "null")
#' @family project setup
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
scenarios <- function(cfg, only = NULL) {
  table <- if (is.list(cfg)) cfg[["scenarios"]] else NULL
  table <- check_scenario_table(table, "CFG$scenarios")
  if (!"real" %in% table$scenario) {
    stop("CFG$scenarios must have a scenario named \"real\".", call. = FALSE)
  }
  # The run log names the scenarios of a run with only = in its history.
  state$all_scenarios <- table$scenario
  if (is.null(only)) {
    state$scenarios <- table
    return(table)
  }
  if (!is.character(only) || length(only) == 0 || anyNA(only)) {
    stop(
      "`only` must be NULL or a character vector of scenarios.",
      call. = FALSE
    )
  }
  unknown <- setdiff(only, table$scenario)
  if (length(unknown) > 0) {
    stop(
      "These scenarios are not in CFG$scenarios: ",
      paste(unknown, collapse = ", "),
      ". The scenarios are: ",
      paste(table$scenario, collapse = ", "),
      call. = FALSE
    )
  }
  out <- table[table$scenario %in% only, , drop = FALSE]
  rownames(out) <- NULL
  state$scenarios <- out
  return(out)
}

# The scenario table `table`, checked, as a plain data.frame with an integer
# column sims. `where` names the table in the errors. "real" is not needed,
# because a chosen table MAY hold fake scenarios only.
check_scenario_table <- function(table, where) {
  if (!is.data.frame(table) || !all(c("scenario", "sims") %in% names(table))) {
    stop(
      where,
      " must be a data.frame with the columns scenario and sims.",
      call. = FALSE
    )
  }
  if (nrow(table) == 0) {
    stop(where, " has no scenario.", call. = FALSE)
  }
  names_ok <- vapply(table$scenario, is_scenario_name, logical(1))
  if (!is.character(table$scenario) || !all(names_ok)) {
    stop(
      where,
      ": each scenario must be a name of lower-case letters, digits and ",
      "underscores.",
      call. = FALSE
    )
  }
  if (anyDuplicated(table$scenario) > 0) {
    stop(where, ": each scenario must be there once.", call. = FALSE)
  }
  if (!all(vapply(table$sims, is_count, logical(1)))) {
    stop(
      where,
      ": sims must be a whole number of 1 or more on each row.",
      call. = FALSE
    )
  }
  out <- data.frame(scenario = table$scenario, sims = as.integer(table$sims))
  if ("output" %in% names(table)) {
    out$output <- check_output_column(table$output, table$scenario, where)
  }
  for (col in setting_columns(table)) {
    out[[col]] <- check_setting(table[[col]], col, where, table$scenario)
  }
  return(out)
}

# Each column of CFG$scenarios other than scenario, sims and output is a
# setting.
setting_columns <- function(table) {
  return(setdiff(names(table), c("scenario", "sims", "output")))
}

# The optional column output: TRUE or FALSE on each row, and FALSE for
# "real". make_outputs() always writes the output of "real".
check_output_column <- function(x, scenario, where) {
  if (!is.logical(x) || !is.null(dim(x)) || anyNA(x)) {
    stop(
      where,
      ": the column output must be TRUE or FALSE on each row.",
      call. = FALSE
    )
  }
  if (any(x[scenario == "real"])) {
    stop(
      where,
      ": the column output must be FALSE for \"real\". ",
      "pbn::make_outputs() always writes the output of the real data.",
      call. = FALSE
    )
  }
  return(x)
}

# A setting column MAY have NA for "real" only. The fake scenarios use the
# setting, and "real" then has no value of it.
check_setting <- function(x, col, where, scenario) {
  if (make.names(col) != col || col %in% reserved_fields) {
    stop(
      where,
      ": the setting column ",
      col,
      " needs another name. A setting name MUST be syntactic and MUST NOT ",
      "be one of ",
      paste(reserved_fields, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!is.atomic(x) || !is.null(dim(x))) {
    stop(
      where,
      ": the setting column ",
      col,
      " must be an atomic vector, for example numbers or strings. ",
      "It is of class ",
      paste(class(x), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  fake_na <- is.na(x) & scenario != "real"
  if (any(fake_na)) {
    stop(
      where,
      ": the setting column ",
      col,
      " has NA for the scenario ",
      scenario[fake_na][1],
      ". Each fake scenario needs a value. Only \"real\" MAY have NA.",
      call. = FALSE
    )
  }
  return(x)
}

# The settings of one scenario: a named list with one element for each
# setting column, or an empty list when there are none. A setting with NA,
# which only "real" MAY have, is not in the list.
settings_of <- function(table, scenario) {
  cols <- setting_columns(table)
  row <- which(table$scenario == scenario)
  cols <- cols[
    !vapply(
      cols,
      function(col) return(is.na(table[[col]][row])),
      logical(1)
    )
  ]
  if (length(cols) == 0) {
    return(list())
  }
  out <- lapply(cols, function(col) return(table[[col]][row]))
  names(out) <- cols
  return(out)
}

#' End the run script here unless the real data ran
#'
#' `end_unless_real()` ends a run script after [save_results()] when the
#' scenario `"real"` is not among the scenarios of the run. The part of the
#' script after it is for the real results only, for example the figures.
#'
#' @param scenarios The scenarios of the run, from [scenarios()].
#' @details
#' - When `"real"` is in `scenarios`, `end_unless_real()` does nothing, and
#'   the script continues.
#' - Otherwise, in a non-interactive R session, for example under `Rscript`,
#'   it prints one line, flushes the output and quits R with status 0. No line
#'   after it runs.
#' - Otherwise, in an interactive R session, it stops with an error of class
#'   `pbn_end_unless_real`. The R session stays open.
#' @return Invisibly, `NULL`, when `"real"` is in `scenarios`.
#' @examples
#' CFG <- list(scenarios = data.frame(scenario = c("real", "null"), sims = 1))
#' pbn::end_unless_real(pbn::scenarios(CFG))
#' @family project setup
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
end_unless_real <- function(scenarios) {
  table <- check_scenario_table(scenarios, "`scenarios`")
  if ("real" %in% table$scenario) {
    return(invisible(NULL))
  }
  msg <- paste0(
    "The run had no scenario \"real\", so it ends here. The output part of ",
    "the script needs the real results."
  )
  if (!is_interactive()) {
    cat("pbn: ", msg, "\n", sep = "")
    utils::flush.console()
    flush(stdout())
    flush(stderr())
    quit(save = "no", status = 0)
  }
  stop(errorCondition(msg, class = "pbn_end_unless_real", call = NULL))
}
