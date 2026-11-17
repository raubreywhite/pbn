#' Get the data or the argset of one analysis at the console
#'
#' `get_data()` returns the data that the analyses of one scenario and
#' simulation of the plan `p` get as `data`. `get_argset()` returns the argset
#' of one analysis, so you do not type the name of the analysis.
#'
#' @param p A plan from `plnr::Plan$new()`, with data from [add_data()].
#'   `get_argset()` also needs analyses from [add_analyses()].
#' @param scenario The scenario. The default is `"real"`.
#' @param sim The simulation, a whole number from 1 to the number of
#'   simulations of the scenario.
#' @param name The name of the analysis kind, for example `"models"`.
#' @param i The counter of the extra dimensions of the kind, see
#'   [add_analyses()]. A kind without extra dimensions has 1 only.
#' @details
#' `get_data(p, "null", 3)` calls `p$get_data()` for the analysis
#' `null__sim_0003__pbn_n_rows`. [add_analyses()] adds this analysis for each
#' scenario and simulation, and it gets the same data as each other analysis
#' of that scenario and simulation. So `get_data()` takes the path of
#' `p$get_data()`: the seed `sim`, the steps of [add_data()], and the cache
#' with its one line. The value is the named list of the data elements, with
#' the element `hash` that plnr adds. `pbn::get_data(p)$d` is the data `d`
#' of `"real"`.
#'
#' `get_argset(p, "null", 3, "models", 2)` is
#' `p$get_argset("null__sim_0003__models__2")`.
#'
#' Call `get_argset()` after [add_analyses()]. Call `get_data()` after
#' [add_data()], before or after [run_plan()]. Before [add_analyses()],
#' `get_data()` takes the scenarios of the last call of [scenarios()] before
#' [add_data()]. [initialize_project()] forgets them. It then calls each data
#' function with the
#' scenario, the simulation and the settings, in the same way as plnr. The
#' value then has no element `hash`. So the line
#' `d <- pbn::get_data(p, "real")$d` works directly after [add_data()] in a
#' run script.
#'
#' Both stop on a scenario or a simulation that the plan does not hold, and
#' the error names the values that it holds. `get_argset()` also stops on a
#' kind or a counter that the plan does not hold for that scenario. An
#' example is a kind with `control = TRUE` for `"real"`.
#'
#' To step through an action function by hand, put these lines first in it:
#' ```
#' if (plnr::is_run_directly()) {
#'   argset <- pbn::get_argset(p, "null", 3, "models")
#'   data <- pbn::get_data(p, "null", 3)
#' }
#' ```
#' @return `get_data()`: a named list with one element for each data
#'   element, and `hash`. `get_argset()`: the argset, a named list.
#' @examples
#' CFG <- list(scenarios = data.frame(scenario = c("real", "null"), sims = 2))
#' p <- plnr::Plan$new()
#' pbn::add_data(p, "d", fn = function(scenario, sim) {
#'   return(data.frame(x = stats::rnorm(3)))
#' })
#' pbn::add_analyses(
#'   p,
#'   pbn::scenarios(CFG),
#'   name = "means",
#'   fn = function(data, argset) return(data.frame(mean = mean(data$d$x))),
#'   outcome = c("y1", "y2")
#' )
#' pbn::get_data(p, "null", 2)$d
#' pbn::get_argset(p, "null", 2, "means", 2)$outcome
#' @family plan
#' @seealso `vignette("pbn")`, which steps through one analysis by hand.
#' @export
get_data <- function(p, scenario = "real", sim = 1) {
  check_plan(p)
  info <- plan_info(p)
  if (!is.null(info) && length(info$hidden) == 0 && length(info$data) > 0) {
    return(data_before_analyses(info, scenario, sim))
  }
  name <- console_name(p, scenario, sim, n_rows_kind, NULL)
  return(p$get_data(name))
}

# The data of one scenario and simulation of a plan that has data and no
# analysis yet. The scenarios come from the last call of scenarios(). Each
# data wrapper gets the scenario, the simulation and the settings that its
# arguments name, as plnr gives them, so the path is that of p$get_data():
# the seed, the steps and the cache with its line.
data_before_analyses <- function(info, scenario, sim) {
  table <- info$scenarios
  if (is.null(table)) {
    stop(
      "The plan has no analysis yet, so pbn::get_data() needs the scenarios ",
      "of the run. Call pbn::scenarios(CFG) before pbn::add_data().",
      call. = FALSE
    )
  }
  check_console_where(table$scenario, table$sims, scenario, sim)
  argset <- c(
    list(scenario = scenario, sim = as.integer(sim)),
    settings_of(table, scenario)
  )
  out <- list()
  for (nm in names(info$data)) {
    wrapper <- info$data_wrappers[[nm]]
    keys <- names(formals(wrapper))
    absent <- setdiff(keys, names(argset))
    if (length(absent) > 0) {
      stop(
        "The data function \"",
        nm,
        "\" has the arguments ",
        paste(absent, collapse = ", "),
        " without a default. pbn allows only scenario, sim and the setting ",
        "columns of CFG$scenarios.",
        call. = FALSE
      )
    }
    out[nm] <- list(do.call(wrapper, argset[keys], quote = TRUE))
  }
  return(out)
}

#' @rdname get_data
#' @export
get_argset <- function(p, scenario = "real", sim = 1, name, i = 1) {
  if (missing(name) || !is_one_string(name)) {
    stop(
      "`name` must be one analysis kind, for example \"models\".",
      call. = FALSE
    )
  }
  return(p$get_argset(console_name(p, scenario, sim, name, i)))
}

# The name of the analysis of the kind `kind` with the counter `i` for one
# scenario and simulation of the plan `p`, or of the analysis that counts the
# rows when `i` is NULL. Stops with the values that the plan holds.
console_name <- function(p, scenario, sim, kind, i) {
  check_plan(p)
  info <- plan_info(p)
  if (is.null(info) || length(info$hidden) == 0) {
    stop(
      "The plan has no analysis from pbn::add_analyses(). Call ",
      "pbn::add_data() and pbn::add_analyses() first.",
      call. = FALSE
    )
  }
  args <- lapply(info$hidden, function(n) return(p$analyses[[n]][["argset"]]))
  have <- vapply(args, function(a) return(a[["scenario"]]), character(1))
  sims <- vapply(args, function(a) return(as.integer(a[["sim"]])), integer(1))
  check_console_where(have, sims, scenario, sim)
  if (is.null(i)) {
    return(analysis_name(scenario, sim, kind))
  }
  return(kind_name(p, info, scenario, sim, kind, i))
}

# Stops when `scenario` is not one of `have`, or when `sim` is not a
# simulation of it. `sims` holds the simulation of each element of `have`.
check_console_where <- function(have, sims, scenario, sim) {
  if (!is_one_string(scenario) || !scenario %in% have) {
    stop(
      "`scenario` must be one of the scenarios of the plan: ",
      paste(unique(have), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  n <- max(sims[have == scenario])
  if (!is_sim_number(sim) || sim > n) {
    stop(
      "`sim` must be a whole number from 1 to ",
      n,
      ", the simulations of the scenario \"",
      scenario,
      "\".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The name of the analysis of the kind `kind` with the counter `i`. Stops
# when the plan has no such analysis for the scenario and simulation.
kind_name <- function(p, info, scenario, sim, kind, i) {
  if (!kind %in% info$kinds$analysis) {
    stop(
      "`name` must be one of the analysis kinds of the plan: ",
      paste(info$kinds$analysis, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  head <- paste0(scenario, "__", sim_folder(sim), "__", kind, "__")
  found <- names(p$analyses)[startsWith(names(p$analyses), head)]
  counters <- sort(as.integer(substring(found, nchar(head) + 1)))
  if (length(counters) == 0) {
    stop(
      "The kind \"",
      kind,
      "\" has no analysis for the scenario \"",
      scenario,
      "\". A kind with control = TRUE has none for \"real\".",
      call. = FALSE
    )
  }
  if (!is_sim_number(i) || !i %in% counters) {
    stop(
      "`i` must be a whole number from 1 to ",
      max(counters),
      ", the counters of the kind \"",
      kind,
      "\".",
      call. = FALSE
    )
  }
  return(analysis_name(scenario, sim, kind, i))
}
