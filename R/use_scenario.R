# The scenario and simulation that use_scenario() chose for the R session.
choice <- new.env(parent = emptyenv())

# interactive(), in one place, so that the tests can mock it.
is_interactive <- function() {
  return(interactive())
}

#' Choose the scenario for stepping through a run script by hand
#'
#' `use_scenario()` chooses the scenario and the simulation that every later
#' [initialize_project()] call in this R session uses. It is for a person who
#' steps through a run script by hand.
#'
#' @param scenario The scenario, one name of lower-case letters, digits and
#'   underscores. [initialize_project()] checks that it is in
#'   `CFG$scenarios`.
#' @param sim The simulation, a whole number of 1 or more.
#' @details
#' Write one commented line for each scenario directly above the
#' [initialize_project()] call of `vNN_run.R`, in the order of
#' `CFG$scenarios`:
#'
#' ```r
#' # Run one of these lines to step through a scenario by hand. The choice lasts for the R session.
#' # pbn::use_scenario("real", sim = 1)
#' # pbn::use_scenario("null", sim = 1)
#' p <- pbn::initialize_project(version = "v01", home = ".", data_raw = "/data/raw")
#' ```
#'
#' To step through a scenario, run one of these lines without the `#`, then
#' run the script line by line. The lines MUST stay commented in the file.
#' `use_scenario()` stops in a non-interactive R session, for example under
#' `Rscript`, and [check_project()] fails an active call.
#'
#' The choice lasts for the R session. Every later [initialize_project()]
#' call uses it, also when you run that call again, until the next
#' `use_scenario()` call. `use_scenario("real", sim = 1)` goes back to the
#' real data. The command-line arguments `scenario=<name>` and `sim=<k>`
#' replace the choice as a whole.
#' @return Invisibly, a list with `scenario` and `sim`.
#' @examples
#' if (interactive()) {
#'   pbn::use_scenario("null", sim = 2)
#' }
#' @family project setup
#' @export
use_scenario <- function(scenario, sim = 1) {
  if (!is_interactive()) {
    stop(
      "pbn::use_scenario() is for stepping through a run script by hand in ",
      "an interactive R session. In the run script, the line MUST stay ",
      "commented out.",
      call. = FALSE
    )
  }
  if (!is_scenario_name(scenario)) {
    stop(
      "`scenario` must be one name of lower-case letters, digits and ",
      "underscores.",
      call. = FALSE
    )
  }
  if (!is_simulation(sim)) {
    stop("`sim` must be a whole number of 1 or more.", call. = FALSE)
  }
  choice$scenario <- scenario
  choice$sim <- as.integer(sim)
  cat(
    "pbn: from now on, pbn::initialize_project() uses the scenario \"",
    scenario,
    "\", simulation ",
    choice$sim,
    ".\n",
    sep = ""
  )
  return(invisible(list(scenario = scenario, sim = choice$sim)))
}

# One whole number of 1 or more.
is_simulation <- function(sim) {
  return(
    is.numeric(sim) &&
      length(sim) == 1 &&
      !is.na(sim) &&
      sim >= 1 &&
      sim == round(sim)
  )
}

# The choice of use_scenario() for the R session, or NULL.
session_choice <- function() {
  if (is.null(choice$scenario)) {
    return(NULL)
  }
  return(list(scenario = choice$scenario, sim = choice$sim))
}

#' End the run script here when the scenario is fake
#'
#' `end_if_fake_scenario()` ends a run script after [save_results()] when the
#' scenario is not `"real"`. The part of the script after it is for the real
#' data only, for example the figures.
#'
#' @details
#' Call it directly after [save_results()] in `vNN_run.R`. It reads the
#' scenario that the last [initialize_project()] call set:
#' - For `"real"`, it does nothing, and the script continues.
#' - For a fake scenario in a non-interactive R session, for example under
#'   `Rscript` or [run_scenarios()], it quits R with status 0. No line after
#'   it runs.
#' - For a fake scenario in an interactive R session, it stops with an error
#'   of class `pbn_end_if_fake_scenario`. The R session stays open.
#'
#' It stops with an error when no [initialize_project()] call set a
#' scenario in this R session.
#' @return Invisibly, `NULL`, when the scenario is `"real"`.
#' @examples
#' home <- file.path(tempdir(), "pbn_end_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines(
#'   "CFG <- list(scenarios = data.frame(scenario = 'real', sims = 1))",
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#' pbn::initialize_project("v01", home, file.path(home, "data"), env = new.env())
#' pbn::end_if_fake_scenario()
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @export
end_if_fake_scenario <- function() {
  scenario <- state$scenario
  if (is.null(scenario)) {
    stop(
      "Call pbn::initialize_project() before pbn::end_if_fake_scenario().",
      call. = FALSE
    )
  }
  if (scenario == "real") {
    return(invisible(NULL))
  }
  msg <- paste0(
    "The scenario \"",
    scenario,
    "\" is fake, so the run ends here. The rest of the script is for the ",
    "real data only."
  )
  if (!is_interactive()) {
    cat("pbn: ", msg, "\n", sep = "")
    utils::flush.console()
    flush(stdout())
    flush(stderr())
    quit(save = "no", status = 0)
  }
  stop(errorCondition(msg, class = "pbn_end_if_fake_scenario", call = NULL))
}
