# The state of the running project version. initialize_project() sets it and
# save_results() reads it.
state <- new.env(parent = emptyenv())

#' Start one scenario of an analysis project version
#'
#' `initialize_project()` sets up one simulation of one scenario of a project
#' version, and sources the functions folder of that version. It MUST be the
#' first call in every `vNN_run.R`.
#'
#' @param version The version, written as `"v"` and two digits, for example
#'   `"v01"`.
#' @param home The project folder. It holds `<version>_run.R` and
#'   `<version>_functions/`.
#' @param data_raw The folder of the real raw data. It MUST exist when the
#'   scenario is `"real"`. pbn does not read it for any other scenario.
#' @param env The environment that the functions folder is sourced into.
#' @details
#' `_cfg.R` in `<version>_functions/` MUST set `CFG$scenarios`, a data.frame
#' with these columns:
#' - `scenario`: unique names that match `^[a-z0-9_]+$`. One name MUST be
#'   `"real"`.
#' - `sims`: the number of simulations of the scenario, a whole number of 1 or
#'   more.
#'
#' The scenario and the simulation come from the first of these that gives
#' them. pbn reads no environment variables.
#' 1. The trailing command-line arguments `scenario=<name>` and `sim=<k>`.
#'    [run_scenarios()] gives both. When only one of them is there, the other
#'    one is `"real"` or 1.
#' 2. The choice of [use_scenario()], for a person who steps through the
#'    script by hand.
#' 3. The scenario `"real"` and the simulation 1.
#'
#' The choice of [use_scenario()] lasts for the R session.
#'
#' In an interactive R session, `initialize_project()` prints one line, for
#' example `pbn: scenario null, simulation 2 of 3 (fake)`. `(fake)` is there
#' for each scenario other than `"real"`.
#'
#' `initialize_project()` does these steps in this order:
#' 1. It calls [org::initialize_project()] with `results` set to
#'    `<home>/<version>_results`, `results_subfolder` set to the scenario,
#'    `folders_to_be_sourced` set to `<version>_functions`, and
#'    `max_loc_per_file` set to 1000.
#' 2. It reads `CFG$scenarios`. The results folder is
#'    `<version>_results/<scenario>/` when the scenario has 1 simulation, and
#'    `<version>_results/<scenario>/sim_<kkkk>/` otherwise. `<kkkk>` is the
#'    simulation with four digits. `org::project$results_today` is set to
#'    this folder.
#' 3. It calls `set.seed(sim)`. It also calls `set.seed(sim)` before step 1,
#'    so random work while org sources the functions folder depends on `sim`
#'    only.
#' 4. For `"real"`, the data folder is `data_raw`. For any other scenario, the
#'    data folder is `data/` in the results folder. `initialize_project()`
#'    deletes it, creates it empty, and calls `fake_<scenario>(dir)` with its
#'    path. The project MUST define `fake_<scenario>()` in
#'    `<version>_functions/`.
#' 5. It computes the md5 of each file directly in the data folder.
#'    [save_results()] stores these values as `manifest$data_md5`, so a file
#'    that the run writes later is not in the list.
#'
#' `initialize_project()` stops in these cases:
#' - `scenario=` or `sim=` is given more than once.
#' - `sim=` is not a whole number of 1 or more.
#' - The scenario is not in `CFG$scenarios`, or `sim` is more than its
#'   `sims`.
#' - `fake_<scenario>()` is missing.
#'
#' When it stops, it deletes each empty results folder that the call created.
#' The state of the run is then empty, so [save_results()] and
#' [end_if_fake_scenario()] stop until a call succeeds. The choice of
#' [use_scenario()] stays. It never creates a folder with today's date.
#' @return Invisibly, a list with `version`, `home`, `scenario`, `sim`,
#'   `sims`, `results` and `data_raw`. `results` is the results folder and
#'   `data_raw` is the data folder of the run.
#' @examples
#' home <- file.path(tempdir(), "pbn_initialize_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines(
#'   c(
#'     "CFG <- list(",
#'     "  scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 3))",
#'     ")",
#'     "fake_null <- function(dir) {",
#'     "  utils::write.csv(data.frame(x = rnorm(5)), file.path(dir, 'd.csv'))",
#'     "}"
#'   ),
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#'
#' utils::write.csv(data.frame(x = 1:5), file.path(home, "data", "d.csv"))
#'
#' env <- new.env()
#' p <- pbn::initialize_project(
#'   version = "v01",
#'   home = home,
#'   data_raw = file.path(home, "data"),
#'   env = env
#' )
#' p$scenario
#' p$results
#' list.files(p$data_raw)
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @export
initialize_project <- function(
  version,
  home,
  data_raw,
  env = .GlobalEnv
) {
  chosen <- session_choice()
  # The state of the run stays empty until the last step sets it. A call that
  # stops leaves no run for save_results() or end_if_fake_scenario().
  rm(list = ls(state, all.names = TRUE), envir = state)
  check_version(version)
  if (!is.character(home) || length(home) != 1 || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  home <- normalizePath(home, winslash = "/", mustWork = TRUE)
  args <- command_args()
  # The command line replaces the choice of use_scenario() as a whole.
  if (is.null(chosen) || any(grepl("^(scenario|sim)=", args))) {
    chosen <- list(scenario = "real", sim = 1L)
  }
  scenario <- choose_scenario(chosen$scenario, args)
  sim <- choose_sim(chosen$sim, args)
  if (scenario == "real") {
    data_raw <- check_data_raw(data_raw)
  }

  root <- file.path(home, paste0(version, "_results"))
  # Deepest first, so that a parent is empty when its child is gone.
  candidates <- c(
    file.path(root, scenario, sim_folder(sim)),
    file.path(root, scenario),
    root
  )
  created <- candidates[!dir.exists(candidates)]
  out <- tryCatch(
    start_scenario(version, home, root, scenario, sim, data_raw, env),
    error = function(e) {
      remove_empty_folders(created)
      stop(e)
    }
  )
  md5 <- data_md5(out$data_raw)
  if (is_interactive()) {
    cat(
      "pbn: scenario ",
      out$scenario,
      ", simulation ",
      out$sim,
      " of ",
      out$sims,
      if (out$scenario != "real") " (fake)",
      "\n",
      sep = ""
    )
  }
  # Every step that can stop is above this line.
  list2env(c(out, list(data_md5 = md5)), envir = state)
  return(invisible(out))
}

# The steps of initialize_project() that can leave a folder behind.
start_scenario <- function(version, home, root, scenario, sim, data_raw, env) {
  # Random work while org sources the functions folder depends on sim only.
  set.seed(sim)
  org::initialize_project(
    env = env,
    home = home,
    results = root,
    results_subfolder = scenario,
    folders_to_be_sourced = paste0(version, "_functions"),
    max_loc_per_file = 1000
  )
  table <- scenario_table(env, version)
  sims <- sims_of(table, scenario, sim)
  results <- file.path(root, scenario)
  if (sims > 1) {
    results <- file.path(results, sim_folder(sim))
    dir.create(results, showWarnings = FALSE, recursive = TRUE)
    # org takes the subfolder before it sources _cfg.R, and only _cfg.R
    # knows the number of simulations. So pbn sets the folder of the
    # simulation itself.
    assign(
      "results_today",
      org::path(org::project[["results"]], scenario, sim_folder(sim), "/"),
      envir = org::project
    )
  }
  set.seed(sim)
  if (scenario == "real") {
    data <- data_raw
  } else {
    data <- make_data(env, version, scenario, results)
  }
  return(list(
    version = version,
    home = home,
    scenario = scenario,
    sim = sim,
    sims = sims,
    results = results,
    data_raw = data
  ))
}

check_version <- function(version) {
  if (
    !is.character(version) ||
      length(version) != 1 ||
      !grepl("^v[0-9]{2}$", version)
  ) {
    stop(
      "`version` must be \"v\" and two digits, for example \"v01\".",
      call. = FALSE
    )
  }
  return(invisible(version))
}

is_scenario_name <- function(x) {
  return(
    is.character(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      grepl("^[a-z0-9_]+$", x)
  )
}

# The trailing command-line arguments, in one place, so that the tests can
# mock them.
command_args <- function() {
  return(commandArgs(trailingOnly = TRUE))
}

# The value of a trailing argument <key>=<value>, or NULL when it is absent.
run_arg <- function(args, key) {
  prefix <- paste0(key, "=")
  hits <- args[startsWith(args, prefix)]
  if (length(hits) > 1) {
    stop(
      "The command line gives ",
      prefix,
      " more than once: ",
      paste(hits, collapse = " "),
      call. = FALSE
    )
  }
  if (length(hits) == 0) {
    return(NULL)
  }
  value <- substring(hits, nchar(prefix) + 1)
  if (!nzchar(value)) {
    stop(
      "The command-line argument ",
      prefix,
      " gives no value.",
      call. = FALSE
    )
  }
  return(value)
}

choose_scenario <- function(default, args) {
  value <- run_arg(args, "scenario")
  scenario <- if (is.null(value)) default else value
  if (!is_scenario_name(scenario)) {
    stop(
      "The scenario must be one name of lower-case letters, digits and ",
      "underscores. It is: ",
      paste(scenario, collapse = " "),
      call. = FALSE
    )
  }
  return(scenario)
}

choose_sim <- function(default, args) {
  value <- run_arg(args, "sim")
  if (is.null(value)) {
    return(as.integer(default))
  }
  sim <- if (grepl("^[0-9]+$", value)) {
    suppressWarnings(as.integer(value))
  } else {
    NA_integer_
  }
  if (is.na(sim) || sim < 1) {
    stop(
      "The command-line argument sim= must be a whole number of 1 or more. ",
      "It is: ",
      value,
      call. = FALSE
    )
  }
  return(sim)
}

check_data_raw <- function(data_raw) {
  if (!is.character(data_raw) || length(data_raw) != 1 || !nzchar(data_raw)) {
    stop("`data_raw` must be the path of one folder.", call. = FALSE)
  }
  if (!dir.exists(data_raw)) {
    stop("The data_raw folder does not exist: ", data_raw, call. = FALSE)
  }
  return(normalizePath(data_raw, winslash = "/", mustWork = TRUE))
}

# The folder of one simulation, relative to the scenario folder.
sim_folder <- function(sim) {
  return(sprintf("sim_%04d", sim))
}

# CFG$scenarios from the sourced functions folder, checked.
scenario_table <- function(env, version) {
  cfg <- get0("CFG", envir = env, inherits = FALSE)
  table <- if (is.list(cfg)) cfg[["scenarios"]] else NULL
  return(check_scenario_table(table, version))
}

check_scenario_table <- function(table, version) {
  where <- paste0("CFG$scenarios in ", version, "_functions/")
  if (!is.data.frame(table) || !all(c("scenario", "sims") %in% names(table))) {
    stop(
      where,
      " must be a data.frame with the columns scenario and sims.",
      call. = FALSE
    )
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
  sims <- table$sims
  if (!is.numeric(sims) || anyNA(sims) || any(sims < 1 | sims != round(sims))) {
    stop(
      where,
      ": sims must be a whole number of 1 or more on each row.",
      call. = FALSE
    )
  }
  if (!"real" %in% table$scenario) {
    stop(where, " must have a scenario named \"real\".", call. = FALSE)
  }
  return(data.frame(scenario = table$scenario, sims = as.integer(sims)))
}

sims_of <- function(table, scenario, sim) {
  if (!scenario %in% table$scenario) {
    stop(
      "The scenario \"",
      scenario,
      "\" is not in CFG$scenarios. The scenarios are: ",
      paste(table$scenario, collapse = ", "),
      call. = FALSE
    )
  }
  sims <- table$sims[table$scenario == scenario]
  if (sim > sims) {
    stop(
      "The simulation ",
      sim,
      " is more than the ",
      sims,
      " simulations of the scenario \"",
      scenario,
      "\" in CFG$scenarios.",
      call. = FALSE
    )
  }
  return(sims)
}

# Writes the fake data of one simulation into <results>/data/, and returns
# the normalised path of that folder.
make_data <- function(env, version, scenario, results) {
  fn_name <- paste0("fake_", scenario)
  fn <- get0(fn_name, envir = env, mode = "function", inherits = FALSE)
  if (is.null(fn)) {
    stop(
      "The scenario \"",
      scenario,
      "\" needs the function ",
      fn_name,
      "(dir) in ",
      version,
      "_functions/. There is no function ",
      fn_name,
      ".",
      call. = FALSE
    )
  }
  dir <- file.path(results, "data")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  fn(dir)
  return(normalizePath(dir, winslash = "/", mustWork = TRUE))
}

remove_empty_folders <- function(folders) {
  for (f in folders) {
    if (
      dir.exists(f) &&
        length(list.files(f, all.files = TRUE, no.. = TRUE)) == 0
    ) {
      unlink(f, recursive = TRUE)
    }
  }
  return(invisible(NULL))
}
