# The state of the running project version. initialize_project() sets it and
# save_results() reads it.
state <- new.env(parent = emptyenv())

#' Start one version of an analysis project
#'
#' `initialize_project()` sets up one version of a project with the house
#' defaults, and sources the functions folder of that version. It MUST be the
#' first call in every `vNN_run.R`.
#'
#' @param version The version, written as `"v"` and two digits, for example
#'   `"v01"`.
#' @param home The project folder. It holds `<version>_run.R` and
#'   `<version>_functions/`.
#' @param data_raw The default folder of the raw data. It MUST exist.
#' @param env The environment that the functions folder is sourced into.
#' @details
#' `initialize_project()` calls [org::initialize_project()] with these
#' arguments:
#' - `results` is `<home>/<version>_results`. In a scenario run with
#'   `data_raw=<path>`, it is
#'   `<home>/<version>_results/scenario/<name>`. `<name>` is the basename of
#'   the normalised path, so `~`, `..` and a trailing slash are resolved
#'   first.
#' - `folders_to_be_sourced` is `<version>_functions`.
#' - `max_loc_per_file` is 1000.
#'
#' A trailing command-line argument `data_raw=<path>` replaces `data_raw`.
#' [run_version()] gives this argument. pbn reads no environment variables.
#' A run with this argument is a scenario run. A scenario run never writes to
#' the results folder of the real run.
#'
#' A scenario run stops before it writes anything in these cases:
#' - The scenario results folder, normalised, is the real results folder.
#' - The data folder is the default `data_raw`. That is the real run.
#' - The data folder is the real results folder, or holds it.
#' - The scenario folder holds a `results.qs2` with a different
#'   `manifest$data_raw`. Two data folders with the same basename share one
#'   scenario folder.
#'
#' The data folder MAY be the scenario results folder itself.
#' `initialize_project()` computes the md5 of each file directly in the data
#' folder. [save_results()] stores these values as `manifest$data_md5`, so a
#' file that the run writes later is not in the list.
#'
#' org creates a dated subfolder in the results folder. pbn writes to the
#' results folder itself. Thus `initialize_project()` deletes each dated
#' subfolder that the call created and that is empty.
#' @return Invisibly, a list with `version`, `home`, `results`, `data_raw` and
#'   `data_raw_source`. `data_raw_source` is `"default"` or `"argument"`.
#' @examples
#' home <- file.path(tempdir(), "pbn_initialize_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines("CFG <- list(alpha = 0.05)", file.path(home, "v01_functions", "_cfg.R"))
#'
#' env <- new.env()
#' p <- pbn::initialize_project(
#'   version = "v01",
#'   home = home,
#'   data_raw = file.path(home, "data"),
#'   env = env
#' )
#' p$results
#' env$CFG$alpha
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @export
initialize_project <- function(version, home, data_raw, env = .GlobalEnv) {
  check_version(version)
  if (!is.character(home) || length(home) != 1 || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  home <- normalizePath(home, winslash = "/", mustWork = TRUE)
  raw <- choose_data_raw(data_raw, commandArgs(trailingOnly = TRUE))
  if (!dir.exists(raw$path)) {
    stop(
      "The data_raw folder does not exist: ",
      raw$path,
      " (source: ",
      raw$source,
      ").",
      call. = FALSE
    )
  }
  raw$path <- normalizePath(raw$path, winslash = "/", mustWork = TRUE)

  results <- results_folder(
    home,
    version,
    if (raw$source == "argument") raw$path else NULL
  )
  if (raw$source == "argument") {
    check_scenario(home, version, raw$path, results, data_raw)
  }
  # Hash the data now. A file written later into the data folder is an
  # output of the run, so data_md5 must not list it.
  md5 <- data_md5(raw$path)
  dated_before <- dated_folders(results)
  org::initialize_project(
    env = env,
    home = home,
    results = results,
    folders_to_be_sourced = paste0(version, "_functions"),
    max_loc_per_file = 1000
  )
  remove_new_empty_dated_folders(results, dated_before)

  out <- list(
    version = version,
    home = home,
    results = results,
    data_raw = raw$path,
    data_raw_source = raw$source
  )
  for (i in names(out)) {
    state[[i]] <- out[[i]]
  }
  state$data_md5 <- md5
  return(invisible(out))
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

# A trailing argument data_raw=<path> replaces the default folder.
choose_data_raw <- function(default, args) {
  hits <- args[startsWith(args, "data_raw=")]
  if (length(hits) > 1) {
    stop(
      "The command line gives data_raw= more than once: ",
      paste(hits, collapse = " "),
      call. = FALSE
    )
  }
  if (length(hits) == 1) {
    path <- substring(hits, nchar("data_raw=") + 1)
    if (!nzchar(path)) {
      stop("The command-line argument data_raw= gives no path.", call. = FALSE)
    }
    return(list(path = path, source = "argument"))
  }
  if (!is.character(default) || length(default) != 1 || !nzchar(default)) {
    stop("`data_raw` must be the path of one folder.", call. = FALSE)
  }
  return(list(path = default, source = "default"))
}

# The results folder of a run. A data folder from the argument data_raw=<path>
# gives a scenario folder, so the run never writes to the real results.
# `data_raw_argument` MUST be a normalised path: the basename of `..` or of
# `data/` is not the name of the folder.
results_folder <- function(home, version, data_raw_argument = NULL) {
  results <- file.path(home, paste0(version, "_results"))
  if (is.null(data_raw_argument)) {
    return(results)
  }
  return(file.path(results, "scenario", basename(data_raw_argument)))
}

# Stops when a scenario run could write over the real results, or over the
# results of a scenario run on another data folder. `data` and `scenario` are
# normalised paths, and `default` is the data_raw argument of the run script.
check_scenario <- function(home, version, data, scenario, default) {
  real <- normalizePath(
    file.path(home, paste0(version, "_results")),
    winslash = "/",
    mustWork = FALSE
  )
  if (normalizePath(scenario, winslash = "/", mustWork = FALSE) == real) {
    stop(
      "The scenario results folder ",
      scenario,
      " is the real results folder ",
      real,
      ". pbn does not run, so the real results stay as they are.",
      call. = FALSE
    )
  }
  if (
    is.character(default) &&
      length(default) == 1 &&
      nzchar(default) &&
      normalizePath(default, winslash = "/", mustWork = FALSE) == data
  ) {
    stop(
      "The argument data_raw=",
      data,
      " is the default data folder. That is the real run: ",
      "run it without the data_raw= argument.",
      call. = FALSE
    )
  }
  inside <- paste0(sub("/$", "", data), "/")
  if (data == real || startsWith(real, inside)) {
    stop(
      "The data folder ",
      data,
      " is the real results folder ",
      real,
      " or holds it. Give a data folder outside ",
      real,
      ".",
      call. = FALSE
    )
  }
  check_scenario_owner(data, scenario)
  return(invisible(NULL))
}

# Two data folders with the same basename share one scenario folder. Stops
# when the results in that folder come from another data folder.
check_scenario_owner <- function(data, scenario) {
  file <- file.path(scenario, "results.qs2")
  if (!file.exists(file)) {
    return(invisible(NULL))
  }
  owner <- qs2::qs_read(file)[["manifest"]][["data_raw"]]
  if (!identical(owner, data)) {
    stop(
      "The scenario folder ",
      scenario,
      " holds the results of the data folder ",
      paste(owner, collapse = " "),
      ", and this run has the data folder ",
      data,
      ". Rename one of the two data folders.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

dated_folders <- function(results) {
  if (!dir.exists(results)) {
    return(character(0))
  }
  found <- list.dirs(results, full.names = FALSE, recursive = FALSE)
  return(grep("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", found, value = TRUE))
}

remove_new_empty_dated_folders <- function(results, before) {
  for (f in setdiff(dated_folders(results), before)) {
    folder <- file.path(results, f)
    if (length(list.files(folder, all.files = TRUE, no.. = TRUE)) == 0) {
      unlink(folder, recursive = TRUE)
    }
  }
  return(invisible(NULL))
}
