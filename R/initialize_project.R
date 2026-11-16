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
#'   `<home>/<version>_results/scenario/<basename of path>`.
#' - `folders_to_be_sourced` is `<version>_functions`.
#' - `max_loc_per_file` is 1000.
#'
#' A trailing command-line argument `data_raw=<path>` replaces `data_raw`.
#' [run_version()] gives this argument. pbn reads no environment variables.
#' A run with this argument is a scenario run. A scenario run never writes to
#' the results folder of the real run.
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

  results <- results_folder(
    home,
    version,
    if (raw$source == "argument") raw$path else NULL
  )
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
    data_raw = normalizePath(raw$path, winslash = "/", mustWork = TRUE),
    data_raw_source = raw$source
  )
  for (i in names(out)) {
    state[[i]] <- out[[i]]
  }
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
results_folder <- function(home, version, data_raw_argument = NULL) {
  results <- file.path(home, paste0(version, "_results"))
  if (is.null(data_raw_argument)) {
    return(results)
  }
  return(file.path(results, "scenario", basename(data_raw_argument)))
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
