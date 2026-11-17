# The state of the project version. initialize_project() sets it, and
# run_plan() and save_results() read it.
state <- new.env(parent = emptyenv())

#' Start a project version
#'
#' `initialize_project()` sets the paths of a project version and sources its
#' functions folder. It MUST be the first call in every `vNN_run.R`.
#'
#' @param version The version, written as `"v"` and two digits, for example
#'   `"v01"`.
#' @param home The project folder. It holds `<version>_run.R` and
#'   `<version>_functions/`.
#' @param data_raw The folder of the real raw data, one string. The folder MAY
#'   be absent, for example on a computer without the real data. Only
#'   [save_results()] for the scenario `"real"` then stops.
#' @param env The environment that the functions folder is sourced into.
#' @details
#' `initialize_project()` does these steps in this order:
#' 1. It computes the md5 of `<version>_run.R` and of each file in
#'    `<version>_functions/`, before it sources them. [save_results()] stores
#'    them in `manifest$source_md5`.
#' 2. It computes the md5 of each file directly in `data_raw`, when the folder
#'    exists. [save_results()] stores them in `manifest$data_md5` of the
#'    scenario `"real"`. A file that the run writes later is not in the list.
#' 3. It calls [org::initialize_project()] with `folders_to_be_sourced` set to
#'    `<version>_functions`, `max_loc_per_file` set to 1000 and `data_raw` set
#'    to `data_raw`. A data function finds the folder at
#'    `org::project$data_raw`.
#'
#' The results folder is `<home>/<version>_results`. [save_results()] creates
#' it. `initialize_project()` creates no folder, also no folder with today's
#' date. It stops when `<version>_functions/` does not exist.
#'
#' `initialize_project()` reads no scenario, no simulation, no command-line
#' argument and no environment variable, and it sets no seed. [scenarios()]
#' chooses the scenarios, and [add_data()] and [add_analyses()] set the seeds.
#' @return Invisibly, a list with `version`, `home`, `results` and
#'   `data_raw`. `results` is the results folder.
#' @examples
#' home <- file.path(tempdir(), "pbn_initialize_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines(
#'   "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 3)))",
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#'
#' env <- new.env()
#' p <- pbn::initialize_project(
#'   version = "v01",
#'   home = home,
#'   data_raw = file.path(home, "data"),
#'   env = env
#' )
#' p$results
#' env$CFG$scenarios
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
initialize_project <- function(
  version,
  home,
  data_raw,
  env = .GlobalEnv
) {
  # The state stays empty until the last step sets it. A call that stops
  # leaves no project for run_plan() or save_results().
  rm(list = ls(state, all.names = TRUE), envir = state)
  check_init_args(version, home, data_raw, env)
  home <- normalizePath(home, winslash = "/", mustWork = TRUE)
  # org creates a missing folder with a warning. pbn stops instead, so that a
  # wrong `home` or `version` leaves no folder.
  folder <- file.path(home, paste0(version, "_functions"))
  if (!dir.exists(folder)) {
    stop("There is no functions folder ", folder, call. = FALSE)
  }
  # Before org sources the functions folder, so the md5 is of the files as
  # the run found them.
  code_md5 <- source_md5(home, version)
  code_stat <- file_stat(file.path(home, names(code_md5)), names(code_md5))
  real_md5 <- NULL
  real_stat <- NULL
  if (dir.exists(data_raw)) {
    data_raw <- normalizePath(data_raw, winslash = "/", mustWork = TRUE)
    real_md5 <- data_md5(data_raw)
    real_stat <- file_stat(file.path(data_raw, names(real_md5)), names(real_md5))
  }
  # The objects that the folder defines are the new ones and those with a
  # new address. run_plan() fails an analysis that changes one of them.
  before <- object_addresses(env)
  # results = NULL: org then creates no folder, also no folder with today's
  # date, and it does not create data_raw.
  org::initialize_project(
    env = env,
    home = home,
    folders_to_be_sourced = paste0(version, "_functions"),
    max_loc_per_file = 1000,
    data_raw = data_raw
  )
  out <- list(
    version = version,
    home = home,
    results = file.path(home, paste0(version, "_results")),
    data_raw = data_raw
  )
  # Every step that can stop is above this line.
  list2env(
    c(
      out,
      list(
        data_md5 = real_md5,
        data_stat = real_stat,
        source_md5 = code_md5,
        source_stat = code_stat,
        env = env,
        sourced = defined_names(env, before)
      )
    ),
    envir = state
  )
  return(invisible(out))
}

check_init_args <- function(version, home, data_raw, env) {
  check_version(version)
  if (!is_one_string(home) || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  if (!is_one_string(data_raw) || !nzchar(data_raw)) {
    stop("`data_raw` must be the path of one folder.", call. = FALSE)
  }
  if (!is.environment(env)) {
    stop("`env` must be an environment.", call. = FALSE)
  }
  return(invisible(NULL))
}

# One whole number of 1 or more, and not Inf.
is_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    return(FALSE)
  }
  return(is.finite(x) && x >= 1 && x == round(x))
}

# Stops when initialize_project() has not set the state of a project.
check_state <- function(caller) {
  if (is.null(state$results)) {
    stop(
      "Call pbn::initialize_project() before pbn::",
      caller,
      "().",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The size and the modification time of each of the files `files`, under the
# names `names`. pbn hashes a file again only when one of them changed.
file_stat <- function(files, names) {
  return(data.frame(
    file = names,
    size = file.size(files),
    mtime = as.numeric(file.mtime(files))
  ))
}

# tools::md5sum() of the files `files`, without names. In one place, so that
# the tests can count the files that pbn hashes.
hash_files <- function(files) {
  return(unname(tools::md5sum(files)))
}

# The md5 of each file directly in `folder`, named by the file.
data_md5 <- function(folder) {
  files <- list.files(folder, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(files)]
  md5 <- unname(tools::md5sum(files))
  names(md5) <- basename(files)
  return(md5)
}

# The md5 of the deparsed code of the functions `fns`, named by the names of
# `fns`. removeSource() and a fixed deparse control leave out the source
# references, so the value is the same with keep.source TRUE or FALSE, for
# example under RStudio and under Rscript.
function_md5 <- function(fns) {
  text <- unlist(lapply(names(fns), function(nm) {
    return(c(
      nm,
      deparse(
        utils::removeSource(fns[[nm]]),
        width.cutoff = 500L,
        control = c("keepNA", "keepInteger", "niceNames", "showAttributes")
      )
    ))
  }))
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  # writeBin(), not writeLines(), so the bytes are the same on each OS.
  writeBin(charToRaw(enc2utf8(paste(text, collapse = "\n"))), file)
  return(unname(tools::md5sum(file)))
}

# The md5 of the run script and of each file in the functions folder of
# `version`, named by the path relative to `home` and sorted in the C locale.
# A file that does not exist is not in the vector.
source_md5 <- function(home, version) {
  folder <- paste0(version, "_functions")
  inner <- list.files(
    file.path(home, folder),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  rel <- c(paste0(version, "_run.R"), file.path(folder, inner, fsep = "/"))
  rel <- sort(rel[file.exists(file.path(home, rel))], method = "radix")
  md5 <- unname(tools::md5sum(file.path(home, rel)))
  names(md5) <- rel
  return(md5)
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

# The name of one simulation, sim_<kkkk>, in the names of its files and
# its analyses.
sim_folder <- function(sim) {
  return(sprintf("sim_%04d", sim))
}
