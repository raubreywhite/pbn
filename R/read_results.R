#' Read the results object of a project version
#'
#' `read_results()` reads the `results.qs2` of one scenario, and the
#' `results_gitexclude.qs2` beside it, the files that [save_results()]
#' writes. The default reads the real results.
#'
#' @param version The version, for example `"v01"`.
#' @param scenario The scenario. The default is `"real"`.
#' @param sim `NULL`, or the number of one simulation.
#' @param home The project folder.
#' @details
#' With `sim = NULL`, `read_results()` reads
#' `<home>/<version>_results/<scenario>/results.qs2`. That is the results of
#' a scenario with 1 simulation, or the simulations that [save_results()]
#' stacked. With a number `k`, it reads
#' `<home>/<version>_results/<scenario>/sim_<kkkk>/results.qs2`, where
#' `<kkkk>` is `k` with four digits.
#'
#' `read_results()` stops when `<home>/<version>_results/<scenario>/check.csv`
#' exists and holds a value of `pass` that is not `TRUE`, also when it reads
#' one simulation. It also stops when that file holds no assertion, or has
#' no column `pass` that R can read. [save_results()] still writes such results, so that you
#' can find the fault. The error names the file and how to read it with
#' [qs2::qs_read()].
#'
#' `read_results()` returns one list with the elements of both files. The
#' order is that of a results object: `manifest`, `exclusions`, and the
#' analysis kinds in the order of `manifest$analyses`. These rules apply to
#' `results_gitexclude.qs2`:
#' - When the manifest names kinds in it and the file is missing, it returns
#'   the elements of `results.qs2`. A clone of the repository has no such
#'   file. It gives one warning that names the missing kinds and the file.
#' - It stops when the file holds other kinds than the manifest names, also
#'   when the manifest names none.
#'
#' A manuscript MUST read the real results only. [check_project()] fails a
#' call in `manuscript/built/` that gives a `scenario` other than `"real"`.
#' @return The results object, a named list. See [save_results()].
#' @examples
#' # See the example of save_results().
#' @family results
#' @export
read_results <- function(version, scenario = "real", sim = NULL, home = ".") {
  check_version(version)
  if (!is_scenario_name(scenario)) {
    stop(
      "`scenario` must be one name of lower-case letters, digits and ",
      "underscores.",
      call. = FALSE
    )
  }
  folder <- file.path(home, paste0(version, "_results"), scenario)
  if (!is.null(sim)) {
    if (!is_sim_number(sim)) {
      stop(
        "`sim` must be NULL or one whole number of 1 or more.",
        call. = FALSE
      )
    }
    folder <- file.path(folder, sim_folder(sim))
  }
  file <- file.path(folder, "results.qs2")
  if (!file.exists(file)) {
    stop("There is no results file at ", file, call. = FALSE)
  }
  check_assertions(file.path(home, paste0(version, "_results"), scenario), file)
  return(merge_gitexclude(qs2::qs_read(file), folder))
}

# Stops when the check.csv of the scenario folder `folder` holds an
# assertion that did not pass, holds no assertion, or has no column pass
# that R can read, for example after an interrupted write. `file` is the results file
# that read_results() would read.
check_assertions <- function(folder, file) {
  csv <- file.path(folder, "check.csv")
  problem <- assertion_problem(folder)
  if (is.null(problem)) {
    return(invisible(NULL))
  }
  stop(
    csv,
    " ",
    problem,
    ", so read_results() does not read ",
    file,
    ". Fix the analysis and run it again. To look at the results for ",
    "debugging, read them with qs2::qs_read(\"",
    file,
    "\").",
    call. = FALSE
  )
}

# What is wrong with the check.csv of the scenario folder `folder`, or NULL
# when it does not exist or when each of its assertions passed.
assertion_problem <- function(folder) {
  csv <- file.path(folder, "check.csv")
  if (!file.exists(csv)) {
    return(NULL)
  }
  found <- tryCatch(
    utils::read.csv(csv, stringsAsFactors = FALSE),
    error = function(e) return(NULL)
  )
  problem <- if (!is.data.frame(found) || !"pass" %in% names(found)) {
    "has no column pass that R can read"
  } else if (nrow(found) == 0) {
    "holds no assertion"
  } else if (!all(found$pass %in% TRUE)) {
    "holds an assertion that did not pass"
  }
  return(problem)
}

is_sim_number <- function(sim) {
  return(
    is.numeric(sim) &&
      length(sim) == 1 &&
      !is.na(sim) &&
      sim >= 1 &&
      sim == round(sim)
  )
}
