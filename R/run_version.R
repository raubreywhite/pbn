#' Run one version of a project in a new R session
#'
#' `run_version()` runs `<home>/<version>_run.R` with [callr::rscript()] in a
#' new R session, with `home` as the working directory. It stops, and shows
#' the output of the script, when the script fails.
#'
#' @param version The version, for example `"v01"`.
#' @param data_raw `NULL`, or the path of a raw data folder. A path goes to
#'   the script as the trailing command-line argument `data_raw=<path>`, which
#'   [initialize_project()] reads.
#' @param home The project folder.
#' @return The path of the results file. It is
#'   `<home>/<version>_results/results.qs2` when `data_raw` is `NULL`, and
#'   `<home>/<version>_results/scenario/<basename of data_raw>/results.qs2`
#'   otherwise.
#' @examples
#' \donttest{
#' home <- file.path(tempdir(), "pbn_run_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines(
#'   c(
#'     'pbn::initialize_project(version = "v01", home = ".", data_raw = "data")',
#'     "pbn::save_results(",
#'     "  exclusions = data.frame(",
#'     "    step = character(0), n_before = numeric(0),",
#'     "    n_excluded = numeric(0), n_after = numeric(0)",
#'     "  ),",
#'     "  estimates = data.frame(term = 'x', estimate = 1),",
#'     "  tables = list()",
#'     ")"
#'   ),
#'   file.path(home, "v01_run.R")
#' )
#' pbn::run_version("v01", home = home)
#'
#' unlink(home, recursive = TRUE)
#' }
#' @family project setup
#' @export
run_version <- function(version, data_raw = NULL, home = ".") {
  check_version(version)
  home <- normalizePath(home, winslash = "/", mustWork = TRUE)
  script <- file.path(home, paste0(version, "_run.R"))
  if (!file.exists(script)) {
    stop("There is no run script at ", script, call. = FALSE)
  }
  cmdargs <- character(0)
  if (!is.null(data_raw)) {
    data_raw <- normalizePath(data_raw, winslash = "/", mustWork = TRUE)
    cmdargs <- paste0("data_raw=", data_raw)
  }
  res <- callr::rscript(
    script,
    cmdargs = cmdargs,
    wd = home,
    show = FALSE,
    stderr = "2>&1",
    fail_on_status = FALSE
  )
  if (res$status != 0) {
    stop(
      script,
      " stopped with status ",
      res$status,
      ". Its output:\n",
      res$stdout,
      call. = FALSE
    )
  }
  return(file.path(results_folder(home, version, data_raw), "results.qs2"))
}
