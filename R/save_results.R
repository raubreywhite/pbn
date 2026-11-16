#' Save the results object of a project version
#'
#' `save_results()` writes `<results>/results.qs2`, the one results object of
#' the project version. It stops before it writes when [check_results()]
#' finds a failure.
#'
#' @param exclusions A data.frame with the columns `step`, `n_before`,
#'   `n_excluded` and `n_after`, one row per exclusion step. Zero rows are
#'   allowed.
#' @param estimates A data.frame of estimates.
#' @param tables A named list of data.frames.
#' @param extra A list of other results.
#' @param d The analysis data, or `NULL`. pbn stores only its number of rows.
#' @details
#' Call [initialize_project()] first. The results folder is the `results`
#' element that [initialize_project()] returns. It is
#' `<home>/<version>_results/<scenario>/` for a scenario with 1 simulation,
#' and `<home>/<version>_results/<scenario>/sim_<kkkk>/` otherwise.
#'
#' The object is a named list with exactly these top-level names:
#' `manifest`, `exclusions`, `estimates`, `tables` and `extra`. pbn builds the
#' `manifest`, a list with these elements:
#' - `pbn_version` and `r_version`.
#' - `packages`, a named character vector. The names are the attached and
#'   loaded packages that are not base packages, and the values are their
#'   versions. `qs2` is always in the list, because it writes the file.
#' - `run_date`, the date of the call as `YYYY-MM-DD`. It has no time, so two
#'   runs on one day with the same inputs and packages write the same bytes.
#' - `version`, `scenario`, `sim`, `sims` and `data_raw`, from
#'   [initialize_project()]. `data_raw` is the data folder of the run.
#' - `data_md5`, a named character vector with the md5 of each file directly
#'   in `data_raw`. [initialize_project()] computes it, so a file that the run
#'   writes after that call is not in the list.
#' - `n_rows_data`, the number of rows of `d`, or `NA` when `d` is `NULL`.
#' @return Invisibly, the path of the results file.
#' @examples
#' home <- file.path(tempdir(), "pbn_save_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines(
#'   "CFG <- list(scenarios = data.frame(scenario = 'real', sims = 1))",
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#' pbn::initialize_project("v01", home, file.path(home, "data"), env = new.env())
#'
#' pbn::save_results(
#'   exclusions = data.frame(
#'     step = "age 18 or more",
#'     n_before = 100,
#'     n_excluded = 4,
#'     n_after = 96
#'   ),
#'   estimates = data.frame(term = "x", estimate = 1.2),
#'   tables = list(counts = data.frame(grp = c("a", "b"), n = c(50, 46)))
#' )
#' names(pbn::read_results("v01", home = home))
#'
#' unlink(home, recursive = TRUE)
#' @family results
#' @export
save_results <- function(
  exclusions,
  estimates,
  tables,
  extra = list(),
  d = NULL
) {
  if (is.null(state$results)) {
    stop(
      "Call pbn::initialize_project() before pbn::save_results().",
      call. = FALSE
    )
  }
  check_save_args(estimates, tables, extra, d)

  x <- list(
    manifest = build_manifest(d),
    exclusions = exclusions,
    estimates = estimates,
    tables = tables,
    extra = extra
  )
  findings <- check_results(x)
  failed <- findings[findings$status == "FAIL", , drop = FALSE]
  if (nrow(failed) > 0) {
    stop(
      "The results object fails check_results(), so pbn did not save it:\n",
      paste0("  ", failed$rule, ": ", failed$detail, collapse = "\n"),
      call. = FALSE
    )
  }
  file <- file.path(state$results, "results.qs2")
  qs2::qs_save(x, file)
  return(invisible(file))
}

check_save_args <- function(estimates, tables, extra, d) {
  if (!is.data.frame(estimates)) {
    stop("`estimates` must be a data.frame.", call. = FALSE)
  }
  if (!is_named_list_of_data_frames(tables)) {
    stop("`tables` must be a named list of data.frames.", call. = FALSE)
  }
  if (!is.list(extra) || is.data.frame(extra)) {
    stop("`extra` must be a list.", call. = FALSE)
  }
  if (!is.null(d) && !is.data.frame(d)) {
    stop("`d` must be a data.frame or NULL.", call. = FALSE)
  }
  return(invisible(NULL))
}

is_named_list_of_data_frames <- function(x) {
  if (!is.list(x) || is.data.frame(x)) {
    return(FALSE)
  }
  if (length(x) == 0) {
    return(TRUE)
  }
  if (is.null(names(x)) || !all(nzchar(names(x)))) {
    return(FALSE)
  }
  return(all(vapply(x, is.data.frame, logical(1))))
}

build_manifest <- function(d) {
  # qs2 writes the file, so it MUST be in the package list.
  loadNamespace("qs2")
  return(list(
    pbn_version = as.character(getNamespaceVersion("pbn")),
    r_version = R.version.string,
    packages = loaded_packages(),
    run_date = format(Sys.Date(), "%Y-%m-%d"),
    version = state$version,
    scenario = state$scenario,
    sim = state$sim,
    sims = state$sims,
    data_raw = state$data_raw,
    data_md5 = state$data_md5,
    n_rows_data = if (is.null(d)) NA_integer_ else nrow(d)
  ))
}

loaded_packages <- function() {
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  pkgs <- union(loadedNamespaces(), attached)
  base <- rownames(utils::installed.packages(
    lib.loc = .Library,
    priority = "base"
  ))
  pkgs <- sort(setdiff(pkgs, base))
  versions <- vapply(
    pkgs,
    function(p) return(as.character(getNamespaceVersion(p))),
    character(1)
  )
  return(versions)
}

data_md5 <- function(folder) {
  files <- list.files(folder, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(files)]
  md5 <- unname(tools::md5sum(files))
  names(md5) <- basename(files)
  return(md5)
}
