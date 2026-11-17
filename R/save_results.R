#' Save the results object of a project version
#'
#' `save_results()` writes `<results>/results.qs2`, the one results object of
#' the project version. It stops before it writes when [check_results()]
#' finds a failure.
#'
#' @param ... The results, each as a named argument, for example
#'   `estimates = e` or `p_interaction = 0.04`. Give at least one. Each name
#'   MUST be syntactic and MUST be there once. A result named `manifest` is
#'   an error, because pbn writes that element. An element MAY be a data.frame, an atomic vector, a factor, a date,
#'   `NULL`, or a list of these to any depth. [check_results()] lists every
#'   allowed object. An element named `exclusions` MUST be the table of the
#'   exclusion steps, with the columns `step`, `n_before`, `n_excluded` and
#'   `n_after`. It MAY have zero rows.
#' @param d The analysis data, or `NULL`. You MUST give `d`. pbn stores only
#'   its number of rows. It then refuses a data.frame with that number of
#'   rows at any depth of the results. It also refuses a vector or a matrix
#'   with that number of values in one dimension. Give `d = NULL` when the
#'   run has no one table of the data.
#' @details
#' Call [initialize_project()] first. The results folder is the `results`
#' element that [initialize_project()] returns. It is
#' `<home>/<version>_results/<scenario>/` for a scenario with 1 simulation,
#' and `<home>/<version>_results/<scenario>/sim_<kkkk>/` otherwise.
#'
#' The object is a named list. Its first element is `manifest`, and the
#' results follow in the order of the call. pbn builds the `manifest`, a
#' list with these elements:
#' - `pbn_version` and `r_version`.
#' - `packages`, a named character vector. The names are the attached and
#'   loaded packages that are not base packages, and the values are their
#'   versions. `qs2` is always in the list, because it writes the file.
#' - `run_date`, the date of the call as `YYYY-MM-DD`. It has no time, so two
#'   runs on one day with the same inputs and packages write the same bytes.
#' - `version`, `scenario`, `sim`, `sims` and `data_raw`, from
#'   [initialize_project()]. `data_raw` is the data folder of the run.
#' - `settings`, the named list of the settings of the scenario from the
#'   setting columns of `CFG$scenarios`. It is an empty list when there are
#'   none.
#' - `source_md5`, a named character vector for every scenario. It holds the
#'   md5 of `<version>_run.R` and of each file in `<version>_functions/`,
#'   named by the path relative to `home` and sorted.
#'   [initialize_project()] computes it before it sources the files. The
#'   `check_results()` rule `manifest_source` needs the run script in it, so
#'   `save_results()` stops when `<version>_run.R` does not exist.
#' - For the scenario `"real"` only, `data_md5`, a named character vector
#'   with the md5 of each file directly in `data_raw`. [initialize_project()]
#'   computes it, so a file that the run writes after that call is not in the
#'   list.
#' - For a fake scenario only, `fake_md5`, one string: the md5 of the code of
#'   `fake_<scenario>()`, without its source references. Two runs of one
#'   simulation on one day with the same code and packages then write the
#'   same bytes. This holds also when `fake_<scenario>()` stamps the time
#'   into a file. A change in a function that `fake_<scenario>()` calls does
#'   not change `fake_md5`.
#' - `n_rows_data`, the number of rows of `d`, or `NA` when `d` is `NULL`.
#' @return Invisibly, the path of the results file.
#' @examples
#' home <- file.path(tempdir(), "pbn_save_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines("# The run script.", file.path(home, "v01_run.R"))
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
#'   counts = list(by_group = data.frame(grp = c("a", "b"), n = c(50, 46))),
#'   p_interaction = 0.04,
#'   d = NULL
#' )
#' names(pbn::read_results("v01", home = home))
#'
#' unlink(home, recursive = TRUE)
#' @family results
#' @export
save_results <- function(..., d) {
  if (is.null(state$results)) {
    stop(
      "Call pbn::initialize_project() before pbn::save_results().",
      call. = FALSE
    )
  }
  if (missing(d)) {
    stop(
      "save_results() needs `d`. Pass the analysis data as `d = d` so pbn ",
      "can refuse a table with one row for each row of the data, or pass ",
      "`d = NULL` if the run has no single data table.",
      call. = FALSE
    )
  }
  if (!is.null(d) && !is.data.frame(d)) {
    stop("`d` must be a data.frame or NULL.", call. = FALSE)
  }
  results <- list(...)
  check_result_names(names(results), length(results))

  x <- c(list(manifest = build_manifest(d)), results)
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

# Stops when the names `nm` of the `n` results of a save_results() call
# break the contract.
check_result_names <- function(nm, n) {
  if (n == 0) {
    stop(
      "save_results() needs at least one named result, for example ",
      "`estimates = e`.",
      call. = FALSE
    )
  }
  if (is.null(nm)) {
    nm <- rep("", n)
  }
  if ("manifest" %in% nm) {
    stop(
      "`manifest` is the name of the element that pbn writes. Give that ",
      "result another name.",
      call. = FALSE
    )
  }
  failures <- name_failures(nm)
  if (length(failures) > 0) {
    stop(
      "Each result of save_results() MUST have a syntactic name, once, as ",
      "in `estimates = e`. In this call, ",
      paste(failures, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

build_manifest <- function(d) {
  # qs2 writes the file, so it MUST be in the package list.
  loadNamespace("qs2")
  # initialize_project() empties the state before it records data_md5
  # (real) or fake_md5 (fake), so only the element of this run is there. An
  # element left from an earlier call would reach the manifest, and
  # check_results() would then refuse to save it.
  fingerprint <- Filter(
    Negate(is.null),
    list(data_md5 = state$data_md5, fake_md5 = state$fake_md5)
  )
  return(c(
    list(
      pbn_version = as.character(getNamespaceVersion("pbn")),
      r_version = R.version.string,
      packages = loaded_packages(),
      run_date = format(Sys.Date(), "%Y-%m-%d"),
      version = state$version,
      scenario = state$scenario,
      sim = state$sim,
      sims = state$sims,
      data_raw = state$data_raw,
      settings = state$settings,
      source_md5 = state$source_md5
    ),
    fingerprint,
    list(n_rows_data = if (is.null(d)) NA_integer_ else nrow(d))
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
