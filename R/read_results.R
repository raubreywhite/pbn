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
#' `<home>/<version>_results/<scenario>/sims_gitexclude/sim_<kkkk>.qs2`,
#' where `<kkkk>` is `k` with four digits. git ignores that folder, so a clone
#' of the repository has no such file. Then `read_results()` stops, and the
#' error says to run the scenario with [run_plan()] and [save_results()].
#' For a scenario with 1 simulation, `sim = 1` reads its `results.qs2`, the
#' results of that simulation.
#' The section The files of [save_results()] shows each file.
#'
#' In a one-file project, `<scenario>` is `fake` for the scenario `"real"`.
#' So `read_results(version)` there reads `<version>_results/fake/`. The
#' section The folder fake/ of [initialize_project()] gives the rule.
#'
#' `read_results()` stops when `<home>/<version>_results/<scenario>/check.csv`
#' exists and holds a value of `pass` that is not `TRUE`, also when it reads
#' one simulation. It also stops when that file holds no assertion, or has
#' no column `pass` that R can read. [save_results()] still writes such results, so that you
#' can find the fault. The error names the file and how to read it with
#' [qs2::qs_read()].
#'
#' For `"real"`, `read_results()` also stops when `manifest$failed_fake_checks`
#' names a fake scenario. [save_results()] writes into it each fake scenario
#' of the same run with a failed assertion. The real data and the fake
#' scenarios run through the same code. So a fake scenario with a failed
#' assertion can show a fault in the code of the real results. The error
#' names each such scenario, the path of its `check.csv` and the fix.
#'
#' The run that writes the real results writes this record. A run of
#' `"real"` alone records no fake scenario with a failed assertion. A later
#' run of a fake scenario alone does not change the record of the real
#' results. Real results of pbn 2026.11.24 or older have no
#' `failed_fake_checks`, and `read_results()` reads them as before.
#'
#' `read_results()` returns one list with the elements of both files. The
#' order is that of a results object: `manifest`, `exclusions`, and the
#' analysis kinds in the order of `manifest$analyses`. These rules apply to
#' `results_gitexclude.qs2`, and to `sim_<kkkk>_gitexclude.qs2` for one
#' simulation:
#' - When the manifest names kinds in it and the file is missing, it returns
#'   the elements of the results file. A clone of the repository has no such
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
  folder <- scenario_dir(version, scenario, home)
  file <- file.path(folder, "results.qs2")
  if (!is.null(sim)) {
    if (!is_sim_number(sim)) {
      stop(
        "`sim` must be NULL or one whole number of 1 or more.",
        call. = FALSE
      )
    }
    file <- file.path(folder, sim_file(sim))
    if (!file.exists(file) && sim == 1 && is_one_sim(folder)) {
      # Simulation 1 of a scenario with 1 simulation is its results.qs2.
      file <- file.path(folder, "results.qs2")
    }
    if (!file.exists(file)) {
      stop_no_sim_file(file, folder, scenario, sim)
    }
  }
  if (!file.exists(file)) {
    stop("There is no results file at ", file, call. = FALSE)
  }
  check_assertions(folder, file)
  results <- qs2::qs_read(file)
  if (scenario == "real") {
    check_run_of_real(folder, file, results[["manifest"]])
  }
  return(merge_gitexclude(results, file))
}

# Stops when the manifest `manifest` of the real results in the scenario
# folder `folder` names a fake scenario with a failed assertion. `file` is the
# results file that read_results() read.
check_run_of_real <- function(folder, file, manifest) {
  clause <- failed_fakes_clause(manifest)
  if (is.null(clause)) {
    return(invisible(NULL))
  }
  failed <- as.character(manifest[["failed_fake_checks"]])
  csv <- file.path(
    dirname(folder),
    if (length(failed) == 1) failed else "<scenario>",
    "check.csv"
  )
  stop(
    "manifest$failed_fake_checks of ",
    file,
    " ",
    clause,
    ". ",
    if (length(failed) == 1) "Its" else "Their",
    " assertions are in ",
    csv,
    ". Correct the analysis, and run the scenario \"real\" again with the ",
    "fake scenarios. To look at the results for debugging, read them with ",
    "qs2::qs_read(\"",
    file,
    "\").",
    call. = FALSE
  )
}

# The clause that says which fake scenarios with a failed assertion the
# element failed_fake_checks of the real manifest `manifest` names. NULL when
# the element is missing, as in the results of pbn 2026.11.24 or older, and
# when it is empty.
failed_fakes_clause <- function(manifest) {
  failed <- if (is.list(manifest)) manifest[["failed_fake_checks"]]
  if (is.null(failed) || identical(failed, character(0))) {
    return(NULL)
  }
  failed <- as.character(failed)
  return(paste0(
    "names ",
    if (length(failed) == 1) {
      "a fake scenario"
    } else {
      paste(length(failed), "fake scenarios")
    },
    " with a failed assertion: ",
    paste0("\"", failed, "\"", collapse = ", ")
  ))
}

# The manifest of the results.qs2 in the scenario folder `folder`, or NULL
# when R cannot read it.
results_manifest <- function(folder) {
  file <- file.path(folder, "results.qs2")
  if (!file.exists(file)) {
    return(NULL)
  }
  return(tryCatch(qs2::qs_read(file)[["manifest"]], error = function(e) {
    return(NULL)
  }))
}

# TRUE when the results.qs2 of the scenario folder `folder` is the results
# of a scenario with 1 simulation: its manifest has sims 1, and it is not
# stacked.
is_one_sim <- function(folder) {
  file <- file.path(folder, "results.qs2")
  if (!file.exists(file)) {
    return(FALSE)
  }
  m <- tryCatch(qs2::qs_read(file)[["manifest"]], error = function(e) {
    return(NULL)
  })
  return(is.list(m) && isTRUE(m[["sims"]] == 1) && !isTRUE(m[["stacked"]]))
}

# Stops, because the results file `file` of the simulation `sim` is missing.
# git does not track it, so a clone of the repository has none.
stop_no_sim_file <- function(file, folder, scenario, sim) {
  stop(
    "There is no results file of the simulation ",
    sim,
    " at ",
    file,
    ". git does not track the results of each simulation, so a clone of ",
    "the repository has none. To write them, run the scenario \"",
    scenario,
    "\" with pbn::run_plan() and pbn::save_results(). read_results() with ",
    "sim = NULL reads ",
    file.path(folder, "results.qs2"),
    ", the results of the scenario, with the simulations stacked.",
    call. = FALSE
  )
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

# The folder of the scenario `scenario` of a project version. `where` is the
# state of initialize_project(), or a version such as "v01" of the project
# folder `home`. The folder is <home>/<version>_results/<scenario>, with one
# exception: in a one-file project, the scenario "real" of this computer is
# in the folder fake/, because its data are fake. The folder real/ there
# holds only the real results that come back from the server. See
# is_one_file_version(). `scenario` MAY be a vector.
scenario_dir <- function(where, scenario, home = ".") {
  if (is.environment(where) || is.list(where)) {
    root <- where[["results"]]
    one_file <- isTRUE(where[["one_file"]])
  } else {
    root <- file.path(home, paste0(where, "_results"))
    one_file <- is_one_file_version(where, home)
  }
  folder <- ifelse(one_file & scenario == "real", "fake", scenario)
  return(file.path(root, folder))
}

# The scenarios with a folder under the results folder of `where`, as
# scenario_dir() takes it: a character vector of the folders, named by the
# scenario, sorted by name in the C locale. In a one-file project, the
# folder fake/ is the scenario "real", and the folder real/, which holds
# the real results of the server, is not a scenario of this computer. In
# another project, a folder fake/ is not a scenario, because scenarios()
# refuses the name.
scenario_dirs <- function(where, home = ".") {
  real <- scenario_dir(where, "real", home)
  root <- dirname(real)
  found <- if (dir.exists(root)) list.files(root) else character(0)
  found <- found[dir.exists(file.path(root, found))]
  if (basename(real) == "fake") {
    found <- sub("^fake$", "real", setdiff(found, "real"))
  } else {
    found <- setdiff(found, "fake")
  }
  found <- sort(found, method = "radix")
  return(stats::setNames(scenario_dir(where, found, home), found))
}

# TRUE when the version `version` of the project folder `home` is a
# one-file project: a top-level call pbn::initialize_project() of its run
# script <version>_run.R gives one_file_home. pbn parses the run script and
# evaluates nothing. FALSE without a run script, and for a run script that R
# cannot parse. The file of to_one_file() is not a run script, so on the
# server the scenario "real" has the folder real/.
is_one_file_version <- function(version, home = ".") {
  script <- file.path(home, paste0(version, "_run.R"))
  if (!file.exists(script)) {
    return(FALSE)
  }
  for (e in as.list(parse_or_null(script))) {
    if (!is.call(e) || !identical(e[[1]], quote(pbn::initialize_project))) {
      next
    }
    matched <- tryCatch(
      match.call(initialize_project, e),
      error = function(err) return(NULL)
    )
    if (!is.null(matched) && !is.null(matched$one_file_home)) {
      return(TRUE)
    }
  }
  return(FALSE)
}
