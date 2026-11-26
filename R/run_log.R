# The run log, <version>_results/run.md: the facts of the last run, kept in
# the state of the project, and the file that pbn writes from them.
# run_log_md.R turns the facts into Markdown.

# The log of a run that save_results() saw. `run` is the record of
# run_plan(), `table` the scenarios, `done` the environment of save_steps(),
# `checks` its value or its error, and `seconds` the wall time of
# save_results().
saved_log <- function(run, table, done, checks, seconds) {
  log <- base_log(run, table$scenario)
  log$scenarios <- merge_scenarios(run$scenarios, table)
  log$saved <- TRUE
  log$contract <- if (is.null(done$contract)) character(0) else done$contract
  log$checks <- done$checks
  log$expected <- vapply(
    table$scenario,
    function(s) return(!is.null(expect_function(s))),
    logical(1)
  )
  if (inherits(checks, "error") && length(log$contract) == 0) {
    log$error <- conditionMessage(checks)
  }
  log$files <- files_written(state$results, table$scenario)
  log$outputs_deleted <- done$outputs_deleted
  log$timing$save <- c(done$seconds, total = seconds)
  log$result <- log_result(log)
  return(log)
}

# The scenarios of the plan, in the order of `table`, the scenarios that
# save_results() got.
merge_scenarios <- function(planned, table) {
  out <- planned[match(table$scenario, planned$scenario), , drop = FALSE]
  out$scenario <- table$scenario
  out$sims <- table$sims
  rownames(out) <- NULL
  return(out)
}

# Writes the log of a run that stopped in run_plan() on failed analyses,
# when initialize_project() set a project. save_results() never runs then.
write_failed_run <- function(run, error = NULL) {
  if (is.null(state$results)) {
    return(invisible(NULL))
  }
  log <- base_log(run, run$scenarios$scenario)
  log$run_error <- error
  log$result <- log_result(log)
  write_run_log(log)
  return(invisible(NULL))
}

# The parts of the log that run_plan() gives: the run, the project and the
# scenarios on the disk that are not in this run.
base_log <- function(run, scenarios) {
  return(list(
    stage = "finished",
    folder = state$project,
    version = state$version,
    folders = folders_table(),
    started = run$started,
    workers = run$workers,
    r_version = R.version.string,
    versions = log_versions(),
    other_packages = other_packages(run$packages),
    run = run,
    scenarios = run$scenarios,
    earlier = earlier_scenarios(state$results, scenarios),
    fake_real_data = isTRUE(state$fake_real_data) &&
      "real" %in% run$scenarios$scenario,
    saved = FALSE,
    contract = character(0),
    checks = NULL,
    expected = NULL,
    error = NULL,
    files = NULL,
    outputs = NULL,
    timing = list(run = run$seconds, save = NULL, outputs = NULL)
  ))
}

# The folders of the project for the run log: the name, the number of files
# of each folder at the last listing, and whether it was absent at
# initialize_project(). It holds no path.
folders_table <- function() {
  nm <- names(state$folders)
  counted <- sub("/.*$", "", state$data_files$file)
  return(data.frame(
    name = nm,
    files = vapply(
      nm,
      function(n) return(sum(counted == n)),
      integer(1),
      USE.NAMES = FALSE
    ),
    absent = nm %in% state$folders_absent
  ))
}

# The result of the run in one sentence, and a second sentence when
# make_outputs() stopped.
log_result <- function(log) {
  out <- log$outputs$failed
  result <- run_result(log)
  if (is.null(out)) {
    return(result)
  }
  return(paste0(
    result,
    " make_outputs() failed for the scenario ",
    md_inline(out$scenario),
    "."
  ))
}

run_result <- function(log) {
  stopped <- stopped_result(log)
  if (!is.null(stopped)) {
    return(stopped)
  }
  failed <- failed_result(log$run)
  if (!is.null(failed)) {
    return(failed)
  }
  if (length(log$contract) > 0) {
    return(paste0(
      length(log$contract),
      " failures of check_results(), so save_results() saved no results."
    ))
  }
  if (!is.null(log$error)) {
    return(
      "save_results() stopped with an error. The section Failures holds it."
    )
  }
  checks <- log$checks
  if (is.null(checks) || nrow(checks) == 0) {
    return("The run has no assertion.")
  }
  bad <- sum(!checks$pass %in% TRUE)
  if (bad == 0) {
    return(paste0("All ", nrow(checks), " assertions passed."))
  }
  return(paste0(bad, " of ", nrow(checks), " assertions failed."))
}

# The result of the run record `run` with a failed analysis, or NULL.
failed_result <- function(run) {
  failed <- run$failed
  skipped <- if (is.null(run$not_run)) 0L else run$not_run
  if (length(failed) == 0 && skipped == 0) {
    return(NULL)
  }
  return(paste0(
    length(failed),
    " of ",
    run$n_analyses,
    " analyses failed, so save_results() did not run.",
    if (skipped > 0) {
      paste0(" The run stopped early: ", skipped, " analyses did not run.")
    }
  ))
}

# The result of a run that stopped before its analyses could all end, or
# NULL: save_results() stopped in the checks of its arguments, or run_plan()
# stopped outside an analysis.
stopped_result <- function(log) {
  if (isTRUE(log$preflight)) {
    return(paste(
      "save_results() stopped before it saved anything. The section",
      "Failures names its error."
    ))
  }
  if (!is.null(log$run_error)) {
    return(paste(
      "run_plan() stopped outside an analysis, so save_results() did not",
      "run. The section Failures names its error."
    ))
  }
  return(NULL)
}

# The number of the other packages of the manifest, see loaded_packages():
# those of this R session and of the workers `workers`, without pbn, plnr
# and cohort.
other_packages <- function(workers = NULL) {
  return(length(setdiff(
    names(loaded_packages(workers)),
    c("pbn", "plnr", "cohort")
  )))
}

# The versions of pbn, plnr and cohort. A package that is not loaded has its
# installed version, and one that is not installed has "not installed".
log_versions <- function() {
  pkgs <- c("pbn", "plnr", "cohort")
  return(vapply(
    pkgs,
    function(p) {
      if (p %in% loadedNamespaces()) {
        return(as.character(getNamespaceVersion(p)))
      }
      return(tryCatch(
        as.character(utils::packageVersion(p)),
        error = function(e) return("not installed")
      ))
    },
    character(1)
  ))
}

# The scenario folders under `root` that are not in `scenarios` and hold a
# results.qs2, with the run_date of its manifest, sorted by name.
earlier_scenarios <- function(root, scenarios) {
  found <- if (dir.exists(root)) list.files(root) else character(0)
  found <- found[dir.exists(file.path(root, found))]
  found <- sort(setdiff(found, scenarios), method = "radix")
  found <- found[file.exists(file.path(root, found, "results.qs2"))]
  dates <- vapply(
    found,
    function(s) {
      x <- tryCatch(
        qs2::qs_read(file.path(root, s, "results.qs2")),
        error = function(e) return(NULL)
      )
      d <- if (is.list(x) && is.list(x$manifest)) x$manifest$run_date else NULL
      return(if (is_one_string(d)) d else "unknown")
    },
    character(1),
    USE.NAMES = FALSE
  )
  return(data.frame(scenario = found, run_date = dates))
}

# The results files of the scenarios `scenarios` under `root`, relative to
# the project, with their sizes. The folder sims_gitexclude/ is one row with
# its number of files and their total size.
files_written <- function(root, scenarios) {
  rel_root <- paste0(state$version, "_results")
  out <- list()
  for (s in scenarios) {
    for (f in c("check.csv", "results.qs2", gitexclude_file)) {
      path <- file.path(root, s, f)
      if (file.exists(path)) {
        out[[length(out) + 1]] <- data.frame(
          file = paste(rel_root, s, f, sep = "/"),
          size = format_size(file.size(path))
        )
      }
    }
    sims <- list.files(file.path(root, s, sims_folder), full.names = TRUE)
    if (length(sims) > 0) {
      out[[length(out) + 1]] <- data.frame(
        file = paste0(paste(rel_root, s, sims_folder, sep = "/"), "/"),
        size = paste0(
          length(sims),
          if (length(sims) == 1) " file, " else " files, ",
          format_size(sum(file.size(sims)))
        )
      )
    }
  }
  if (length(out) == 0) {
    return(data.frame(file = character(0), size = character(0)))
  }
  return(do.call(rbind, out))
}

# A size in bytes as kB or MB with one decimal.
format_size <- function(bytes) {
  if (bytes < 1e6) {
    return(sprintf("%.1f kB", bytes / 1e3))
  }
  return(sprintf("%.1f MB", bytes / 1e6))
}

# Adds the files and the seconds of make_outputs() to the log of the last run
# of this R session, and writes the log again. Without such a log, for
# example in a new R session, it writes nothing.
log_outputs <- function(done) {
  log <- state$run_log
  if (is.null(log)) {
    return(invisible(NULL))
  }
  log$outputs <- done
  log$timing$outputs <- done$seconds
  log$result <- log_result(log)
  write_run_log(log)
  return(invisible(NULL))
}

# Writes the log to <version>_results/run.md, and the text of its errors to
# <version>_results/run_errors_gitexclude.md when git ignores that file.
# Without an error, it deletes an old errors file. The errors file comes
# first, so run.md never names an error that is not written. Each file goes
# through a temporary file in the same folder that pbn renames. The state of
# the project keeps the log, so that make_outputs() can add to it.
write_run_log <- function(log) {
  root <- state$results
  create_folder(root)
  log$errors <- log_errors(log)
  log$errors_file <- errors_file_status(log$errors)
  errors_file <- file.path(root, errors_file_name)
  if (identical(log$errors_file, "written")) {
    write_lines_atomic(errors_lines(log, log$errors), errors_file)
  } else if (file.exists(errors_file)) {
    delete_paths(errors_file)
  }
  file <- file.path(root, "run.md")
  log$history <- history_rows(file, log)
  write_lines_atomic(run_log_lines(log), file)
  state$run_log <- log
  cat(
    "pbn: wrote the run log ",
    paste0(state$version, "_results/run.md"),
    ".\n",
    sep = ""
  )
  return(invisible(file))
}

# At the start of run_plan(), after initialize_project(): replaces run.md with
# a log that says the run started and has not finished, and deletes the
# errors file. The full log replaces it when save_results() ends, or when
# run_plan() stops on failed analyses. A run that ends in between, for
# example when R stops, leaves this log, which is then true. Its history
# holds the line "did not finish" for this run, which the full log replaces.
write_started_log <- function(scenarios, started, workers = NULL) {
  if (is.null(state$results)) {
    return(invisible(NULL))
  }
  root <- state$results
  create_folder(root)
  delete_paths(file.path(root, errors_file_name))
  log <- list(
    stage = "started",
    folder = state$project,
    version = state$version,
    started = started,
    workers = workers,
    scenarios = scenarios,
    earlier = earlier_scenarios(root, scenarios$scenario),
    fake_real_data = isTRUE(state$fake_real_data) &&
      "real" %in% scenarios$scenario,
    timing = list(run = NULL, save = NULL, outputs = NULL),
    result = paste(
      "The run has not finished. run_plan() started it, and no later step",
      "wrote this log again."
    )
  )
  file <- file.path(root, "run.md")
  log$history <- history_rows(file, log)
  write_lines_atomic(run_log_lines(log), file)
  state$run_log <- NULL
  return(invisible(NULL))
}

# The log of a call of save_results() that stopped in its checks of its
# arguments, with the error `e`. It holds the record of the last run of the
# plan, or no record when the plan has none.
write_preflight_log <- function(info, e) {
  if (is.null(state$results)) {
    return(invisible(NULL))
  }
  run <- if (is.environment(info)) info$run else NULL
  if (is.null(run)) {
    run <- empty_run(Sys.time())
  }
  log <- base_log(run, run$scenarios$scenario)
  log$preflight <- TRUE
  log$error <- conditionMessage(e)
  log$result <- log_result(log)
  write_run_log(log)
  return(invisible(NULL))
}

# The record of a run that pbn does not know: no scenario, no data, no
# analysis.
empty_run <- function(started) {
  return(list(
    started = started,
    workers = NA,
    seconds = NA_real_,
    scenarios = data.frame(
      scenario = character(0),
      sims = integer(0),
      analyses = integer(0)
    ),
    rows = NULL,
    exclusions = list(),
    data = data_records(list()),
    steps = step_records(list()),
    analyses = data.frame(
      scenario = character(0),
      kind = character(0),
      seconds = numeric(0),
      at = numeric(0)
    ),
    failed = character(0),
    packages = NULL,
    not_run = 0L,
    n_analyses = 0L
  ))
}

# The lines of the run log `lines` without the sections When, Timings and
# History, which change in every run. Two runs of the same code on the same
# inputs give the same lines. A CR at the end of a line goes.
run_log_stable <- function(lines) {
  lines <- sub("\r$", "", lines)
  heads <- grep("^## ", lines)
  drop <- logical(length(lines))
  varying <- c("## When", "## Timings", "## History")
  for (h in heads[lines[heads] %in% varying]) {
    after <- heads[heads > h]
    end <- if (length(after) > 0) after[1] - 1 else length(lines)
    drop[seq(h, end)] <- TRUE
  }
  return(lines[!drop])
}
