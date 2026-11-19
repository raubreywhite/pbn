#' Run a version and check the project
#'
#' `verify()` runs `<version>_run.R` in a new R session, and then checks the
#' project with the rules of [check_project()]. It prints a short verdict:
#' the exit status of the run, each failed assertion of `check.csv`, and
#' each FAIL and WARN finding.
#'
#' @param version The version, `"v"` and two digits, for example `"v01"`.
#' @param home The project folder. The run script runs with it as the
#'   working directory, as `Rscript v01_run.R` does when you start it there.
#' @details
#' The new R session is that of `callr::rscript()`, with the library paths
#' of this session. The output of the run shows as it comes.
#'
#' `verify()` reads only the `check.csv` files of this run. These are the
#' files `<version>_results/<scenario>/check.csv` that changed at or after
#' the start of the run. Before the run starts, `verify()` waits for the
#' next whole second, up to 1 second, and that second is the start. So a
#' `check.csv` of an earlier run is older than the start, also on a file
#' system that keeps the time of a change in whole seconds. The failed
#' assertions are the rows of these files with a value of `pass` that is not
#' `TRUE`.
#'
#' The verdict is FAIL when one of these is true:
#' - the run ends with a status other than 0.
#' - the run wrote no `check.csv`. Then it checked no fake scenario, for
#'   example because it stopped before [save_results()].
#' - a `check.csv` of the run has a failed assertion. This holds at each
#'   level of the rule `scenario_checks` in `pbn.yml`, also `off`.
#' - a rule of `check_project()` fails.
#'
#' Else the verdict is PASS. The last line names each reason of a FAIL. A
#' PASS of a run that did not write `<version>_results/real/results.qs2`
#' has the last line
#' ``Verdict: PASS on the fake scenarios only. The scenario `real` did not run.``
#' It covers the fake data only. Its verdict is still PASS. In a
#' non-interactive R session, `verify()` quits R with status 1 after a
#' verdict FAIL, as `check_project()` does.
#' @return Invisibly, a list with three elements. `status` is the exit
#'   status of the run. `assertions` is a data.frame of the failed
#'   assertions, with the columns `file`, `assertion`, `expected` and
#'   `observed`. `findings` holds the findings, as `check_project()` returns
#'   them.
#' @examplesIf interactive()
#' pbn::verify("v01")
#' @family checks
#' @export
verify <- function(version, home = ".") {
  result <- verify_result(version, home)
  cat(attr(result, "lines"), sep = "\n")
  if (!interactive() && identical(attr(result, "verdict"), "FAIL")) {
    quit(save = "no", status = 1)
  }
  attr(result, "lines") <- NULL
  attr(result, "verdict") <- NULL
  return(invisible(result))
}

# The value of verify(), with the lines of its verdict as the attribute
# `lines` and the verdict, PASS or FAIL, as the attribute `verdict`. The
# tests read it without the quit().
verify_result <- function(version, home) {
  check_version(version)
  script <- paste0(version, "_run.R")
  if (!file.exists(file.path(home, script))) {
    stop(
      "verify() needs ",
      script,
      " in ",
      home,
      ". Give the folder of the project as `home`.",
      call. = FALSE
    )
  }
  rules <- read_rules(home)
  start <- wait_for_next_second()
  run <- callr::rscript(
    normalizePath(file.path(home, script)),
    cmdargs = character(0),
    wd = home,
    show = TRUE,
    stderr = "2>&1",
    fail_on_status = FALSE
  )
  files <- run_check_files(home, version, start)
  real_ran <- real_results_written(home, version, start)
  assertions <- failed_assertions(home, files)
  report <- project_report(home, rules)
  findings <- attr(report, "findings")
  open <- findings[findings$status %in% c("FAIL", "WARN"), , drop = FALSE]
  n_fail <- sum(open$status == "FAIL")
  reasons <- c(
    if (run$status != 0) paste("the run ended with status", run$status),
    if (length(files) == 0) "the run wrote no check.csv",
    if (nrow(assertions) > 0) "an assertion failed",
    if (n_fail > 0) "a rule of check_project() failed"
  )
  verdict <- if (length(reasons) > 0) "FAIL" else "PASS"
  lines <- c(
    paste0(
      "verify(",
      version,
      "): ",
      script,
      " ended with status ",
      run$status,
      "."
    ),
    paste0(
      "check.csv files of this run: ",
      length(files),
      if (length(files) == 0) {
        ". The run wrote no check of a fake scenario."
      } else {
        "."
      }
    ),
    paste0("Failed assertions: ", nrow(assertions), "."),
    paste0(
      "  ",
      assertions$file,
      ": ",
      assertions$assertion,
      ": expected ",
      assertions$expected,
      ", observed ",
      assertions$observed,
      recycle0 = TRUE
    ),
    paste0(
      "check_project(): ",
      n_fail,
      " FAIL, ",
      sum(open$status == "WARN"),
      " WARN."
    ),
    trimws(paste0(
      open$status,
      "  ",
      open$rule,
      "  ",
      open$detail,
      recycle0 = TRUE
    )),
    verdict_line(verdict, reasons, real_ran)
  )
  out <- list(status = run$status, assertions = assertions, findings = findings)
  attr(out, "lines") <- lines
  attr(out, "verdict") <- verdict
  return(out)
}

# The last line of verify(): the verdict, the reasons of a FAIL, and for a
# PASS whether the scenario real ran in this run.
verdict_line <- function(verdict, reasons, real_ran) {
  if (verdict == "FAIL") {
    return(paste0("Verdict: FAIL, because ", and_list(reasons), "."))
  }
  if (!real_ran) {
    return(paste(
      "Verdict: PASS on the fake scenarios only. The scenario `real` did",
      "not run."
    ))
  }
  return("Verdict: PASS")
}

# TRUE when the run wrote <version>_results/real/results.qs2 in `home` at or
# after the time `since`, so the scenario real ran in this run.
real_results_written <- function(home, version, since) {
  file <- file.path(home, paste0(version, "_results"), "real", "results.qs2")
  return(file.exists(file) && file.mtime(file) >= since)
}

# The files <version>_results/<scenario>/check.csv in `home` that changed at
# or after the time `since`, relative to `home` and sorted in the C locale.
run_check_files <- function(home, version, since) {
  folder <- paste0(version, "_results")
  files <- list.files(
    file.path(home, folder),
    pattern = "^check\\.csv$",
    recursive = TRUE
  )
  files <- file.path(
    folder,
    files[dirname(files) != "." & !grepl("/", dirname(files), fixed = TRUE)]
  )
  fresh <- file.mtime(file.path(home, files)) >= since
  return(sort(files[fresh], method = "radix"))
}

# The rows of the check.csv files `files`, relative to `home`, with a value
# of pass that is not TRUE, as a data.frame with `file`, `assertion`,
# `expected` and `observed`, all text. A file that R cannot read, or that
# has no column pass, gives no row.
failed_assertions <- function(home, files) {
  out <- list(data.frame(
    file = character(0),
    assertion = character(0),
    expected = character(0),
    observed = character(0)
  ))
  for (f in files) {
    found <- read_check_csv(file.path(home, f))
    bad <- !found$pass %in% TRUE
    out[[f]] <- data.frame(
      file = rep(f, sum(bad)),
      assertion = csv_column(found, "assertion")[bad],
      expected = csv_column(found, "expected")[bad],
      observed = csv_column(found, "observed")[bad]
    )
  }
  return(do.call(rbind, unname(out)))
}

# The check.csv `file` as a data.frame of text, or a data.frame with no row
# when R cannot read it or it has no column pass.
read_check_csv <- function(file) {
  found <- tryCatch(
    utils::read.csv(file, stringsAsFactors = FALSE, colClasses = "character"),
    error = function(e) return(NULL)
  )
  if (!is.data.frame(found) || !"pass" %in% names(found)) {
    return(data.frame(pass = character(0)))
  }
  found$pass <- found$pass == "TRUE"
  return(found)
}

# The column `name` of the data.frame `x` as text, or NA for each row when
# `x` has no such column.
csv_column <- function(x, name) {
  if (!name %in% names(x)) {
    return(rep(NA_character_, nrow(x)))
  }
  return(as.character(x[[name]]))
}

# Waits until the next whole second, plus 10 ms, and returns that second as
# a POSIXct. Each file that changed before the call then has a time of
# change before the value, also on a file system with whole seconds. Each
# file that changes after the call has a time at or after it.
wait_for_next_second <- function() {
  now <- as.numeric(Sys.time())
  start <- floor(now) + 1
  Sys.sleep(start - now + 0.01)
  return(as.POSIXct(start, origin = "1970-01-01"))
}
