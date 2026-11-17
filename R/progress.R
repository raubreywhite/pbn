#' Show the progress inside one analysis
#'
#' `progress()` shows how far a long analysis is, for example a bootstrap
#' loop, on the line of its scenario in the display of [run_plan()]. Outside
#' a run of `run_plan()` it only checks its arguments.
#'
#' @param done The number of steps done, one number of 0 or more.
#' @param total The number of steps of the analysis, one number of 1 or more.
#' @param label Optional. A short text, for example `"bootstrap"`.
#' @details
#' Call `progress()` in the loop of an action function. The line of the
#' scenario then shows the kind of the analysis, the values of its extra
#' dimensions, `label`, `done`/`total` and the percent. When at least 3 steps
#' and at least 10% of the steps are done, it also shows the time left. That
#' time comes from the rate of the steps of this analysis. The count of the finished
#' analyses of the scenario does not change.
#'
#' After the first failed analysis of the run, no new analysis starts, and
#' an analysis that runs goes on to its end. Its calls of `progress()` still
#' show.
#'
#' `progress()` drops a call less than 0.5 seconds after the last one that it
#' showed. So a call in each pass of a fast loop costs little. It checks its
#' arguments at each call, also outside `run_plan()`, so a wrong argument
#' stops the analysis however fast it runs.
#'
#' Apart from the check of its arguments, `progress()` does nothing in these
#' cases:
#' - outside `run_plan()`, for example when you step through an analysis by
#'   hand, or call the action function yourself.
#' - in a data function.
#' - in a forked child process, for example inside [parallel::mclapply()].
#'   Several children would write to the one connection of the worker at the
#'   same time. Call `progress()` in the loop of the analysis itself, for
#'   example between chunks of the replicates, as the example shows.
#' @return `NULL`, invisibly.
#' @examples
#' fit_boot <- function(data, argset) {
#'   n_boot <- 200
#'   chunks <- split(seq_len(n_boot), ceiling(seq_len(n_boot) / 50))
#'   est <- numeric(0)
#'   for (chunk in chunks) {
#'     # With forks, for example parallel::mclapply(chunk, ...), call
#'     # progress() here, between the chunks.
#'     est <- c(est, vapply(chunk, function(b) {
#'       return(mean(sample(data$d$x, replace = TRUE)))
#'     }, numeric(1)))
#'     pbn::progress(length(est), n_boot, "bootstrap")
#'   }
#'   return(data.frame(estimate = mean(data$d$x), se = stats::sd(est)))
#' }
#' # Outside run_plan(), progress() does nothing, so this call returns the
#' # value at once.
#' fit_boot(list(d = data.frame(x = stats::rnorm(20))), list())
#' @family plan
#' @seealso `vignette("pbn")`, whose section Progress shows the display.
#' @export
progress <- function(done, total, label = NULL) {
  check_progress_args(done, total, label)
  run <- getOption("pbn.run")
  if (is.null(run)) {
    return(invisible(NULL))
  }
  argset <- run$cur_argset
  if (is.null(argset)) {
    return(invisible(NULL))
  }
  now <- proc.time()[["elapsed"]]
  # A forked child has the pid of another session than the one of `run`.
  if (
    (run$cur_wrote && now - run$cur_last < progress_interval) ||
      Sys.getpid() != run$here
  ) {
    return(invisible(NULL))
  }
  run$cur_last <- now
  run$cur_wrote <- TRUE
  elapsed <- now - run$cur_start
  run$progressor(
    amount = 0,
    pbn_detail = list(
      scenario = argset[["scenario"]],
      key = as.character(argset[["index_analysis"]]),
      text = progress_text(run, argset, done, total, label),
      share = min(done / total, 1),
      t0 = as.numeric(Sys.time()) - elapsed,
      left = progress_left(elapsed, done, total)
    )
  )
  return(invisible(NULL))
}

# progress() shows a call at most every this many seconds.
progress_interval <- 0.5

check_progress_args <- function(done, total, label) {
  if (!is_one_number(done, 0) || !is_one_number(total, 1)) {
    stop(
      "pbn::progress() needs `done`, one number of 0 or more, and `total`, ",
      "one number of 1 or more.",
      call. = FALSE
    )
  }
  if (!is.null(label) && !is_one_string(label)) {
    stop("The `label` of pbn::progress() must be one string.", call. = FALSE)
  }
  return(invisible(NULL))
}

# TRUE when `x` is one number, not NA, of `least` or more.
is_one_number <- function(x, least) {
  return(is.numeric(x) && length(x) == 1 && !is.na(x) && x >= least)
}

# The text of a call of progress(): the kind, the values of the extra
# dimensions, the simulation of a scenario with more than one, `label`,
# done/total and the percent, for example "sem (ppv6, main) bootstrap
# 1240/2000 62%".
progress_text <- function(run, argset, done, total, label) {
  kind <- argset[["analysis"]]
  inner <- vapply(
    run$dims[[kind]],
    function(d) return(format(argset[[d]])),
    character(1)
  )
  s <- argset[["scenario"]]
  if (isTRUE(run$sims[s] > 1)) {
    inner <- c(inner, paste("sim", argset[["sim"]]))
  }
  out <- kind
  if (length(inner) > 0) {
    out <- paste0(out, " (", paste(inner, collapse = ", "), ")")
  }
  if (!is.null(label)) {
    out <- paste(out, label)
  }
  return(paste0(
    out,
    " ",
    format(done),
    "/",
    format(total),
    " ",
    floor(100 * min(done / total, 1)),
    "%"
  ))
}

# The seconds left from the rate of the steps done so far: after `elapsed`
# seconds, `done` of `total` steps are done. NA before at least 3 steps and
# at least 10% of the steps are done, and after the last step.
progress_left <- function(elapsed, done, total) {
  if (done < 3 || done < 0.1 * total || done >= total) {
    return(NA_real_)
  }
  return(elapsed * (total - done) / done)
}
