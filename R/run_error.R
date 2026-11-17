# The end of run_plan() after an error outside the wrappers of pbn, for
# example of a worker that stopped, or of plnr. on.exit() of run_on_cluster()
# already set the future plan and the foreach backend back, and stopped the
# cluster.

# Writes the run log of the run as failed with the error `e`, prints the
# whole error, and stops with a message that holds the error cut below 900
# bytes. No value and no timing of an analysis came back, so the log holds
# the data records of this R session only.
stop_on_run_error <- function(p, info, e, workers, start, started) {
  msg <- conditionMessage(e)
  index <- analysis_index(p, info)
  r <- stats::setNames(vector("list", nrow(index)), index$name)
  taken <- list(
    r = r,
    seconds = rep(NA_real_, nrow(index)),
    data = info$log[["pending"]]
  )
  info$run <- run_record(p, info, r, taken, workers, start, started)
  write_failed_run(info$run, error = msg)
  cat("pbn: run_plan() stopped outside an analysis:\n", msg, "\n", sep = "")
  utils::flush.console()
  stop(run_error_message(msg), call. = FALSE)
}

# The message of stop_on_run_error(), with the error `msg` cut so that the
# message holds at most `max_bytes` bytes.
run_error_message <- function(msg, max_bytes = 899L) {
  fixed <- paste0(
    "run_plan() stopped outside an analysis: a worker stopped, or plnr or ",
    "the backend of the workers failed. pbn printed the whole error above, ",
    "and saved nothing. The results files of an earlier run stay as they ",
    "are. The error is: \""
  )
  end <- "\"."
  room <- max_bytes - nchar(fixed, "bytes") - nchar(end, "bytes")
  return(paste0(fixed, cut_bytes(msg, room), end))
}
