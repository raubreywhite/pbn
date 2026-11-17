# The section History of the run log: one line for each run of this
# version, oldest first. pbn reads the lines from the run.md on the disk,
# and adds or replaces the line of this run. So the history belongs to the
# results folder of the version, and a deleted run.md starts it again.

# The names of the columns of the table of the history.
history_columns <- c(
  "Started",
  "Result",
  "Scenarios",
  "Workers",
  "run_plan()",
  "save_results()",
  "make_outputs()",
  "pbn"
)

# The rows of the history of the log `log`: the rows that pbn can read in
# the run log `file`, with the row of this run added at the end. The row of
# this run replaces the last row when this R session wrote that row for the
# same run, for example the row "did not finish" of the start of the run.
# So each run has one row.
history_rows <- function(file, log) {
  old <- read_history(file)
  row <- history_row(log)
  n <- nrow(old)
  same <- n > 0 &&
    identical(state$history_started, log$started) &&
    identical(old$Started[n], row$Started)
  if (same) {
    old <- old[-n, , drop = FALSE]
  }
  state$history_started <- log$started
  out <- rbind(old, row)
  rownames(out) <- NULL
  return(out)
}

# The row of the history for the log `log`, as a data.frame of strings. A
# step that did not run has an empty cell.
history_row <- function(log) {
  t <- log$timing
  wall <- function(x) return(if (length(x) == 0) "" else secs(sum(x)))
  return(data.frame(
    Started = format(log$started, "%Y-%m-%d %H:%M:%S"),
    Result = history_result(log),
    Scenarios = history_scenarios(log$scenarios$scenario),
    Workers = if (is.null(log$workers)) "" else as.character(log$workers),
    `run_plan()` = wall(t$run),
    `save_results()` = wall(t$save[["total"]]),
    `make_outputs()` = wall(t$outputs),
    pbn = as.character(getNamespaceVersion("pbn")),
    check.names = FALSE
  ))
}

# The scenarios of a run for the history: their number, or their names when
# the run had a part of the scenarios of CFG$scenarios, from
# scenarios(only = ).
history_scenarios <- function(scenarios) {
  every <- state$all_scenarios
  if (!is.null(every) && !all(every %in% scenarios)) {
    return(paste(scenarios, collapse = ", "))
  }
  return(as.character(length(scenarios)))
}

# The result of the log `log` in a few words, for the history.
history_result <- function(log) {
  out <- history_outcome(log)
  if (!is.null(log$outputs$failed)) {
    out <- paste0(out, ", make_outputs() failed")
  }
  return(out)
}

history_outcome <- function(log) {
  if (identical(log$stage, "started")) {
    return("did not finish")
  }
  if (isTRUE(log$preflight)) {
    return("save_results() stopped")
  }
  if (!is.null(log$run_error)) {
    return("run_plan() stopped outside an analysis")
  }
  failed <- length(log$run$failed)
  skipped <- if (is.null(log$run$not_run)) 0L else log$run$not_run
  if (failed > 0 || skipped > 0) {
    return(paste0(
      failed,
      " of ",
      log$run$n_analyses,
      " analyses failed",
      if (skipped > 0) ", stopped early"
    ))
  }
  return(history_saved(log))
}

# The result of a log of save_results() for the history.
history_saved <- function(log) {
  if (length(log$contract) > 0) {
    return(paste(length(log$contract), "failures of check_results()"))
  }
  if (!is.null(log$error)) {
    return("save_results() stopped")
  }
  checks <- log$checks
  if (is.null(checks) || nrow(checks) == 0) {
    return("No assertion")
  }
  bad <- sum(!checks$pass %in% TRUE)
  if (bad == 0) {
    return(paste("All", nrow(checks), "assertions passed"))
  }
  return(paste(bad, "of", nrow(checks), "assertions failed"))
}

# The rows of the section History of the run log `file`, as a data.frame
# of strings with the columns history_columns. No file or no section gives
# no row. A line of the table that pbn cannot read is left out, and pbn
# prints one line. It never stops the run.
read_history <- function(file) {
  empty <- as.data.frame(
    stats::setNames(
      rep(list(character(0)), length(history_columns)),
      history_columns
    ),
    check.names = FALSE
  )
  lines <- tryCatch(
    if (file.exists(file)) readLines(file, warn = FALSE) else character(0),
    error = function(e) return(NULL)
  )
  if (is.null(lines)) {
    history_unread(0L, 1L)
    return(empty)
  }
  lines <- sub("\r$", "", lines)
  start <- match("## History", lines)
  if (is.na(start)) {
    return(empty)
  }
  section <- lines[-seq_len(start)]
  end <- grep("^## ", section)
  if (length(end) > 0) {
    section <- section[seq_len(end[1] - 1)]
  }
  table <- section[startsWith(section, "|")]
  # The header and the row of dashes are not runs.
  table <- table[!grepl("^\\|[-:| ]*\\|$", table)]
  table <- table[!startsWith(table, "| Started ")]
  cells <- lapply(table, history_cells)
  ok <- vapply(cells, function(x) return(!is.null(x)), logical(1))
  if (any(!ok)) {
    history_unread(sum(ok), sum(!ok))
  }
  if (!any(ok)) {
    return(empty)
  }
  rows <- do.call(rbind, cells[ok])
  out <- as.data.frame(rows, stringsAsFactors = FALSE)
  names(out) <- history_columns
  rownames(out) <- NULL
  return(out)
}

# Prints the one line that says that pbn could not read `bad` lines of the
# section History, and kept `good` lines.
history_unread <- function(good, bad) {
  rel <- paste0(state$version, "_results/run.md")
  cat(
    "pbn: pbn could not read ",
    if (good > 0) {
      paste0(
        bad,
        if (bad == 1) " line" else " lines",
        " of the section History of ",
        rel,
        ", and the history leaves ",
        if (bad == 1) "it" else "them",
        " out.\n"
      )
    } else {
      paste0("the section History of ", rel, ", so the history starts again.\n")
    },
    sep = ""
  )
  return(invisible(NULL))
}

# The cells of one row `line` of the table of the history, or NULL when
# the row does not have the columns of the history and a time of its start.
history_cells <- function(line) {
  inner <- sub("^\\|", "", sub("\\|[[:space:]]*$", "", line))
  cells <- strsplit(inner, "(?<!\\\\)\\|", perl = TRUE)[[1]]
  cells <- trimws(gsub("\\|", "|", cells, fixed = TRUE))
  if (length(cells) != length(history_columns)) {
    return(NULL)
  }
  if (
    !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$", cells[1])
  ) {
    return(NULL)
  }
  return(cells)
}

# The section History of the log `log`, from log$history.
md_history <- function(log) {
  rows <- log$history
  if (is.null(rows)) {
    return(character(0))
  }
  return(c(
    "## History",
    "",
    paste0(
      "One line for each run of this version, oldest first. The columns of ",
      "the steps give their wall seconds, and an empty cell means that the ",
      "step did not run. pbn keeps the history in this file, so a deleted ",
      "run.md starts it again."
    ),
    "",
    md_table(
      rows,
      right = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
    ),
    ""
  ))
}
