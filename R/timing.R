# The timings of a run. pbn measures each one with proc.time() in the R
# session that does the work. The timings go to the registry of the plan and
# to the run log, and never to the value of run_plan(), to a results file or
# to a cache key.

# The elapsed seconds since `start`, a value of proc.time()[["elapsed"]].
elapsed_since <- function(start) {
  return(proc.time()[["elapsed"]] - start)
}

# Empties the log of the timings of the plan before a run.
reset_log <- function(info) {
  assign("pending", list(), envir = info$log)
  return(invisible(NULL))
}

# `r`, the value of p$run_all(), with each pbn_timed value replaced by the
# value of its analysis. `seconds` holds the seconds of each analysis, NA
# where there are none. `data` holds the records of the data that came back
# with the values, and those still in `log`, the log of this R session. A
# value holds the records of its own R session only, see take_records(), so
# each record is there once.
unwrap_timed <- function(r, log) {
  seconds <- rep(NA_real_, length(r))
  parts <- vector("list", length(r))
  for (i in seq_along(r)) {
    x <- r[[i]]
    if (inherits(x, "pbn_timed")) {
      r[i] <- list(x[["value"]])
      seconds[i] <- x[["seconds"]]
      parts[[i]] <- x[["data"]]
    }
  }
  data <- c(do.call(c, parts), log[["pending"]])
  return(list(r = r, seconds = seconds, data = data))
}

# One row for each record of the data: the data, its scenario and
# simulation, whether it came from the cache or failed, its cache line and
# its elapsed seconds.
data_records <- function(records) {
  field <- function(name, type) {
    return(vapply(records, function(d) return(d[[name]]), type))
  }
  return(data.frame(
    data = field("data", character(1)),
    scenario = field("scenario", character(1)),
    sim = field("sim", integer(1)),
    reused = field("reused", logical(1)),
    failed = field("failed", logical(1)),
    line = field("line", character(1)),
    seconds = field("seconds", numeric(1))
  ))
}

# One row for each step of each record of the data, with its elapsed
# seconds. A record from the cache has the one step "read the cache". The
# rows are built as vectors, because a run can hold thousands of records.
step_records <- function(records) {
  steps <- lapply(records, function(d) {
    if (isTRUE(d[["reused"]])) {
      return(c("read the cache" = d[["seconds"]]))
    }
    return(d[["steps"]])
  })
  n <- lengths(steps)
  field <- function(name) {
    return(vapply(records, function(d) return(d[[name]]), character(1)))
  }
  seconds <- unlist(steps)
  return(data.frame(
    data = rep(field("data"), n),
    scenario = rep(field("scenario"), n),
    step = if (is.null(seconds)) character(0) else names(seconds),
    seconds = if (is.null(seconds)) numeric(0) else unname(seconds)
  ))
}

# The facts of one run of run_plan() for the run log: the scenarios, the
# rows and the exclusions of the data, the cache, the failed analyses and
# the timings. `taken` is the value of unwrap_timed(), or NULL.
run_record <- function(p, info, r, taken, workers, start, started) {
  index <- analysis_index(p, info)
  kinds <- !index$hidden
  seconds <- if (is.null(taken)) rep(NA_real_, nrow(index)) else taken$seconds
  records <- if (is.null(taken)) list() else taken$data
  return(list(
    started = started,
    workers = workers,
    seconds = elapsed_since(start),
    scenarios = plan_scenarios(index),
    rows = data_rows(r, index),
    exclusions = first_exclusions(r, index),
    data = data_records(records),
    steps = step_records(records),
    analyses = data.frame(
      scenario = index$scenario[kinds],
      kind = index$analysis[kinds],
      seconds = seconds[kinds]
    ),
    failed = failed_analyses(r),
    n_analyses = length(r)
  ))
}

# The scenarios of the plan in the order of the plan, with the number of
# simulations and of analyses, without the analyses that count the rows.
plan_scenarios <- function(index) {
  s <- unique(index$scenario)
  count <- function(f) return(vapply(s, f, integer(1), USE.NAMES = FALSE))
  return(data.frame(
    scenario = s,
    sims = count(function(x) return(max(index$sim[index$scenario == x]))),
    analyses = count(function(x) {
      return(sum(index$scenario == x & !index$hidden))
    })
  ))
}

# The rows of each data element for each scenario and simulation, from the
# analyses that count the rows: the rows of the data, the rows given to the
# cohort and the rows after its exclusions. A failed count has no row. `r`
# and `index` have one element for each analysis, in the same order.
data_rows <- function(r, index) {
  hidden <- which(index$hidden)
  ok <- vapply(
    hidden,
    function(i) return(is.numeric(r[[i]]) && !is.null(names(r[[i]]))),
    logical(1)
  )
  hidden <- hidden[ok]
  if (length(hidden) == 0) {
    return(NULL)
  }
  counts <- lapply(hidden, function(i) return(cohort_counts(r[[i]])))
  n <- lengths(r[hidden])
  return(data.frame(
    data = unlist(lapply(r[hidden], names)),
    scenario = rep(index$scenario[hidden], n),
    sim = rep(index$sim[hidden], n),
    n_rows = unlist(lapply(r[hidden], function(x) return(unname(as.integer(x))))),
    n_before = unname(unlist(lapply(counts, function(x) return(x$before)))),
    n_after = unname(unlist(lapply(counts, function(x) return(x$after))))
  ))
}

# The exclusion table of simulation 1 of each scenario, named by the
# scenario. NULL for a scenario without a cohort or whose count failed.
first_exclusions <- function(r, index) {
  out <- list()
  for (i in which(index$hidden & index$sim == 1)) {
    tables <- attr(r[[i]], "exclusions", exact = TRUE)
    out[index$scenario[i]] <- list(exclusions_element(tables)$exclusions)
  }
  return(out)
}
