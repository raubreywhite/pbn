# The run context of run_plan(): one environment that each wrapper of the
# plan holds as .pbn_run while run_plan() runs, and NULL otherwise. A future
# of the workers serialises it with the plan, so each future has its own
# copy. The functions run_enter(), run_begin(), run_done() and
# take_records() run where the analyses run, also on a worker without pbn.
# Their environment is the base environment.

# The run context of one run. `index` is analysis_index() of the plan,
# `progressor` the progressr progressor of the run, and `stop_path` the path
# of the folder that the first failed analysis creates, see run_failed().
run_context <- function(info, index, workers, progressor, stop_path) {
  run <- new.env(parent = emptyenv())
  # The R session that set up its copy of the context. run_enter() sets up
  # each other R session once.
  run$here <- Sys.getpid()
  run$progressor <- progressor
  run$stop_path <- stop_path
  run$send <- run_send
  run$batch_add <- run_batch_add
  run$reset_state <- run_reset
  # A session sends the count of its finished analyses at most every this
  # many seconds, and after each analysis of flush_points().
  run$every <- 0.5
  run$flush <- flush_points(info, index, workers)
  run$dims <- info$dims
  # The kind of random number generator of the run. run_rng() sets it again
  # before each seed.
  run$rng <- RNGkind()
  sims <- tapply(index$sim, index$scenario, max)
  run$sims <- stats::setNames(as.integer(sims), names(sims))
  env <- state$env
  run$env <- if (is.environment(env)) env else emptyenv()
  run$checked <- checked_names(env)
  run$fingerprint <- objects_fingerprint
  run$changed <- objects_changed
  run$describe <- object_change
  run$change_text <- objects_change_text
  run$globals <- NULL
  run$global_names <- character(0)
  if (workers > 1) {
    found <- worker_globals(env)
    run$globals <- found$env
    run$global_names <- found$names
  }
  run_reset(run)
  return(run)
}

# Empties the state of one R session in the run context `run`: the batch of
# finished analyses not sent yet, with its scenario `bs`, its count `bn`,
# its failed analyses `bf` and its start `t0`, the current analysis, the
# fingerprint of the objects, and whether this session knows that the run
# stops, see run_stopping().
run_reset <- function(run) {
  run$sent <- -Inf
  run$bs <- NULL
  run$bn <- 0
  run$bf <- 0
  run$stopped <- FALSE
  run$stop_checked <- -Inf
  run$t0 <- NULL
  run$ended <- character(0)
  run$cur_argset <- NULL
  run$cur_start <- NA_real_
  run$cur_last <- -Inf
  run$cur_wrote <- FALSE
  run$fp <- NULL
  run$gl <- NULL
  return(invisible(run))
}
environment(run_reset) <- baseenv()

# Sets .pbn_run in the environment of each wrapper of the plan.
set_run <- function(info, run) {
  for (e in info$wrappers) {
    assign(".pbn_run", run, envir = e)
  }
  return(invisible(NULL))
}

# TRUE for each analysis of the plan after which a session sends its count
# at once. With 1 worker these are the last analysis that counts of each
# scenario, so the display shows its end at once. With more workers, a copy
# of the run context lives as long as one future, and a count that it did
# not send is lost. plnr runs the analyses of one scenario and simulation in
# one task, when each data function with arguments has scenario and sim, and
# a future runs whole tasks. So these are the last analysis that counts of
# each scenario and simulation. A plan without a data function with
# arguments runs each analysis as a task of its own, so each one sends.
flush_points <- function(info, index, workers) {
  n <- nrow(index)
  counted <- which(!index$hidden)
  if (workers == 1) {
    key <- index$scenario[counted]
  } else {
    keyed <- Filter(function(fn) return(length(key_fields(fn)) > 0), info$data)
    if (length(keyed) == 0) {
      return(rep(TRUE, n))
    }
    key <- paste(index$scenario, index$sim)[counted]
  }
  out <- rep(FALSE, n)
  out[counted[!duplicated(key, fromLast = TRUE)]] <- TRUE
  return(out)
}

# The number of tasks of plnr that one future of the workers runs. plnr has
# one task for each scenario and simulation when each data function with
# arguments has scenario and sim, and one for each analysis when no data
# function has arguments. pbn then gives each worker about 4 futures. A
# future serialises the whole plan, so fewer futures cost less. For other
# plans each task is one future.
future_chunk_size <- function(info, index, workers) {
  keyed <- Filter(function(fn) return(length(key_fields(fn)) > 0), info$data)
  if (length(keyed) == 0) {
    units <- nrow(index)
  } else if (
    all(vapply(
      keyed,
      function(fn) return(all(c("scenario", "sim") %in% key_fields(fn))),
      logical(1)
    ))
  ) {
    units <- length(unique(paste(index$scenario, index$sim)))
  } else {
    return(1L)
  }
  return(max(1L, as.integer(units %/% (workers * 4))))
}

# The objects that each worker gets in its global environment: every object
# of the sourced environment `env` other than a plnr plan and .Random.seed.
# `env` holds them. For the global environment it is a new environment with
# the same objects, because R serialises the global environment as a
# reference to the global environment of the worker.
worker_globals <- function(env) {
  if (!is.environment(env)) {
    return(list(env = NULL, names = character(0)))
  }
  objects <- ls(env, all.names = TRUE)
  keep <- vapply(
    objects,
    function(o) return(!inherits(get(o, envir = env), "Plan")),
    logical(1)
  )
  objects <- setdiff(objects[keep], ".Random.seed")
  if (identical(env, globalenv())) {
    holder <- new.env(parent = emptyenv())
    for (o in objects) {
      assign(o, get(o, envir = env), envir = holder)
    }
    env <- holder
  }
  return(list(env = env, names = objects))
}

# Called by each wrapper at its start while run_plan() runs. The first call
# in another R session than the main one, that is the first call of each
# future of a worker, sets up that session:
# - it empties the state of `run`, which the copy brought from the main
#   session.
# - it puts the objects of the sourced environment into the global
#   environment. A future starts with an empty global environment.
# - it removes the option mc.cores, which a future sets to 1. A worker of
#   pbn had no mc.cores before, and parallel::mclapply() then uses 2 cores.
# - it sets the option pbn.run, which pbn::progress() reads.
# A data wrapper, `data` TRUE, starts the clock `t0` of an empty batch, and
# forgets the names of the global environment, because a data function MAY
# add one there.
run_enter <- function(run, data = FALSE) {
  if (is.null(run)) {
    return(invisible(NULL))
  }
  if (!identical(run$here, Sys.getpid())) {
    run$here <- Sys.getpid()
    run$reset_state(run)
    if (!is.null(run$globals)) {
      list2env(mget(run$global_names, envir = run$globals), envir = globalenv())
    }
    options(mc.cores = NULL, pbn.run = run)
  }
  if (data) {
    if (run$bn == 0 && is.null(run$t0)) {
      run$t0 <- as.numeric(Sys.time())
    }
    run$gl <- NULL
  }
  return(invisible(NULL))
}
environment(run_enter) <- baseenv()

# Marks the analysis with the argset `argset`, which started at the elapsed
# time `start`, as the current analysis of `run`, for pbn::progress().
run_begin <- function(run, argset, start) {
  if (is.null(run)) {
    return(invisible(NULL))
  }
  run$cur_argset <- argset
  run$cur_start <- start
  run$cur_wrote <- FALSE
  return(invisible(NULL))
}
environment(run_begin) <- baseenv()

# Counts the analysis with the argset `argset` as finished, at the elapsed
# time `now`, and as failed with `failed`. A batch holds the analyses of one
# scenario. The session sends it with run_send() when the analysis failed or
# is a flush point, when it called pbn::progress(), when `run$every` seconds
# passed since the last count, or before an analysis of another scenario.
# `t0` of a batch is the start of its data, or of its first analysis, or the
# time now when neither is known, for example after its data failed.
run_done <- function(run, argset, now, failed = FALSE) {
  if (is.null(run)) {
    return(invisible(NULL))
  }
  s <- argset[["scenario"]]
  i <- argset[["index_analysis"]]
  wrote <- run$cur_wrote
  if (wrote) {
    run$ended <- c(run$ended, structure(as.character(i), names = s))
  }
  run$batch_add(run, s, now, failed)
  run$cur_argset <- NULL
  run$cur_start <- NA_real_
  run$cur_wrote <- FALSE
  # i is the position of the analysis, so a flush point is TRUE there.
  if (
    failed || wrote || isTRUE(run$flush[i]) || now - run$sent >= run$every
  ) {
    run$send(run, now)
  }
  return(invisible(NULL))
}
environment(run_done) <- baseenv()

# Adds the analysis of the scenario `s` that ended at the elapsed time
# `now`, and failed with `failed`, to the batch of `run`. A batch of another
# scenario goes first. The start `t0` of a new batch is the start of its
# data, or of the current analysis, whichever came first, or the time now
# when neither is known, for example after the data failed.
run_batch_add <- function(run, s, now, failed) {
  if (run$bn > 0 && !identical(run$bs, s)) {
    run$send(run, now)
  }
  if (run$bn == 0) {
    run$bs <- s
    begun <- c(run$t0, as.numeric(Sys.time()) - (now - run$cur_start))
    begun <- begun[!is.na(begun)]
    run$t0 <- if (length(begun) > 0) min(begun) else as.numeric(Sys.time())
  }
  run$bn <- run$bn + 1
  if (failed) {
    run$bf <- run$bf + 1
  }
  return(invisible(NULL))
}
environment(run_batch_add) <- baseenv()

# Sends the batch of `run` with its progressor, at the elapsed time `now`,
# and empties it. A count is the number of analyses of the scenario, named
# by it, with the number of them that failed, `t0`, and the analyses whose
# text of pbn::progress() ends, named by their scenario. The display counts
# the analyses itself, and an amount of 0 keeps the cost of a progression in
# progressr fixed.
run_send <- function(run, now) {
  done <- run$bn
  names(done) <- run$bs
  failed <- run$bf
  names(failed) <- run$bs
  if (is.function(run$progressor)) {
    run$progressor(
      amount = 0,
      pbn_done = done,
      pbn_failed = failed,
      pbn_t0 = run$t0,
      pbn_ended = run$ended
    )
  }
  run$bn <- 0
  run$bf <- 0
  run$bs <- NULL
  run$t0 <- NULL
  run$ended <- character(0)
  run$sent <- now
  return(invisible(NULL))
}
environment(run_send) <- baseenv()

# The stop of a run at its first failed analysis. run_plan() saves nothing
# when an analysis fails, so no later analysis can change the result of the
# run. The first failed analysis, in this R session or on a worker, creates
# the folder `run$stop_path`. All R sessions of the run are on this
# computer, so each of them sees it.

# Marks the run `run` as stopped after the analysis with the argset `argset`
# failed: in this R session at once, and for the other R sessions with the
# folder `run$stop_path`. It also tells the display. A second call creates
# nothing new.
run_failed <- function(run, argset) {
  if (is.null(run)) {
    return(invisible(NULL))
  }
  run$stopped <- TRUE
  # A second worker MAY create the folder at the same time. dir.create()
  # then gives FALSE and a warning, and the folder is there all the same.
  suppressWarnings(dir.create(run$stop_path))
  if (is.function(run$progressor)) {
    run$progressor(amount = 0, pbn_stop = argset[["scenario"]])
  }
  return(invisible(NULL))
}
environment(run_failed) <- baseenv()

# TRUE when the run `run` stops, so that no new data function or analysis
# starts. This R session knows it after its own failure. Otherwise it looks
# for the folder `run$stop_path`: with `force`, before each data function,
# and else at most every `run$every` seconds, before an analysis.
run_stopping <- function(run, force = FALSE) {
  if (is.null(run)) {
    return(FALSE)
  }
  if (isTRUE(run$stopped)) {
    return(TRUE)
  }
  now <- proc.time()[["elapsed"]]
  if (force || now - run$stop_checked >= run$every) {
    run$stop_checked <- now
    run$stopped <- dir.exists(run$stop_path)
  }
  return(run$stopped)
}
environment(run_stopping) <- baseenv()

# The value of an analysis or of a data function that did not run, because
# the run stopped before it.
not_run <- function() {
  return(structure(list(), class = "pbn_not_run"))
}
environment(not_run) <- baseenv()

# Sets the kind of random number generator of the run `run` again, when an
# earlier analysis or data function of this R session changed it. The
# wrappers call it directly before each set.seed(), so the change does not
# reach the draws of a later analysis.
run_rng <- function(run) {
  if (is.null(run)) {
    return(invisible(NULL))
  }
  kind <- run$rng
  if (!identical(RNGkind(), kind)) {
    RNGkind(kind[1], kind[2], kind[3])
  }
  return(invisible(NULL))
}
environment(run_rng) <- baseenv()

# Takes the records of this R session from the log `log` of the timings. A
# worker gets a copy of the log with each future, and the copy holds the
# records of the main session too. These stay in the log of the main
# session, which run_plan() reads after the run, so each record comes back
# once.
take_records <- function(log) {
  pending <- log[["pending"]]
  mine <- vapply(
    pending,
    function(d) return(identical(d[["pid"]], Sys.getpid())),
    logical(1)
  )
  assign("pending", pending[!mine], envir = log)
  return(pending[mine])
}
environment(take_records) <- baseenv()
