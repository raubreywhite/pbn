# The simulations of run_scenarios(): which to run, and the pool of R
# sessions that runs them.

check_workers <- function(workers) {
  # is_sim_number() passes Inf, because Inf == round(Inf).
  if (!is_sim_number(workers) || !is.finite(workers)) {
    stop("`workers` must be one whole number of 1 or more.", call. = FALSE)
  }
  return(invisible(workers))
}

# One row for each simulation of each scenario in `table`, in the order of
# the table, with the results folder, the results file, the log and the
# completion marker run.ok of the simulation.
simulation_jobs <- function(table, home, version) {
  scenario <- rep(table$scenario, table$sims)
  sims <- rep(table$sims, table$sims)
  sim <- sequence(table$sims)
  folder <- file.path(home, paste0(version, "_results"), scenario)
  folder <- ifelse(sims > 1, file.path(folder, sim_folder(sim)), folder)
  return(data.frame(
    scenario = scenario,
    sim = as.integer(sim),
    sims = as.integer(sims),
    folder = folder,
    file = file.path(folder, "results.qs2"),
    log = file.path(folder, "run.log"),
    ok = file.path(folder, "run.ok")
  ))
}

# For each job, NA when its results.qs2 can stay, or the reason to run it
# again. It prints one line for each kept simulation. run_scenarios() gives
# "" to each job when resume is FALSE.
keep_reasons <- function(jobs, source_md5, fake_md5) {
  reasons <- character(nrow(jobs))
  for (i in seq_len(nrow(jobs))) {
    reasons[i] <- keep_reason(
      jobs[i, , drop = FALSE],
      source_md5,
      unname(fake_md5[jobs$scenario[i]])
    )
    if (is.na(reasons[i])) {
      message(
        "pbn: scenario ",
        jobs$scenario[i],
        ", simulation ",
        jobs$sim[i],
        ": kept"
      )
    }
  }
  return(reasons)
}

# NA when the run of `job` completed, its results.qs2 passes check_results()
# and it comes from the same scenario, simulation and code. Otherwise the
# reason to run it again.
keep_reason <- function(job, source_md5, fake_md5) {
  if (!file.exists(job$file)) {
    return("there is no results.qs2")
  }
  reason <- completion_reason(job)
  if (!is.na(reason)) {
    return(reason)
  }
  found <- read_valid_results(job)
  if (!is.null(found$problem)) {
    return(found$problem)
  }
  return(manifest_reason(found$manifest, job, source_md5, fake_md5))
}

# NA when run.ok of `job` holds the md5 of its results.qs2 now. The pool
# writes run.ok only after a run that completed.
completion_reason <- function(job) {
  if (!file.exists(job$ok)) {
    return("the earlier run did not complete")
  }
  stored <- readLines(job$ok, warn = FALSE)
  if (!identical(stored, unname(tools::md5sum(job$file)))) {
    return("results.qs2 changed after the run")
  }
  return(NA_character_)
}

# The manifest of the results.qs2 of `job`, and the problem of the file or
# NULL. A file has a problem when qs2 cannot read it, when it fails
# check_results(), or when its manifest names another scenario, simulation
# or number of simulations.
read_valid_results <- function(job) {
  x <- tryCatch(qs2::qs_read(job$file), error = function(e) return(e))
  if (inherits(x, "error")) {
    return(list(problem = "qs2 cannot read results.qs2"))
  }
  found <- check_results(x)
  failed <- found$rule[found$status == "FAIL"]
  if (length(failed) > 0) {
    return(list(
      problem = paste0(
        "results.qs2 fails check_results(): ",
        paste(failed, collapse = ", ")
      )
    ))
  }
  m <- x[["manifest"]]
  if (
    !identical(m[["scenario"]], job$scenario) ||
      !identical(m[["sim"]], job$sim) ||
      !identical(m[["sims"]], job$sims)
  ) {
    return(list(
      problem = "the manifest names another scenario or simulation"
    ))
  }
  return(list(manifest = m, problem = NULL))
}

# NA when the manifest `m` comes from the code now. Otherwise the reason to
# run the job again.
manifest_reason <- function(m, job, source_md5, fake_md5) {
  if (!identical(m[["source_md5"]], source_md5)) {
    return("the run script or the functions folder changed")
  }
  if (job$scenario != "real" && !identical(m[["fake_md5"]], fake_md5)) {
    return(paste0("fake_", job$scenario, "() changed"))
  }
  return(NA_character_)
}

# Deletes the check.csv of each scenario in `table`, the stacked results.qs2
# of each scenario with more than 1 simulation, and the results.qs2 and the
# run.ok of each job that runs. A run that writes nothing MUST NOT leave an
# older file to be read or kept.
delete_stale <- function(jobs, table, home, version) {
  folders <- file.path(home, paste0(version, "_results"), table$scenario)
  stale <- c(
    file.path(folders, "check.csv"),
    file.path(folders[table$sims > 1], "results.qs2"),
    jobs$file[!is.na(jobs$reason)],
    jobs$ok[!is.na(jobs$reason)]
  )
  unlink(stale)
  left <- stale[file.exists(stale)]
  if (length(left) > 0) {
    stop(
      "pbn could not delete an older file, so it ran no simulation: ",
      paste(left, collapse = ", "),
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Runs each job in a new R session, at most `workers` at the same time. After
# the first failure it starts no new run, lets the started runs end, and then
# stops with each failed run.
run_pool <- function(jobs, script, home, workers) {
  running <- list()
  failed <- character(0)
  on.exit(
    for (r in running) {
      r$process$kill()
    },
    add = TRUE
  )
  next_job <- 1
  repeat {
    if (length(failed) == 0) {
      free <- min(workers - length(running), nrow(jobs) - next_job + 1)
      for (k in seq_len(free)) {
        running[[length(running) + 1]] <- start_run(
          jobs[next_job, , drop = FALSE],
          script,
          home
        )
        next_job <- next_job + 1
      }
    }
    if (length(running) == 0) {
      break
    }
    running[[1]]$process$wait(100)
    done <- !vapply(
      running,
      function(r) return(r$process$is_alive()),
      logical(1)
    )
    for (r in running[done]) {
      failed <- c(failed, finish_run(r))
    }
    running <- running[!done]
  }
  stop_on_failed_runs(failed)
  return(invisible(NULL))
}

stop_on_failed_runs <- function(failed) {
  if (length(failed) == 0) {
    return(invisible(NULL))
  }
  stop(
    paste(
      c(
        failed,
        paste(
          "pbn printed the log of each failed run above. It started no run",
          "after the first failure, and it stacked and checked no scenario."
        )
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
}

# Starts the run script for one job in a new R session. Its stdout and stderr
# go to the run.log of the simulation.
start_run <- function(job, script, home) {
  dir.create(job$folder, showWarnings = FALSE, recursive = TRUE)
  message(
    "pbn: scenario ",
    job$scenario,
    ", simulation ",
    job$sim,
    ": started",
    if (nzchar(job$reason)) paste0(", because ", job$reason)
  )
  process <- callr::rscript_process$new(callr::rscript_process_options(
    script = script,
    cmdargs = c(paste0("scenario=", job$scenario), paste0("sim=", job$sim)),
    wd = home,
    stdout = job$log,
    stderr = "2>&1"
  ))
  return(list(job = job, process = process))
}

# NULL when the run of `r` succeeded, and then it writes run.ok with the md5
# of results.qs2. A run succeeds when it quits with status 0 and its
# results.qs2 passes read_valid_results(). Otherwise it prints the whole log
# and returns one line that names the run and the log.
finish_run <- function(r) {
  job <- r$job
  status <- r$process$get_exit_status()
  what <- paste0(
    "The run of scenario \"",
    job$scenario,
    "\", simulation ",
    job$sim,
    ", "
  )
  problem <- if (!identical(status, 0L)) {
    paste0(what, "stopped with status ", status)
  } else if (!file.exists(job$file)) {
    paste0(what, "stopped with status 0 but wrote no results file ", job$file)
  } else {
    found <- read_valid_results(job)$problem
    if (!is.null(found)) {
      paste0(what, "stopped with status 0, but ", found, ": ", job$file)
    }
  }
  if (is.null(problem)) {
    writeLines(unname(tools::md5sum(job$file)), job$ok)
    message("pbn: scenario ", job$scenario, ", simulation ", job$sim, ": ran")
    return(NULL)
  }
  cat(
    "pbn: the log of the failed run of scenario \"",
    job$scenario,
    "\", ",
    "simulation ",
    job$sim,
    ", ",
    job$log,
    ":\n",
    sep = ""
  )
  cat(read_log(job$log))
  cat("pbn: the end of the log ", job$log, "\n", sep = "")
  return(paste0(problem, ". Its log: ", job$log))
}

read_log <- function(file) {
  if (!file.exists(file)) {
    return("")
  }
  text <- readChar(file, file.size(file), useBytes = TRUE)
  if (length(text) == 0) {
    return("")
  }
  if (!endsWith(text, "\n")) {
    text <- paste0(text, "\n")
  }
  return(text)
}
