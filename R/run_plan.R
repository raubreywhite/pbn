#' Run a plan
#'
#' `run_plan()` runs every analysis of the plnr plan `p`, in this R session or
#' on `workers` new R sessions. It stops the run at the first failed analysis,
#' and then it prints each failed analysis with its error.
#'
#' @param p A plan from `plnr::Plan$new()`, with data from [add_data()] and
#'   analyses from [add_analyses()].
#' @param workers The number of R sessions that run the analyses, one whole
#'   number of 1 or more.
#' @details
#' With `workers = 1`, `run_plan()` calls `p$run_all()` in this R session.
#'
#' With `workers` 2 or more, `run_plan()` needs the packages future and
#' doFuture. It starts a PSOCK cluster of `workers` new R sessions with
#' [parallel::makePSOCKcluster()]. It runs `p$run_all()` with the foreach path
#' of plnr, on [doFuture::registerDoFuture()] with the future plan
#' [future::cluster] on that cluster. A PSOCK cluster does not fork, so it
#' also works on Windows. Each future runs a batch of tasks of plnr on a
#' worker, and each task holds the analyses that get the data of one key.
#' Each worker gets about 4 batches when each data function with arguments
#' has both `scenario` and `sim`, or when no data function has arguments.
#' Otherwise each task is a batch of its own.
#'
#' Each worker starts with the library paths of this R session, from
#' [.libPaths()]. While it starts the cluster, `run_plan()` sets the
#' environment variable `R_LIBS` to these paths, and then it sets
#' `R_LIBS` back. So a worker also finds a library that this R session added
#' with `.libPaths()`, for example the temporary library of `R CMD build`.
#' Each worker runs with 1 thread. While `run_plan()` starts the cluster, it
#' sets the environment variables `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`
#' and `MKL_NUM_THREADS` to 1, as it sets `R_LIBS`. Then it sets them back
#' in this R session. OpenMP and the BLAS libraries OpenBLAS and MKL
#' read them when they start their threads, so the variables MUST be there
#' when the worker starts. The section One thread for each worker gives the
#' rules.
#'
#' Before the run, `run_plan()` gives each worker these:
#' - the library paths of this R session again, with [.libPaths()].
#' - 1 thread: the three variables again, for a library that the worker
#'   loads later, and `data.table::setDTthreads(1)` when data.table is
#'   installed.
#' - the packages attached in this R session, in the order of [search()],
#'   for example data.table after `library(data.table)` in the run script.
#' - the kind of random number generator of this R session, from
#'   [RNGkind()].
#' - the paths in `org::project`, such as `org::project$home`.
#' - the folders of [initialize_project()], so that [folder()] works on the
#'   worker. A worker then loads pbn.
#'
#' A future starts with an empty global environment. [initialize_project()]
#' sources the functions folder into an environment, `.GlobalEnv` by
#' default. Each batch takes every object of that environment to the global
#' environment of its worker. A data function and an action function then find `CFG` and
#' the other functions of the folder. A plnr plan and `.Random.seed` are left
#' out. Each batch takes a copy of each object, so keep large objects out of
#' that environment.
#'
#' `run_plan()` does not copy the R options, for example
#' `options(digits = 3)`, and no other state of this R session. A future sets
#' the option `mc.cores` to 1. `run_plan()` removes it on a worker, so
#' [parallel::mclapply()] in an analysis uses its default number of cores.
#' The output, the messages and the warnings of a worker come to this R
#' session at the end of each batch.
#'
#' After the run, `run_plan()` stops the cluster and registers the sequential
#' foreach backend, also after an error. It sets the future plan and the
#' option `doFuture.rng.onMisuse` back to their values before the run. Each
#' data function and each analysis sets its own seed, so `run_plan()` turns
#' off the warning of doFuture about random numbers for the run.

#' plnr calls each data function once for each scenario and simulation, see
#' [add_data()]. Each data function and each analysis sets its own seed
#' before it runs, see [add_analyses()]. Take a function that depends only on
#' its arguments, the objects of the functions folder such as `CFG`, the
#' attached packages and the seed. Its results with `workers = 1` and with
#' `workers = 2` are `identical()`. A function that reads an R option or another state
#' of the main R session can give other results on a worker. A
#' data.table loses its self-reference on the way back from a worker, and
#' `run_plan()` gives it a new one with [data.table::setalloccol()]. It does
#' this for each data.table in a value or in a plain list in a value, at any
#' depth. It does not do this for a data.table in a list column of a
#' data.frame. Such a value MAY differ in `identical()` between 1 worker and
#' more.
#'
#' An error in an action function fails that analysis, and an error in a
#' data function fails each analysis that gets the data. pbn saves no results
#' of a run with a failed analysis, so the first failed analysis stops the
#' run. The section Stop at the first failure gives the rules. After the run,
#' `run_plan()` prints each failed analysis with its name and its full error
#' message with `cat()`, and then it stops. R cuts an error message at 1000
#' bytes by default, and `cat()` does not. The error message of `run_plan()`
#' gives the number of failed analyses and of the analyses that did not run.
#' It also names the first failed analysis in the order of the plan, and
#' holds its error. pbn cuts that error to keep the message below 900 bytes.
#' When 2 or more analyses fail and all have the same error, the message says
#' so. The results files of an earlier run stay as they are. After
#' [initialize_project()], `run_plan()` writes the run log
#' `<version>_results/run.md` with the failures before it stops. The section
#' The run log of [save_results()] describes it.
#'
#' After [initialize_project()], `run_plan()` first replaces the run log with
#' one that says that the run has not finished, and deletes the errors file
#' `<version>_results/run_errors_gitexclude.md`. A call of `run_plan()`
#' without [save_results()] after it leaves this log.
#'
#' `run_plan()` measures the elapsed time of each data step and of each
#' analysis where it runs, also on a worker, and the time of the clock of the
#' computer at its end. The times go to the record of the plan, and
#' [save_results()] writes them to the run log. They are not in the value, so
#' two runs stay `identical()`.
#'
#' The value carries the attribute `pbn_run`, a stamp of the plan. It holds
#' no count of the runs, so two runs of one plan can be `identical()`.
#' [save_results()] stops on a value without the stamp of its plan.
#'
#' `run_plan()` sets `use_foreach` of the plan: `FALSE` for 1 worker and
#' `TRUE` for more.
#'
#' After [initialize_project()], `run_plan()` first lists the files of the
#' folders of the project again, with their sizes and modification times.
#' It reads no content of a file. The value carries this fingerprint in its
#' stamp, and [save_results()] writes it to `manifest$data_files`.
#'
#' An analysis that changes the shared data or an object of the functions
#' folder in place fails. An analysis that leaves a new object in the global
#' environment fails too. [add_analyses()] gives the rules.
#'
#' An error outside the wrappers of pbn ends the run. Examples are a worker
#' that stops, and an error of plnr or of the backend of the workers.
#' `run_plan()` then prints the whole error, and writes the run log with it
#' after [initialize_project()]. It stops with one message that holds the
#' error, cut below 900 bytes, and saves nothing. The future plan, the
#' foreach backend and the options go back as after any run, and the
#' cluster stops.
#'
#' For a data function with `cache = TRUE`, `run_plan()` needs
#' [initialize_project()] first, and it prints one line for each scenario and
#' simulation. The section Cache of [add_data()] gives the rules. A data
#' function that returns a `cohort::CohortPipeline` runs on the worker, and
#' only its data.table and its exclusion table come back. The section A
#' cohort as the data of [add_data()] gives the rules.
#' @section One thread for each worker:
#' Some packages start threads of their own, for example data.table,
#' fixest, a threaded BLAS and OpenMP code. By default, such a package uses
#' many cores. data.table, for example, uses half of the cores of the
#' computer. With `workers` 2 or more, each worker would do that, and the
#' computer would run far more threads than cores. So `run_plan()` makes each worker
#' use 1 thread. There is no argument to change it. An analysis that needs
#' more threads in its worker sets them in its own code, for example
#' `data.table::setDTthreads(4)` at the top of its action function.
#'
#' With `workers = 1`, `run_plan()` changes no thread setting, and the
#' analyses run with the settings of this R session.
#'
#' The results of the test projects of pbn are `identical()` with 1 and 2
#' workers. pbn does not check this for other code. A threaded BLAS MAY sum
#' in another order with another number of threads. So a matrix product of
#' such a BLAS MAY differ in its last digits between `workers = 1` and more
#' workers. With 1 worker it has the threads of this R session, and with
#' more it has 1 thread.
#' @section Stop at the first failure:
#' A failed data function makes each analysis of its scenario and
#' simulation fail, and [save_results()] needs a run without a failed
#' analysis. So no analysis after the first failure can give a result, and
#' `run_plan()` stops the run there. There is no option to run on. These are
#' the rules:
#' - After the first failed analysis, no new data function and no new
#'   analysis starts. An analysis whose data failed fails at once, also
#'   after the stop, without a call of its action function.
#' - With 1 worker, the stop comes directly after the failed analysis.
#' - With workers, the failed analysis creates a folder in the temporary
#'   folder of this R session. Each worker looks for it before each data
#'   function, and at most every 0.5 seconds before an analysis. pbn does not
#'   stop an analysis that runs. So the longest delay is the rest of the
#'   longest analysis that runs at the time of the failure, plus 0.5
#'   seconds. The batches that did not start still go to the workers, and
#'   each of them ends at once.
#' - Each analysis that did not run counts as not run. It is not a failure.
#'
#' The display shows the stop at once, and the run log says that the run
#' stopped early and how many analyses did not run.
#' @section Progress:
#' `run_plan()` shows one line for each scenario: its finished analyses of
#' all its analyses, the failed ones among them, the time since its start,
#' and a note. The note is one of these:
#' - `"waiting"` before the scenario starts, and `"done"` at its end.
#' - `"stopping"` after the first failed analysis of the run, and
#'   `"stopped early"` with the analyses that did not run at the end.
#' - the last text of [progress()] from an analysis of the scenario, while
#'   that analysis runs.
#' - otherwise the time left, from the analyses of the scenario that ended
#'   without a failure, after at least 3 and at least 10% of them.
#'
#' The analyses that count the rows do not count. The display replaces the
#' progress bar of plnr. The header of the display in a terminal gives the
#' time since the start of the run. So does the start of each line of a log,
#' for example `pbn: [12m 05s] null 522/1200 analyses, 8m 53s, about 12m
#' left`.
#'
#' In an interactive terminal, the display draws its lines again in place.
#' In RStudio and in another console without ANSI codes, it draws one line
#' again in place. It shows the lines of each scenario at the end. While
#' the display is shown, progressr holds back other output, messages and
#' warnings, and shows them above it.
#'
#' Without an interactive session, for example with `Rscript` into a log,
#' in knitr and in testthat, the display only adds lines:
#' - nothing in the first 60 seconds of the run, other than the line of the
#'   first failed analysis. A short run without a failure adds no line.
#' - one line at once when the first analysis fails, also in the first 60
#'   seconds.
#' - after 60 seconds, a header, and one line for each scenario that started.
#' - one line when a scenario finishes.
#' - one line for a running scenario at most every 60 seconds. The line
#'   comes only when 10% more of the scenario is done, in analyses or in its
#'   last text of [progress()].
#'
#' A worker sends the count of its finished analyses at most every 0.5
#' seconds, and at the end of each scenario and simulation. The display uses
#' progressr with a handler of its own, inside [progressr::with_progress()].
#' It sets no progressr handler of the R session, so the handlers that
#' another package sets stay as they are.
#' @return Invisibly, a named list with one element for each analysis of the
#'   plan, in the order of the plan. Element `i` is the value of the action
#'   function of analysis `i`. Give it to [save_results()].
#' @examples
#' CFG <- list(scenarios = data.frame(scenario = c("real", "null"), sims = 2))
#' p <- plnr::Plan$new()
#' pbn::add_data(p, "d", fn = function(scenario, sim) {
#'   return(data.frame(x = stats::rnorm(5)))
#' })
#' pbn::add_analyses(
#'   p,
#'   pbn::scenarios(CFG),
#'   name = "mean_x",
#'   fn = function(data, argset) return(data.frame(mean = mean(data$d$x)))
#' )
#' r <- pbn::run_plan(p)
#' r[["null__sim_0002__mean_x__1"]]
#' @family plan
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
run_plan <- function(p, workers = 1) {
  start <- proc.time()[["elapsed"]]
  started <- Sys.time()
  check_plan(p)
  if (!is_count(workers)) {
    stop("`workers` must be one whole number of 1 or more.", call. = FALSE)
  }
  info <- plan_info(p)
  if (is.null(info) || length(info$names) == 0) {
    stop(
      "The plan has no analysis from pbn::add_analyses().",
      call. = FALSE
    )
  }
  check_data_keys(info)
  index <- analysis_index(p, info)
  if (workers > 1) {
    check_parallel_packages()
  }
  # run.md says that this run started and has not finished, until the full
  # log replaces it.
  write_started_log(plan_scenarios(index), started, workers)
  # The fingerprint of the raw data of this run, also after a change of a
  # file since initialize_project(). The stamp carries it to the manifest.
  if (!is.null(state$results)) {
    refresh_data_files()
  }
  data_files <- state$data_files
  cached <- set_cache(p, info, workers)
  on.exit(clear_cache(info, cached), add = TRUE)
  set_catch(info, TRUE)
  on.exit(set_catch(info, FALSE), add = TRUE)
  reset_log(info)
  # The packages that the workers loaded. With 1 worker, this R session is
  # the worker, and save_results() reads its packages.
  info$worker_packages <- NULL
  # An error outside the wrappers, for example of a worker that stopped,
  # reaches this frame. The run then ends with its own log and message.
  run <- tryCatch(
    run_shown(p, info, index, workers, started),
    error = function(e) return(e)
  )
  if (inherits(run, "error")) {
    stop_on_run_error(p, info, run, workers, start, started)
  }
  r <- run$r
  info$worker_packages <- run$packages
  # The timings leave the value here, so they never reach it.
  taken <- unwrap_timed(r, info$log)
  r <- unbox_results(taken$r, names(p$analyses))
  if (workers > 1 && length(cached) > 0) {
    print_cache_lines(r, info)
  }
  info$run <- run_record(p, info, r, taken, workers, start, started)
  if (length(info$run$failed) > 0 || info$run$not_run > 0) {
    write_failed_run(info$run)
  }
  stop_on_failed(r)
  # The stamp binds the value to this plan. save_results() checks it. It
  # holds no count of the runs, so two runs of one plan stay identical().
  attr(r, "pbn_run") <- list(plan = info$token, data_files = data_files)
  return(invisible(r))
}

# Stops when a package of a run with workers > 1 is missing.
check_parallel_packages <- function() {
  missing <- c("future", "doFuture")
  missing <- missing[!vapply(missing, has_package, logical(1))]
  if (length(missing) > 0) {
    stop(
      "run_plan() with workers > 1 needs the packages ",
      paste(missing, collapse = " and "),
      ": install them, or use workers = 1.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# p$run_all() inside progressr::with_progress() with the display of pbn, in
# this R session or on `workers` workers. In a console, progressr holds back
# other output and the messages and warnings while the display is drawn, and
# shows them above it. The value is that of run_progressed().
run_shown <- function(p, info, index, workers, started) {
  display <- new_display(
    plan_scenarios(index),
    workers,
    started,
    display_mode(),
    lag = max(0, as.numeric(Sys.time()) - as.numeric(started))
  )
  console <- display$mode != "log"
  return(progressr::with_progress(
    run_progressed(p, info, index, workers, display),
    handlers = list(display_handler(display)),
    delay_stdout = console,
    delay_conditions = if (console) c("message", "warning") else character(0),
    enable = TRUE,
    interval = 0
  ))
}

# p$run_all() with one progressor for the whole plan. The display counts each
# analysis but those that count the rows. The display of pbn replaces the
# progress bar of plnr: the plan gets a progressor that does nothing for the
# run, and its old one after. At the end, the display gets the analyses
# whose count did not reach it, and the one step of the progressor ends it.
# progressr keeps a vector as long as the steps, and walks it at each
# progression, so the progressor has 1 step and each count has the amount
# 0. The value is a list: `r`, the value of p$run_all(), and `packages`,
# NULL or the packages of the workers. The first failed analysis creates
# the folder `stop_path`, see run_failed(), and the folder goes at the end.
run_progressed <- function(p, info, index, workers, display) {
  progressor <- progressr::progressor(steps = 1, on_exit = FALSE)
  stop_path <- tempfile("pbn_stop_")
  on.exit(unlink(stop_path, recursive = TRUE), add = TRUE)
  run <- run_context(info, index, workers, progressor, stop_path)
  set_run(info, run)
  on.exit(set_run(info, NULL), add = TRUE)
  old_option <- options(pbn.run = run)
  on.exit(options(old_option), add = TRUE)
  old_progressor <- plan_progressor(p)
  p$set_progressor(function(...) return(invisible(NULL)))
  on.exit(p$set_progressor(old_progressor), add = TRUE)
  if (workers == 1) {
    p$set_use_foreach(FALSE)
    out <- list(r = p$run_all(), packages = NULL)
  } else {
    out <- run_on_cluster(p, workers, future_chunk_size(info, index, workers))
  }
  if (length(out$r) == nrow(index)) {
    progressor(amount = 0, pbn_final = final_counts(out$r, index))
  } else {
    rest <- display_rest(display)
    if (sum(rest) > 0) {
      progressor(amount = 0, pbn_done = rest[rest > 0])
    }
  }
  progressor(amount = 1)
  return(out)
}

# The counts of each scenario at the end of the run, from `r`, the value of
# p$run_all(), without the analyses that count the rows: `done`, the
# analyses that ran, `failed`, those of them that failed, and `not_run`,
# those that did not run because the run stopped. Each is named by the
# scenario.
final_counts <- function(r, index) {
  shown <- !index$hidden
  value <- lapply(r[shown], function(x) {
    return(if (inherits(x, "pbn_timed")) x[["value"]] else x)
  })
  skipped <- vapply(value, inherits, logical(1), what = "pbn_not_run")
  failed <- vapply(
    value,
    inherits,
    logical(1),
    what = c("pbn_failed_analysis", "try-error")
  )
  s <- factor(index$scenario[shown], levels = unique(index$scenario))
  count <- function(x) return(c(tapply(x, s, sum, default = 0L)))
  return(list(
    done = count(!skipped),
    failed = count(failed),
    not_run = count(skipped)
  ))
}

# The progressr progressor of the plnr plan `p`, from its private fields, or
# NULL.
plan_progressor <- function(p) {
  private <- p$.__enclos_env__$private
  if (!is.environment(private)) {
    return(NULL)
  }
  return(private$pb_progressor)
}

# p$run_all() with the foreach path of plnr, on doFuture with a future
# cluster of `workers` R sessions, a PSOCK cluster that pbn makes and
# prepares. Each future runs `chunk_size` tasks of plnr. The future plan
# and the option doFuture.rng.onMisuse go back to their values before, and
# the cluster stops, on exit, also after an error. pbn sets each seed
# itself, so the warning of doFuture about random numbers is off for the
# run. The value is a list: `r`, the value of p$run_all(), and `packages`,
# the versions of the packages loaded on the workers after the run.
run_on_cluster <- function(p, workers, chunk_size = 1L) {
  cl <- make_cluster(workers)
  old_options <- options(doFuture.rng.onMisuse = "ignore")
  old_plan <- NULL
  # The steps that cannot fail come first. After a worker stopped, the
  # connection of its node is gone, and stop_nodes() skips that node, so
  # the error of the run stays the one that the caller sees.
  on.exit(
    {
      options(old_options)
      foreach::registerDoSEQ()
      if (!is.null(old_plan)) {
        future::plan(old_plan)
      }
      stop_nodes(cl)
    },
    add = TRUE
  )
  prepare_workers(cl)
  old_plan <- future::plan(future::cluster, workers = cl)
  doFuture::registerDoFuture()
  p$set_use_foreach(TRUE)
  r <- repair_tables(p$run_all(.plnr.options = list(chunk_size = chunk_size)))
  found <- parallel::clusterCall(cl, namespace_versions)
  packages <- unlist(unname(found))
  packages <- packages[!duplicated(names(packages))]
  return(list(r = r, packages = packages))
}

# Stops each node of the cluster `cl`. A node whose connection is no longer
# valid, for example of a worker that stopped, is already gone: closing it
# raises "invalid connection", and stop_nodes() goes on with the next node.
stop_nodes <- function(cl) {
  for (i in seq_along(cl)) {
    tryCatch(
      parallel::stopCluster(cl[i]),
      error = function(e) return(invisible(NULL))
    )
  }
  return(invisible(NULL))
}

# The version of each loaded namespace of the R session that runs it, named
# by the package. Its environment is the base environment, so a worker
# needs no pbn to run it.
namespace_versions <- function() {
  pkgs <- loadedNamespaces()
  return(vapply(
    pkgs,
    function(p) return(as.character(getNamespaceVersion(p))),
    character(1)
  ))
}
environment(namespace_versions) <- baseenv()

# A PSOCK cluster of `workers` R sessions that start with the library paths
# of this R session. A worker reads R_LIBS when it starts, before it
# unserialises any object, so it finds a library that this R session added
# with .libPaths() after its start, for example the temporary library of
# R CMD build.
#
# Each worker also starts with 1 thread for OpenMP and the BLAS library,
# from the variables of thread_variables, because a library reads them
# when it starts its pool of threads. These variables, like R_LIBS, are a
# transport between pbn's own processes and no configuration. This R
# session gets its own values back on exit.
make_cluster <- function(workers) {
  vars <- c("R_LIBS", thread_variables)
  old <- Sys.getenv(vars, unset = NA, names = TRUE)
  on.exit(
    {
      for (v in vars) {
        if (is.na(old[[v]])) {
          Sys.unsetenv(v)
        } else {
          do.call(Sys.setenv, stats::setNames(list(old[[v]]), v))
        }
      }
    },
    add = TRUE
  )
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  do.call(
    Sys.setenv,
    stats::setNames(
      as.list(rep("1", length(thread_variables))),
      thread_variables
    )
  )
  return(parallel::makePSOCKcluster(workers))
}

# The variables of the number of threads that OpenMP and the BLAS libraries
# OpenBLAS and MKL read when they start.
thread_variables <- c(
  "OMP_NUM_THREADS",
  "OPENBLAS_NUM_THREADS",
  "MKL_NUM_THREADS"
)

# Makes the R session that runs it use 1 thread: the variables of
# thread_variables for a library that it loads later, and
# data.table::setDTthreads(1) when data.table is installed. Its environment
# is the base environment, so a worker needs no pbn to run it.
single_thread <- function(vars) {
  do.call(Sys.setenv, stats::setNames(as.list(rep("1", length(vars))), vars))
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(1L)
  }
  return(invisible(NULL))
}
environment(single_thread) <- baseenv()

# requireNamespace(), in one place, so that the tests can mock it.
has_package <- function(pkg) {
  return(requireNamespace(pkg, quietly = TRUE))
}

# Sets .pbn_catch in the environment of each wrapper of the plan.
set_catch <- function(info, value) {
  for (e in info$wrappers) {
    assign(".pbn_catch", value, envir = e)
  }
  return(invisible(NULL))
}

# Stops when a data function has a key field that the argsets of pbn do not
# all hold: scenario, sim or a setting column.
check_data_keys <- function(info) {
  allowed <- c("scenario", "sim", info$settings)
  for (nm in names(info$data)) {
    bad <- setdiff(key_fields(info$data[[nm]]), allowed)
    if (length(bad) > 0) {
      stop(
        "The data function \"",
        nm,
        "\" has the arguments ",
        paste(bad, collapse = ", "),
        " without a default. plnr calls a data function with its arguments ",
        "without a default, and pbn allows only scenario, sim and the ",
        "setting columns of CFG$scenarios. pbn ran nothing.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# Gives each worker of the cluster `cl` the library paths, the attached
# packages, the kind of random number generator, the paths of org::project
# and the folders of the project of this R session. These stay for each future of the worker.
# A future empties the global environment of the worker, so the objects of
# the sourced environment go with each future instead, see run_enter().
prepare_workers <- function(cl) {
  parallel::clusterCall(cl, set_lib_paths, .libPaths())
  parallel::clusterCall(cl, single_thread, thread_variables)
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  parallel::clusterCall(cl, attach_packages, attached)
  kinds <- RNGkind()
  parallel::clusterCall(cl, RNGkind, kinds[1], kinds[2], kinds[3])
  paths <- mget(ls(org::project), envir = org::project)
  paths <- Filter(is.character, paths)
  parallel::clusterCall(
    cl,
    function(paths) {
      for (nm in names(paths)) {
        assign(nm, paths[[nm]], envir = org::project)
      }
      return(invisible(NULL))
    },
    paths
  )
  folders <- worker_folders()
  if (!is.null(folders)) {
    parallel::clusterCall(cl, set_worker_folders, folders)
  }
  return(invisible(NULL))
}

# Sets the library paths of the R session that runs it to `paths`. A call
# clusterCall(cl, .libPaths, paths) changes nothing on the worker: it sends
# a copy of .libPaths() with a copy of the environment that holds the paths.
# This function calls the .libPaths() of the worker.
set_lib_paths <- function(paths) {
  .libPaths(paths)
  return(invisible(.libPaths()))
}
environment(set_lib_paths) <- baseenv()

# Attaches the packages `pkgs`, in the order of search(), that are not
# attached yet. library() puts each package at position 2, so the last one
# goes first. Its environment is the base environment, so a worker needs no
# pbn to run it.
attach_packages <- function(pkgs) {
  have <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  for (pkg in rev(setdiff(pkgs, have))) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  return(invisible(NULL))
}
environment(attach_packages) <- baseenv()

# `x` with each data.table in it, at any depth of plain lists, given a valid
# self-reference again. A data.table that comes back from a worker lost it
# in serialize(), so identical() then differs from the same table made in
# this R session, and data.table warns on the first `:=`.
repair_tables <- function(x) {
  if (data.table::is.data.table(x)) {
    return(data.table::setalloccol(x))
  }
  if (is.list(x) && !is.data.frame(x)) {
    for (i in seq_along(x)) {
      if (!is.null(x[[i]])) {
        x[[i]] <- repair_tables(x[[i]])
      }
    }
  }
  return(x)
}

# The results of the run, named by analysis, with each pbn_null back as
# NULL.
unbox_results <- function(r, names) {
  if (length(r) != length(names)) {
    stop(
      "plnr returned ",
      length(r),
      " results for ",
      length(names),
      " analyses.",
      call. = FALSE
    )
  }
  for (i in seq_along(r)) {
    if (inherits(r[[i]], "pbn_null")) {
      r[i] <- list(NULL)
    }
  }
  names(r) <- names
  return(r)
}

# The error message of each failed analysis in `r`, named by the analysis.
failed_analyses <- function(r) {
  failed <- character(0)
  for (nm in names(r)) {
    x <- r[[nm]]
    if (inherits(x, "pbn_failed_analysis")) {
      failed[[nm]] <- x[["message"]]
    } else if (inherits(x, "try-error")) {
      cond <- attr(x, "condition")
      failed[[nm]] <- if (inherits(cond, "condition")) {
        conditionMessage(cond)
      } else {
        paste(as.character(x), collapse = "\n")
      }
    }
  }
  return(failed)
}

# Prints each failed analysis in full with cat(), and stops when there is
# one, or when an analysis did not run because the run stopped early.
stop_on_failed <- function(r) {
  failed <- failed_analyses(r)
  skipped <- sum(vapply(r, inherits, logical(1), what = "pbn_not_run"))
  if (length(failed) == 0 && skipped == 0) {
    return(invisible(NULL))
  }
  for (nm in names(failed)) {
    cat("pbn: the analysis ", nm, " failed:\n", failed[[nm]], "\n", sep = "")
  }
  utils::flush.console()
  if (length(failed) == 0) {
    stop(
      "The run stopped early: ",
      skipped,
      " of ",
      length(r),
      " analyses did not run. pbn saved nothing.",
      call. = FALSE
    )
  }
  stop(failed_message(failed, length(r), skipped), call. = FALSE)
}

# The message of stop_on_failed(): the count, the analyses that did not run
# because the run stopped early, then the name and the error of the first
# failed analysis. A knitr build or a log can keep the message and lose the
# output of cat(). The error is cut so that the message holds at most
# `max_bytes` bytes, below the 1000 bytes where R cuts it.
failed_message <- function(failed, n, skipped = 0L, max_bytes = 899L) {
  fixed <- paste0(
    length(failed),
    " of ",
    n,
    " analyses failed",
    if (skipped > 0) {
      paste0(", and the run stopped early: ", skipped, " analyses did not run")
    },
    ". pbn printed each failed analysis and its error above, ",
    "and saved nothing. The results files of an earlier run stay as they are.",
    " The first failed analysis is ",
    names(failed)[1],
    ", with this error: \""
  )
  end <- "\"."
  if (length(failed) > 1 && length(unique(unname(failed))) == 1) {
    end <- paste0(end, " All ", length(failed), " failed with this error.")
  }
  room <- max_bytes - nchar(fixed, "bytes") - nchar(end, "bytes")
  return(paste0(fixed, cut_bytes(failed[[1]], room), end))
}

# `x` cut to at most `room` bytes, at a whole character, and marked with
# " [...]" when it is cut.
cut_bytes <- function(x, room) {
  if (!validUTF8(x)) {
    x <- iconv(x, "latin1", "UTF-8")
  }
  if (nchar(x, "bytes") <= room) {
    return(x)
  }
  mark <- " [...]"
  chars <- strsplit(x, "")[[1]]
  keep <- cumsum(nchar(chars, "bytes")) <= room - nchar(mark, "bytes")
  return(paste0(paste(chars[keep], collapse = ""), mark))
}
