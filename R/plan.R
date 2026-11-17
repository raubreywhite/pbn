# What pbn knows about each plnr plan: the data functions of add_data(), the
# analysis kinds of add_analyses(), and the environments of the wrappers. A
# plnr::Plan is a locked R6 object with no field for this, so pbn keeps it
# here, under the address of the plan. A finalizer removes the entry when R
# frees the plan, so a new plan at the same address never finds it.
plans <- new.env(parent = emptyenv())

# The number of plans that pbn has seen in this R session. Each plan gets a
# token from it, and run_plan() stamps its value with the token.
plan_count <- new.env(parent = emptyenv())
plan_count$n <- 0

# The suffix of the analysis that counts the rows of the data, one for each
# scenario and simulation. A kind name MUST NOT start with "pbn_".
n_rows_kind <- "pbn_n_rows"

check_plan <- function(p) {
  if (!inherits(p, "Plan") || !is.environment(p)) {
    stop("`p` must be a plan from plnr::Plan$new().", call. = FALSE)
  }
  return(invisible(p))
}

# The entry of the plan `p`, or NULL. With `create`, a new empty entry when
# there is none.
plan_info <- function(p, create = FALSE) {
  key <- data.table::address(p)
  info <- get0(key, envir = plans, inherits = FALSE)
  if (is.null(info) && create) {
    info <- new.env(parent = emptyenv())
    info$data <- list()
    info$steps <- list()
    info$cache <- list()
    info$data_envs <- list()
    info$scenario_settings <- list()
    info$kinds <- data.frame(analysis = character(0), control = logical(0))
    info$dims <- list()
    info$seeds <- list()
    info$settings <- character(0)
    info$names <- character(0)
    info$hidden <- character(0)
    info$wrappers <- list()
    info$data_wrappers <- list()
    info$log <- new_log()
    plan_count$n <- plan_count$n + 1
    info$token <- paste0("plan_", plan_count$n, "_", key)
    assign(key, info, envir = plans)
    # The finalizer MUST NOT hold the plan, or R never frees it. So its
    # environment holds the key and the registry only.
    reg.finalizer(p, registry_finalizer(key, plans), onexit = FALSE)
  }
  return(info)
}

# A finalizer that removes the entry `key` from `registry`. Its environment
# holds these two only, and never the plan.
registry_finalizer <- function(key, registry) {
  force(key)
  force(registry)
  return(function(e) {
    if (exists(key, envir = registry, inherits = FALSE)) {
      rm(list = key, envir = registry)
    }
    return(invisible(NULL))
  })
}

# .pbn_fn, .pbn_catch, .pbn_shape, .pbn_finish, .pbn_log, .pbn_drain,
# .pbn_cache, .pbn_plan, .pbn_clock, .pbn_run, .pbn_enter, .pbn_begin,
# .pbn_done, .pbn_take, .pbn_objects, .pbn_objects_value, .pbn_data_checked,
# .pbn_rng, .pbn_globals, .pbn_stopping, .pbn_failed, .pbn_not_run,
# .pbn_action and .pbn_call live in the environment of each
# wrapper, which wrapper_env(),
# action_wrapper() and data_wrapper_env() make.
utils::globalVariables(c(
  ".pbn_fn",
  ".pbn_catch",
  ".pbn_shape",
  ".pbn_finish",
  ".pbn_log",
  ".pbn_drain",
  ".pbn_cache",
  ".pbn_plan",
  ".pbn_clock",
  ".pbn_run",
  ".pbn_enter",
  ".pbn_begin",
  ".pbn_done",
  ".pbn_take",
  ".pbn_objects",
  ".pbn_objects_value",
  ".pbn_data_checked",
  ".pbn_rng",
  ".pbn_globals",
  ".pbn_stopping",
  ".pbn_failed",
  ".pbn_not_run",
  ".pbn_action",
  ".pbn_call"
))

# The log of the timings of the data of a plan. Each data wrapper adds one
# record to `pending` for each call while run_plan() runs. The analysis that
# counts the rows takes the records of its own R session and gives them back
# with its value, so they come back from a worker too.
new_log <- function() {
  return(list2env(list(pending = list()), parent = emptyenv()))
}

# The key fields of a data function, as plnr reads them: its arguments
# without a default, other than `...`.
key_fields <- function(fn) {
  fm <- formals(fn)
  required <- vapply(
    seq_along(fm),
    function(i) {
      return(is.name(fm[[i]]) && !nzchar(as.character(fm[[i]])))
    },
    logical(1)
  )
  return(as.character(setdiff(names(fm)[required], "...")))
}

# The environment of a wrapper. Its parent is the base environment, so a
# PSOCK worker needs neither pbn nor an object of the main session to run
# the wrapper. `.pbn_catch` is TRUE only while run_plan() runs, and
# `.pbn_run` then holds the run context of run_context(). `log` is the log of
# the timings of the plan, or NULL. With `drain`, the wrapper takes the
# records of that log.
wrapper_env <- function(fn, log = NULL, drain = FALSE) {
  return(list2env(
    list(
      .pbn_fn = fn,
      .pbn_catch = FALSE,
      .pbn_run = NULL,
      .pbn_shape = data_shape,
      .pbn_finish = action_value,
      .pbn_enter = run_enter,
      .pbn_begin = run_begin,
      .pbn_done = run_done,
      .pbn_take = take_records,
      .pbn_objects = objects_print,
      .pbn_objects_value = objects_value,
      .pbn_data_checked = data_checked,
      .pbn_rng = run_rng,
      .pbn_globals = new_globals,
      .pbn_stopping = run_stopping,
      .pbn_failed = run_failed,
      .pbn_not_run = not_run,
      .pbn_log = log,
      .pbn_drain = drain
    ),
    parent = baseenv()
  ))
}

# The shape of each data.frame element of `data`: its names, its number of
# rows, its data.table key and the address of each column. An action that
# adds, removes, renames or replaces a column, changes the number of rows or
# sets a key changes the shape.
# plnr adds the element hash, which is not data.
data_shape <- function(data) {
  data <- data[names(data) != "hash"]
  return(lapply(data, function(x) {
    if (!is.data.frame(x)) {
      return(NULL)
    }
    # paste0() makes a new vector. data.table::setnames() changes the names
    # in place, and would change a stored reference to them too.
    return(list(
      names = paste0(names(x)),
      nrow = nrow(x),
      sorted = attr(x, "sorted", exact = TRUE),
      columns = vapply(
        seq_along(x),
        function(i) return(data.table::address(x[[i]])),
        character(1)
      )
    ))
  }))
}
environment(data_shape) <- baseenv()

# The seed of one analysis: a whole number from 10000 to 2147483628 that
# depends on the kind `name`, its counter `k` and the simulation `sim` only.
# It is a polynomial hash with base 257, modulo the prime 2147483629, of the
# code points of "<name>__<k>__<sim>". Integer arithmetic in doubles is exact
# here, so the value is the same on each platform and in each R session. The
# offset 10000 makes the seed differ from each sim of 1 to 9999, the seed of
# the data.
analysis_seed <- function(name, k, sim) {
  h <- 0
  for (code in utf8ToInt(enc2utf8(paste0(name, "__", k, "__", sim)))) {
    h <- (h * 257 + code) %% 2147483629
  }
  return(as.integer(10000 + h %% (2147483629 - 10000)))
}

# A data function with the key fields of `fn`. It sets the seed to `sim`
# when `sim` is a key field, and calls `fn` with the key fields by name.
# add_data() allows `sim` only together with `scenario`. For the steps form,
# `fn` is the step raw, and run_steps() gives its value to the other steps
# in `steps`, after the one seed. data_value() turns a
# cohort::CohortPipeline into its data, and reads or writes the cache while
# run_plan() runs with cache = TRUE.
# While run_plan() runs, an error becomes an object of class pbn_data_error,
# so the error reaches each analysis that needs the data. After the first
# failed analysis of the run, the wrapper calls nothing and returns a
# pbn_not_run, see run_stopping(). `log` is the log of the timings of the
# plan, or NULL.
data_wrapper <- function(fn, name, plan_key = NULL, steps = NULL, log = NULL) {
  keys <- key_fields(fn)
  call <- as.call(c(
    list(as.name(".pbn_fn")),
    stats::setNames(lapply(keys, as.name), keys)
  ))
  if (!is.null(steps)) {
    call <- bquote(run_steps(.(call), .(name)))
  }
  # While run_plan() runs, .pbn_rng() first sets the kind of random number
  # generator of the run, which an earlier analysis may have changed.
  seed <- if ("sim" %in% keys) {
    quote({
      .pbn_rng(.pbn_run)
      set.seed(sim)
    })
  } else {
    NULL
  }
  where <- if (all(c("scenario", "sim") %in% keys)) {
    quote(list(scenario = scenario, sim = sim))
  } else {
    NULL
  }
  wrapper <- function() {
    return(NULL)
  }
  formals(wrapper) <- stats::setNames(
    rep(list(quote(expr = )), length(keys)),
    keys
  )
  # The seed is set in compute(), directly before the data function, so the
  # reads of the cache draw no number before it.
  value <- bquote(data_value(
    function() {
      .(seed)
      return(.(call))
    },
    .(name),
    .(where),
    .pbn_cache,
    .pbn_plan
  ))
  body(wrapper) <- bquote({
    if (!.pbn_catch) {
      return(.(value))
    }
    .pbn_enter(.pbn_run, data = TRUE)
    if (.pbn_stopping(.pbn_run, force = TRUE)) {
      return(.pbn_not_run())
    }
    .pbn_before <- .pbn_objects(.pbn_run)
    .pbn_out <- tryCatch(.(value), error = function(e) {
      return(structure(
        list(data = .(name), message = conditionMessage(e)),
        class = "pbn_data_error"
      ))
    })
    return(.pbn_data_checked(.pbn_out, .pbn_run, .pbn_before, .(name)))
  })
  environment(wrapper) <- data_wrapper_env(fn, plan_key, steps, log)
  return(wrapper)
}

# An action function that sets the seed to argset$seed and calls
# fn(data, argset). While run_plan() runs, an error, also an error of a data
# function, becomes an object of class pbn_failed_analysis, and NULL becomes
# an object of class pbn_null. plnr drops a NULL in some paths. While
# run_plan() runs, an action that changes the shape of the shared data also
# fails.
# While run_plan() runs, the wrapper returns an object of class pbn_timed:
# `value`, the value above, `seconds`, the elapsed time of fn(data, argset)
# only, `at`, the time of the clock of the computer at its end, and `data`,
# the records that it took from the log `log`. Only the
# analysis that counts the rows gets a `log`. run_plan() takes the value out
# again, so the timings never reach its value.
# While run_plan() runs, an analysis that changes an object of the functions
# folder also fails, see objects_value(). Each analysis that does not count
# the rows tells the run context in .pbn_run that it finished, see
# run_done(). A failed analysis stops the run, see run_failed(). After that,
# the wrapper calls nothing, and its value is a pbn_not_run, unless its data
# failed: then the analysis fails at once.
# plnr keeps one copy of the wrapper for each analysis, and each future of
# run_plan() serialises each copy. So the wrapper is one call, and its body
# is action_run() in the environment that the analyses of one kind share.
action_wrapper <- function(fn, log = NULL) {
  wrapper <- function(data, argset) {
    return(.pbn_action(data, argset))
  }
  e <- wrapper_env(fn, log, drain = !is.null(log))
  action <- action_run
  environment(action) <- e
  assign(".pbn_action", action, envir = e)
  call <- action_call
  environment(call) <- e
  assign(".pbn_call", call, envir = e)
  environment(wrapper) <- e
  return(wrapper)
}

# The body of action_wrapper(). Its environment is that of the wrapper.
action_run <- function(data, argset) {
  if (!.pbn_catch) {
    set.seed(argset[["seed"]])
    return(.pbn_fn(data, argset))
  }
  run <- .pbn_run
  .pbn_enter(run)
  taken <- NULL
  if (.pbn_drain) {
    taken <- .pbn_take(.pbn_log)
  }
  # An analysis whose data failed fails at no cost, also after the stop, so
  # each analysis of that data counts as failed.
  failed <- Filter(function(x) return(inherits(x, "pbn_data_error")), data)
  if (length(failed) == 0 && .pbn_stopping(run)) {
    return(structure(
      list(value = .pbn_not_run(), seconds = NA_real_, at = NA_real_, data = taken),
      class = "pbn_timed"
    ))
  }
  if (length(failed) > 0) {
    done <- list(
      out = structure(
        list(
          message = paste0(
            "The data function '",
            failed[[1]][["data"]],
            "' failed: ",
            failed[[1]][["message"]]
          )
        ),
        class = "pbn_failed_analysis"
      ),
      seconds = NA_real_,
      end = proc.time()[["elapsed"]]
    )
  } else {
    done <- .pbn_call(data, argset, run)
  }
  bad <- inherits(done$out, "pbn_failed_analysis")
  if (bad) {
    .pbn_failed(run, argset)
  }
  if (!.pbn_drain) {
    .pbn_done(run, argset, done$end, bad)
  }
  return(structure(
    list(
      value = done$out,
      seconds = done$seconds,
      at = as.numeric(Sys.time()),
      data = taken
    ),
    class = "pbn_timed"
  ))
}

# The call of fn(data, argset) of action_run(), with the run context `run`,
# and the checks of the data, the objects and the global environment after
# it. Returns a list: `out`, the value, `seconds`, the elapsed time of the
# call, and `end`, the elapsed time at its end. Its environment is that of
# the wrapper.
action_call <- function(data, argset, run) {
  before <- .pbn_shape(data)
  objects <- if (.pbn_drain) NULL else .pbn_objects(run)
  globals <- if (.pbn_drain) NULL else .pbn_globals(run, NULL)
  .pbn_rng(run)
  set.seed(argset[["seed"]])
  start <- proc.time()[["elapsed"]]
  if (!.pbn_drain) {
    .pbn_begin(run, argset, start)
  }
  out <- tryCatch(.pbn_fn(data, argset), error = function(e) {
    call <- conditionCall(e)
    return(structure(
      list(
        message = paste0(
          # The call of the wrapper itself, .pbn_fn(data, argset), names
          # nothing that the project wrote.
          if (!is.null(call) && !identical(call[[1]], as.name(".pbn_fn"))) {
            paste0("Error in ", paste(deparse(call), collapse = " "), ": ")
          } else {
            "Error: "
          },
          conditionMessage(e)
        )
      ),
      class = "pbn_failed_analysis"
    ))
  })
  end <- proc.time()[["elapsed"]]
  out <- .pbn_finish(out, before, .pbn_shape(data))
  out <- .pbn_objects_value(out, run, objects)
  out <- .pbn_globals(run, globals, out)
  return(list(out = out, seconds = end - start, end = end))
}

# The value of an analysis after fn(data, argset) returned `out`: `out`, or
# a pbn_failed_analysis when the shape of the data changed from `before` to
# `after`, or a pbn_null for NULL. `after` is computed only when `out` did
# not fail. Its environment is the base environment, so a worker needs no
# pbn to run it.
action_value <- function(out, before, after) {
  if (inherits(out, "pbn_failed_analysis")) {
    return(out)
  }
  changed <- names(after)[!mapply(identical, before, after)]
  if (length(changed) > 0) {
    return(structure(
      list(
        message = paste0(
          "The analysis changed the data '",
          paste(changed, collapse = "', '"),
          "' in place: a column, its vector or the rows changed. plnr ",
          "gives every analysis of one scenario and simulation the same ",
          "data object, so the next analysis would get the changed data. ",
          "Copy the data first, for example d <- data.table::copy(data$d)."
        )
      ),
      class = "pbn_failed_analysis"
    ))
  }
  if (is.null(out)) {
    return(structure(list(), class = "pbn_null"))
  }
  return(out)
}
environment(action_value) <- baseenv()

# The action of the analysis that counts the rows of the data: the number of
# rows of each data element that is a data.frame. For a list whose elements
# are all data.frames with one number of rows, for example a list of imputed
# data sets, it is the number of rows of one of them. For each other element
# it is NA. plnr adds the element hash, which is not data.
# The value carries these attributes, each only when a data element has it:
# - exclusions, the exclusion table of each data element from a cohort.
# - n_before, the number of rows given to the cohort of each such element.
# - cache, the cache record of each data element with cache = TRUE.
# This analysis is how they come back from a worker.
count_rows <- function(data, argset) {
  data <- data[names(data) != "hash"]
  out <- vapply(
    data,
    function(x) {
      if (is.data.frame(x)) {
        return(nrow(x))
      }
      if (
        is.list(x) &&
          length(x) > 0 &&
          all(vapply(x, is.data.frame, logical(1)))
      ) {
        n <- unique(vapply(x, nrow, integer(1)))
        if (length(n) == 1) {
          return(n)
        }
      }
      return(NA_integer_)
    },
    integer(1)
  )
  out <- stats::setNames(out, names(data))
  for (a in c("exclusions", "n_before", "cache")) {
    found <- lapply(data, attr, which = paste0("pbn_", a), exact = TRUE)
    found <- found[!vapply(found, is.null, logical(1))]
    if (length(found) > 0) {
      attr(out, a) <- found
    }
  }
  return(out)
}
environment(count_rows) <- baseenv()

#' Add data to a plan
#'
#' `add_data()` adds the data `name` to the plnr plan `p`, as the steps
#' `raw`, `clean`, `cohort` and `after_cohort`, or as one data function `fn`.
#' plnr calls it once for each scenario and simulation, and gives the data to
#' each analysis of that scenario and simulation.
#'
#' @param p A plan from `plnr::Plan$new()`.
#' @param name The name of the data. Each action function gets it as
#'   `data[[name]]`.
#' @param fn The free form: one data function, for data that does not fit
#'   the steps. Give `fn` or `raw`, not both.
#' @param raw The first step, normally `data_raw`, a `function(scenario,
#'   sim)` that reads the real data or makes fake data.
#' @param clean Optional. The step that gets the value of `raw`, normally
#'   `data_clean`, for example for names and types. It MUST return a
#'   data.frame with the same number of rows.
#' @param cohort Optional. The step that gets the value of `clean`, or of
#'   `raw` without `clean`, normally `data_cohort`. It MUST return a
#'   `cohort::CohortPipeline` that starts with every row that it got.
#' @param after_cohort Optional, and only with `cohort`. The step that gets
#'   the `cohort::CohortPipeline`, for example an imputation. It returns the
#'   data for the analyses, and pbn puts the exclusion table of the cohort on
#'   it. See the section A cohort as the data.
#' @param cache `TRUE` saves the data on disk for each scenario and
#'   simulation. The next run reads it while the cache key is the same. See
#'   the section Cache.
#' @details
#' In the steps form, each rule for `fn` in this section and in the section
#' Cache applies to `raw`.
#'
#' plnr keys a data function on its arguments without a default. These are
#' the key fields. Each key field of `fn` MUST be `scenario`, `sim` or a
#' setting column of `CFG$scenarios`. Otherwise [run_plan()] stops before it
#' runs anything. plnr calls `fn` once for each set of values of
#' the key fields, and gives each analysis with those values the same data.
#'
#' `add_data()` stops on these arguments of `fn`:
#' - `scenario` or `sim` with a default. plnr does not key on it, so every
#'   scenario or simulation would get the same data.
#' - `sim` without `scenario`. Every scenario would get the same data.
#'
#' A data function without `scenario` and `sim`, for example one that reads a
#' lookup table, runs once for the whole run and sets no seed.
#'
#' pbn adds `fn` in a wrapper. The wrapper has the key fields of `fn` as its
#' arguments, so plnr keys it in the same way. It calls `fn` with them by
#' name, so another argument of `fn` with a default keeps its default. When
#' `sim` is an argument, the wrapper calls `set.seed(sim)` before `fn`. The
#' data of simulation `k` is thus the same in each run, and does not depend
#' on the other scenarios of the run. A function that is called with no `sim`
#' sets no seed.
#'
#' The wrapper needs no object of the R session that adds it. [run_plan()]
#' gives each worker the objects that the functions folder defines.
#'
#' `name` MUST be one syntactic name that starts with a letter and holds
#' only letters, digits, `.` and `_`, for example `"d"`. pbn names each step
#' of the data `<name>$<step>` in the code that `fake_md5` hashes. A `$` in a
#' name could thus match a step of another data. `name` MUST NOT be
#' `"hash"`, because plnr adds `data$hash` to the data of each analysis. Two data functions of
#' one plan MUST NOT have the same name. Add data with `add_data()`, not with
#' `p$add_data()`. [save_results()] reads the data functions of `add_data()`
#' only.
#'
#' While [run_plan()] runs, an error in `fn` or in a step fails each
#' analysis that gets the data. When you call [get_data()] or `p$get_data()`
#' by hand, the error stops the call.
#' @section The steps:
#' pbn runs the steps in the order `raw`, `clean`, `cohort` and
#' `after_cohort`, and gives the value of each step to the next one. Each
#' step after `raw` MUST take exactly one argument without a default, or
#' `add_data()` stops. pbn calls `set.seed(sim)` once, before `raw`. So
#' `raw = data_raw, clean = data_clean` draws the same numbers as
#' `fn = function(scenario, sim) data_clean(data_raw(scenario, sim))`.
#'
#' pbn sees the value between the steps, so it checks the rule that the
#' cleaning drops no row. Each exclusion goes through the cohort, which counts
#' it. These values fail the data, as an error in `fn` does:
#' - `raw` returns a data.frame, and `clean` returns a value that is not a
#'   data.frame, or a data.frame with another number of rows.
#' - `cohort` returns a value that is not a `cohort::CohortPipeline`.
#' - `cohort` gets a data.frame, and its pipeline starts with another number
#'   of rows, `cp$n_total()`.
#'
#' The error gives both numbers of rows. The checks compare the numbers of
#' rows only, so the order of the rows and the columns MAY change. The checks
#' compare the number of rows, not which rows: a step that drops one row and
#' repeats another passes.
#'
#' When `raw` returns a value that is not a data.frame, pbn does not check
#' `clean`. An example is a list of two tables that `clean` joins. That list
#' has no number of rows to compare, and it is a valid start. pbn does not
#' check the rows of `after_cohort`.
#'
#' Use `fn` for data that does not fit the steps, for example data that
#' joins two cohorts. pbn then checks no rows inside `fn`.
#'
#' An exclusion MAY apply to the rows of a reshape, for example one row for
#' each person and time point. Add these rows as a second data element, with
#' a second call of `add_data()`. Its `raw` builds the reshaped rows, and its
#' `cohort` applies the exclusion, so the cohort counts it. Its `raw` MAY
#' reshape the value of the `raw` of the first element. pbn sets the seed to
#' `sim` before each data element, so both draw the same numbers.
#' @section A cohort as the data:
#' `fn` or the step `cohort` MAY return a `cohort::CohortPipeline` after its
#' exclusions. The analyses then get `data[[name]]` as the data.table of
#' `$get_included(<cohort>)`. `<cohort>` is the one cohort of the pipeline
#' that no other cohort branches from. A pipeline with more than one such
#' cohort fails the data. Each other value goes to the analyses as it is.
#'
#' The step `after_cohort` reads the data of the cohort with
#' [cohort_data()], and returns its value. The value MAY be any R object
#' other than `NULL`, for example a list of imputed data sets. pbn puts the
#' exclusion table of the cohort and its number of rows at the start on the
#' value, as [keep_exclusions()] does. These replace the attributes that the
#' step set. So a call of `keep_exclusions()` in the step changes nothing,
#' and a table from another cohort does not stay. The step MAY return the
#' `cohort::CohortPipeline` that it got. Another `cohort::CohortPipeline`
#' fails the data, because its exclusion table would replace that of the
#' step `cohort`.
#'
#' A step after the cohort in `fn` returns its value through
#' [keep_exclusions()], which puts the exclusion table on it.
#'
#' pbn keeps the exclusion table of the cohort, and [save_results()] stores
#' it in the element `exclusions` of the results. The table has one row for
#' each exclusion step of the cohort and of the cohorts above it, in the
#' order of the exclusion steps. It is a data.frame with these columns:
#' - `data`, the name of the data.
#' - `step`, the reason of `$exclude_and_track()`.
#' - `n_before`, `n_excluded` and `n_after`, the number of rows before the
#'   step, excluded by the step, and after the step. `n_excluded` and
#'   `n_after` are `n_excluded` and `n_remaining` of `$consort()`.
#'
#' The value carries the table in its attribute `pbn_exclusions`, and the
#' number of rows given to the pipeline in its attribute `pbn_n_before`.
#' [save_results()] writes that number to `manifest$n_rows_before_exclusions`.
#' The attributes are how both come back from a worker: the R6 object stays
#' on the worker.
#' @section Cache:
#' With `cache = TRUE`, `fn` MUST have the arguments `scenario` and `sim`
#' without a default. Call [initialize_project()] first. For
#' each scenario and simulation, [run_plan()] writes the value to
#' `<home>/<version>_cache_gitexclude/<name>/<scenario>/sim_<kkkk>.qs2`. The
#' value is the data.table and its exclusion table for a cohort. The cache
#' key goes to `sim_<kkkk>_key.qs2` in the same folder. The cache key is the
#' md5 of these parts:
#' - the scenario, the simulation and the settings of the scenario.
#' - for `"real"` only, the fingerprint of the data at the time of the use.
#'   It holds the name `<folder name>/<path in the folder>`, the size and
#'   the modification time of each file under each folder of `folders` of
#'   [initialize_project()]. pbn reads no content of a data file. The path
#'   of a folder is not in the key. So the same files in another place give
#'   the same key when they keep their modification times. A copy with new
#'   times looks like changed data, so the data is computed again. A file
#'   that changed in place, and kept its size and its modification time, is
#'   not seen.
#' - the md5 of each file of `<version>_functions/` whose name starts with
#'   `data_`, for example `data_raw.R`, `data_clean.R`, `data_cohort.R` and
#'   `data_impute.R`. [initialize_project()] computes them before it sources
#'   the files.
#' - the code of `fn`, or of each step, deparsed without its source
#'   references.
#' - `CFG$data`, when `CFG` has the element `data`. Put the config that the
#'   data code reads there. A change in another element of `CFG` does not
#'   change the key.
#' - the versions of R and pbn. The version of R covers the base packages.
#' - the three parts of [RNGkind()].
#'
#' The key file also records the version of each package that was loaded in
#' the R session that computed the value, after `fn` returned. On a worker,
#' that is the worker. The record leaves out the base packages. It also
#' leaves out the packages that run the plan, tests and documents: pbn,
#' plnr, foreach, doParallel, doFuture, future, future.apply, globals,
#' listenv, parallelly, progressr, iterators, qs2, digest, uuid, progress,
#' testthat, pkgload, devtools, callr, processx, ps, knitr, rmarkdown and
#' evaluate. A later use
#' compares each recorded version with [utils::packageVersion()] now. A
#' package with another version, or one that is not installed now, computes
#' the value again.
#'
#' Each use of the cache stops when a `data_*.R` file on disk differs from
#' the file that [initialize_project()] hashed and sourced: changed, added or
#' removed. The error names the file. Run [initialize_project()] again. A
#' file with its recorded size and modification time is not hashed again,
#' so a change that keeps both is not found.
#'
#' Keep the data code in the files `data_*.R`. The key holds no other file.
#' A function of the data code can be in another file. A change in it then
#' leaves the key the same, and the run reads the old data. This is the one
#' change of the data code that the cache does not find.
#'
#' When the key is the same, [run_plan()] reads the value from the cache and
#' does not call `fn` or a step. Each analysis sets its own seed, so its
#' results are the same as with a new value. Otherwise, or when a cache file
#' cannot be read, it calls `fn` or the steps and writes the cache. To compute every value again,
#' delete the folder `<version>_cache_gitexclude/`. No argument does that.
#'
#' For each scenario and simulation, [run_plan()] prints one line with
#' `cat()`. The line says that the data came from the cache, with the date
#' and time of the cache file. Or it says that pbn computed it, and why.
#' The reason names each changed `data_*.R` file, `CFG$data`, or the
#' settings. It names changed raw data with its folders, for example "the
#' raw data in the folder lookup". It names a changed `fn` as "the data function of
#' add_data()", and each changed step as, for example, "the function given
#' as `clean`". It can also name the kind of random
#' number generator, or each package with another version. With `workers = 1`, the line comes before the
#' data function runs. With more workers, [run_plan()] prints the lines after
#' the run, in the order of the plan.
#'
#' [run_plan()] writes each file to a temporary file in the same folder, and
#' renames it. plnr runs the data of one scenario and simulation in one task,
#' so two workers of one run never write the same file. When two runs write
#' one file at the same time, the last rename wins, and each file is whole.
#' The cache file holds its key too, so a cache file and a key file from two
#' runs give a new computation.
#'
#' The cache folder holds data for each person. When the project is a git
#' repository, git MUST ignore it: [run_plan()] stops before it runs anything
#' when git does not ignore the cache files. Add the line `*gitexclude*` to
#' the `.gitignore` of the project.
#'
#' `p$get_data()` and `p$run_one()` by hand use the cache too, and print the
#' same line. pbn computes the key at the time of the call, with the same
#' rules.
#' @return Invisibly, the name of the data.
#' @examples
#' p <- plnr::Plan$new()
#' pbn::add_data(
#'   p,
#'   "d",
#'   raw = function(scenario, sim) return(data.frame(X = stats::rnorm(3))),
#'   clean = function(d) return(stats::setNames(d, tolower(names(d))))
#' )
#' @family plan
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @importFrom plnr Plan
#' @export
add_data <- function(
  p,
  name,
  fn = NULL,
  raw = NULL,
  clean = NULL,
  cohort = NULL,
  after_cohort = NULL,
  cache = FALSE
) {
  check_plan(p)
  check_data_name(name)
  steps <- check_data_steps(
    fn,
    list(raw = raw, clean = clean, cohort = cohort, after_cohort = after_cohort)
  )
  if (!is.null(steps)) {
    fn <- steps[["raw"]]
  }
  check_data_formals(fn)
  check_cache_arg(cache, fn)
  info <- plan_info(p, create = TRUE)
  if (name %in% names(info$data)) {
    stop(
      "The plan already has the data \"",
      name,
      "\". Give each data function its own name.",
      call. = FALSE
    )
  }
  wrapper <- data_wrapper(
    fn,
    name,
    if (cache) data.table::address(p) else NULL,
    steps,
    info$log
  )
  p$add_data(name, fn = wrapper)
  info$data[[name]] <- fn
  info$data_wrappers[[name]] <- wrapper
  # The scenarios of the run when the data was added, for get_data() before
  # add_analyses().
  info$scenarios <- state$scenarios
  info$steps[[name]] <- steps
  info$cache[[name]] <- cache
  info$data_envs[[name]] <- environment(wrapper)
  info$wrappers <- c(info$wrappers, list(environment(wrapper)))
  return(invisible(name))
}

#' Add one analysis for each scenario and simulation to a plan
#'
#' `add_analyses()` adds the analysis kind `name` to the plnr plan `p`: one
#' analysis for each scenario and simulation of `scenarios`, and for each
#' combination of the extra dimensions in `...`.
#'
#' @param p A plan from `plnr::Plan$new()`.
#' @param scenarios The scenarios of the run, from [scenarios()].
#' @param name The name of the analysis kind, for example `"models"`. It is
#'   the name of the element of the results object. It MUST be a syntactic
#'   name that starts with a letter. It MUST NOT be `"manifest"` or
#'   `"exclusions"`, and it MUST NOT start with `"pbn_"`. Each kind of one
#'   plan MUST have its own name. [save_results()] names the exclusion table
#'   of a cohort `exclusions`.
#' @param fn The action function, `function(data, argset)`.
#' @param ... The extra dimensions, each named, for example
#'   `outcome = CFG$outcomes`. Each MUST be an atomic vector with at least one
#'   value, with no `NA` and no value twice. A vector of length 1 is a
#'   constant of the kind.
#' @param control `TRUE` adds the kind to every fake scenario of
#'   `scenarios`, and never to `"real"`. Use it for a check that is not a
#'   result, for example a model without adjustment that a fake scenario
#'   tests.
#' @details
#' Each analysis gets the name
#' `<scenario>__sim_<kkkk>__<name>__<i>`, for example
#' `null__sim_0003__models__2`. `<kkkk>` is the simulation with four digits,
#' also for a scenario with 1 simulation. `<i>` counts the combinations of the
#' extra dimensions from 1, with the last dimension of `...` changing
#' fastest. A kind without extra dimensions has `<i>` 1. To step through one
#' analysis by hand, use [get_argset()] and [get_data()]. They take the
#' scenario, the simulation, the kind and `<i>`, so you do not type the name.
#'
#' The argset of each analysis holds these fields:
#' - `scenario`, `sim`, and `analysis`, the kind `name`.
#' - `seed`, the seed of the analysis.
#' - the value of each extra dimension.
#' - the value of each setting column of its scenario.
#'
#' The name of an extra dimension MUST follow these rules:
#' - It MUST be syntactic.
#' - It MUST NOT be the name of a setting column.
#' - It MUST NOT be one of `scenario`, `sim`, `analysis`, `seed`,
#'   `index_analysis`, `name`, `fn`, `fn_name`, `data` and `argset`.
#'
#' pbn adds `fn` in a wrapper. The wrapper calls `set.seed(argset$seed)` and
#' then `fn(data, argset)`. `fn` runs inside the wrapper, so
#' `plnr::is_run_directly()` is `FALSE` in it when the plan runs.
#'
#' The seed depends on the kind `name`, the counter `<i>` and `sim` only:
#' - It does not depend on the order of the analyses, the worker count or
#'   the R session.
#' - Two kinds of one simulation get different seeds, so they draw different
#'   numbers.
#' - One kind gets the same seed in each scenario, so its draws in `null`
#'   and in `real` of one simulation are the same.
#' - It is 10000 or more, so it differs from the seed `sim` of the data of
#'   simulations 1 to 9999.
#'
#' The seed is a polynomial hash with base 257, modulo the prime 2147483629,
#' of the code points of `"<name>__<i>__<sim>"`, plus 10000. Two analyses of
#' one simulation with the same seed stop `add_analyses()` with an error
#' that names both kinds.
#'
#' While [run_plan()] runs, an analysis fails when it changes the shared data
#' in place. plnr gives every analysis of one scenario and simulation the
#' same object. pbn compares the names, the number of rows, the
#' data.table key and the address of each column of each data.frame before
#' and after the analysis. That finds an added, removed, renamed or replaced
#' column, for example `data$d[, x := 1]`, and a new key from
#' `data.table::setkey()`. It does not find a change of values in place:
#' `data$d[1, x := 0]`, `data$d[g == "a", y := 0]`, `data.table::set()` on
#' some rows, `data.table::setorder()` and `data.table::setattr()` on a
#' column. Copy the data first with [data.table::copy()] when the analysis
#' changes it.
#'
#' While [run_plan()] runs, an analysis also fails when it changes an object
#' of the functions folder, for example `CFG`. plnr runs many analyses in one
#' R session, so the change would reach each later analysis of that session.
#' A result would then depend on the analyses that ran before it there. pbn
#' checks each object that the functions folder made or changed when
#' [initialize_project()] sourced it, other than a function, a plnr plan and
#' `.Random.seed`. It compares the bytes of [serialize()] of each object
#' before and after the analysis. That finds a new value, a new element and
#' a new attribute. It compares them before and after each data function or
#' data step too. A data function that changes such an object fails each
#' analysis that gets its data. The error names the first top-level element
#' that changed, for example `CFG$table`. It also says how: a new attribute
#' such as the index of a data.table, a new or a gone element, or another
#' value. The
#' output function of [make_outputs()] and each `expect_` function of
#' [save_results()] get the same check in this R session.
#'
#' While [run_plan()] runs, two more parts of the state of an R session
#' cannot pass from one analysis to the next:
#' - An analysis fails when it leaves a new object in the global
#'   environment, for example with `<<-` or `assign()`. `.Random.seed` does
#'   not count.
#' - pbn takes the kind of random number generator of the run from
#'   [RNGkind()] at the start of [run_plan()]. It sets that kind again
#'   before each seed of a data function and of an analysis. An analysis
#'   that calls `RNGkind()` then changes no draw of a later analysis.
#'
#' A data.table subset with a logical, for example
#' `CFG$table[group == "a"]`, adds an index to the table in place. Copy the
#' table first with [data.table::copy()], or set
#' `options(datatable.auto.index = FALSE)` in the project. pbn sets neither.
#'
#' The check has these limits:
#' - It does not see a function inside an object, because R compiles a
#'   function after some calls.
#' - It does not see the contents of an environment inside an object, for
#'   example that of a formula. It does not call an active binding.
#' - It does not see a change of an object that already was in the global
#'   environment, other than an object of the functions folder.
#' - The analysis that made the change fails, and the change stays in that
#'   R session. The run stops at that failure, see [run_plan()].
#' - Without [initialize_project()], pbn checks no object.
#'
#' The check costs time in proportion to the size of the objects. One
#' computer took about 45 microseconds for each analysis with a `CFG` of 49
#' elements. These held 5 data.tables of 100 rows, 20 KB in [serialize()].
#'
#' For each scenario and simulation, pbn also adds one analysis named
#' `<scenario>__sim_<kkkk>__pbn_n_rows`. It counts the rows of each data
#' element. [save_results()] stores the counts in `manifest$n_rows_data`, and
#' leaves the analysis out of the results.
#'
#' `add_analyses()` stops before it adds anything when an analysis of that
#' name is in the plan already.
#' @return Invisibly, the names of the analyses of the kind, in the order of
#'   the plan.
#' @examples
#' CFG <- list(
#'   scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 2)),
#'   outcomes = c("y1", "y2")
#' )
#' p <- plnr::Plan$new()
#' pbn::add_data(p, "d", fn = function(scenario, sim) {
#'   return(data.frame(y1 = stats::rnorm(5), y2 = stats::rnorm(5)))
#' })
#' pbn::add_analyses(
#'   p,
#'   pbn::scenarios(CFG),
#'   name = "means",
#'   fn = function(data, argset) {
#'     return(data.frame(mean = mean(data$d[[argset$outcome]])))
#'   },
#'   outcome = CFG$outcomes
#' )
#' pbn::get_argset(p, "null", 2, "means", 2)
#' @family plan
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
add_analyses <- function(p, scenarios, name, fn, ..., control = FALSE) {
  check_plan(p)
  table <- check_scenario_table(scenarios, "`scenarios`")
  check_kind_name(name)
  if (!is.function(fn)) {
    stop("`fn` must be a function(data, argset).", call. = FALSE)
  }
  if (!isTRUE(control) && !isFALSE(control)) {
    stop("`control` must be TRUE or FALSE.", call. = FALSE)
  }
  dims <- list(...)
  check_dims(dims, setting_columns(table))
  info <- plan_info(p, create = TRUE)
  if (name %in% info$kinds$analysis) {
    stop(
      "The plan already has the analysis kind \"",
      name,
      "\". Give each call of add_analyses() its own name.",
      call. = FALSE
    )
  }
  if (control) {
    table <- table[table$scenario != "real", , drop = FALSE]
  }
  planned <- planned_analyses(table, name, dims)
  hidden <- planned$hidden[!planned$hidden$name %in% names(p$analyses), ]
  taken <- intersect(c(hidden$name, planned$kind$name), names(p$analyses))
  if (length(taken) > 0) {
    stop(
      "The plan already has analyses with these names, so pbn added none: ",
      paste(taken, collapse = ", "),
      call. = FALSE
    )
  }
  seeds <- check_seeds(
    info$seeds,
    c(hidden$name, planned$kind$name),
    c(hidden$argset, planned$kind$argset)
  )
  count <- action_wrapper(count_rows, info$log)
  action <- action_wrapper(fn)
  for (i in seq_len(nrow(hidden))) {
    do.call(
      p$add_analysis,
      c(list(name = hidden$name[i], fn = count), hidden$argset[[i]])
    )
  }
  for (i in seq_len(nrow(planned$kind))) {
    do.call(
      p$add_analysis,
      c(
        list(name = planned$kind$name[i], fn = action),
        planned$kind$argset[[i]]
      )
    )
  }
  info$kinds <- rbind(
    info$kinds,
    data.frame(analysis = name, control = control)
  )
  info$dims[[name]] <- names(dims)
  info$seeds <- seeds
  info$settings <- union(info$settings, setting_columns(table))
  for (s in setdiff(table$scenario, names(info$scenario_settings))) {
    info$scenario_settings[[s]] <- settings_of(table, s)
  }
  info$names <- c(info$names, planned$kind$name)
  info$hidden <- c(info$hidden, hidden$name)
  info$wrappers <- c(
    info$wrappers,
    list(environment(count), environment(action))
  )
  return(invisible(planned$kind$name))
}

# Stops when `name` is not a valid name of the data of add_data().
check_data_name <- function(name) {
  if (
    !is.character(name) || length(name) != 1 || is.na(name) || !nzchar(name)
  ) {
    stop("`name` must be one non-empty string.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9._]*$", name) || make.names(name) != name) {
    stop(
      "`name` must be a syntactic name that starts with a letter and holds ",
      "only letters, digits, \".\" and \"_\", for example \"d\".",
      call. = FALSE
    )
  }
  if (name %in% c("hash", "data__________go_up_one_level")) {
    stop(
      "`name` must not be \"",
      name,
      "\", because plnr uses that name.",
      call. = FALSE
    )
  }
  return(invisible(name))
}

# Stops when `cache` is not TRUE or FALSE, or when it is TRUE for a data
# function without the key fields scenario and sim.
check_cache_arg <- function(cache, fn) {
  if (!isTRUE(cache) && !isFALSE(cache)) {
    stop("`cache` must be TRUE or FALSE.", call. = FALSE)
  }
  if (cache && !all(c("scenario", "sim") %in% key_fields(fn))) {
    stop(
      "With cache = TRUE, the data function needs the arguments scenario ",
      "and sim without a default, because the cache keeps one file for each ",
      "scenario and simulation.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Stops when `scenario` or `sim` is an argument of `fn` with a default, or
# when `sim` is an argument without `scenario`. A defaulted `sim` gives each
# simulation the same data, and `sim` without `scenario` gives each scenario
# the same data.
check_data_formals <- function(fn) {
  nm <- names(formals(fn))
  defaulted <- setdiff(intersect(c("scenario", "sim"), nm), key_fields(fn))
  if (length(defaulted) > 0) {
    stop(
      "The data function has ",
      paste(defaulted, collapse = " and "),
      " with a default. plnr keys a data function on its arguments without ",
      "a default, so every ",
      if ("sim" %in% defaulted) "simulation" else "scenario",
      " would get the same data. Remove the default.",
      call. = FALSE
    )
  }
  if ("sim" %in% nm && !"scenario" %in% nm) {
    stop(
      "The data function has the argument sim but not scenario, so every ",
      "scenario would get the same data. Use function(scenario, sim).",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

check_kind_name <- function(name) {
  ok <- is_one_string(name) &&
    !any(
      !grepl("^[A-Za-z]", name),
      make.names(name) != name,
      name %in% c("manifest", "exclusions"),
      startsWith(name, "pbn_")
    )
  if (!ok) {
    stop(
      "`name` must be one syntactic name that starts with a letter, for ",
      "example \"models\". It must not be \"manifest\" or \"exclusions\", ",
      "which save_results() writes, and must not start with \"pbn_\".",
      call. = FALSE
    )
  }
  return(invisible(name))
}

# Stops when the extra dimensions `dims` of add_analyses() break a rule.
check_dims <- function(dims, settings) {
  if (length(dims) == 0) {
    return(invisible(NULL))
  }
  nm <- names(dims)
  if (is.null(nm) || !all(nzchar(nm)) || anyDuplicated(nm) > 0) {
    stop(
      "Each extra dimension in `...` needs its own name, for example ",
      "`outcome = CFG$outcomes`.",
      call. = FALSE
    )
  }
  bad <- nm[make.names(nm) != nm | nm %in% c(reserved_fields, "control")]
  if (length(bad) > 0) {
    stop(
      "These extra dimensions need another name: ",
      paste(bad, collapse = ", "),
      ". A name MUST be syntactic and MUST NOT be one of ",
      paste(reserved_fields, collapse = ", "),
      call. = FALSE
    )
  }
  clash <- intersect(nm, settings)
  if (length(clash) > 0) {
    stop(
      "These extra dimensions have the name of a setting column of ",
      "CFG$scenarios: ",
      paste(clash, collapse = ", "),
      ". Give the dimension or the setting another name.",
      call. = FALSE
    )
  }
  for (d in nm) {
    check_dim_values(dims[[d]], d)
  }
  return(invisible(NULL))
}

# Stops when the values `v` of the extra dimension `d` break a rule.
check_dim_values <- function(v, d) {
  if (!is.atomic(v) || !is.null(dim(v)) || any(length(v) == 0, anyNA(v))) {
    stop(
      "The extra dimension ",
      d,
      " must be an atomic vector with at least one value and no NA.",
      call. = FALSE
    )
  }
  if (anyDuplicated(v) > 0) {
    stop(
      "The extra dimension ",
      d,
      " has a value twice. Give each value once.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The seeds of each simulation of the plan, `seeds`, with those of the new
# analyses `names` with the argsets `argsets` added. Each element is named
# by the simulation and holds the seeds named by the end of the analysis
# name, <kind>__<k>, which is the same in each scenario. Two analyses of one
# simulation with one seed would draw the same numbers, so a seed that is
# there twice in one simulation stops add_analyses() before it adds
# anything.
check_seeds <- function(seeds, names, argsets) {
  for (i in seq_along(argsets)) {
    a <- argsets[[i]]
    sim <- as.character(a[["sim"]])
    head <- paste0(a[["scenario"]], "__", sim_folder(a[["sim"]]), "__")
    key <- substring(names[[i]], nchar(head) + 1)
    have <- seeds[[sim]]
    if (key %in% names(have)) {
      next
    }
    clash <- names(have)[have == a[["seed"]]]
    if (length(clash) > 0) {
      stop(
        "Two analyses of simulation ",
        sim,
        " get the seed ",
        a[["seed"]],
        ": the kinds ",
        sub("__[0-9]+$", "", clash[1]),
        " and ",
        a[["analysis"]],
        ". Give one of the kinds another name. pbn added nothing.",
        call. = FALSE
      )
    }
    seeds[[sim]] <- c(have, stats::setNames(a[["seed"]], key))
  }
  return(seeds)
}

# The combinations of the extra dimensions, in the order of the counter: the
# last dimension changes fastest. A kind without extra dimensions has one
# empty combination.
dims_grid <- function(dims) {
  if (length(dims) == 0) {
    return(list(list()))
  }
  index <- expand.grid(
    rev(lapply(dims, seq_along)),
    KEEP.OUT.ATTRS = FALSE
  )
  return(lapply(seq_len(nrow(index)), function(j) {
    values <- lapply(names(dims), function(d) {
      return(dims[[d]][[index[[d]][j]]])
    })
    return(stats::setNames(values, names(dims)))
  }))
}

# The analyses of one kind, and the analyses that count the rows, with their
# names and argsets, in the order of the plan.
planned_analyses <- function(table, name, dims) {
  combos <- dims_grid(dims)
  kind_names <- character(0)
  kind_args <- list()
  hidden_names <- character(0)
  hidden_args <- list()
  for (r in seq_len(nrow(table))) {
    s <- table$scenario[r]
    settings <- settings_of(table, s)
    for (sim in seq_len(table$sims[r])) {
      base <- list(scenario = s, sim = sim)
      hidden_names <- c(hidden_names, analysis_name(s, sim, n_rows_kind))
      hidden_args <- c(
        hidden_args,
        list(c(
          base,
          list(
            analysis = n_rows_kind,
            seed = analysis_seed(n_rows_kind, 0, sim)
          ),
          settings
        ))
      )
      for (k in seq_along(combos)) {
        kind_names <- c(kind_names, analysis_name(s, sim, name, k))
        kind_args <- c(
          kind_args,
          list(c(
            base,
            list(analysis = name, seed = analysis_seed(name, k, sim)),
            combos[[k]],
            settings
          ))
        )
      }
    }
  }
  return(list(
    kind = list_frame(kind_names, kind_args),
    hidden = list_frame(hidden_names, hidden_args)
  ))
}

# A data.frame with the column name and the list column argset.
list_frame <- function(names, argsets) {
  out <- data.frame(name = names)
  out$argset <- argsets
  return(out)
}

# The name of an analysis: <scenario>__sim_<kkkk>__<kind>__<k>, or without
# __<k> for the analysis that counts the rows.
analysis_name <- function(scenario, sim, kind, k = NULL) {
  out <- paste0(scenario, "__", sim_folder(sim), "__", kind)
  if (!is.null(k)) {
    out <- paste0(out, "__", k)
  }
  return(out)
}
