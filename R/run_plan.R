#' Run a plan
#'
#' `run_plan()` runs every analysis of the plnr plan `p`, in this R session or
#' on `workers` new R sessions. It stops when an analysis fails, and then it
#' prints each failed analysis with its error.
#'
#' @param p A plan from `plnr::Plan$new()`, with data from [add_data()] and
#'   analyses from [add_analyses()].
#' @param workers The number of R sessions that run the analyses, one whole
#'   number of 1 or more.
#' @details
#' With `workers = 1`, `run_plan()` calls `p$run_all()` in this R session.
#'
#' With `workers` 2 or more, `run_plan()` needs the packages doParallel and
#' foreach. It starts a PSOCK cluster of `workers` new R sessions with
#' [parallel::makePSOCKcluster()], registers it with
#' [doParallel::registerDoParallel()], and calls `p$run_all()` with the
#' foreach path of plnr. A PSOCK cluster does not fork, so it also works on
#' Windows. Before the run, `run_plan()` gives each worker these:
#' - the library paths of this R session, from [.libPaths()].
#' - the packages attached in this R session, in the order of [search()],
#'   for example data.table after `library(data.table)` in the run script.
#' - the kind of random number generator of this R session, from
#'   [RNGkind()].
#' - every object of the environment that [initialize_project()] sourced the
#'   functions folder into, `.GlobalEnv` by default. So a data function and an
#'   action function find `CFG` and the other functions of the folder. A plnr
#'   plan and `.Random.seed` are left out. Each object is copied to each
#'   worker, so keep large objects out of that environment.
#' - the paths in `org::project`, such as `org::project$data_raw`.
#'
#' `run_plan()` does not copy the R options, for example
#' `options(digits = 3)`, and no other state of this R session.
#'
#' After the run, `run_plan()` stops the cluster and registers the sequential
#' foreach backend, also after an error.
#'
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
#' depth.
#'
#' `run_plan()` runs every analysis, also after an analysis fails. An error in
#' an action function fails that analysis, and an error in a data function
#' fails each analysis that gets the data. This is the same with 1 worker and
#' with more. After the run, `run_plan()` prints each failed analysis with its
#' name and its full error message with `cat()`, and then it stops. R cuts an
#' error message at 1000 bytes by default, and `cat()` does not. pbn then
#' saves nothing, because [save_results()] needs the value of `run_plan()`.
#' The results files of an earlier run stay as they are.
#'
#' The value carries the attribute `pbn_run`, a stamp of the plan. It holds
#' no count of the runs, so two runs of one plan can be `identical()`.
#' [save_results()] stops on a value without the stamp of its plan.
#'
#' `run_plan()` sets `use_foreach` of the plan: `FALSE` for 1 worker and
#' `TRUE` for more.
#'
#' After [initialize_project()], `run_plan()` first computes the md5 of each
#' raw data file again when its size or its modification time changed. The
#' value carries the md5 in its stamp, and [save_results()] writes it to
#' `manifest$data_md5`.
#'
#' For a data function with `cache = TRUE`, `run_plan()` needs
#' [initialize_project()] first, and it prints one line for each scenario and
#' simulation. The section Cache of [add_data()] gives the rules. A data
#' function that returns a `cohort::CohortPipeline` runs on the worker, and
#' only its data.table and its exclusion table come back. The section A
#' cohort as the data of [add_data()] gives the rules.
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
  if (workers > 1) {
    check_parallel_packages()
  }
  # The md5 of the raw data of this run, also after a change of a file since
  # initialize_project(). The stamp carries it to the manifest.
  if (!is.null(state$results)) {
    refresh_data_md5()
  }
  data_md5 <- state$data_md5
  cached <- set_cache(p, info, workers)
  on.exit(clear_cache(info, cached), add = TRUE)
  set_catch(info, TRUE)
  on.exit(set_catch(info, FALSE), add = TRUE)
  if (workers == 1) {
    p$set_use_foreach(FALSE)
    r <- p$run_all()
  } else {
    r <- run_on_cluster(p, workers)
  }
  r <- unbox_results(r, names(p$analyses))
  if (workers > 1 && length(cached) > 0) {
    print_cache_lines(r, info)
  }
  stop_on_failed(r)
  # The stamp binds the value to this plan. save_results() checks it. It
  # holds no count of the runs, so two runs of one plan stay identical().
  attr(r, "pbn_run") <- list(plan = info$token, data_md5 = data_md5)
  return(invisible(r))
}

# Stops when a package of a run with workers > 1 is missing.
check_parallel_packages <- function() {
  missing <- c("doParallel", "foreach")
  missing <- missing[!vapply(missing, has_package, logical(1))]
  if (length(missing) > 0) {
    stop(
      "run_plan() with workers > 1 needs the packages ",
      paste(missing, collapse = " and "),
      ". Install them, or use workers = 1.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# p$run_all() with the foreach path of plnr, on a PSOCK cluster of
# `workers` R sessions. The cluster stops on exit, also after an error.
run_on_cluster <- function(p, workers) {
  cl <- parallel::makePSOCKcluster(workers)
  on.exit(
    {
      parallel::stopCluster(cl)
      foreach::registerDoSEQ()
    },
    add = TRUE
  )
  prepare_workers(cl)
  doParallel::registerDoParallel(cl)
  p$set_use_foreach(TRUE)
  return(repair_tables(p$run_all()))
}

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

# Gives each worker of the cluster `cl` the library paths, the kind of
# random number generator, the objects of the sourced environment and the
# paths of org::project of this R session.
prepare_workers <- function(cl) {
  parallel::clusterCall(cl, .libPaths, .libPaths())
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  parallel::clusterCall(cl, attach_packages, attached)
  kinds <- RNGkind()
  parallel::clusterCall(cl, RNGkind, kinds[1], kinds[2], kinds[3])
  env <- state$env
  if (is.environment(env)) {
    objects <- ls(env, all.names = TRUE)
    keep <- vapply(
      objects,
      function(o) return(!inherits(get(o, envir = env), "Plan")),
      logical(1)
    )
    objects <- setdiff(objects[keep], ".Random.seed")
    parallel::clusterExport(cl, objects, envir = env)
  }
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
  return(invisible(NULL))
}

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
# one.
stop_on_failed <- function(r) {
  failed <- failed_analyses(r)
  if (length(failed) == 0) {
    return(invisible(NULL))
  }
  for (nm in names(failed)) {
    cat("pbn: the analysis ", nm, " failed:\n", failed[[nm]], "\n", sep = "")
  }
  utils::flush.console()
  stop(
    length(failed),
    " of ",
    length(r),
    " analyses failed. pbn printed each failed analysis and its error above, ",
    "and saved nothing. The results files of an earlier run stay as they are.",
    call. = FALSE
  )
}
