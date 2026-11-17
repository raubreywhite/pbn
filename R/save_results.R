#' Save the results of a run
#'
#' `save_results()` writes one results object for each scenario and
#' simulation of a run, and stacks the simulations. It checks the fake
#' scenarios with their `expect_` functions, and writes nothing when an
#' object fails [check_results()].
#'
#' @param r The value of [run_plan()] for the plan `p`.
#' @param p The plan, with data from [add_data()] and analyses from
#'   [add_analyses()].
#' @param scenarios The scenarios of the run, from [scenarios()]. They MUST
#'   be the scenarios of the analyses of `p`.
#' @param gitexclude `NULL`, or the names of the analysis kinds whose
#'   results hold data for each person. See the section Two files.
#' @details
#' Call [initialize_project()] first. For each scenario and simulation,
#' `save_results()` builds one results object, a named list. Its first
#' element is `manifest`. When a data function returned a
#' `cohort::CohortPipeline`, the next element is `exclusions`, the exclusion
#' tables of the cohorts row-bound in the order of [add_data()]. The column
#' `data` names the data of each row. The section A cohort as the data of
#' [add_data()] gives the columns. A plan without a cohort has no element
#' `exclusions`. Then comes one element for each analysis kind of
#' [add_analyses()], under the kind name and in the order of the calls. The
#' scenario `"real"` has no element of a kind with `control = TRUE`. The
#' element of a kind holds the values of its analyses for that scenario and
#' simulation, in the order of the plan:
#' - When each value is a data.frame, the element is the values row-bound.
#'   Each extra dimension of the kind comes first as a column, unless the
#'   data.frame has a column of that name already. A data.table stays a
#'   data.table. A kind without extra dimensions has the data.frame of its one
#'   analysis.
#' - Otherwise, a kind without extra dimensions has the value of its one
#'   analysis.
#' - Otherwise, the element is a named list with one entry for each
#'   analysis. Each name is `<name>__<i>`, the analysis name without the
#'   scenario and the simulation. So the names are the same in each
#'   simulation. [add_analyses()] gives the order of `<i>`.
#'
#' The `manifest` is a list with these elements:
#' - `pbn_version` and `r_version`.
#' - `packages`, a named character vector. The names are the attached and
#'   loaded packages of this R session that are not base packages, and the
#'   values are their versions. `qs2` is always in the list, because it writes
#'   the file.
#' - `run_date`, the date of the call as `YYYY-MM-DD`. It has no time, so two
#'   runs on one day with the same inputs and packages write the same bytes.
#' - `version`, `scenario`, `sim` and `sims`.
#' - `settings`, the named list of the setting columns of the scenario. It is
#'   an empty list when there are none.
#' - `source_md5`, the md5 of `<version>_run.R` and of each file in
#'   `<version>_functions/`, named by the path relative to `home`.
#'   [initialize_project()] computes it before it sources the files. The
#'   rule `manifest_source` needs the run script in it, so `save_results()`
#'   stops when `<version>_run.R` did not exist.
#' - For the scenario `"real"` only, `data_md5`, the md5 of each file directly
#'   in `data_raw` when [run_plan()] started. So it names the data that the
#'   run read, also after a change of a file since [initialize_project()].
#'   `save_results()` stops for `"real"` when `data_raw` did not exist when
#'   [initialize_project()] ran.
#' - For a fake scenario only, `fake_md5`, one md5 of the code of the data
#'   functions of [add_data()], without their source references: each `fn`,
#'   and each step of the steps form. A change in a function that `fn` or a
#'   step calls does not change `fake_md5`. `source_md5` holds the md5 of
#'   the files of those functions.
#' - `n_rows_data`, a named integer vector with the number of rows of each
#'   data element. A list of data.frames with one number of rows, for
#'   example a list of imputed data sets, has the rows of one of them. Each
#'   other element that is not a data.frame has `NA`. The
#'   analysis `<scenario>__sim_<kkkk>__pbn_n_rows` of [add_analyses()] counts
#'   them where the data is, also on a worker. The rule `no_row_per_person`
#'   reads it.
#' - `n_rows_before_exclusions`, a named integer vector with the number of
#'   rows given to the cohort of each data element, or `NA` for data without
#'   a cohort. `no_row_per_person` reads it too.
#' - `n_rows_after_exclusions`, the same for the rows after the exclusions:
#'   `n_after` of the last step of the exclusion table. It has a number also
#'   when `n_rows_data` is `NA`, for example for a `mids` object.
#' - `analyses`, a data.frame with the columns `analysis`, `control` and
#'   `file`: the analysis kinds in the object, and the file of each,
#'   `"results.qs2"` or `"results_gitexclude.qs2"`.
#' - `data_cache`, a data.frame with one row for each data element of
#'   [add_data()] and the columns `data`, `cache`, `from_cache` and
#'   `cache_key`. `cache` is the argument of [add_data()]. `from_cache` is
#'   `TRUE` when [run_plan()] read the data from the cache. `cache_key` is the
#'   md5 of the cache key, or `NA` without a cache.
#'
#' `save_results()` does these steps in this order. It stops at the first step
#' that fails:
#' 1. It checks its arguments. It stops when `r` has no stamp of
#'    [run_plan()] for `p`. It stops when the settings of `scenarios` are not
#'    the settings in the argsets of `p`. It stops when a fake scenario has no function
#'    `expect_<scenario>(results)` in the environment of
#'    [initialize_project()], or when an analysis of `r` failed. It writes
#'    nothing then.
#' 2. It builds every results object, and the stacked object of each scenario
#'    with more than 1 simulation, and checks each with [check_results()]. On
#'    a failure, it prints each failed rule with `cat()`, writes nothing and
#'    stops.
#' 3. For each scenario, it replaces `check.csv` with one failed assertion,
#'    `not checked yet`, so that [read_results()] refuses the new results
#'    until step 4 checks them. It writes
#'    `<version>_results/<scenario>/results.qs2` for 1 simulation. For more
#'    simulations, it writes `<version>_results/<scenario>/sim_<kkkk>/results.qs2`
#'    for each, and then the stacked
#'    `<version>_results/<scenario>/results.qs2`. The section Stacking gives
#'    the rules. The files of the scenarios that are not in `scenarios` stay
#'    as they are.
#' 4. For each scenario with a function `expect_<scenario>()`, it calls that
#'    function on the results of the scenario. `expect_real()` is optional.
#'    The function MUST return a data.frame with at least one row and the
#'    columns `assertion`, `expected`, `observed` and `pass`.
#'    `save_results()` writes it to `<version>_results/<scenario>/check.csv`
#'    and prints it. For `"real"` without `expect_real()`, it deletes
#'    `check.csv`. An error, zero rows or another shape is one failed
#'    assertion. This rule is the same for `expect_real()` and for the fake
#'    scenarios. `save_results()` writes `check.csv` to a temporary file in
#'    the same folder and renames it, so a reader never sees a part of it.
#' 5. After every scenario, it stops when a value of `pass` is not `TRUE`.
#'    The error lists the failed assertions. The files stay, so that you can
#'    find the fault, and [read_results()] stops on them until the assertions
#'    pass.
#' @section Stacking:
#' The stacked results hold every element of the results of simulation 1,
#' in the same order. These rules apply at each depth:
#' - A data.frame gets a first column `sim`, and the rows of the simulations
#'   in order. It keeps the class of simulation 1, so a data.table stays a
#'   data.table.
#' - A list is stacked element by element, and pbn matches the elements by
#'   name. So each element of a list MUST have its own name.
#' - Each other element becomes a list with one entry for each simulation,
#'   named `sim_0001`, `sim_0002` and so on. This includes an atomic vector,
#'   a matrix, a factor, a date and `NULL`.
#'
#' Stacking stops when two simulations differ in the structure at one path,
#' and the error names the path and the two simulations. It also stops on a
#' data.frame with a column `sim`, because pbn adds that column.
#'
#' The `manifest` is that of simulation 1. In it, `sim` and `n_rows_data`
#' are `NA`, `sims` is the number of simulations, and `stacked` is `TRUE`.
#' The columns `from_cache` and `cache_key` of `data_cache` are `NA`.
#' Stacking stops when the files of the simulations are not exactly the
#' simulations 1 to `sims`, for example after `sims` became smaller. Delete
#' the folders of the higher simulations then.
#' @section Two files:
#' `save_results()` writes the results of the kinds in `gitexclude` to
#' `results_gitexclude.qs2`, beside each `results.qs2`. `results.qs2` holds
#' `manifest`, `exclusions` and every other kind. Use `gitexclude` for a
#' kind with fitted models, imputations or one row for each person. The
#' project's `.gitignore` MUST hold the line `*gitexclude*`, so that git
#' never tracks that file. [read_results()] reads both files.
#'
#' These rules apply:
#' - `save_results()` stops before it writes anything when a name in
#'   `gitexclude` is not an analysis kind of `p`.
#' - In a git repository, it also stops when git does not ignore
#'   `results_gitexclude.qs2`.
#' - [check_results()] checks `results.qs2` only. So `results.qs2` MUST hold
#'   an element other than `manifest`: one kind, or `exclusions`.
#' - The simulations of the two files are stacked apart, with the rules of
#'   the section Stacking.
#' - Without kinds in `gitexclude`, `save_results()` deletes an old
#'   `results_gitexclude.qs2` of the scenario and of each of its
#'   simulations. It prints one line for each deleted file.
#' @return Invisibly, a data.frame of every assertion, with the columns
#'   `scenario`, `assertion`, `expected`, `observed` and `pass`.
#' @examples
#' home <- file.path(tempdir(), "pbn_save_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' writeLines("# The run script.", file.path(home, "v01_run.R"))
#' writeLines(
#'   c(
#'     "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = 1))",
#'     "expect_null <- function(results) {",
#'     "  data.frame(assertion = 'one row', expected = 1,",
#'     "    observed = nrow(results$means), pass = nrow(results$means) == 1)",
#'     "}"
#'   ),
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#' env <- new.env()
#' pbn::initialize_project("v01", home, file.path(home, "data"), env = env)
#'
#' scenarios <- pbn::scenarios(env$CFG)
#' p <- plnr::Plan$new()
#' pbn::add_data(p, "d", fn = function(scenario, sim) {
#'   return(data.frame(x = stats::rnorm(5)))
#' })
#' pbn::add_analyses(p, scenarios, name = "means", fn = function(data, argset) {
#'   return(data.frame(mean_x = mean(data$d$x)))
#' })
#' r <- pbn::run_plan(p)
#' pbn::save_results(r, p, scenarios)
#' names(pbn::read_results("v01", home = home))
#'
#' unlink(home, recursive = TRUE)
#' @family results
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
save_results <- function(r, p, scenarios, gitexclude = NULL) {
  check_state("save_results")
  check_plan(p)
  table <- check_scenario_table(scenarios, "`scenarios`")
  info <- plan_info(p)
  check_run(r, p, info)
  gitexclude <- check_gitexclude(gitexclude, info, table)
  index <- analysis_index(p, info)
  check_coverage(index, table, info)
  check_settings(index, table, info)
  check_inputs(table)

  # The md5 of the raw data that run_plan() read, from its stamp.
  stamp <- attr(r, "pbn_run", exact = TRUE)
  data_md5 <- stamp[["data_md5"]]
  if (is.null(data_md5)) {
    data_md5 <- state$data_md5
  }
  objects <- build_objects(r, index, table, info, gitexclude, data_md5)
  check_objects(objects)
  root <- state$results
  for (i in which(table$sims > 1)) {
    check_sim_folders(root, table$scenario[i], table$sims[i])
  }
  for (s in table$scenario) {
    write_scenario(objects[[s]], root, s)
  }
  checks <- list()
  for (s in table$scenario) {
    fn <- expect_function(s)
    if (!is.null(fn)) {
      checks[[s]] <- check_expectations(fn, root, s)
    } else {
      # Only "real" MAY have no expect_ function.
      unlink(file.path(root, s, "check.csv"))
    }
  }
  checks <- if (length(checks) == 0) {
    empty_checks()
  } else {
    do.call(rbind, unname(checks))
  }
  stop_on_failed_checks(checks)
  return(invisible(checks))
}

# Stops when the plan has no data or analysis of pbn, or when `r` is not the
# value of run_plan(p): other names, an analysis that pbn did not add, or a
# failed analysis.
check_run <- function(r, p, info) {
  if (is.null(info) || length(info$names) == 0 || length(info$data) == 0) {
    stop(
      "save_results() needs a plan with data from pbn::add_data() and ",
      "analyses from pbn::add_analyses().",
      call. = FALSE
    )
  }
  check_stamp(r, p, info)
  other <- setdiff(names(p$analyses), c(info$names, info$hidden))
  if (length(other) > 0) {
    stop(
      "pbn::add_analyses() did not add these analyses of the plan, so pbn ",
      "saved nothing: ",
      paste(other, collapse = ", "),
      call. = FALSE
    )
  }
  stop_on_failed(r)
  return(invisible(NULL))
}

# Stops when `r` is not a list with the analyses of `p` and the stamp that
# run_plan() gives the value of the plan `p`.
check_stamp <- function(r, p, info) {
  stamp <- attr(r, "pbn_run", exact = TRUE)
  problem <- if (!is.list(r) || !identical(names(r), names(p$analyses))) {
    "It has other analyses."
  } else if (is.null(stamp)) {
    "It has no stamp of run_plan()."
  } else if (!identical(stamp[["plan"]], info$token)) {
    "Its stamp names another plan."
  }
  if (!is.null(problem)) {
    stop(
      "`r` must be the value of pbn::run_plan(p) for the plan `p`. ",
      problem,
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# One row for each analysis of the plan: its name, scenario, simulation,
# kind, whether it counts the rows, and its argset.
analysis_index <- function(p, info) {
  nm <- names(p$analyses)
  argsets <- lapply(nm, function(n) return(p$analyses[[n]][["argset"]]))
  out <- data.frame(
    name = nm,
    scenario = vapply(argsets, function(a) return(a[["scenario"]]), ""),
    sim = vapply(argsets, function(a) return(as.integer(a[["sim"]])), 1L),
    analysis = vapply(argsets, function(a) return(a[["analysis"]]), ""),
    hidden = nm %in% info$hidden
  )
  out$argset <- argsets
  return(out)
}

# Stops when the scenarios and simulations of the plan are not those of
# `table`, or when a kind has no analysis for one of them.
check_coverage <- function(index, table, info) {
  want <- paste(rep(table$scenario, table$sims), sequence(table$sims))
  have <- unique(paste(index$scenario, index$sim))
  if (!setequal(want, have)) {
    stop(
      "The analyses of the plan are not for the scenarios and simulations ",
      "of `scenarios`. Give save_results() the scenarios that ",
      "add_analyses() got. In the plan only: ",
      paste(setdiff(have, want), collapse = ", "),
      ". In `scenarios` only: ",
      paste(setdiff(want, have), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (i in seq_len(nrow(info$kinds))) {
    kind <- info$kinds$analysis[i]
    need <- want
    if (info$kinds$control[i]) {
      need <- want[!startsWith(want, "real ")]
    }
    of_kind <- paste(index$scenario, index$sim)[index$analysis == kind]
    absent <- setdiff(need, of_kind)
    if (length(absent) > 0) {
      stop(
        "The analysis kind \"",
        kind,
        "\" has no analysis for these scenarios and simulations: ",
        paste(absent, collapse = ", "),
        ". Give each call of add_analyses() the same scenarios.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# Stops when the settings of a scenario in `table` are not the settings in
# the argsets of its analyses, for example when `scenarios` comes from
# another CFG than the plan.
check_settings <- function(index, table, info) {
  cols <- union(setting_columns(table), info$settings)
  bad <- character(0)
  for (i in seq_len(nrow(index))) {
    want <- settings_of(table, index$scenario[i])
    got <- index$argset[[i]][intersect(cols, names(index$argset[[i]]))]
    # An empty list from settings_of() has no names, and an empty subset of
    # an argset has names character(0). Both mean no settings.
    same <- length(got) == 0 && length(want) == 0
    if (!same && !identical(got[sort(names(got))], want[sort(names(want))])) {
      bad <- union(bad, index$scenario[i])
    }
  }
  if (length(bad) > 0) {
    stop(
      "The settings of these scenarios in `scenarios` are not the settings ",
      "that add_analyses() put in the argsets: ",
      paste(bad, collapse = ", "),
      ". Give save_results() the scenarios that add_analyses() got. pbn ",
      "saved nothing.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Stops when the run script is not in source_md5, when "real" has no
# data_md5, or when a fake scenario has no expect_ function.
check_inputs <- function(table) {
  script <- paste0(state$version, "_run.R")
  if (!script %in% names(state$source_md5)) {
    stop(
      "There was no run script ",
      script,
      " in ",
      state$home,
      " when initialize_project() ran, so the manifest cannot name it. ",
      "pbn saved nothing.",
      call. = FALSE
    )
  }
  if ("real" %in% table$scenario && is.null(state$data_md5)) {
    stop(
      "The data_raw folder ",
      state$data_raw,
      " did not exist when initialize_project() ran, so pbn cannot save ",
      "the scenario \"real\". pbn saved nothing.",
      call. = FALSE
    )
  }
  fake <- table$scenario[table$scenario != "real"]
  missing <- fake[vapply(
    fake,
    function(s) return(is.null(expect_function(s))),
    logical(1)
  )]
  if (length(missing) > 0) {
    stop(
      "Each fake scenario needs a function expect_<scenario>(results) in ",
      state$version,
      "_functions/. These are missing: ",
      paste0("expect_", missing, "()", collapse = ", "),
      ". pbn saved nothing.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The function expect_<scenario>() of the sourced environment, or NULL.
expect_function <- function(scenario) {
  return(get0(
    paste0("expect_", scenario),
    envir = state$env,
    mode = "function",
    inherits = FALSE
  ))
}

# For each scenario of `table`, a list with `sims`, the object of
# results.qs2 of each simulation in `runs`, the object of
# results_gitexclude.qs2 or NULL of each simulation in `private`, and, for
# more than 1 simulation, the stacked object of results.qs2.
build_objects <- function(r, index, table, info, gitexclude, data_md5) {
  fake_md5 <- function_md5(data_code(info))
  out <- list()
  for (i in seq_len(nrow(table))) {
    s <- table$scenario[i]
    runs <- lapply(seq_len(table$sims[i]), function(sim) {
      x <- build_object(r, index, table, info, s, sim, fake_md5, data_md5)
      return(split_results(x, gitexclude))
    })
    out[[s]] <- list(
      sims = table$sims[i],
      runs = lapply(runs, function(x) return(x$public)),
      private = lapply(runs, function(x) return(x$private))
    )
    if (table$sims[i] > 1) {
      out[[s]]$stacked <- stack_results(out[[s]]$runs, s)
    }
  }
  return(out)
}

# The results object of one scenario and simulation, with every element.
# split_results() splits it into the two files.
build_object <- function(r, index, table, info, s, sim, fake_md5, data_md5) {
  here <- index[index$scenario == s & index$sim == sim, , drop = FALSE]
  kinds <- info$kinds
  if (s == "real") {
    kinds <- kinds[!kinds$control, , drop = FALSE]
  }
  counted <- r[[here$name[here$hidden]]]
  results <- exclusions_element(attr(counted, "exclusions", exact = TRUE))
  for (kind in kinds$analysis) {
    rows <- !here$hidden & here$analysis == kind
    results[kind] <- list(combine_kind(
      r[here$name[rows]],
      here$argset[rows],
      kind,
      info$dims[[kind]]
    ))
  }
  rownames(kinds) <- NULL
  kinds$file <- rep("results.qs2", nrow(kinds))
  n_rows <- counted
  attributes(n_rows) <- list(names = names(counted))
  cohort_n <- cohort_counts(counted)
  manifest <- c(
    list(
      pbn_version = as.character(getNamespaceVersion("pbn")),
      r_version = R.version.string,
      packages = loaded_packages(),
      run_date = format(Sys.Date(), "%Y-%m-%d"),
      version = state$version,
      scenario = s,
      sim = sim,
      sims = table$sims[table$scenario == s],
      settings = settings_of(table, s),
      source_md5 = state$source_md5
    ),
    if (s == "real") {
      list(data_md5 = data_md5)
    } else {
      list(fake_md5 = fake_md5)
    },
    list(
      n_rows_data = n_rows,
      n_rows_before_exclusions = cohort_n$before,
      n_rows_after_exclusions = cohort_n$after,
      analyses = kinds,
      data_cache = data_cache_table(info, attr(counted, "cache", exact = TRUE))
    )
  )
  return(c(list(manifest = manifest), results))
}

# The rows given to the cohort of each data element, `before`, and the rows
# after its exclusions, `after`, from the value `counted` of the analysis
# that counts the rows. `after` is n_after of the last step of the exclusion
# table, or `before` for a cohort without a step. Both are NA for data
# without a cohort.
cohort_counts <- function(counted) {
  start <- attr(counted, "n_before", exact = TRUE)
  tables <- attr(counted, "exclusions", exact = TRUE)
  nm <- names(counted)
  before <- vapply(
    nm,
    function(n) return(if (is.null(start[[n]])) NA_integer_ else start[[n]]),
    integer(1)
  )
  after <- vapply(
    nm,
    function(n) {
      e <- tables[[n]]
      if (is.null(e)) {
        return(NA_integer_)
      }
      if (nrow(e) == 0) {
        return(before[[n]])
      }
      return(as.integer(e$n_after[nrow(e)]))
    },
    integer(1)
  )
  return(list(before = before, after = after))
}

# The element exclusions: the exclusion tables of the cohorts, row-bound in
# the order of the data. An empty list without a cohort.
exclusions_element <- function(tables) {
  if (length(tables) == 0) {
    return(list())
  }
  out <- do.call(rbind, unname(tables))
  rownames(out) <- NULL
  return(list(exclusions = out))
}

# One row for each data element of the plan: whether it has a cache, whether
# the data came from the cache, and the md5 of its cache key. `cache` holds
# the record of each data element with a cache, from the analysis that
# counts the rows.
data_cache_table <- function(info, cache) {
  nm <- names(info$data)
  record <- function(n, field, none) {
    x <- cache[[n]]
    return(if (is.null(x)) none else x[[field]])
  }
  return(data.frame(
    data = nm,
    cache = vapply(nm, function(n) return(isTRUE(info$cache[[n]])), TRUE),
    from_cache = vapply(nm, record, TRUE, field = "from_cache", none = FALSE),
    cache_key = vapply(nm, record, "", field = "key", none = NA_character_),
    row.names = NULL
  ))
}

# The element of one analysis kind from the values `values` of its analyses
# and their argsets. `dims` names the extra dimensions of the kind.
combine_kind <- function(values, argsets, kind, dims) {
  if (all(vapply(values, is.data.frame, logical(1)))) {
    return(bind_frames(values, argsets, kind, dims))
  }
  if (length(dims) == 0) {
    return(values[[1]])
  }
  return(stats::setNames(
    unname(values),
    paste0(kind, "__", seq_along(values))
  ))
}

# The data.frames `values` row-bound, each with its extra dimensions first,
# in the class of the first data.frame.
bind_frames <- function(values, argsets, kind, dims) {
  if (length(dims) == 0) {
    return(values[[1]])
  }
  parts <- lapply(values, function(v) return(as.data.frame(v)))
  for (i in seq_along(parts)) {
    lead <- setdiff(dims, names(parts[[i]]))
    if (length(lead) == 0) {
      next
    }
    first <- as.data.frame(argsets[[i]][lead], stringsAsFactors = FALSE)
    first <- first[rep(1L, nrow(parts[[i]])), , drop = FALSE]
    parts[[i]] <- cbind(first, parts[[i]])
  }
  out <- tryCatch(do.call(rbind, unname(parts)), error = function(e) {
    stop(
      "pbn cannot row-bind the data.frames of the analysis kind \"",
      kind,
      "\": ",
      conditionMessage(e),
      ". Give each analysis of the kind the same columns.",
      call. = FALSE
    )
  })
  rownames(out) <- NULL
  if (data.table::is.data.table(values[[1]])) {
    data.table::setDT(out)
  } else if (!identical(class(out), class(values[[1]]))) {
    class(out) <- class(values[[1]])
  }
  return(out)
}

# Prints each failed rule of each object with cat(), and stops when there is
# one.
check_objects <- function(objects) {
  failures <- character(0)
  for (s in names(objects)) {
    o <- objects[[s]]
    all <- o$runs
    names(all) <- if (o$sims == 1) {
      s
    } else {
      file.path(s, sim_folder(seq_len(o$sims)))
    }
    if (!is.null(o$stacked)) {
      all[[paste(s, "stacked")]] <- o$stacked
    }
    for (nm in names(all)) {
      found <- check_results(all[[nm]])
      failed <- found[found$status == "FAIL", , drop = FALSE]
      failures <- c(
        failures,
        paste0(nm, ": ", failed$rule, ": ", failed$detail, recycle0 = TRUE)
      )
    }
  }
  if (length(failures) == 0) {
    return(invisible(NULL))
  }
  cat("pbn: these results fail check_results():\n")
  cat(paste0("  ", failures, "\n"), sep = "")
  stop(
    length(failures),
    " failures of check_results(), printed above. pbn saved nothing.",
    call. = FALSE
  )
}

# Writes the results of one scenario under `root`, and stacks them when the
# scenario has more than 1 simulation.
write_scenario <- function(o, root, s) {
  folder <- file.path(root, s)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  # Until the expect_ function has checked the new results, check.csv holds
  # one failed assertion, so read_results() refuses them. A reader thus
  # never gets new results with the check.csv of an earlier run.
  write_csv_atomic(
    failed_assertion(
      "save_results() checks these results with expect_<scenario>()",
      "not checked yet"
    ),
    file.path(folder, "check.csv")
  )
  if (o$sims == 1) {
    qs2::qs_save(o$runs[[1]], file.path(folder, "results.qs2"))
    write_private(o$private[[1]], folder)
    return(invisible(NULL))
  }
  unlink(file.path(folder, "results.qs2"))
  for (sim in seq_len(o$sims)) {
    dir.create(file.path(folder, sim_folder(sim)), showWarnings = FALSE)
    qs2::qs_save(
      o$runs[[sim]],
      file.path(folder, sim_folder(sim), "results.qs2")
    )
    write_private(o$private[[sim]], file.path(folder, sim_folder(sim)))
  }
  stack_scenario(root, s, o$sims)
  return(invisible(NULL))
}

stop_on_failed_checks <- function(checks) {
  failed <- checks[!checks$pass %in% TRUE, , drop = FALSE]
  if (nrow(failed) == 0) {
    return(invisible(NULL))
  }
  stop(
    "These assertions failed:\n",
    paste0(
      "  ",
      failed$scenario,
      ": ",
      failed$assertion,
      " (expected ",
      failed$expected,
      ", observed ",
      failed$observed,
      ")",
      collapse = "\n"
    ),
    call. = FALSE
  )
}

loaded_packages <- function() {
  # qs2 writes the file, so it MUST be in the package list.
  loadNamespace("qs2")
  attached <- sub("^package:", "", grep("^package:", search(), value = TRUE))
  pkgs <- union(loadedNamespaces(), attached)
  base <- rownames(utils::installed.packages(
    lib.loc = .Library,
    priority = "base"
  ))
  pkgs <- sort(setdiff(pkgs, base))
  versions <- vapply(
    pkgs,
    function(p) return(as.character(getNamespaceVersion(p))),
    character(1)
  )
  return(versions)
}
