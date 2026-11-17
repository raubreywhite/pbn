#' Run every scenario of a project version in new R sessions
#'
#' `run_scenarios()` runs `<home>/<version>_run.R` once for each simulation of
#' each scenario, each time in a new R session. It then stacks the
#' simulations of each scenario and checks the assertions of the project.
#'
#' @param version The version, for example `"v01"`.
#' @param scenario `NULL` for every scenario, or a character vector of the
#'   scenarios to run.
#' @param home The project folder.
#' @param workers The number of simulations that run at the same time, one
#'   whole number of 1 or more. Each simulation runs in its own new R session.
#' @param resume `FALSE` runs every simulation of the chosen scenarios. `TRUE`
#'   keeps each simulation with results from the same code, and runs the
#'   others. See the section Resume.
#' @details
#' `run_scenarios()` does these steps in this order:
#' 1. It reads `CFG$scenarios` in a new R session that sources
#'    `<version>_functions/` with [org::initialize_project()]. The R session
#'    of the caller does not change.
#'    It stops when a scenario other than `"real"` has no function
#'    `expect_<scenario>()`. It checks every chosen scenario before it runs
#'    any of them.
#' 2. With `resume = TRUE`, it chooses the simulations to keep, and prints
#'    one line for each of them.
#' 3. It deletes these files, and it stops before any run when one of them
#'    stays:
#'    - the `check.csv` of each chosen scenario.
#'    - the stacked `results.qs2` of each chosen scenario with more than 1
#'      simulation.
#'    - the `results.qs2` and the `run.ok` of each simulation that it runs.
#'
#'    Files of the scenarios that it does not run stay as they are.
#' 4. It runs the script once for each simulation that it does not keep. Each
#'    run is a new R session with the arguments `scenario=<name> sim=<k>`,
#'    and with `home` as the working directory. At most `workers` runs go at
#'    the same time. The stdout and the stderr of each run go to `run.log` in
#'    the results folder of the simulation. Each run writes `run.log` again,
#'    also a run that succeeds.
#'
#'    A run succeeds when all of these hold:
#'    - It quits with status 0, so [end_if_fake_scenario()] can end the run
#'      of a fake scenario.
#'    - Its `results.qs2` exists and passes [check_results()].
#'    - `manifest$scenario`, `manifest$sim` and `manifest$sims` are those of
#'      the simulation.
#'
#'    After a run that succeeds, `run_scenarios()` writes `run.ok` beside
#'    `run.log`. `run.ok` holds one line, the md5 of `results.qs2`. A run
#'    that stops after it writes `results.qs2` thus has no `run.ok`. This is
#'    also the case when the caller is interrupted. `run.log` and `run.ok`
#'    are run artefacts. List both in the `.gitignore` of the project.
#'
#'    Each other run fails. For a run that fails, `run_scenarios()` prints
#'    the whole `run.log` with `cat()`. After the first failure, it starts no
#'    new run, and the runs that started go on to their end. It then stops
#'    with an error that names the scenario, the simulation and the `run.log`
#'    of each failed run. It stacks and checks no scenario.
#' 5. For each scenario with more than 1 simulation, it stacks the files
#'    `<version>_results/<scenario>/sim_<kkkk>/results.qs2` into
#'    `<version>_results/<scenario>/results.qs2`. It stops when the files are
#'    not exactly the simulations 1 to `sims`. After `sims` in
#'    `CFG$scenarios` becomes smaller, the folders of the higher simulations
#'    stay. Stacking then stops, and the error names their files. Delete
#'    those folders to stack again. The section Stacking gives the rules.
#' 6. For each scenario that has a function `expect_<scenario>()` in
#'    `<version>_functions/`, it calls that function on the results of the
#'    scenario in a new R session. `expect_real()` is optional. The function
#'    MUST return a data.frame with at least one row and the columns
#'    `assertion`, `expected`, `observed` and `pass`. `run_scenarios()`
#'    writes it to `<version>_results/<scenario>/check.csv` and prints it.
#'
#' Each of these counts as one failed assertion, and `run_scenarios()` writes
#' it to `check.csv` too:
#' - `expect_<scenario>()` stops with an error. The assertion holds the error
#'   message.
#' - `expect_<scenario>()` returns zero rows.
#' - `expect_<scenario>()` returns something other than a data.frame with the
#'   four columns.
#'
#' After every scenario is done, `run_scenarios()` stops when a value of
#' `pass` is not `TRUE`. The error lists the failed assertions.
#' @section Stacking:
#' The stacked results hold every element of the results of simulation 1,
#' in the same order. These rules apply at each depth:
#' - A data.frame gets a first column `sim`, and the rows of the simulations
#'   in order. It keeps the class of simulation 1, so a data.table stays a
#'   data.table.
#' - A list is stacked element by element, and pbn matches the elements by
#'   name. So each element of a list MUST have its own name. An unnamed or
#'   partly named list stops stacking. A scenario with 1 simulation MAY hold
#'   such a list.
#' - Each other element becomes a list with one entry for each simulation,
#'   named `sim_0001`, `sim_0002` and so on. This includes an atomic vector,
#'   a matrix, a factor, a date and `NULL`.
#'
#' Stacking stops when two simulations differ in the structure at one path.
#' The error names the path and the two simulations. Examples:
#' - a list with other names or another length.
#' - a data.frame with other columns, a column of another class, or a factor
#'   column with other levels.
#' - a list in the place of a data.frame, or `NULL` in the place of a value.
#'
#' Stacking also stops on a data.frame with a column `sim`, because pbn adds
#' that column.
#'
#' The `manifest` is that of simulation 1. In it, `sim` and `n_rows_data`
#' are `NA`, `sims` is the number of simulations, and `stacked` is `TRUE`.
#' For a fake scenario, stacking stops when a simulation has no valid
#' `manifest$fake_md5`, or when the simulations do not have one value of it.
#' It also stops when the simulations do not have one value of
#' `manifest$settings`.
#'
#' A scenario with 1 simulation is not stacked. Its `results.qs2` is the file
#' that the run wrote.
#' @section Workers:
#' Each simulation depends only on its scenario and its number, so `workers`
#' does not change the results files or `check.csv`. Only the order of the
#' lines on the console can change. pbn does not limit the threads that R or
#' the BLAS library starts in each R session.
#' @section Resume:
#' With `resume = TRUE`, `run_scenarios()` keeps a simulation when all of
#' these hold:
#' - Its `results.qs2` exists and passes [check_results()].
#' - Its `run.ok` exists and holds the md5 of `results.qs2` now. So the run
#'   completed, and nothing changed `results.qs2` after it.
#' - `manifest$scenario`, `manifest$sim` and `manifest$sims` are those of the
#'   simulation.
#' - `manifest$source_md5` is the md5 of the files of `<version>_run.R` and
#'   `<version>_functions/` now.
#' - For a fake scenario, `manifest$fake_md5` is the md5 of the code of
#'   `fake_<scenario>()` now.
#'
#' Each other simulation runs again, and the console line of its start gives
#' the reason. The stacking and the `expect_` assertions always run again.
#'
#' Resume does not find a change in the real data, or in the version of R or
#' of a package. It also does not find a change in a file outside
#' `<version>_run.R` and `<version>_functions/`. After such a change, use
#' `resume = FALSE`.
#' @return Invisibly, a data.frame of every assertion, with the columns
#'   `scenario`, `assertion`, `expected`, `observed` and `pass`.
#' @examples
#' \donttest{
#' home <- file.path(tempdir(), "pbn_run_scenarios_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' dir.create(file.path(home, "data"))
#' utils::write.csv(data.frame(x = 1:4), file.path(home, "data", "d.csv"))
#' writeLines(
#'   c(
#'     "CFG <- list(",
#'     "  scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 2))",
#'     ")",
#'     "fake_null <- function(dir) {",
#'     "  utils::write.csv(data.frame(x = rnorm(4)), file.path(dir, 'd.csv'))",
#'     "}",
#'     "expect_null <- function(results) {",
#'     "  n <- length(unique(results$estimates$sim))",
#'     "  data.frame(assertion = 'two simulations', expected = 2,",
#'     "    observed = n, pass = n == 2)",
#'     "}"
#'   ),
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#' writeLines(
#'   c(
#'     'p <- pbn::initialize_project("v01", home = ".", data_raw = "data")',
#'     "d <- utils::read.csv(file.path(p$data_raw, 'd.csv'))",
#'     "pbn::save_results(",
#'     "  estimates = data.frame(term = 'mean_x', estimate = mean(d$x)),",
#'     "  n_rows = nrow(d),",
#'     "  d = d",
#'     ")"
#'   ),
#'   file.path(home, "v01_run.R")
#' )
#' pbn::run_scenarios("v01", home = home)
#' stacked <- pbn::read_results("v01", scenario = "null", home = home)
#' stacked$estimates
#' stacked$n_rows
#'
#' unlink(home, recursive = TRUE)
#' }
#' @family project setup
#' @export
run_scenarios <- function(
  version,
  scenario = NULL,
  home = ".",
  workers = 1,
  resume = FALSE
) {
  check_version(version)
  check_workers(workers)
  if (!isTRUE(resume) && !isFALSE(resume)) {
    stop("`resume` must be TRUE or FALSE.", call. = FALSE)
  }
  home <- normalizePath(home, winslash = "/", mustWork = TRUE)
  script <- file.path(home, paste0(version, "_run.R"))
  if (!file.exists(script)) {
    stop("There is no run script at ", script, call. = FALSE)
  }
  info <- read_project_functions(home, version)
  table <- select_scenarios(info$scenarios, scenario)
  no_expect <- setdiff(table$scenario[table$scenario != "real"], info$expect)
  if (length(no_expect) > 0) {
    stop(
      "Each fake scenario needs a function expect_<scenario>(results) in ",
      version,
      "_functions/. These are missing: ",
      paste0("expect_", no_expect, "()", collapse = ", "),
      ". pbn ran no scenario.",
      call. = FALSE
    )
  }

  jobs <- simulation_jobs(table, home, version)
  jobs$reason <- if (resume) {
    keep_reasons(jobs, source_md5(home, version), info$fake_md5)
  } else {
    rep("", nrow(jobs))
  }
  delete_stale(jobs, table, home, version)
  run_pool(jobs[!is.na(jobs$reason), , drop = FALSE], script, home, workers)

  for (i in which(table$sims > 1)) {
    stack_scenario(home, version, table$scenario[i], table$sims[i])
  }
  checks <- list()
  for (s in intersect(table$scenario, info$expect)) {
    checks[[s]] <- check_expectations(home, version, s)
  }
  checks <- if (length(checks) == 0) {
    empty_checks()
  } else {
    do.call(rbind, unname(checks))
  }
  failed <- checks[!checks$pass %in% TRUE, , drop = FALSE]
  if (nrow(failed) > 0) {
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
  return(invisible(checks))
}

# Sources <version>_functions/ in a new R session, and returns the checked
# scenario table and the names of the scenarios that have expect_<name>().
read_project_functions <- function(home, version) {
  found <- callr::r(
    function(home, folder) {
      env <- new.env()
      suppressMessages(org::initialize_project(
        env = env,
        home = home,
        folders_to_be_sourced = folder,
        max_loc_per_file = 1000
      ))
      cfg <- get0("CFG", envir = env, inherits = FALSE)
      fns <- ls(env)
      fns <- fns[startsWith(fns, "expect_") | startsWith(fns, "fake_")]
      fns <- fns[vapply(
        fns,
        function(f) return(is.function(get(f, envir = env))),
        logical(1)
      )]
      # Each fake_<scenario>() without its source references and with the
      # global environment, so that the closure does not carry the sourced
      # environment back to the caller.
      fakes <- lapply(fns[startsWith(fns, "fake_")], function(f) {
        fn <- utils::removeSource(get(f, envir = env))
        environment(fn) <- globalenv()
        return(fn)
      })
      names(fakes) <- sub("^fake_", "", fns[startsWith(fns, "fake_")])
      return(list(
        scenarios = if (is.list(cfg)) cfg[["scenarios"]] else NULL,
        expect = sub("^expect_", "", fns[startsWith(fns, "expect_")]),
        fakes = fakes
      ))
    },
    args = list(home = home, folder = paste0(version, "_functions")),
    show = FALSE
  )
  return(list(
    scenarios = check_scenario_table(found$scenarios, version),
    expect = found$expect,
    fake_md5 = vapply(found$fakes, function_md5, character(1))
  ))
}

select_scenarios <- function(table, scenario) {
  if (is.null(scenario)) {
    return(table)
  }
  if (!is.character(scenario) || length(scenario) == 0 || anyNA(scenario)) {
    stop(
      "`scenario` must be NULL or a character vector of scenarios.",
      call. = FALSE
    )
  }
  unknown <- setdiff(scenario, table$scenario)
  if (length(unknown) > 0) {
    stop(
      "These scenarios are not in CFG$scenarios: ",
      paste(unknown, collapse = ", "),
      ". The scenarios are: ",
      paste(table$scenario, collapse = ", "),
      call. = FALSE
    )
  }
  return(table[table$scenario %in% scenario, , drop = FALSE])
}

# Stacks the simulation files of one scenario into
# <version>_results/<scenario>/results.qs2.
stack_scenario <- function(home, version, scenario, sims) {
  folder <- file.path(home, paste0(version, "_results"), scenario)
  want <- file.path(sim_folder(seq_len(sims)), "results.qs2")
  present <- list.files(folder, pattern = "^sim_[0-9]+$")
  present <- file.path(present, "results.qs2")
  present <- present[file.exists(file.path(folder, present))]
  missing <- setdiff(want, present)
  extra <- setdiff(present, want)
  if (length(missing) > 0 || length(extra) > 0) {
    stop(
      "The scenario \"",
      scenario,
      "\" has ",
      sims,
      " simulations in CFG$scenarios, so pbn did not stack it. ",
      "Missing: ",
      if (length(missing) > 0) paste(missing, collapse = ", ") else "none",
      ". Not in 1 to ",
      sims,
      ": ",
      if (length(extra) > 0) paste(extra, collapse = ", ") else "none",
      ".",
      call. = FALSE
    )
  }
  x <- stack_results(lapply(file.path(folder, want), qs2::qs_read), scenario)
  findings <- check_results(x)
  failed <- findings[findings$status == "FAIL", , drop = FALSE]
  if (nrow(failed) > 0) {
    stop(
      "The stacked results of the scenario \"",
      scenario,
      "\" fail check_results(), so pbn did not save them:\n",
      paste0("  ", failed$rule, ": ", failed$detail, collapse = "\n"),
      call. = FALSE
    )
  }
  file <- file.path(folder, "results.qs2")
  qs2::qs_save(x, file)
  return(invisible(file))
}

# Simulation k of `runs` is simulation k of the scenario.
stack_results <- function(runs, scenario) {
  # The stacked manifest is that of simulation 1, so its fake_md5 MUST be the
  # fake_md5 of each simulation.
  fake <- lapply(runs, function(r) return(r[["manifest"]][["fake_md5"]]))
  bad <- which(!vapply(fake, is_md5, logical(1)))
  if (scenario != "real" && length(bad) > 0) {
    stop(
      "The simulations ",
      paste(bad, collapse = ", "),
      " of the scenario \"",
      scenario,
      "\" have no valid manifest$fake_md5, so pbn cannot stack them. ",
      "Run the scenario again.",
      call. = FALSE
    )
  }
  if (!all(vapply(fake, identical, logical(1), fake[[1]]))) {
    stop(
      "The simulations of the scenario \"",
      scenario,
      "\" have different values of manifest$fake_md5, so a different ",
      "fake_",
      scenario,
      "() wrote their data. pbn cannot stack them. Run the scenario again.",
      call. = FALSE
    )
  }
  settings <- lapply(runs, function(r) return(r[["manifest"]][["settings"]]))
  if (!all(vapply(settings, identical, logical(1), settings[[1]]))) {
    stop(
      "The simulations of the scenario \"",
      scenario,
      "\" have different values of manifest$settings, so pbn cannot stack ",
      "them. Run the scenario again.",
      call. = FALSE
    )
  }
  manifest <- runs[[1]][["manifest"]]
  manifest$sim <- NA_integer_
  manifest$sims <- length(runs)
  manifest$n_rows_data <- NA_integer_
  manifest$stacked <- TRUE
  results <- lapply(runs, function(r) return(r[names(r) != "manifest"]))
  return(c(list(manifest = manifest), stack_element(results, "x", scenario)))
}

# The element at the path `where`, stacked from `values`, its value in each
# simulation. A data.frame gets a first column sim. A list is stacked element
# by element. Each other value becomes a list with one entry for each
# simulation, named sim_0001, sim_0002 and so on.
stack_element <- function(values, where, scenario) {
  kinds <- vapply(values, stack_kind, character(1))
  k <- match(TRUE, kinds != kinds[1])
  if (!is.na(k)) {
    stop_structure(scenario, where, k, kinds[1], kinds[k])
  }
  if (kinds[1] == "a data.frame") {
    return(stack_frames(values, where, scenario))
  }
  if (kinds[1] == "a list") {
    return(stack_list(values, where, scenario))
  }
  return(stats::setNames(values, sim_folder(seq_along(values))))
}

stack_kind <- function(o) {
  if (is.null(o)) {
    return("NULL")
  }
  if (is.data.frame(o)) {
    return("a data.frame")
  }
  if (is.list(o)) {
    return("a list")
  }
  return("a value")
}

# The lists `values` stacked element by element and matched by name. Each
# list MUST have a unique name for each element, so that no element of one
# simulation is stacked with another element of another simulation.
stack_list <- function(values, where, scenario) {
  first <- values[[1]]
  nm <- names(first)
  for (k in seq_along(values)) {
    nm_k <- names(values[[k]])
    named <- length(values[[k]]) == 0 ||
      (!is.null(nm_k) && all(nzchar(nm_k)) && !anyDuplicated(nm_k))
    if (!named) {
      stop(
        "pbn cannot stack the scenario \"",
        scenario,
        "\": in simulation ",
        k,
        ", the list at ",
        where,
        " has an element without a name or a name twice. Give each ",
        "element of the list its own name, for example list(a = 1, b = 2).",
        call. = FALSE
      )
    }
    if (!setequal(nm_k, nm) || length(values[[k]]) != length(first)) {
      stop_structure(
        scenario,
        where,
        k,
        list_shape(first),
        list_shape(values[[k]])
      )
    }
  }
  out <- lapply(seq_along(first), function(i) {
    parts <- lapply(values, function(v) return(v[[nm[[i]]]]))
    return(stack_element(parts, child_path(first, i, where), scenario))
  })
  names(out) <- nm
  return(out)
}

list_shape <- function(o) {
  if (is.null(names(o))) {
    return(paste("a list of length", length(o)))
  }
  return(paste(
    "a list with the names",
    paste(dQuote(names(o), FALSE), collapse = ", ")
  ))
}

# Stops, because simulation k is `found` at the path `where`, and simulation 1
# is `want`. `advice` is NULL or a sentence that says how to fix it.
stop_structure <- function(scenario, where, k, want, found, advice = NULL) {
  stop(
    "The simulations 1 and ",
    k,
    " of the scenario \"",
    scenario,
    "\" differ at ",
    where,
    ": it is ",
    want,
    " in simulation 1 and ",
    found,
    " in simulation ",
    k,
    ", so pbn cannot stack them.",
    if (!is.null(advice)) paste0(" ", advice),
    call. = FALSE
  )
}

# The frames with a first column sim, in one object of the class of
# frames[[1]]. A data.table stays a data.table.
stack_frames <- function(frames, where, scenario) {
  check_frames(frames, where, scenario)
  parts <- lapply(seq_along(frames), function(k) {
    f <- as.data.frame(frames[[k]])
    return(data.frame(sim = rep(k, nrow(f)), f, check.names = FALSE))
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  cls <- class(frames[[1]])
  if (data.table::is.data.table(frames[[1]])) {
    # setDT() gives the data.table its internal self-reference, which a
    # class set by hand does not.
    data.table::setDT(out)
  }
  if (!identical(class(out), cls)) {
    class(out) <- cls
  }
  return(out)
}

# Stops when the frames of the simulations at the path `where` cannot be
# row-bound without a change: a column sim, other columns, a column of
# another class, or a factor column with other levels.
check_frames <- function(frames, where, scenario) {
  columns <- names(frames[[1]])
  for (k in seq_along(frames)) {
    repeated <- unique(names(frames[[k]])[duplicated(names(frames[[k]]))])
    if (length(repeated) > 0) {
      stop(
        "pbn cannot stack the scenario \"",
        scenario,
        "\": in simulation ",
        k,
        ", the data.frame at ",
        where,
        " has more than one column named ",
        paste(repeated, collapse = ", "),
        ". Give each column its own name.",
        call. = FALSE
      )
    }
    if ("sim" %in% names(frames[[k]])) {
      stop(
        "pbn cannot stack the scenario \"",
        scenario,
        "\": in simulation ",
        k,
        ", the data.frame at ",
        where,
        " has a column sim. pbn adds that column when it stacks the ",
        "simulations, so give the column another name.",
        call. = FALSE
      )
    }
    if (
      ncol(frames[[k]]) != length(columns) ||
        !setequal(names(frames[[k]]), columns)
    ) {
      stop_structure(
        scenario,
        where,
        k,
        paste("a data.frame with the columns", paste(columns, collapse = ", ")),
        paste(
          "a data.frame with the columns",
          paste(names(frames[[k]]), collapse = ", ")
        )
      )
    }
    for (col in columns) {
      check_column(
        frames[[1]][[col]],
        frames[[k]][[col]],
        k,
        where,
        col,
        scenario
      )
    }
  }
  return(invisible(NULL))
}

# Stops when the column `col` of simulation k, `b`, has another class than in
# simulation 1, `a`, or other factor levels.
check_column <- function(a, b, k, where, col, scenario) {
  at <- paste0(where, ", column ", col)
  if (!identical(class(a), class(b))) {
    stop_structure(
      scenario,
      at,
      k,
      paste("of the class", paste(class(a), collapse = "/")),
      paste("of the class", paste(class(b), collapse = "/"))
    )
  }
  if (is.factor(a) && !identical(levels(a), levels(b))) {
    stop_structure(
      scenario,
      at,
      k,
      paste("a factor with the levels", paste(levels(a), collapse = ", ")),
      paste("a factor with the levels", paste(levels(b), collapse = ", ")),
      paste(
        "Convert the column to character, or give the factor the same",
        "levels in each simulation in the analysis."
      )
    )
  }
  return(invisible(NULL))
}

# Calls expect_<scenario>() in a new R session on the results of the
# scenario, writes check.csv and prints the table. An error, zero rows or a
# wrong shape is one failed assertion.
check_expectations <- function(home, version, scenario) {
  folder <- file.path(home, paste0(version, "_results"), scenario)
  fn_name <- paste0("expect_", scenario, "()")
  found <- tryCatch(
    callr::r(
      function(home, folder, fn_name, file) {
        env <- new.env()
        suppressMessages(org::initialize_project(
          env = env,
          home = home,
          folders_to_be_sourced = folder,
          max_loc_per_file = 1000
        ))
        fn <- get(fn_name, envir = env, inherits = FALSE)
        return(fn(qs2::qs_read(file)))
      },
      args = list(
        home = home,
        folder = paste0(version, "_functions"),
        fn_name = paste0("expect_", scenario),
        file = file.path(folder, "results.qs2")
      ),
      show = FALSE
    ),
    error = function(e) {
      cause <- if (inherits(e$parent, "condition")) e$parent else e
      return(failed_assertion(
        paste(fn_name, "runs without an error"),
        gsub("\\s+", " ", conditionMessage(cause))
      ))
    }
  )
  columns <- names(empty_checks())[-1]
  if (
    !is.data.frame(found) ||
      !identical(sort(names(found), na.last = TRUE), sort(columns))
  ) {
    found <- failed_assertion(
      paste(
        fn_name,
        "returns a data.frame with the columns",
        paste(columns, collapse = ", ")
      ),
      if (is.data.frame(found)) {
        paste("the columns", paste(names(found), collapse = ", "))
      } else {
        paste("an object of class", paste(class(found), collapse = ", "))
      }
    )
  } else if (nrow(found) == 0) {
    found <- failed_assertion(
      paste(fn_name, "returns at least one assertion"),
      "0 rows"
    )
  }
  found <- as.data.frame(found)[, columns, drop = FALSE]
  utils::write.csv(found, file.path(folder, "check.csv"), row.names = FALSE)
  cat("Assertions of the scenario \"", scenario, "\":\n", sep = "")
  print(found, row.names = FALSE)
  return(data.frame(scenario = rep(scenario, nrow(found)), found))
}

failed_assertion <- function(assertion, observed) {
  return(data.frame(
    assertion = assertion,
    expected = "see the assertion",
    observed = observed,
    pass = FALSE
  ))
}

empty_checks <- function() {
  return(data.frame(
    scenario = character(0),
    assertion = character(0),
    expected = character(0),
    observed = character(0),
    pass = logical(0)
  ))
}
