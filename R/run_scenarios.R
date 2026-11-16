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
#' @details
#' `run_scenarios()` does these steps in this order:
#' 1. It reads `CFG$scenarios` in a new R session that sources
#'    `<version>_functions/` with [org::initialize_project()]. The R session
#'    of the caller does not change.
#'    It stops when a scenario other than `"real"` has no function
#'    `expect_<scenario>()`. It checks every chosen scenario before it runs
#'    any of them.
#' 2. It makes a [plnr::Plan] with one analysis for each scenario and
#'    simulation. Each analysis runs the script with [callr::rscript()] and
#'    the arguments `scenario=<name> sim=<k>`, with `home` as the working
#'    directory. A run that fails stops `run_scenarios()` with the output of
#'    the run.
#'
#'    Before each run, `run_scenarios()` deletes the `results.qs2` of that
#'    simulation. Before simulation 1, it also deletes the `results.qs2` and
#'    the `check.csv` in `<version>_results/<scenario>/`. A run that writes
#'    no `results.qs2` stops `run_scenarios()`. A run that quits with
#'    status 0 after it writes `results.qs2` succeeds, so
#'    [end_if_fake_scenario()] can end the run of a fake scenario. Files of
#'    the scenarios that it does not run stay as they are.
#' 3. For each scenario with more than 1 simulation, it stacks the files
#'    `<version>_results/<scenario>/sim_<kkkk>/results.qs2` into
#'    `<version>_results/<scenario>/results.qs2`. It stops when the files are
#'    not exactly the simulations 1 to `sims`. `estimates`, `exclusions` and
#'    each data.frame in `tables` get a first column `sim`. `extra` is an
#'    empty list. The `manifest` is that of simulation 1. In it, `sim` and
#'    `n_rows_data` are `NA`, `sims` is the number of simulations, and
#'    `stacked` is `TRUE`.
#' 4. For each scenario that has a function `expect_<scenario>()` in
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
#'     "  exclusions = data.frame(step = character(0), n_before = numeric(0),",
#'     "    n_excluded = numeric(0), n_after = numeric(0)),",
#'     "  estimates = data.frame(term = 'mean_x', estimate = mean(d$x)),",
#'     "  tables = list()",
#'     ")"
#'   ),
#'   file.path(home, "v01_run.R")
#' )
#' pbn::run_scenarios("v01", home = home)
#' pbn::read_results("v01", scenario = "null", home = home)$estimates
#'
#' unlink(home, recursive = TRUE)
#' }
#' @family project setup
#' @export
run_scenarios <- function(version, scenario = NULL, home = ".") {
  check_version(version)
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

  plan <- scenario_plan(
    table,
    script,
    home,
    file.path(home, paste0(version, "_results"))
  )
  plan$run_all()

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
      fns <- fns[startsWith(fns, "expect_")]
      is_fn <- vapply(
        fns,
        function(f) return(is.function(get(f, envir = env))),
        logical(1)
      )
      return(list(
        scenarios = if (is.list(cfg)) cfg[["scenarios"]] else NULL,
        expect = sub("^expect_", "", fns[is_fn])
      ))
    },
    args = list(home = home, folder = paste0(version, "_functions")),
    show = FALSE
  )
  return(list(
    scenarios = check_scenario_table(found$scenarios, version),
    expect = found$expect
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

scenario_plan <- function(table, script, home, results) {
  plan <- plnr::Plan$new(verbose = FALSE, use_foreach = FALSE)
  for (i in seq_len(nrow(table))) {
    for (k in seq_len(table$sims[i])) {
      plan$add_argset(
        name = paste0(table$scenario[i], "/", sim_folder(k)),
        scenario = table$scenario[i],
        sim = k,
        sims = table$sims[i],
        script = script,
        home = home,
        results = results
      )
    }
  }
  plan$apply_action_fn_to_all_argsets(fn = run_one_simulation)
  return(plan)
}

# The action of one analysis in the plan of run_scenarios().
run_one_simulation <- function(data, argset) {
  scenario <- argset[["scenario"]]
  sim <- argset[["sim"]]
  folder <- file.path(argset[["results"]], scenario)
  file <- if (argset[["sims"]] > 1) {
    file.path(folder, sim_folder(sim), "results.qs2")
  } else {
    file.path(folder, "results.qs2")
  }
  # A run that writes nothing MUST NOT leave an older file to be read.
  stale <- file
  if (sim == 1) {
    stale <- unique(c(stale, file.path(folder, c("results.qs2", "check.csv"))))
  }
  unlink(stale)
  left <- stale[file.exists(stale)]
  if (length(left) > 0) {
    stop(
      "pbn could not delete an older file before the run of scenario \"",
      scenario,
      "\", simulation ",
      sim,
      ": ",
      paste(left, collapse = ", "),
      call. = FALSE
    )
  }
  message("pbn: scenario ", scenario, ", simulation ", sim)
  res <- callr::rscript(
    argset[["script"]],
    cmdargs = c(paste0("scenario=", scenario), paste0("sim=", sim)),
    wd = argset[["home"]],
    show = FALSE,
    stderr = "2>&1",
    fail_on_status = FALSE
  )
  if (res$status != 0) {
    stop(
      "The run of scenario \"",
      scenario,
      "\", simulation ",
      sim,
      ", stopped with status ",
      res$status,
      ". Its output:\n",
      res$stdout,
      call. = FALSE
    )
  }
  if (!file.exists(file)) {
    stop(
      "The run of scenario \"",
      scenario,
      "\", simulation ",
      sim,
      ", stopped with status 0 but wrote no results file ",
      file,
      ". Its output:\n",
      res$stdout,
      call. = FALSE
    )
  }
  return(invisible(NULL))
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
  table_names <- names(runs[[1]][["tables"]])
  for (k in seq_along(runs)) {
    if (!identical(names(runs[[k]][["tables"]]), table_names)) {
      stop(
        "The simulations of the scenario \"",
        scenario,
        "\" do not have the same tables, so pbn cannot stack them.",
        call. = FALSE
      )
    }
  }
  part <- function(name) {
    return(stack_frames(lapply(runs, function(r) return(r[[name]]))))
  }
  tables <- lapply(table_names, function(nm) {
    return(stack_frames(lapply(runs, function(r) return(r[["tables"]][[nm]]))))
  })
  names(tables) <- table_names
  manifest <- runs[[1]][["manifest"]]
  manifest$sim <- NA_integer_
  manifest$sims <- length(runs)
  manifest$n_rows_data <- NA_integer_
  manifest$stacked <- TRUE
  return(list(
    manifest = manifest,
    exclusions = part("exclusions"),
    estimates = part("estimates"),
    tables = tables,
    extra = list()
  ))
}

stack_frames <- function(frames) {
  parts <- lapply(seq_along(frames), function(k) {
    f <- as.data.frame(frames[[k]])
    return(data.frame(sim = rep(k, nrow(f)), f, check.names = FALSE))
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  return(out)
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
