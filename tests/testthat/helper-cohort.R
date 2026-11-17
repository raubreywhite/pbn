# A synthetic project whose data function is the three steps
# data_raw() |> data_clean() |> data_cohort(), with a cohort and one
# exclusion. Nothing in it comes from a real project.

# The config. CFG$data is the config of the data code, and CFG$outcome is
# config of the analysis only. `counter` is the file that data_raw() appends
# one line to on each call.
cohort_cfg_lines <- function(counter, age_min = 18, n_boot_null = 5) {
  return(c(
    "CFG <- list(",
    "  scenarios = data.frame(",
    "    scenario = c(\"real\", \"null\"),",
    "    sims = c(1, 2),",
    paste0("    n_boot = c(100, ", n_boot_null, ")"),
    "  ),",
    paste0("  data = list(age_min = ", age_min, "),"),
    "  outcome = \"y\",",
    paste0("  counter = \"", counter, "\""),
    ")"
  ))
}

cohort_function_files <- function() {
  return(list(
    "data_raw.R" = c(
      "data_raw <- function(scenario, sim) {",
      counter_lines("  "),
      "  if (scenario == \"real\") {",
      "    return(utils::read.csv(pbn::folder(\"main\", \"d.csv\")))",
      "  }",
      "  data.frame(",
      "    Age = sample(15:60, 30, replace = TRUE),",
      "    Group = rep(c(\"a\", \"b\"), 15),",
      "    Y = stats::rnorm(30)",
      "  )",
      "}"
    ),
    "data_clean.R" = c(
      "data_clean <- function(d) {",
      "  d <- data.table::as.data.table(d)",
      "  data.table::setnames(d, tolower(names(d)))",
      "  d",
      "}"
    ),
    "data_cohort.R" = c(
      "data_cohort <- function(d) {",
      "  cp <- cohort::CohortPipeline$new(d)",
      "  cp$exclude_and_track(",
      "    \"root\",",
      "    paste(\"age under\", CFG$data$age_min),",
      "    paste(\"age <\", CFG$data$age_min)",
      "  )",
      "  cp",
      "}"
    ),
    # One draw, so a seed that differs between two runs changes the result.
    "analyses.R" = c(
      "fit_mean <- function(data, argset) {",
      "  d <- data$d",
      "  data.frame(",
      "    class = class(d)[1],",
      "    n = nrow(d),",
      "    mean_y = mean(d[[CFG$outcome]]),",
      "    draw = stats::runif(1)",
      "  )",
      "}",
      "",
      "# The mean of y over the imputed data sets of data_impute().",
      "fit_mean_imp <- function(data, argset) {",
      "  data.frame(",
      "    n_imp = length(data$d),",
      "    n = nrow(data$d[[1]]),",
      "    mean_y = mean(vapply(data$d, function(x) mean(x$y), 1)),",
      "    draw = stats::runif(1)",
      "  )",
      "}"
    ),
    # Three draws of y for each person of the cohort, after the exclusions.
    # impute_draws() returns the plain list, for the step after_cohort, and
    # data_impute() keeps the exclusion table, as a data function fn does.
    "data_impute.R" = c(
      "impute_draws <- function(cp) {",
      "  d <- pbn::cohort_data(cp)",
      "  lapply(1:3, function(k) {",
      "    x <- data.table::copy(d)",
      "    data.table::set(x, j = \"y\", value = x$y + stats::rnorm(nrow(x), 0, 0.1))",
      "    x",
      "  })",
      "}",
      "",
      "data_impute <- function(cp) {",
      "  pbn::keep_exclusions(impute_draws(cp), cp)",
      "}"
    ),
    "expect_null.R" = c(
      "expect_null <- function(results) {",
      "  n <- length(unique(results$means$sim))",
      "  data.frame(assertion = \"simulations\", expected = \"2\",",
      "    observed = as.character(n), pass = n == 2)",
      "}"
    )
  ))
}

# The real data: 8 rows, and 2 of them under 18.
write_cohort_data <- function(folder, age = c(16, 17, 20, 30, 40, 50, 60, 25)) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  d <- data.frame(
    Age = age,
    Group = rep(c("a", "b"), 4),
    Y = c(1, 2, 3, 4, 5, 6, 7, 8)
  )
  utils::write.csv(d, file.path(folder, "d.csv"), row.names = FALSE)
  return(invisible(folder))
}

local_cohort_project <- function(env = parent.frame()) {
  root <- tempfile("pbncohort")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  root <- normalizePath(root, winslash = "/")
  # git does not look above `root`, so the project is a git repository only
  # when a test makes it one.
  withr::local_envvar(GIT_CEILING_DIRECTORIES = root, .local_envir = env)
  home <- file.path(root, "project")
  data_raw <- normalizePath(
    write_cohort_data(file.path(root, "data_raw")),
    winslash = "/"
  )
  counter <- file.path(root, "calls.txt")
  write_lines(home, "v01_run.R", "# The run script.")
  write_lines(home, "pbn.yml", house_line)
  write_lines(home, "v01_functions/_cfg.R", cohort_cfg_lines(counter))
  files <- cohort_function_files()
  for (f in names(files)) {
    write_lines(home, file.path("v01_functions", f), files[[f]])
  }
  return(list(
    home = home,
    root = root,
    data_default = data_raw,
    counter = counter
  ))
}

# The data function of the run script, as a project writes it.
cohort_data_fn <- function(scenario, sim) {
  return(data_cohort(data_clean(data_raw(scenario, sim))))
}

# The data function with the imputation after the cohort.
impute_data_fn <- function(scenario, sim) {
  return(data_impute(data_cohort(data_clean(data_raw(scenario, sim)))))
}

# The steps of cohort_data_fn() and of impute_data_fn(), for the steps form
# of add_data(). A string is the name of a function of the functions folder.
cohort_steps <- list(
  raw = "data_raw",
  clean = "data_clean",
  cohort = "data_cohort"
)
impute_steps <- c(cohort_steps, list(after_cohort = "data_impute"))
# The step after_cohort without keep_exclusions(): pbn keeps the table.
plain_steps <- c(cohort_steps, list(after_cohort = "impute_draws"))

# The steps `steps` as functions: a string is the function of that name in
# `sourced`, and a function gets `sourced` as its environment.
sourced_steps <- function(steps, sourced) {
  return(lapply(steps, function(s) {
    if (is.character(s)) {
      return(sourced[[s]])
    }
    environment(s) <- sourced
    return(s)
  }))
}

# Starts the project and builds the plan, as the run script does up to the
# add_ calls. The data is `fn`, or the steps form with `steps` when it is
# not NULL. `env` is the test.
cohort_plan <- function(
  p,
  cache = TRUE,
  fn = cohort_data_fn,
  analysis = "fit_mean",
  steps = NULL,
  env = parent.frame()
) {
  sourced <- init(p, env = env)
  scenarios <- scenarios(sourced$CFG)
  plan <- plnr::Plan$new()
  if (is.null(steps)) {
    environment(fn) <- sourced
    add_data(plan, "d", fn = fn, cache = cache)
  } else {
    do.call(
      add_data,
      c(list(plan, "d"), sourced_steps(steps, sourced), list(cache = cache))
    )
  }
  add_analyses(plan, scenarios, name = "means", fn = sourced[[analysis]])
  return(list(plan = plan, scenarios = scenarios, sourced = sourced))
}

# The run script of the cohort project, up to the add_ calls, and then the
# lines `tail`.
cohort_script_lines <- function(p, tail) {
  return(c(
    paste0(
      "pbn::initialize_project(version = \"v01\", home = \".\", folders = c(main = \"",
      p$data_default,
      "\"))"
    ),
    "scenarios <- pbn::scenarios(CFG)",
    "p <- plnr::Plan$new()",
    "pbn::add_data(p, \"d\", fn = function(scenario, sim) {",
    "  return(data_cohort(data_clean(data_raw(scenario, sim))))",
    "}, cache = TRUE)",
    "pbn::add_analyses(p, scenarios, name = \"means\", fn = fit_mean)",
    tail
  ))
}

# Starts the project with initialize_project(), builds the plan with the data
# function `fn` and runs it. `env` is the test, so the state of the project
# lasts until the test ends. Returns the value of run_plan(), the printed
# lines, the plan, the scenarios and the sourced environment.
cohort_run <- function(
  p,
  workers = 1,
  cache = TRUE,
  fn = cohort_data_fn,
  analysis = "fit_mean",
  steps = NULL,
  env = parent.frame()
) {
  run <- cohort_plan(p, cache, fn, analysis, steps = steps, env = env)
  out <- utils::capture.output(r <- run_plan(run$plan, workers = workers))
  return(c(list(r = r, out = out), run))
}

# The value of run_plan() without its stamp, which names the plan. Two runs
# of two plans with the same content are then identical().
unstamped <- function(r) {
  attr(r, "pbn_run") <- NULL
  return(r)
}

# The value of run_plan() without the analyses that count the rows, which
# carry the cache record.
without_counts <- function(r) {
  return(r[!grepl("__pbn_n_rows$", names(r))])
}

# The cache file and the key file of one scenario and simulation.
cache_file <- function(p, scenario, sim, key = FALSE) {
  return(file.path(
    p$home,
    "v01_cache_gitexclude",
    "d",
    scenario,
    sprintf(if (key) "sim_%04d_key.qs2" else "sim_%04d.qs2", sim)
  ))
}

# The printed line of each scenario and simulation for the reasons `why`,
# in the order real 1, null 1, null 2. NA is a line of a reuse.
cache_lines <- function(p, why) {
  where <- c("real sim_0001", "null sim_0001", "null sim_0002")
  files <- c(cache_file(p, "real", 1), cache_file(p, "null", 1:2))
  return(ifelse(
    is.na(why),
    paste0(
      "pbn: data d, ",
      where,
      ": reused the cache of ",
      format(file.mtime(files), "%Y-%m-%d %H:%M"),
      "."
    ),
    paste0("pbn: data d, ", where, ": computed, because ", why, ".")
  ))
}

# Runs git in `dir`, and stops when git fails.
git_in <- function(dir, ...) {
  out <- system2(
    "git",
    c("-C", shQuote(dir), ...),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!is.null(attr(out, "status"))) {
    stop("git failed: ", paste(out, collapse = "\n"), call. = FALSE)
  }
  return(invisible(out))
}
