# A valid synthetic project in a temporary folder, on the plnr design: the
# run script is a plnr plan, the real data is one scenario among fake ones,
# and the data is one keyed data function. Every project rule passes on it,
# and nothing in it comes from a real project.

write_lines <- function(home, rel, lines) {
  file <- file.path(home, rel)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file)
  return(invisible(file))
}

# The run script of the house skeleton, as a project writes it. `only` gives
# the argument `only` of scenarios() as R code, `workers` the workers of
# run_plan(), and `extra` lines after run_plan(). The output part after
# end_unless_real() writes output.txt for the real data only.
run_script_lines <- function(
  version,
  data_raw,
  only = NULL,
  workers = 1,
  extra = character(0)
) {
  return(c(
    paste0(
      "pbn::initialize_project(version = \"",
      version,
      "\", home = \".\", data_raw = \"",
      data_raw,
      "\")"
    ),
    if (is.null(only)) {
      "scenarios <- pbn::scenarios(CFG)"
    } else {
      paste0("scenarios <- pbn::scenarios(CFG, only = ", only, ")")
    },
    "",
    "p <- plnr::Plan$new()",
    "pbn::add_data(p, \"d\", fn = function(scenario, sim) get_data(scenario, sim))",
    "pbn::add_analyses(p, scenarios, name = \"table1\", fn = make_table1)",
    paste0(
      "pbn::add_analyses(p, scenarios, name = \"models\", fn = fit_models, ",
      "outcome = CFG$outcomes)"
    ),
    paste0(
      "pbn::add_analyses(p, scenarios, name = \"models_crude\", ",
      "fn = fit_models, outcome = CFG$outcomes, adjust = \"none\", ",
      "control = TRUE)"
    ),
    paste0(
      "r <- pbn::run_plan(p",
      if (workers != 1) paste0(", workers = ", workers),
      ")"
    ),
    extra,
    "pbn::save_results(r, p, scenarios)",
    "",
    "pbn::end_unless_real(scenarios)",
    "",
    "# PART 3 -- OUTPUT ====",
    "res <- pbn::read_results(\"v01\")",
    "writeLines(\"output\", file.path(\"v01_results\", \"real\", \"output.txt\"))"
  ))
}

# The position of the initialize_project() call in run_script_lines().
init_line <- function(lines) {
  return(which(startsWith(lines, "pbn::initialize_project(")))
}

# The scenarios real (1 simulation), known (1) and null (3). n_boot is a
# setting that each argset holds. `counter` is NULL, or the path of a file
# that get_data() appends one line to on each call.
cfg_lines <- function(counter = NULL) {
  return(c(
    "CFG <- list(",
    "  scenarios = data.frame(",
    "    scenario = c(\"real\", \"known\", \"null\"),",
    "    sims = c(1, 1, 3),",
    "    n_boot = c(200, 5, 5)",
    "  ),",
    "  outcomes = c(\"y\", \"z\"),",
    if (!is.null(counter)) paste0("  counter = \"", counter, "\","),
    "  vars = data.table::data.table(",
    "    var = c(\"x\", \"y\"),",
    "    type = c(\"bin\", \"cont\")",
    "  )",
    ")"
  ))
}

# The data function. "real" reads d.csv of data_raw. "known" plants the
# values by hand: group a holds 4 ones of x, and the mean of y is 1 in group
# a and 3 in group b. "null" draws with the seed that pbn sets.
get_data_lines <- function() {
  return(c(
    "get_data <- function(scenario, sim) {",
    "  if (!is.null(CFG$counter)) {",
    "    # One string, so one write for each call, also from two workers.",
    "    cat(paste0(scenario, \" \", sim, \"\\n\"), file = CFG$counter, append = TRUE)",
    "  }",
    "  if (scenario == \"real\") {",
    "    return(utils::read.csv(file.path(org::project$data_raw, \"d.csv\")))",
    "  }",
    "  if (scenario == \"known\") {",
    "    return(data.frame(",
    "      g = rep(c(\"a\", \"b\"), each = 5),",
    "      x = c(1, 1, 1, 1, 0, 1, 0, 0, 0, 0),",
    "      y = c(0, 1, 1, 1, 2, 2, 3, 3, 3, 4),",
    "      z = 1:10",
    "    ))",
    "  }",
    "  return(data.frame(",
    "    g = rep(c(\"a\", \"b\"), each = 20),",
    "    x = stats::rbinom(40, 1, 0.5),",
    "    y = stats::rnorm(40),",
    "    z = stats::rnorm(40)",
    "  ))",
    "}"
  ))
}

# The action functions. fit_models() fits outcome ~ g, adjusted for x
# unless argset$adjust is "none". It adds one draw, so a seed that differs
# between two runs changes its results.
analysis_lines <- function() {
  return(c(
    "make_table1 <- function(data, argset) {",
    "  return(pbn::table1(data$d, by = \"g\", vars = CFG$vars))",
    "}",
    "",
    "fit_models <- function(data, argset) {",
    "  rhs <- if (identical(argset$adjust, \"none\")) \"g\" else \"g + x\"",
    "  fit <- stats::lm(stats::as.formula(paste(argset$outcome, \"~\", rhs)), data = data$d)",
    "  data.frame(",
    "    term = names(stats::coef(fit)),",
    "    estimate = unname(stats::coef(fit)),",
    "    draw = stats::runif(1)",
    "  )",
    "}"
  ))
}

# The crude model of y on g in known is the difference of the means, 3 - 1.
expect_known_lines <- function(events_a = 4) {
  return(c(
    "expect_known <- function(results) {",
    "  t1 <- results$table1",
    "  events_a <- t1$events[t1$var == \"x\" & t1$grp == \"a\"]",
    "  m <- results$models_crude",
    "  diff_y <- m$estimate[m$outcome == \"y\" & m$term == \"gb\"]",
    "  data.frame(",
    "    assertion = c(\"events of x in group a\", \"crude difference of y\"),",
    paste0("    expected = c(", events_a, ", 2),"),
    "    observed = c(events_a, diff_y),",
    paste0(
      "    pass = c(events_a == ",
      events_a,
      ", isTRUE(all.equal(diff_y, 2)))"
    ),
    "  )",
    "}"
  ))
}

expect_null_lines <- function() {
  return(c(
    "expect_null <- function(results) {",
    "  m <- results$models",
    "  n_sims <- length(unique(m$sim))",
    "  b <- mean(m$estimate[m$term == \"gb\"])",
    "  data.frame(",
    "    assertion = c(\"simulations\", \"mean difference over the simulations\"),",
    "    expected = c(\"3\", \"-1 to 1\"),",
    "    observed = c(as.character(n_sims), format(b)),",
    "    pass = c(n_sims == 3, abs(b) < 1)",
    "  )",
    "}"
  ))
}

write_data <- function(folder) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  # Planted: group a holds 3 ones of x.
  d <- data.frame(
    g = rep(c("a", "b"), each = 5),
    x = c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
    y = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6),
    z = 10:1
  )
  utils::write.csv(d, file.path(folder, "d.csv"), row.names = FALSE)
  return(invisible(folder))
}

local_project <- function(env = parent.frame(), counter = NULL) {
  root <- tempfile("pbntest")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  root <- normalizePath(root, winslash = "/")
  home <- file.path(root, "project")
  data_default <- normalizePath(
    write_data(file.path(root, "data_default")),
    winslash = "/"
  )

  write_lines(home, "v01_run.R", run_script_lines("v01", data_default))
  write_lines(home, "v01_functions/_cfg.R", cfg_lines(counter))
  write_lines(home, "v01_functions/get_data.R", get_data_lines())
  write_lines(home, "v01_functions/analyses.R", analysis_lines())
  write_lines(home, "v01_functions/expect_known.R", expect_known_lines())
  write_lines(home, "v01_functions/expect_null.R", expect_null_lines())
  write_lines(
    home,
    "v01_functions/double_it.R",
    "double_it <- function(x) return(2 * x)"
  )
  write_lines(
    home,
    "v01_test/test-double_it.R",
    "testthat::expect_equal(double_it(2), 4)"
  )
  write_lines(home, "check_project.R", "pbn::check_project()")
  write_lines(home, "manuscript/manual/v01_AB.docx", "")
  dir.create(file.path(home, "manuscript", "built", "v01"), recursive = TRUE)
  write_lines(home, "background/Received Protocol.PDF", "")
  dir.create(file.path(home, "adhoc"))
  dir.create(file.path(home, "data_describe"))
  write_lines(home, "README.md", "# A synthetic project")
  write_lines(home, "CHANGELOG.md", "# Changelog")
  write_lines(home, "AGENTS.md", "# Agents")
  write_lines(home, ".gitignore", "v01_results/")

  return(list(home = home, root = root, data_default = data_default))
}

failed_rules <- function(findings) {
  return(unique(findings$rule[findings$status == "FAIL"]))
}

# The run-script, worker and check_project() tests start new R sessions, and
# those sessions need pbn installed. R CMD check installs it. load_all() does
# not, so run the suite with pbn in a throwaway library.
skip_if_pbn_not_installed <- function() {
  where <- find.package("pbn", lib.loc = .libPaths(), quiet = TRUE)
  installed <- length(where) == 1 &&
    file.exists(file.path(where, "Meta", "package.rds"))
  testthat::skip_if_not(installed, "pbn is not installed in .libPaths()")
  return(invisible(TRUE))
}

# Runs a script of the project in a new R session.
run_script <- function(home, script = "v01_run.R") {
  return(callr::rscript(
    file.path(home, script),
    wd = home,
    show = FALSE,
    stderr = "2>&1",
    fail_on_status = FALSE
  ))
}

md5_of <- function(file) {
  return(unname(tools::md5sum(file)))
}

# Every file under `folder`, relative to it, with its md5 and mtime.
file_states <- function(folder) {
  files <- sort(list.files(folder, recursive = TRUE, all.files = TRUE))
  full <- file.path(folder, files)
  return(data.frame(
    file = files,
    md5 = md5_of(full),
    mtime = as.numeric(file.mtime(full))
  ))
}

# A valid results object of the real scenario. Every check_results() rule
# passes on it.
valid_results <- function() {
  return(list(
    manifest = list(
      version = "v01",
      scenario = "real",
      sim = 1L,
      sims = 1L,
      data_md5 = c(d.csv = "0123456789abcdef0123456789abcdef"),
      source_md5 = c(
        "v01_functions/_cfg.R" = "00112233445566778899aabbccddeeff",
        v01_run.R = "ffeeddccbbaa99887766554433221100"
      ),
      n_rows_data = 40L
    ),
    exclusions = data.frame(
      step = c("age 18 or more", "no missing outcome"),
      n_before = c(50, 45),
      n_excluded = c(5, 5),
      n_after = c(45, 40)
    ),
    estimates = data.frame(term = c("a", "b"), estimate = c(0.1, 0.2)),
    tables = list(counts = data.frame(grp = c("a", "b"), n = c(20, 20))),
    extra = list(note = "synthetic")
  ))
}

# Empties the state of the project now, and again when the calling test ends.
local_clean_state <- function(env = parent.frame()) {
  clean <- function() {
    rm(list = ls(state, all.names = TRUE), envir = state)
    return(invisible(NULL))
  }
  clean()
  withr::defer(clean(), envir = env)
  return(invisible(NULL))
}

# Starts the project in this R session, as the first line of the run script
# does, with the functions folder sourced into a new environment. Returns
# that environment.
init <- function(p, data_raw = p$data_default, env = parent.frame()) {
  local_clean_state(env)
  # The parent is the global environment, as for a project. An environment
  # of the test would go to each worker with the functions.
  sourced <- new.env(parent = globalenv())
  suppressMessages(initialize_project("v01", p$home, data_raw, env = sourced))
  return(sourced)
}

# The plan of the run script, built in this R session from the functions in
# `sourced`.
build_plan <- function(sourced, scenarios) {
  p <- plnr::Plan$new()
  fn <- function(scenario, sim) {
    return(sourced$get_data(scenario, sim))
  }
  environment(fn) <- list2env(list(sourced = sourced), parent = globalenv())
  add_data(p, "d", fn = fn)
  add_analyses(p, scenarios, name = "table1", fn = sourced$make_table1)
  add_analyses(
    p,
    scenarios,
    name = "models",
    fn = sourced$fit_models,
    outcome = sourced$CFG$outcomes
  )
  add_analyses(
    p,
    scenarios,
    name = "models_crude",
    fn = sourced$fit_models,
    outcome = sourced$CFG$outcomes,
    adjust = "none",
    control = TRUE
  )
  return(p)
}

results_files <- function(p) {
  return(sort(list.files(
    file.path(p$home, "v01_results"),
    recursive = TRUE,
    all.files = TRUE
  )))
}

# The files that the run script of local_project() writes for every
# scenario.
expected_tree <- c(
  "known/check.csv",
  "known/results.qs2",
  "null/check.csv",
  "null/results.qs2",
  "null/sim_0001/results.qs2",
  "null/sim_0002/results.qs2",
  "null/sim_0003/results.qs2",
  "real/output.txt",
  "real/results.qs2"
)
