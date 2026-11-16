# A valid synthetic project in a temporary folder. Every project rule passes
# on it, and nothing in it comes from a real project.

write_lines <- function(home, rel, lines) {
  file <- file.path(home, rel)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, file)
  return(invisible(file))
}

run_script_lines <- function(version, data_raw) {
  return(c(
    paste0(
      "p <- pbn::initialize_project(version = \"",
      version,
      "\", home = \".\", data_raw = \"",
      data_raw,
      "\")"
    ),
    "d <- data.table::fread(file.path(p$data_raw, \"d.csv\"))",
    "t1 <- pbn::table1(d, by = \"g\", vars = CFG$vars)",
    "pbn::save_results(",
    "  exclusions = data.frame(",
    "    step = \"x not missing\",",
    "    n_before = nrow(d),",
    "    n_excluded = 0,",
    "    n_after = nrow(d)",
    "  ),",
    "  estimates = data.frame(term = \"mean_x\", estimate = mean(d$x)),",
    "  tables = list(table1 = t1),",
    "  d = d",
    ")"
  ))
}

# The scenarios real (1 simulation), known (1) and null (3).
cfg_lines <- function() {
  return(c(
    "CFG <- list(",
    "  scenarios = data.frame(",
    "    scenario = c(\"real\", \"known\", \"null\"),",
    "    sims = c(1, 1, 3)",
    "  ),",
    "  vars = data.table::data.table(",
    "    var = c(\"x\", \"y\"),",
    "    type = c(\"bin\", \"cont\")",
    "  )",
    ")"
  ))
}

# Planted by hand: group a holds 4 ones of x and group b holds 1, so the mean
# of x is 0.5.
fake_known_lines <- function() {
  return(c(
    "fake_known <- function(dir) {",
    "  d <- data.frame(",
    "    g = rep(c(\"a\", \"b\"), each = 5),",
    "    x = c(1, 1, 1, 1, 0, 1, 0, 0, 0, 0),",
    "    y = 31:40",
    "  )",
    "  utils::write.csv(d, file.path(dir, \"d.csv\"), row.names = FALSE)",
    "}"
  ))
}

# Draws with the seed that initialize_project() set.
fake_null_lines <- function() {
  return(c(
    "fake_null <- function(dir) {",
    "  d <- data.frame(",
    "    g = rep(c(\"a\", \"b\"), each = 20),",
    "    x = stats::rbinom(40, 1, 0.5),",
    "    y = stats::rnorm(40)",
    "  )",
    "  utils::write.csv(d, file.path(dir, \"d.csv\"), row.names = FALSE)",
    "}"
  ))
}

expect_known_lines <- function(events_a = 4) {
  return(c(
    "expect_known <- function(results) {",
    "  t1 <- results$tables$table1",
    "  events_a <- t1$events[t1$var == \"x\" & t1$grp == \"a\"]",
    "  mean_x <- results$estimates$estimate",
    "  data.frame(",
    "    assertion = c(\"events of x in group a\", \"mean of x\"),",
    paste0("    expected = c(", events_a, ", 0.5),"),
    "    observed = c(events_a, mean_x),",
    paste0(
      "    pass = c(events_a == ",
      events_a,
      ", isTRUE(all.equal(mean_x, 0.5)))"
    ),
    "  )",
    "}"
  ))
}

expect_null_lines <- function() {
  return(c(
    "expect_null <- function(results) {",
    "  e <- results$estimates",
    "  n_sims <- length(unique(e$sim))",
    "  mean_x <- mean(e$estimate)",
    "  data.frame(",
    "    assertion = c(\"simulations\", \"mean of x over the simulations\"),",
    "    expected = c(\"3\", \"0.3 to 0.7\"),",
    "    observed = c(as.character(n_sims), format(mean_x)),",
    "    pass = c(n_sims == 3, mean_x > 0.3 && mean_x < 0.7)",
    "  )",
    "}"
  ))
}

write_data <- function(folder, x, y) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  d <- data.frame(g = rep(c("a", "b"), each = 5), x = x, y = y)
  utils::write.csv(d, file.path(folder, "d.csv"), row.names = FALSE)
  return(invisible(folder))
}

local_project <- function(env = parent.frame()) {
  root <- tempfile("pbntest")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  home <- file.path(root, "project")
  # Planted: group a holds 3 ones of x, so the mean of x is 0.4.
  data_default <- normalizePath(
    write_data(
      file.path(root, "data_default"),
      x = c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
      y = 1:10
    ),
    winslash = "/"
  )

  write_lines(home, "v01_run.R", run_script_lines("v01", data_default))
  write_lines(home, "v01_functions/_cfg.R", cfg_lines())
  write_lines(home, "v01_functions/fake_known.R", fake_known_lines())
  write_lines(home, "v01_functions/fake_null.R", fake_null_lines())
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

# The run-script and check_project() tests start a new R session, and that
# session needs pbn installed. R CMD check installs it. load_all() does not.
skip_if_pbn_not_installed <- function() {
  where <- find.package("pbn", lib.loc = .libPaths(), quiet = TRUE)
  installed <- length(where) == 1 &&
    file.exists(file.path(where, "Meta", "package.rds"))
  testthat::skip_if_not(installed, "pbn is not installed in .libPaths()")
  return(invisible(TRUE))
}

# Runs v01_run.R in a new R session with the trailing arguments `args`.
run_script <- function(home, args = character(0)) {
  return(callr::rscript(
    file.path(home, "v01_run.R"),
    cmdargs = args,
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
