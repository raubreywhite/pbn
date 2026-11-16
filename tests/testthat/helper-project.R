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
  data_default <- normalizePath(
    write_data(
      file.path(root, "data_default"),
      x = c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0),
      y = 1:10
    ),
    winslash = "/"
  )

  write_lines(home, "v01_run.R", run_script_lines("v01", data_default))
  write_lines(
    home,
    "v01_functions/_cfg.R",
    c(
      "CFG <- list(",
      "  vars = data.table::data.table(",
      "    var = c(\"x\", \"y\"),",
      "    type = c(\"bin\", \"cont\")",
      "  )",
      ")"
    )
  )
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
