# Helpers of the tests of to_one_file() and check_one_file().

# The order in which org sources the .R files of the functions folder of the
# project `p`, measured by org itself: a copy of the folder where each file
# appends its own name.
org_order <- function(p) {
  probe <- file.path(p$root, "probe")
  files <- list.files(file.path(p$home, "v01_functions"), pattern = "\\.[rR]$")
  dir.create(file.path(probe, "f"), recursive = TRUE)
  for (f in files) {
    writeLines(
      paste0(".seen <- c(.seen, \"", f, "\")"),
      file.path(probe, "f", f)
    )
  }
  env <- new.env()
  env$.seen <- character(0)
  saved <- mget(ls(org::project), envir = org::project)
  on.exit(list2env(saved, envir = org::project), add = TRUE)
  suppressMessages(org::initialize_project(
    env = env,
    home = probe,
    folders_to_be_sourced = "f"
  ))
  return(paste0("v01_functions/", env$.seen))
}

# Adds files to the functions folder of `p` whose names test the order of
# org, an expect_real() and blocks.
add_one_file_files <- function(p) {
  write_lines(p$home, "v01_functions/Zeta.R", "zeta <- function() return(1)")
  write_lines(p$home, "v01_functions/get.R", "get_it <- function() return(2)")
  write_lines(
    p$home,
    "v01_functions/01_first.R",
    "first <- function() return(3)"
  )
  write_lines(
    p$home,
    "v01_functions/expect_real.R",
    c(
      "# The check of the real data.",
      "expect_real <- function(results) {",
      "  data.frame(assertion = \"rows\", expected = 1, observed = 1, pass = TRUE)",
      "}"
    )
  )
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Table 1\" = function(results) results$table1,",
      "  \"Models\" = function(results) results$models",
      ")"
    )
  )
  return(invisible(p))
}
