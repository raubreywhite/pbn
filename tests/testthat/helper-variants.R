# Variants of the synthetic project of local_project() with findings of
# every rule of check_project(). The test that the house rule set changes
# nothing compares the output of check_project() on these variants with the
# output of pbn at commit 3fa42ee, before pbn.yml existed. That output is
# the file fixtures/check_project_before_rules.txt.
#
# Each finding is in a file of its own, so that the order of the lines does
# not depend on how the OS sorts the names of files.

# The names of the variants, in the order of the fixture file.
project_variant_names <- c("valid", "findings", "allowed", "git")

# Changes the project `p` of local_project() into the variant `name`.
# Returns the exceptions of the variant: a list of the paths of each rule.
make_project_variant <- function(p, name) {
  home <- p$home
  if (name == "valid") {
    return(list())
  }
  if (name == "git") {
    git_in(home, "init", "-q")
    write_lines(home, "v01_results/real/table_gitexclude.csv", "1")
    git_in(home, "add", "-f", "v01_results/real/table_gitexclude.csv")
    return(list())
  }
  if (name == "allowed") {
    write_lines(home, "adhoc/Notes.txt", "x")
    write_lines(home, "adhoc/branchy.R", branchy_lines("tidy", 16))
    write_lines(home, "adhoc/early.R", c("a <- 1", "assign(\"b\", a)"))
    write_lines(home, "notes.txt", "x")
    return(list(
      lower_case_names = "adhoc/Notes.txt",
      cyclomatic_complexity = "adhoc/branchy.R",
      no_local_assign = "adhoc/early.R",
      unknown_path = "notes.txt"
    ))
  }
  # "findings": one finding of each rule other than gitexclude_not_tracked.
  write_lines(home, "adhoc/Notes.txt", "x")
  run <- readLines(file.path(home, "v01_run.R"))
  run[1] <- sub("\"v01\"", "\"v03\"", run[1], fixed = TRUE)
  writeLines(run, file.path(home, "v01_run.R"))
  dir.create(file.path(home, "v02_functions"))
  x <- valid_results()
  x$exclusions$n_after[2] <- 41
  dir.create(file.path(home, "v01_results", "real"), recursive = TRUE)
  qs2::qs_save(x, file.path(home, "v01_results", "real", "results.qs2"))
  write_lines(
    home,
    "v01_results/null/check.csv",
    c("\"assertion\",\"pass\"", "\"one\",FALSE")
  )
  write_lines(home, "manuscript/manual/v01_cd.docx", "")
  write_lines(
    home,
    "manuscript/built/v01/tables.R",
    "x <- pbn::read_results(\"v01\", scenario = \"null\")"
  )
  write_lines(home, "v01_results/known/output_fake_data/figure.png", "")
  write_lines(home, "v01_functions/tables.R", "table1 <- function(d) return(d)")
  write_lines(home, "adhoc/long.R", paste0("x", seq_len(1001), " <- 1"))
  write_lines(home, "adhoc/branchy.R", branchy_lines("tidy", 16))
  write_lines(
    home,
    "v01_functions/key.R",
    "key <- function() return(Sys.getenv(\"KEY\"))"
  )
  write_lines(home, "adhoc/early.R", c("a <- 1", "assign(\"b\", a)", "c <<- 2"))
  write_lines(home, "notes.txt", "x")
  return(list())
}

# The lines of a function `name` with `ifs` branches. cyclocomp scores it
# ifs + 1.
branchy_lines <- function(name, ifs) {
  return(c(
    paste0(name, " <- function(x) {"),
    "  y <- 0",
    paste0("  if (x == ", seq_len(ifs), ") y <- ", seq_len(ifs)),
    "  return(y)",
    "}"
  ))
}

# The lines of the fixture file for the variant `name`.
fixture_lines <- function(file, name) {
  lines <- readLines(file, warn = FALSE)
  start <- which(lines == paste("##", name))
  ends <- c(which(startsWith(lines, "## ")), length(lines) + 1)
  end <- min(ends[ends > start])
  return(lines[seq(start + 1, end - 1)])
}
