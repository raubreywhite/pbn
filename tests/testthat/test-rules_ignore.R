# The key ignore of pbn.yml: top-level folders that no rule reads, except the
# locked rules.

ignore_error <- function(home, ...) {
  write_pbn_yml(home, ...)
  err <- tryCatch(read_rules(home), error = function(e) return(e))
  if (!inherits(err, "error")) {
    stop("read_rules() did not stop")
  }
  return(conditionMessage(err))
}

write_archive <- function(home) {
  write_lines(home, "archive/Old_Run.R", c(
    "x <- read.csv(\"/data/project/d.csv\")",
    "assign(\"y\", 1)"
  ))
  write_lines(home, "archive/Notes/Draft.md", "x")
  return(invisible(home))
}

test_that("an ignored folder gives no finding of any rule that is not locked", {
  p <- local_project()
  write_archive(p$home)
  write_pbn_yml(p$home)
  found <- project_findings(p$home)
  expect_true(any(grepl("^archive", found$path)))
  for (entry in c("archive", "archive/")) {
    write_pbn_yml(p$home, paste0("ignore: [", entry, "]"))
    found <- project_findings(p$home)
    expect_false(any(grepl("^archive", found$path)), info = entry)
    expect_false(any(found$status %in% c("FAIL", "WARN")), info = entry)
  }
})

test_that("ignore covers the folder and nothing whose name starts with it", {
  p <- local_project()
  write_archive(p$home)
  write_lines(p$home, "archive2/Old.R", "x <- 1")
  write_pbn_yml(p$home, "ignore: [archive/]")
  found <- project_findings(p$home)
  expect_false(any(grepl("^archive/", found$path)))
  expect_true("archive2/Old.R" %in% found$path[found$rule == "lower_case_names"])
})

test_that("a locked rule still reads an ignored folder", {
  p <- local_project()
  write_lines(p$home, "archive/fake_data_fig.png", "x")
  write_pbn_yml(p$home, "ignore: [archive/]")
  found <- project_findings(p$home)
  expect_true("archive/fake_data_fig.png" %in% found$path[found$rule == "fake_data_names" & found$status == "FAIL"])
})

test_that("a tracked gitexclude file in an ignored folder still fails", {
  skip_if(!nzchar(Sys.which("git")))
  p <- local_project()
  writeLines("*gitexclude*", file.path(p$home, ".gitignore"))
  write_lines(p$home, "archive/imp_gitexclude.qs2", "x")
  write_pbn_yml(p$home, "ignore: [archive/]")
  git <- function(...) {
    return(system2("git", c("-C", shQuote(p$home), ...), stdout = FALSE, stderr = FALSE))
  }
  git("init", "-q")
  git("add", "-A")
  git("add", "-f", "archive/imp_gitexclude.qs2")
  write_archive(p$home)
  git("add", "-A")
  found <- project_findings(p$home)
  expect_true("archive/imp_gitexclude.qs2" %in% found$path[found$rule == "gitexclude_not_tracked" & found$status == "FAIL"])
  expect_identical(unique(found$rule[grepl("^archive", found$path)]), "gitexclude_not_tracked")
})

test_that("ignore stops on a name of the house layout, a nested path, a dot, a duplicate and a file", {
  p <- local_project()
  for (entry in c("v01_functions/", "v01_run.R", "v01_results", "manuscript", "background/", "adhoc", "pbn.yml", "README.md", "v03_frozen.md5", "v01_one_file.R", "v01_check_functions.R", "check_project.R", ".git")) {
    expect_match(ignore_error(p$home, paste0("ignore: [", entry, "]")), "house layout", info = entry)
  }
  expect_match(ignore_error(p$home, "ignore: [archive/old/]"), "one folder at the top level")
  expect_match(ignore_error(p$home, "ignore: ['.']"), "one folder at the top level")
  expect_match(ignore_error(p$home, "ignore: ['..']"), "one folder at the top level")
  expect_match(ignore_error(p$home, "ignore: [1]"), "one folder name")
  expect_match(ignore_error(p$home, "ignore: [archive, archive/]"), "twice")
  expect_match(ignore_error(p$home, "ignore: archive"), "list")
  expect_match(ignore_error(p$home, "ignore: {archive: other}"), "list")
  write_lines(p$home, "notes", "x")
  expect_match(ignore_error(p$home, "ignore: [notes]"), "not a folder")
})

test_that("a missing ignored folder is no error, and the first line counts the ignored folders", {
  p <- local_project()
  write_pbn_yml(p$home, "ignore: [archive/, old/]")
  rules <- read_rules(p$home)
  expect_identical(rules$ignore, c("archive", "old"))
  expect_identical(
    rules_line(rules),
    paste0("Rules: ", rules$set, ", 0 overrides, 0 allowed paths, 2 ignored folders (pbn.yml)")
  )
  write_pbn_yml(p$home, "ignore: [archive/]")
  expect_match(rules_line(read_rules(p$home)), "1 ignored folder \\(pbn.yml\\)$")
  write_pbn_yml(p$home)
  expect_identical(read_rules(p$home)$ignore, character(0))
  expect_match(rules_line(read_rules(p$home)), "0 allowed paths \\(pbn.yml\\)$")
})
