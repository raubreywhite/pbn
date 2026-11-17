# save_results(gitexclude = ...) on the plan of the synthetic project. The
# kind models goes to results_gitexclude.qs2.

split_run <- function(p, extra = NULL, env = parent.frame()) {
  sourced <- init(p, env = env)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  if (!is.null(extra)) {
    add_analyses(plan, sc, name = "rows", fn = extra)
  }
  return(list(plan = plan, scenarios = sc, r = run_plan(plan)))
}

split_save <- function(run, gitexclude = NULL) {
  out <- utils::capture.output(
    save_results(run$r, run$plan, run$scenarios, gitexclude = gitexclude)
  )
  return(out)
}

# Every results_gitexclude.qs2 that the split run writes.
private_tree <- c(
  "known/results_gitexclude.qs2",
  "null/results_gitexclude.qs2",
  "null/sims_gitexclude/sim_0001_gitexclude.qs2",
  "null/sims_gitexclude/sim_0002_gitexclude.qs2",
  "null/sims_gitexclude/sim_0003_gitexclude.qs2",
  "real/results_gitexclude.qs2"
)

test_that("gitexclude writes its kinds to results_gitexclude.qs2 beside each results.qs2, and read_results returns one list with every element", {
  p <- local_project()
  run <- split_run(p)
  split_save(run)
  whole <- lapply(
    c("real", "known", "null"),
    read_results,
    version = "v01",
    home = p$home
  )
  whole_sim <- read_results("v01", "null", sim = 2, home = p$home)

  split_save(run, gitexclude = "models")
  expect_identical(
    results_files(p),
    sort(c(setdiff(expected_tree, "real/output.txt"), private_tree))
  )
  folder <- file.path(p$home, "v01_results")
  public <- qs2::qs_read(file.path(folder, "real", "results.qs2"))
  expect_identical(names(public), c("manifest", "table1"))
  expect_identical(
    public$manifest$analyses,
    data.frame(
      analysis = c("table1", "models"),
      control = FALSE,
      file = c("results.qs2", "results_gitexclude.qs2")
    )
  )
  private <- qs2::qs_read(file.path(folder, "null", "results_gitexclude.qs2"))
  expect_identical(names(private), "models")
  expect_identical(unique(private$models$sim), 1:3)
  expect_identical(
    names(qs2::qs_read(file.path(folder, "known", "results.qs2"))),
    c("manifest", "table1", "models_crude")
  )

  # read_results() gives the object of a run without gitexclude, but for the
  # column file of the manifest.
  merged <- lapply(
    c("real", "known", "null"),
    read_results,
    version = "v01",
    home = p$home
  )
  merged_sim <- read_results("v01", "null", sim = 2, home = p$home)
  unsplit <- function(x) {
    x$manifest$analyses$file <- "results.qs2"
    return(x)
  }
  expect_identical(lapply(merged, unsplit), whole)
  expect_identical(unsplit(merged_sim), whole_sim)

  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
})

test_that("an unknown name in gitexclude stops save_results before it writes anything", {
  p <- local_project()
  run <- split_run(p)
  expect_error(
    split_save(run, gitexclude = c("models", "fits")),
    paste0(
      "These names in `gitexclude` are not analysis kinds of the plan: ",
      "fits. The kinds are: table1, models, models_crude. pbn saved nothing."
    ),
    fixed = TRUE
  )
  expect_error(split_save(run, gitexclude = c("models", "models")), "each once")
  expect_false(dir.exists(file.path(p$home, "v01_results")))
})

test_that("without results_gitexclude.qs2, read_results gives one warning that names the kinds and the file, and returns the other elements", {
  p <- local_project()
  run <- split_run(p)
  split_save(run, gitexclude = "models")
  file <- file.path(p$home, "v01_results", "null", "results_gitexclude.qs2")
  unlink(file)
  warnings <- character(0)
  x <- withCallingHandlers(
    read_results("v01", "null", home = p$home),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(
    warnings,
    paste0(
      "The manifest names the analysis kinds models in ",
      file,
      ", and that file is missing, for example in a clone of the ",
      "repository. read_results() returns the other elements."
    )
  )
  expect_identical(names(x), c("manifest", "table1", "models_crude"))
})

test_that("a kind with one row for each person passes in results_gitexclude.qs2 and fails in results.qs2", {
  p <- local_project()
  rows <- function(data, argset) return(data.frame(y = data$d$y))
  run <- split_run(p, extra = rows)
  out <- utils::capture.output(expect_error(
    save_results(run$r, run$plan, run$scenarios),
    "failures of check_results()"
  ))
  expect_true(any(grepl("real: no_row_per_person", out, fixed = TRUE)))
  expect_false(dir.exists(file.path(p$home, "v01_results")))

  split_save(run, gitexclude = "rows")
  x <- read_results("v01", home = p$home)
  expect_identical(nrow(x$rows), 10L)
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
})

test_that("results_contract fails a results_gitexclude.qs2 with other kinds than the manifest names", {
  p <- local_project()
  run <- split_run(p)
  split_save(run, gitexclude = "models")
  file <- file.path(p$home, "v01_results", "known", "results_gitexclude.qs2")
  qs2::qs_save(list(fits = 1), file)
  found <- project_findings(p$home)
  bad <- found[found$rule == "results_contract", ]
  expect_identical(bad$status, "FAIL")
  expect_identical(bad$path, "v01_results/known/results.qs2")
  expect_match(
    bad$detail,
    "results_gitexclude.qs2 holds the elements fits, and the manifest of results.qs2 names models",
    fixed = TRUE
  )
  expect_error(
    read_results("v01", "known", home = p$home),
    "holds the elements fits",
    fixed = TRUE
  )
})

test_that("save_results deletes an old results_gitexclude.qs2 when the new run has no gitexclude kinds, and says so", {
  p <- local_project()
  run <- split_run(p)
  split_save(run, gitexclude = "models")
  out <- split_save(run)
  folder <- file.path(p$home, "v01_results")
  expect_identical(results_files(p), setdiff(expected_tree, "real/output.txt"))
  deleted <- grep("^pbn: deleted ", out, value = TRUE)
  expect_identical(
    sort(deleted),
    sort(paste0(
      "pbn: deleted ",
      file.path(folder, grep("^[a-z]+/results_gitexclude", private_tree, value = TRUE)),
      ", because this run puts no analysis kind in it."
    ))
  )
  # The files of each simulation, with those of gitexclude, go with the
  # folder sims_gitexclude/ of the earlier run.
  expect_identical(
    grep("^pbn: emptied ", out, value = TRUE),
    paste0(
      "pbn: emptied ",
      file.path(folder, "null", "sims_gitexclude"),
      "/ before it wrote the scenario \"null\": it held 6 files of an ",
      "earlier run."
    )
  )
  x <- read_results("v01", "null", home = p$home)
  expect_identical(names(x), c("manifest", "table1", "models", "models_crude"))

  # A file that the manifest does not name stops read_results().
  qs2::qs_save(
    list(models = 1),
    file.path(folder, "null", "results_gitexclude.qs2")
  )
  expect_error(
    read_results("v01", "null", home = p$home),
    "the manifest of results.qs2 names no analysis kind in results_gitexclude.qs2",
    fixed = TRUE
  )
})

test_that("in a git repository, save_results with gitexclude stops until git ignores results_gitexclude.qs2", {
  p <- local_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*.log")
  run <- split_run(p)
  expect_error(
    split_save(run, gitexclude = "models"),
    "git does not ignore v01_results/real/results_gitexclude.qs2",
    fixed = TRUE
  )
  expect_false(dir.exists(file.path(p$home, "v01_results")))
  write_lines(p$home, ".gitignore", "*gitexclude*")
  split_save(run, gitexclude = "models")
  expect_true(file.exists(file.path(
    p$home,
    "v01_results",
    "real",
    "results_gitexclude.qs2"
  )))
})

test_that("gitexclude_not_tracked fails a tracked path with gitexclude in its name, in a real git repository", {
  p <- local_project()
  rule <- function() {
    found <- project_findings(p$home)
    return(found[
      found$rule == "gitexclude_not_tracked",
      c("status", "path", "detail")
    ])
  }
  expect_identical(
    rule(),
    data.frame(
      status = "PASS",
      path = "",
      detail = "the project is not a git repository, so nothing to check"
    ),
    ignore_attr = TRUE
  )
  local({
    local_mocked_bindings(git_binary = function() return(""))
    expect_identical(
      rule()$detail,
      "git is not installed, so nothing to check"
    )
  })

  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*gitexclude*")
  write_lines(p$home, "v01_cache_gitexclude/d/real/sim_0001.qs2", "x")
  write_lines(p$home, "adhoc/notes_gitexclude.txt", "x")
  git_in(p$home, "add", "README.md")
  expect_identical(rule()$status, "PASS")
  expect_identical(
    rule()$detail,
    "git tracks no path with gitexclude in its name"
  )

  git_in(p$home, "add", "-f", "v01_cache_gitexclude/d/real/sim_0001.qs2")
  git_in(p$home, "add", "-f", "adhoc/notes_gitexclude.txt")
  found <- rule()
  expect_identical(found$status, c("FAIL", "FAIL"))
  expect_identical(
    found$path,
    c("adhoc/notes_gitexclude.txt", "v01_cache_gitexclude/d/real/sim_0001.qs2")
  )
  expect_match(found$detail[1], "git tracks it", fixed = TRUE)
})

test_that("every kind in gitexclude and no cohort: results.qs2 holds the manifest only and passes", {
  p <- local_project()
  run <- split_run(p)
  split_save(run, gitexclude = c("table1", "models", "models_crude"))
  folder <- file.path(p$home, "v01_results")
  expect_identical(names(qs2::qs_read(file.path(folder, "real", "results.qs2"))), "manifest")
  expect_identical(names(qs2::qs_read(file.path(folder, "null", "results.qs2"))), "manifest")
  expect_identical(
    names(read_results("v01", "null", home = p$home)),
    c("manifest", "table1", "models", "models_crude")
  )
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "results_contract"], "PASS")
})

test_that("check_gitignored checks 1000 paths in one call of git, and names the first 3 that git does not ignore", {
  p <- local_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*gitexclude*")
  rel <- paste0("v01_results/null/sims_gitexclude/sim_", sprintf("%04d", 1:1000), ".qs2")
  calls <- 0L
  real_ignored <- git_ignored
  local_mocked_bindings(git_ignored = function(git, home, rel) {
    calls <<- calls + 1L
    return(real_ignored(git, home, rel))
  })
  expect_null(check_gitignored(p$home, rel, "done."))
  expect_identical(calls, 1L)
  open <- paste0("v01_results/null/open_", 1:5, ".qs2")
  rel <- c(rel[1:400], open[1], rel[401:900], open[2:5], rel[901:1000])
  expect_error(
    check_gitignored(p$home, rel, "done."),
    paste0(
      "git does not ignore v01_results/null/open_1.qs2, ",
      "v01_results/null/open_2.qs2, v01_results/null/open_3.qs2 and 2 more. ",
      "These files hold data for each person."
    ),
    fixed = TRUE
  )
  expect_error(
    check_gitignored(p$home, open[1], "done."),
    "git does not ignore v01_results/null/open_1.qs2. That file holds",
    fixed = TRUE
  )
})

test_that("git_ignored gives git each path on a line that ends in LF, also on Windows, so that a CR never becomes part of a path", {
  file <- withr::local_tempfile()
  write_lf_lines(c("a/b.qs2", "c_gitexclude/\u00e6.qs2"), file)
  expect_identical(
    readBin(file, "raw", 100),
    charToRaw(enc2utf8("a/b.qs2\nc_gitexclude/\u00e6.qs2\n"))
  )

  # git reads "sim_0001.qs2\r" as a path that *.qs2 does not match.
  p <- local_project()
  git_in(p$home, "init", "-q")
  write_lines(p$home, ".gitignore", "*.qs2")
  rel <- c(
    "v01_results/null/sims_gitexclude/sim_0001.qs2",
    "v01_results/real/results_gitexclude.qs2"
  )
  expect_identical(git_ignored(git_binary(), p$home, rel), rel)
})
