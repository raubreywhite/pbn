# Deletions that MUST happen, and file names that differ only in case.

test_that("delete_paths stops and names each file that is still there", {
  root <- withr::local_tempdir()
  a <- file.path(root, "a.qs2")
  writeLines("x", a)
  delete_paths(a)
  expect_false(file.exists(a))
  writeLines("x", a)
  local_mocked_bindings(remove_paths = function(paths, recursive) {
    return(invisible(NULL))
  })
  expect_error(
    delete_paths(c(a, file.path(root, "gone.qs2"))),
    paste0(
      "pbn could not delete ",
      a,
      ". Another program may hold it open, for example a viewer or an ",
      "editor. Close that program, and run the call again."
    ),
    fixed = TRUE
  )
})

# unlink() that deletes nothing, as on Windows for a file that another
# program holds open.
local_stuck_files <- function(env = parent.frame()) {
  local_mocked_bindings(
    remove_paths = function(paths, recursive) return(invisible(NULL)),
    .env = env
  )
  return(invisible(NULL))
}

test_that("save_results stops when it cannot empty sims_gitexclude/ of an earlier run", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  local_stuck_files()
  utils::capture.output(expect_error(
    save_results(r, plan, sc),
    paste0(
      "pbn could not delete ",
      file.path(p$home, "v01_results", "null", "sims_gitexclude"),
      ". Another program may hold it open"
    ),
    fixed = TRUE
  ))
})

test_that("save_results stops when it cannot delete an old results_gitexclude.qs2", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG, only = "real")
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc, gitexclude = "models"))
  local_stuck_files()
  utils::capture.output(expect_error(
    save_results(r, plan, sc),
    paste0(
      "pbn could not delete ",
      file.path(p$home, "v01_results", "real", "results_gitexclude.qs2"),
      ". Another program may hold it open"
    ),
    fixed = TRUE
  ))
})

test_that("make_outputs stops when it cannot empty the output folder of an earlier run", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  fn <- function(results, dir) {
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  utils::capture.output(make_outputs(fn, sc))
  local_stuck_files()
  utils::capture.output(expect_error(
    make_outputs(fn, sc),
    paste0(
      "pbn could not delete ",
      file.path(p$home, "v01_results", "real", "output"),
      ". Another program may hold it open"
    ),
    fixed = TRUE
  ))
})

test_that("make_outputs finds a clash of names that differ only in case, as on Windows and macOS", {
  p <- local_project()
  cfg <- cfg_lines()
  cfg[cfg == "    sims = c(1, 1, 3),"] <- "    sims = c(1, 1, 3), output = c(FALSE, TRUE, FALSE),"
  write_lines(p$home, "v01_functions/_cfg.R", cfg)
  sourced <- init(p)
  sc <- scenarios(sourced$CFG, only = "known")
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  clash <- function(results, dir) {
    writeLines("x", file.path(dir, "Figure.png"))
    writeLines("y", file.path(dir, "fake_data_figure.png"))
    return(invisible(NULL))
  }
  utils::capture.output(expect_error(
    make_outputs(clash, sc),
    "with and without the prefix fake_data_: fake_data_Figure.png",
    fixed = TRUE
  ))
  # The file that the rename would have replaced is as the function wrote it.
  expect_identical(
    readLines(file.path(p$home, "v01_results", "known", "output_fake_data", "fake_data_figure.png")),
    "y"
  )
})
