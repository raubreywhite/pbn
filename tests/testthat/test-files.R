# Deletions and folders that MUST happen, and file names that differ only in
# case.

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

test_that("create_folder creates a folder with the folders above it, and passes a folder that is there", {
  root <- withr::local_tempdir()
  dir <- file.path(root, "a", "b")
  expect_identical(create_folder(dir), dir)
  expect_true(dir.exists(dir))
  expect_identical(create_folder(dir), dir)
})

test_that("create_folder tries again when dir.create() fails, as on a network drive for a folder that was just deleted", {
  root <- withr::local_tempdir()
  dir <- file.path(root, "output")
  tries <- 0
  local_mocked_bindings(make_folder = function(dir) {
    tries <<- tries + 1
    if (tries < 3) {
      warning("cannot create dir 'output', reason 'Permission denied'")
      return(FALSE)
    }
    return(dir.create(dir, recursive = TRUE))
  })
  expect_identical(create_folder(dir, wait = 0), dir)
  expect_identical(tries, 3)
  expect_true(dir.exists(dir))
})

test_that("create_folder stops and names the folder and what R said when the folder is not there after the last try", {
  root <- withr::local_tempdir()
  dir <- file.path(root, "output")
  tries <- 0
  local_mocked_bindings(make_folder = function(dir) {
    tries <<- tries + 1
    warning("cannot create dir 'output', reason 'Permission denied'")
    return(FALSE)
  })
  expect_error(
    create_folder(dir, wait = 0),
    paste0(
      "pbn could not create the folder ",
      dir,
      " in 8 tries. R says: cannot create dir 'output', reason 'Permission ",
      "denied'. Check that the folder above it exists and that you may write ",
      "there. On a network drive, wait a moment, and run the call again."
    ),
    fixed = TRUE
  )
  expect_identical(tries, 8)
  # dir.create() that says nothing and creates nothing.
  local_mocked_bindings(make_folder = function(dir) return(FALSE))
  expect_error(
    create_folder(dir, tries = 1, wait = 0),
    paste0(
      "pbn could not create the folder ",
      dir,
      " in 1 try. R says: nothing."
    ),
    fixed = TRUE
  )
})

test_that("create_folder does not count a folder that refuses a file, as a folder pending delete on a network drive does", {
  root <- withr::local_tempdir()
  dir <- file.path(root, "output")
  looks <- 0
  local_mocked_bindings(folder_writable = function(dir) {
    looks <<- looks + 1
    return(looks >= 3)
  })
  expect_identical(create_folder(dir, wait = 0), dir)
  expect_identical(looks, 3)
  local_mocked_bindings(folder_writable = function(dir) return(FALSE))
  expect_error(
    create_folder(dir, tries = 2, wait = 0),
    "pbn could not create the folder",
    fixed = TRUE
  )
})

test_that("folder_writable writes and deletes a file in a folder, and is FALSE for a folder that is not there", {
  root <- withr::local_tempdir()
  expect_true(folder_writable(root))
  expect_identical(list.files(root, all.files = TRUE, no.. = TRUE), character(0))
  expect_false(folder_writable(file.path(root, "absent")))
})

test_that("make_outputs stops when it cannot create the output folder, and does not call the output function", {
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG, only = "real")
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  called <- FALSE
  fn <- function(results, dir) {
    called <<- TRUE
    return(invisible(NULL))
  }
  local_mocked_bindings(make_folder = function(dir) return(FALSE))
  utils::capture.output(expect_error(
    make_outputs(fn, sc),
    paste0(
      "pbn could not create the folder ",
      file.path(p$home, "v01_results", "real", "output"),
      " in 8 tries."
    ),
    fixed = TRUE
  ))
  expect_false(called)
})
