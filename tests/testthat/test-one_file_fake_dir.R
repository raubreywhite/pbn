# In a one-file project, the scenario "real" of this computer is in
# vNN_results/fake/. The folder real/ is for the real results that come back
# from the server, and no function of pbn creates it.

# Makes the project `p` of local_project() a one-file project: the call
# pbn::initialize_project() of its run script gives the paths of the server
# of init_server(). The last line of the run script writes into real/, so
# it goes.
make_one_file <- function(p) {
  file <- file.path(p$home, "v01_run.R")
  lines <- readLines(file)
  i <- init_line(lines)
  lines[i] <- sub(
    "\\)\\)$",
    "), one_file_home = \"N:/x\", one_file_folders = c(main = \"N:/d\"))",
    lines[i]
  )
  writeLines(lines[-length(lines)], file)
  return(invisible(p))
}

# Runs and saves every scenario of the one-file project `p` in this R
# session, with the paths of the server of its run script.
save_one_file_run <- function(p, env = parent.frame()) {
  add_one_file_files(p)
  make_one_file(p)
  return(save_run(
    p,
    env = env,
    one_file_home = "N:/x",
    one_file_folders = c(main = "N:/d")
  ))
}

test_that("a one-file project run with save_results() writes fake/results.qs2 and fake/blocks.qs2, and no real/", {
  p <- local_project()
  res <- save_one_file_run(p)
  expect_false(inherits(res$err, "error"))
  results <- file.path(p$home, "v01_results")
  expect_true(file.exists(file.path(results, "fake", "results.qs2")))
  expect_true(file.exists(file.path(results, "fake", "blocks.qs2")))
  expect_true(file.exists(file.path(results, "fake", "check.csv")))
  expect_false(dir.exists(file.path(results, "real")))
  # The fake scenarios keep their own folders.
  expect_true(file.exists(file.path(results, "known", "results.qs2")))
  expect_true(file.exists(file.path(results, "null", "results.qs2")))
})

test_that("read_results(v, \"real\") of a one-file project reads fake/results.qs2", {
  p <- local_project()
  save_one_file_run(p)
  file <- file.path(p$home, "v01_results", "fake", "results.qs2")
  expect_true(file.exists(file))
  # The expected value comes from fake/ only when that file is there, so a
  # read of another folder fails an assertion and stops nothing.
  expected <- if (file.exists(file)) {
    merge_gitexclude(qs2::qs_read(file), file)
  }
  expect_identical(read_results("v01", "real", home = p$home), expected)
  expect_identical(read_results("v01", home = p$home), expected)
  expect_identical(
    read_results("v01", "real", sim = 1, home = p$home),
    expected
  )
})

test_that("the manifest of fake/ of a one-file project has fake_real_data TRUE", {
  p <- local_project()
  save_one_file_run(p)
  file <- file.path(p$home, "v01_results", "fake", "results.qs2")
  expect_true(file.exists(file))
  manifest <- if (file.exists(file)) qs2::qs_read(file)$manifest else list()
  expect_identical(manifest$scenario, "real")
  expect_identical(manifest$fake_real_data, TRUE)
  # The run log labels the scenario real as fake real data.
  md <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_true(any(grepl("real (fake real data)", md, fixed = TRUE)))
  expect_true(any(grepl("v01_results/fake/results.qs2", md, fixed = TRUE)))
  expect_false(any(grepl("v01_results/real/", md, fixed = TRUE)))
})

test_that("a one-file project run by its run script passes check_one_file() and verify(), and neither creates real/", {
  skip_if_pbn_not_installed()
  p <- local_project()
  add_one_file_files(p)
  # Zeta.R tests the order of org, and its capital letter is a FAIL of
  # lower_case_names that has nothing to do with the folders of the results.
  unlink(file.path(p$home, "v01_functions", "Zeta.R"))
  make_one_file(p)
  real <- file.path(p$home, "v01_results", "real")
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  expect_true(file.exists(file.path(
    p$home,
    "v01_results",
    "fake",
    "results.qs2"
  )))
  expect_false(dir.exists(real))
  init_server(p)
  utils::capture.output(to_one_file())
  out <- utils::capture.output(ok <- check_one_file())
  expect_true(ok)
  expect_match(
    out,
    "^PASS: .*Each page of the 2 blocks prints the same for both\\.$"
  )
  expect_false(dir.exists(real))
  out <- utils::capture.output(v <- verify_result("v01", p$home))
  expect_identical(v$status, 0L)
  expect_false(any(v$findings$status == "FAIL"))
  expect_identical(attr(v, "verdict"), "PASS")
  # The verdict sees the real results of the run in fake/.
  expect_identical(utils::tail(attr(v, "lines"), 1), "Verdict: PASS")
  expect_false(dir.exists(real))
})

test_that("make_outputs() of a one-file project writes the output of real under fake/ and creates no real/", {
  p <- local_project()
  res <- save_one_file_run(p)
  fn <- function(results, dir) {
    writeLines(results$manifest$scenario, file.path(dir, "note.txt"))
    return(invisible(NULL))
  }
  utils::capture.output(out <- make_outputs(fn, res$scenarios))
  results <- file.path(p$home, "v01_results")
  expect_true(file.exists(file.path(
    results,
    "fake",
    "output_fake_data",
    "fake_data_note.txt"
  )))
  expect_identical(
    out$file,
    "v01_results/fake/output_fake_data/fake_data_note.txt"
  )
  expect_false(dir.exists(file.path(results, "real")))
})

test_that("update_blocks() of a one-file project writes fake/blocks.qs2 for real and creates no real/", {
  p <- local_project()
  save_one_file_run(p)
  results <- file.path(p$home, "v01_results")
  unlink(file.path(results, c("fake", "known", "null"), "blocks.qs2"))
  out <- utils::capture.output(update_blocks())
  expect_identical(out, "pbn: wrote blocks.qs2 for known, null, real.")
  expect_true(file.exists(file.path(results, "fake", "blocks.qs2")))
  expect_false(dir.exists(file.path(results, "real")))
})

test_that("initialize_project() with one_file_home and fake_real_data = TRUE stops, and fake_real_data alone stays valid", {
  p <- local_project()
  expect_error(
    init_server(p, fake_real_data = TRUE),
    paste0(
      "A one-file project takes no fake_real_data = TRUE: one_file_home ",
      "already says that the data on this computer are fake. Drop ",
      "fake_real_data from the call."
    ),
    fixed = TRUE
  )
  expect_null(state$results)
  init(p, fake_real_data = TRUE)
  expect_true(state$fake_real_data)
})

test_that("a project that is not one-file writes real/ as before, also with the paths of the server in this R session only", {
  p <- local_project()
  res <- save_run(p, only = c("real", "known"))
  expect_false(inherits(res$err, "error"))
  results <- file.path(p$home, "v01_results")
  expect_true(file.exists(file.path(results, "real", "results.qs2")))
  expect_false(dir.exists(file.path(results, "fake")))
  file <- file.path(results, "real", "results.qs2")
  expect_identical(
    read_results("v01", home = p$home),
    merge_gitexclude(qs2::qs_read(file), file)
  )
  expect_false(qs2::qs_read(file)$manifest$fake_real_data)
  # The run script decides, and it gives no one_file_home.
  unlink(results, recursive = TRUE)
  res <- save_run(
    p,
    only = c("real", "known"),
    one_file_home = "N:/x",
    one_file_folders = c(main = "N:/d")
  )
  expect_false(inherits(res$err, "error"))
  expect_true(file.exists(file.path(results, "real", "results.qs2")))
  expect_false(dir.exists(file.path(results, "fake")))
})

test_that("a scenario named fake errors", {
  cfg <- list(
    scenarios = data.frame(scenario = c("real", "fake"), sims = c(1, 1))
  )
  expect_error(
    scenarios(cfg),
    paste0(
      "CFG$scenarios: the name fake is reserved. In a one-file project, the ",
      "folder fake/ holds the scenario real of this computer. Give the ",
      "scenario another name."
    ),
    fixed = TRUE
  )
})
