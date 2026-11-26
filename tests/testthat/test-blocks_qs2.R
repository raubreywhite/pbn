# save_results() calls each block on simulation 1 of each scenario and
# writes the values to <version>_results/<scenario>/blocks.qs2.

# Writes the blocks `lines`, the lines inside list(), to blocks.R of `p`.
write_blocks <- function(p, lines) {
  return(write_lines(
    p$home,
    "v01_functions/blocks.R",
    c("blocks <- list(", lines, ")")
  ))
}

# The files blocks.qs2 under v01_results/ of `p`.
blocks_files <- function(p) {
  return(sort(list.files(
    file.path(p$home, "v01_results"),
    pattern = "^blocks[.]qs2$",
    recursive = TRUE
  )))
}

good_blocks <- c(
  "  \"Table 1\" = function(results) results$table1,",
  "  \"Rows of models\" = function(results) data.frame(rows = nrow(results$models)),",
  "  \"Results para 1: lines\" = function(results) c(\"first line\", results$manifest$scenario)"
)

test_that("a good block writes blocks.qs2 in every scenario, and its value equals the output of the block", {
  p <- local_project()
  withr::defer(options(pbn_blocks_qs2_probe = NULL))
  write_blocks(
    p,
    c(
      good_blocks[1:2],
      paste0(good_blocks[3], ","),
      paste0(
        "  \"Sets an option\" = function(results) {",
        "options(pbn_blocks_qs2_probe = TRUE); \"set\"}"
      )
    )
  )
  run <- save_run(p)
  expect_false(inherits(run$err, "error"))
  # save_results() sets back the options that a block function sets.
  expect_null(getOption("pbn_blocks_qs2_probe"))
  expect_identical(
    blocks_files(p),
    c("known/blocks.qs2", "null/blocks.qs2", "real/blocks.qs2")
  )
  blocks <- state$env$blocks
  for (s in c("real", "known", "null")) {
    saved <- qs2::qs_read(file.path(p$home, "v01_results", s, "blocks.qs2"))
    results <- read_results("v01", s, sim = 1, home = p$home)
    expect_identical(names(saved), names(blocks))
    expect_equal(saved, lapply(blocks, function(f) f(results)), info = s)
    expect_identical(saved[["Results para 1: lines"]], c("first line", s))
    expect_s3_class(saved[["Table 1"]], "data.frame")
    expect_identical(nrow(saved[["Table 1"]]), 6L)
  }
})

test_that("a block that errors makes save_results() stop, the message names the block and the scenario, and no blocks.qs2 stays", {
  p <- local_project()
  write_blocks(p, good_blocks)
  expect_false(inherits(save_run(p)$err, "error"))
  expect_length(blocks_files(p), 3)
  # The block fails on the fake scenario null only.
  write_blocks(
    p,
    c(
      "  \"Table 1\" = function(results) results$table1,",
      paste0(
        "  \"Broken\" = function(results) if (results$manifest$scenario == ",
        "\"null\") stop(\"no such table\") else \"fine\""
      )
    )
  )
  run <- save_run(p)
  expect_s3_class(run$err, "error")
  expect_identical(
    conditionMessage(run$err),
    "block 2 \"Broken\" failed on scenario null: no such table"
  )
  # The results are saved and checked before the blocks run. The values of
  # the blocks of the earlier run are gone.
  expect_true(file.exists(file.path(
    p$home,
    "v01_results",
    "null",
    "results.qs2"
  )))
  expect_identical(blocks_files(p), character(0))

  # A value that is not a data.frame or a character vector fails too.
  write_blocks(p, "  \"Not a table\" = function(results) 1")
  run <- save_run(p)
  expect_identical(
    conditionMessage(run$err),
    paste0(
      "block 1 \"Not a table\" failed on scenario real: it returned an object ",
      "of the class numeric, and a block MUST return a data.frame or a ",
      "character vector"
    )
  )
  expect_identical(blocks_files(p), character(0))

  # check_blocks() stops on blocks without names.
  write_blocks(p, "  function(results) \"no name\"")
  run <- save_run(p)
  expect_s3_class(run$err, "error")
  expect_identical(
    if (inherits(run$err, "error")) conditionMessage(run$err),
    paste0(
      "blocks MUST be a list of functions of results, each with a name of ",
      "its own. pbn::check_project() names the fault in its rule blocks_list."
    )
  )
  expect_identical(blocks_files(p), character(0))
})

test_that("a version with no blocks passes, and writes no blocks.qs2", {
  p <- local_project()
  expect_false(file.exists(file.path(p$home, "v01_functions", "blocks.R")))
  run <- save_run(p)
  expect_false(inherits(run$err, "error"))
  expect_null(get0("blocks", envir = state$env, inherits = FALSE))
  expect_identical(blocks_files(p), character(0))
  expect_true(file.exists(file.path(
    p$home,
    "v01_results",
    "real",
    "results.qs2"
  )))
})

test_that("on a scenario with 2 sims, the saved value is the block on sim 1, and not on the stacked results", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/_cfg.R",
    sub("sims = c(1, 1, 3)", "sims = c(1, 1, 2)", cfg_lines(), fixed = TRUE)
  )
  write_lines(
    p$home,
    "v01_functions/expect_null.R",
    sub(
      "n_sims == 3",
      "n_sims == 2",
      sub("c(\"3\",", "c(\"2\",", expect_null_lines(), fixed = TRUE),
      fixed = TRUE
    )
  )
  write_blocks(p, good_blocks)
  run <- save_run(p, only = "null")
  expect_false(inherits(run$err, "error"))
  expect_identical(run$scenarios$sims, 2L)
  saved <- qs2::qs_read(file.path(p$home, "v01_results", "null", "blocks.qs2"))
  block <- state$env$blocks[["Rows of models"]]
  sim1 <- block(read_results("v01", "null", sim = 1, home = p$home))
  stacked <- block(read_results("v01", "null", home = p$home))
  expect_equal(saved[["Rows of models"]], sim1)
  expect_false(isTRUE(all.equal(saved[["Rows of models"]], stacked)))
  # The stacked models hold the rows of both simulations.
  expect_identical(2L * saved[["Rows of models"]]$rows, stacked$rows)
})

test_that("a run script with a block that errors exits with an error that names the block, and verify() gives FAIL", {
  skip_if_pbn_not_installed()
  p <- local_project()
  write_blocks(
    p,
    c(
      "  \"Table 1\" = function(results) results$table1,",
      "  \"Broken\" = function(results) stop(\"no such table\")"
    )
  )
  res <- run_script(p$home)
  expect_false(identical(res$status, 0L))
  expect_match(
    res$stdout,
    "block 2 \"Broken\" failed on scenario real: no such table",
    fixed = TRUE
  )
  out <- utils::capture.output(v <- verify_result("v01", p$home))
  expect_false(identical(v$status, 0L))
  expect_identical(attr(v, "verdict"), "FAIL")
})
