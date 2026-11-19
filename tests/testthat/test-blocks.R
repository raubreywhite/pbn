# show_blocks() and show_block(): the numbers of the manuscript on the
# screen.

# The project `p` with blocks in its functions folder, run and saved in this
# R session. `extra` is more lines of the list blocks.
blocks_project <- function(p, extra = character(0), env = parent.frame()) {
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Table 1\" = function(results) results$table1,",
      "  \"Long\" = function(results) data.frame(i = 1:80, sq = (1:80)^2),",
      "  \"Wide\" = function(results) data.table::data.table(i = 1:150, label = strrep(\"x\", 200)),",
      "  \"Results para 1: lines\" = function(results) c(\"first line\", \"second line\"),",
      "  \"Fails\" = function(results) stop(\"no such table\"),",
      "  \"Not a table\" = function(results) 1",
      ")"
    )
  )
  save_run(p, only = c("real", "known"), env = env)
  return(invisible(p))
}

test_that("show_blocks prints the index with the rows and pages of each block, and the error of a block in the place of its rows", {
  p <- local_project()
  blocks_project(p)
  out <- utils::capture.output(index <- show_blocks())
  expect_identical(
    out,
    c(
      paste0(
        "pbn: 6 blocks of v01, from the real results of ",
        format(Sys.Date(), "%Y-%m-%d"),
        ":"
      ),
      "  1  Table 1                6 rows, 1 page",
      "  2  Long                   80 rows, 3 pages",
      "  3  Wide                   150 rows, 5 pages",
      "  4  Results para 1: lines  2 rows, 1 page",
      "  5  Fails                  error: no such table",
      paste0(
        "  6  Not a table            error: it returned an object of the class ",
        "numeric, and a block MUST return a data.frame or a character vector"
      ),
      "Type pbn::show_block(k) for block k, and pbn::show_block(k, page = 2) for its page 2."
    )
  )
  expect_identical(index$rows, c(6L, 80L, 150L, 2L, NA, NA))
  expect_identical(index$pages, c(1L, 3L, 5L, 1L, NA, NA))
})

test_that("show_block prints one page with its header, by number or by name, and stops on a failed block with its name", {
  p <- local_project()
  blocks_project(p)
  out <- utils::capture.output(value <- show_block(2, page = 2))
  expect_identical(
    out[1:3],
    c("[2] Long", "page 2 of 3, rows 36 to 70 of 80", "")
  )
  expect_identical(trimws(out[4]), "i   sq")
  expect_identical(trimws(out[5:39]), paste(36:70, (36:70)^2))
  expect_length(out, 39)
  expect_identical(nrow(value), 80L)
  out <- utils::capture.output(show_block("Results para 1: lines"))
  expect_identical(
    out,
    c(
      "[4] Results para 1: lines",
      "page 1 of 1, rows 1 to 2 of 2",
      "",
      "first line",
      "second line"
    )
  )
  out <- utils::capture.output(show_block(2, page = 1, rows = 100))
  expect_identical(out[2], "page 1 of 1, rows 1 to 80 of 80")
  expect_error(
    show_block(5),
    "The block 5, \"Fails\", failed: no such table",
    fixed = TRUE
  )
  expect_error(
    show_block(2, page = 4),
    "The block 2 has 3 pages, so it has no page 4.",
    fixed = TRUE
  )
  expect_error(
    show_block(7),
    "There is no block 7. The project has 6 blocks",
    fixed = TRUE
  )
  expect_error(
    show_block("Table 9"),
    "There is no block \"Table 9\".",
    fixed = TRUE
  )
})

test_that("show_block prints a data.table page whole, without row names and with the width 270, and sets the options back", {
  p <- local_project()
  blocks_project(p)
  old_width <- getOption("width")
  out <- utils::capture.output(show_block(3, rows = 150))
  expect_identical(getOption("width"), old_width)
  body <- out[-(1:3)]
  # A header line and 150 rows, each row with its 200 x, cut by nothing.
  expect_length(body, 151)
  expect_identical(trimws(body[2]), paste(1, strrep("x", 200)))
  expect_identical(trimws(body[151]), paste(150, strrep("x", 200)))
  expect_false(any(grepl("^ *[0-9]+:", body)))
  expect_lte(max(nchar(body)), 270)
})

test_that("show_blocks needs initialize_project(), the blocks of the functions folder, and real results that pass their checks", {
  p <- local_project()
  local_clean_state()
  expect_error(
    show_blocks(),
    "Call pbn::initialize_project() before pbn::show_blocks() or pbn::show_block().",
    fixed = TRUE
  )
  save_run(p, only = c("real", "known"))
  expect_error(
    show_blocks(),
    "v01_functions/ defines no object blocks. ?pbn::show_block says how to declare it.",
    fixed = TRUE
  )
  blocks_project(p)
  expect_error(show_blocks(version = "v02"), "not of v02", fixed = TRUE)
  write_lines(
    p$home,
    "v01_results/real/check.csv",
    c("\"assertion\",\"expected\",\"observed\",\"pass\"", "\"rows\",1,2,FALSE")
  )
  expect_error(
    show_block(1),
    conditionMessage(tryCatch(
      read_results("v01", home = p$home),
      error = function(e) return(e)
    )),
    fixed = TRUE
  )
  # A failed fake scenario that the real manifest records stops them too.
  unlink(file.path(p$home, "v01_results", "real", "check.csv"))
  file <- file.path(p$home, "v01_results", "real", "results.qs2")
  x <- qs2::qs_read(file)
  x$manifest$failed_fake_checks <- "known"
  qs2::qs_save(x, file)
  expect_error(
    show_blocks(),
    "names a fake scenario with a failed assertion: \"known\".",
    fixed = TRUE
  )
})

test_that("show_block and show_blocks set back each option that a block function sets or adds", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Sets options\" = function(results) {",
      "    options(digits = 3, pbn.test.block = TRUE)",
      "    results$table1",
      "  },",
      "  \"Stops\" = function(results) {",
      "    options(pbn.test.stop = 1)",
      "    stop(\"no table\")",
      "  }",
      ")"
    )
  )
  save_run(p, only = c("real", "known"))
  before <- options()
  utils::capture.output(show_block(1))
  expect_identical(options(), before)
  utils::capture.output(show_blocks())
  expect_identical(options(), before)
  expect_error(utils::capture.output(show_block(2)), "no table", fixed = TRUE)
  expect_identical(options(), before)
  expect_null(getOption("pbn.test.block"))
  expect_null(getOption("pbn.test.stop"))
})

test_that("a block that calls a function that R does not find stops with the hint to name its package", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(\"Table\" = function(results) no_such_function_pbn(results$table1))"
    )
  )
  save_run(p, only = c("real", "known"))
  expect_error(
    utils::capture.output(show_block(1)),
    paste0(
      "The block 1, \"Table\", failed: could not find function ",
      "\"no_such_function_pbn\". A block function MUST name the package of ",
      "each function that it calls, for example data.table::data.table(), ",
      "because pbn::show_block() does not attach the packages of the run ",
      "script"
    ),
    fixed = TRUE
  )
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(\"Table\" = function(results) stop(\"no table\"))"
    )
  )
  init(p)
  expect_error(
    utils::capture.output(show_block(1)),
    "The block 1, \"Table\", failed: no table",
    fixed = TRUE
  )
})
