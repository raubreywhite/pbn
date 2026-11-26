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
  today <- format(Sys.Date(), "%Y-%m-%d")
  expect_identical(
    out[1:4],
    c(
      "[2] Long",
      "page 2 of 3, rows 36 to 70 of 80",
      paste0("v01, run ", today),
      ""
    )
  )
  expect_identical(trimws(out[5]), "i   sq")
  expect_identical(trimws(out[6:40]), paste(36:70, (36:70)^2))
  expect_length(out, 40)
  expect_identical(nrow(value), 80L)
  out <- utils::capture.output(show_block("Results para 1: lines"))
  expect_identical(
    out,
    c(
      "[4] Results para 1: lines",
      "page 1 of 1, rows 1 to 2 of 2",
      paste0("v01, run ", today),
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
  body <- out[-(1:4)]
  # A header line and 150 rows, each row with its 200 x, cut by nothing.
  expect_length(body, 151)
  expect_identical(trimws(body[2]), paste(1, strrep("x", 200)))
  expect_identical(trimws(body[151]), paste(150, strrep("x", 200)))
  expect_false(any(grepl("^ *[0-9]+:", body)))
  expect_lte(max(nchar(body)), 270)
  # A page with no line wider than 270 ends with its last row, and no warning.
  expect_identical(trimws(utils::tail(out, 1)), paste(150, strrep("x", 200)))
  expect_false(any(grepl("wider than 270", out, fixed = TRUE)))
})

test_that("each page of show_block names the version and the run date under the page line, and an unknown date when the manifest has none", {
  p <- local_project()
  blocks_project(p)
  line <- paste0("v01, run ", format(Sys.Date(), "%Y-%m-%d"))
  # A data.frame, a data.table and a character vector, on each of their pages.
  pages <- c(1, 3, 5, 1)
  for (k in 1:4) {
    for (page in seq_len(pages[k])) {
      out <- utils::capture.output(show_block(k, page = page))
      expect_identical(out[3], line, label = paste("block", k, "page", page))
    }
  }
  local({
    real <- read_results
    local_mocked_bindings(read_results = function(...) {
      x <- real(...)
      x$manifest$run_date <- NULL
      return(x)
    })
    out <- utils::capture.output(show_block(1))
    expect_identical(out[3], "v01, run an unknown date")
  })
  # check_one_file() compares the pages of two runs without the run line,
  # because two runs MAY have two run dates.
  prints <- block_prints(p$home)
  expect_identical(prints$blocks$Long$text[1:3], c("[2] Long", "page 1 of 3, rows 1 to 35 of 80", ""))
  expect_false(any(grepl("^v01, run ", unlist(prints))))
  # Real results from fake real data: the label stays second.
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  out <- utils::capture.output(show_block(4))
  expect_identical(
    out[1:4],
    c(
      "[4] Results para 1: lines",
      "FAKE REAL DATA",
      "page 1 of 1, rows 1 to 2 of 2",
      line
    )
  )
})

test_that("a page with a line wider than 270 ends with a warning that names the block and the width of the widest line", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Wide cells\" = function(results) data.frame(a = c(\"y\", strrep(\"y\", 400))),",
      "  \"Long line\" = function(results) c(\"short\", strrep(\"z\", 400)),",
      "  \"Narrow\" = function(results) data.frame(a = strrep(\"w\", 260))",
      ")"
    )
  )
  save_run(p, only = c("real", "known"))
  # R breaks a data.frame only between columns, so it cannot break a column
  # of 400 characters.
  out <- utils::capture.output(show_block(1))
  body <- out[5:(length(out) - 1)]
  widest <- max(nchar(body, type = "width"))
  expect_gt(widest, 270)
  expect_identical(
    utils::tail(out, 1),
    paste0(
      "Warning: the block 1, \"Wide cells\", has a line ",
      widest,
      " characters wide, wider than 270. The screen wraps it."
    )
  )
  # A character block: the warning comes on its own line after the last line.
  out <- utils::capture.output(show_block(2))
  expect_identical(
    out[5:7],
    c(
      "short",
      strrep("z", 400),
      paste0(
        "Warning: the block 2, \"Long line\", has a line 400 characters ",
        "wide, wider than 270. The screen wraps it."
      )
    )
  )
  expect_length(out, 7)
  # A line of up to 270 characters gets no warning.
  out <- utils::capture.output(show_block(3))
  expect_lte(max(nchar(out, type = "width")), 270)
  expect_gt(max(nchar(out, type = "width")), 250)
  expect_false(any(grepl("wider than 270", out, fixed = TRUE)))
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

test_that("a formatter that returns a data.table prints through show_block() with the same text as a plain block that returns that data.table", {
  p <- local_project()
  # A list column prints differently in a data.table and in a data.frame,
  # so the text shows the class that show_block() printed.
  table <- "data.table::data.table(i = 1:40, label = c(NA, rep(\"ab\", 39)), v = c(1.5, NA, 1:38), parts = rep(list(1:3), 40))"
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c("blocks <- list(", paste0("  \"Wide\" = function(results) ", table), ")")
  )
  save_run(p, only = c("real", "known"))
  plain <- lapply(1:2, function(page) {
    return(utils::capture.output(show_block(1, page = page)))
  })
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Wide\" = pbn::block(",
      "    function(results) data.frame(n = 1:40),",
      "    format = \"wide_dt\"",
      "  )",
      ")"
    )
  )
  write_lines(
    p$home,
    "v01_functions/formatters.R",
    paste0("wide_dt <- function(x) return(", table, ")")
  )
  init(p)
  formatted <- lapply(1:2, function(page) {
    return(utils::capture.output(show_block(1, page = page)))
  })
  expect_identical(formatted, plain)
  value <- NULL
  utils::capture.output(value <- show_block(1))
  expect_true(data.table::is.data.table(value))
})

test_that("show_block prints the formatted value of a pbn::block(), and show_blocks stops on a raw value with a formatted number and names the block, the column and the cell", {
  p <- local_project()
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Effect\" = pbn::block(",
      "    function(results) data.frame(est = c(0.8, 0.46), lo = c(0.46, -0.0712), hi = c(1.16, 1)),",
      "    format = \"ci\"",
      "  ),",
      "  \"Broken formatter\" = pbn::block(function(results) data.frame(x = 1), format = \"ci\")",
      ")"
    )
  )
  save_run(p, only = c("real", "known"))
  out <- utils::capture.output(value <- show_block(1))
  expect_identical(
    out,
    c(
      "[1] Effect",
      "page 1 of 1, rows 1 to 2 of 2",
      paste0("v01, run ", format(Sys.Date(), "%Y-%m-%d")),
      "",
      "0.80 (95% CI 0.46 to 1.16)",
      "0.46 (95% CI −0.07 to 1.00)"
    )
  )
  expect_identical(value[2], "0.46 (95% CI −0.07 to 1.00)")
  # A formatter that stops is an error in the index, and show_blocks goes on.
  out <- utils::capture.output(index <- show_blocks())
  expect_identical(index$rows, c(2L, NA))
  expect_match(
    out[3],
    "error: The formatter ci of the block \"Broken formatter\" failed: the formatter ci needs the column est",
    fixed = TRUE
  )
  # A raw value with a formatted number stops show_blocks().
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Effect\" = pbn::block(function(results) data.frame(est = 1, lo = 0, hi = 2), format = \"ci\"),",
      "  \"Labels\" = pbn::block(function(results) data.frame(group = c(\"a\", \"68.7%\"), n = 1:2), format = \"p\")",
      ")"
    )
  )
  init(p)
  expect_error(
    utils::capture.output(show_blocks()),
    paste0(
      "The block 2, \"Labels\", failed: it holds a formatted number in the ",
      "column group, row 2: \"68.7%\""
    ),
    fixed = TRUE
  )
  expect_error(
    utils::capture.output(show_block(2)),
    "column group, row 2: \"68.7%\"",
    fixed = TRUE
  )
})
