# pbn::block(), the house formatters, the project formatters of
# formatters.R, and pbn::txt().

# The project `p` under the set house_2026_11_26, with the blocks `lines`,
# the lines inside list(), in blocks.R, and the lines `formatters` in
# formatters.R when given.
write_new_blocks <- function(p, lines, formatters = NULL) {
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c("blocks <- list(", lines, ")")
  )
  if (!is.null(formatters)) {
    write_lines(p$home, "v01_functions/formatters.R", formatters)
  }
  return(invisible(p))
}

numeric_blocks <- c(
  "  \"Effect\" = pbn::block(",
  "    function(results) data.frame(est = 0.8, lo = 0.46, hi = 1.16),",
  "    format = \"ci\",",
  "    items = c(\"strobe:17\")",
  "  ),",
  "  \"Sample\" = pbn::block(",
  "    function(results) data.frame(group = c(\"all\", \"cases\"), n = c(1037, 12), total = 1509),",
  "    format = \"n_pct\"",
  "  )"
)

test_that("ci formats row 1 of the formatter table", {
  x <- data.frame(est = 0.8, lo = 0.46, hi = 1.16)
  expect_identical(find_formatter("ci")(x), "0.80 (95% CI 0.46 to 1.16)")
})

test_that("ci formats row 2 of the formatter table, with the minus sign U+2212", {
  x <- data.frame(est = 0.46, lo = -0.0712, hi = 1.0)
  expect_identical(
    find_formatter("ci")(x),
    "0.46 (95% CI −0.07 to 1.00)"
  )
})

test_that("p formats row 3 of the formatter table, p < 0.001", {
  expect_identical(find_formatter("p")(data.frame(p = 0.0002)), "p < 0.001")
})

test_that("p formats row 4 of the formatter table, 0.0499 as p = 0.050", {
  expect_identical(find_formatter("p")(data.frame(p = 0.0499)), "p = 0.050")
})

test_that("p formats row 5 of the formatter table, 0.0474 as p = 0.047", {
  expect_identical(find_formatter("p")(data.frame(p = 0.0474)), "p = 0.047")
})

test_that("effect formats row 6 of the formatter table, with the unit", {
  x <- data.frame(est = 0.8, lo = 0.46, hi = 1.16, p = 0.0002, unit = "points")
  expect_identical(
    find_formatter("effect")(x),
    "(0.80 points, 95% CI 0.46 to 1.16, p < 0.001)"
  )
})

test_that("effect formats row 7 of the formatter table, without a unit column", {
  x <- data.frame(est = 0.8, lo = 0.46, hi = 1.16, p = 0.0002)
  expect_identical(
    find_formatter("effect")(x),
    "(0.80, 95% CI 0.46 to 1.16, p < 0.001)"
  )
})

test_that("n_pct formats row 8 of the formatter table, 1,037 (68.7%)", {
  x <- data.frame(n = 1037, total = 1509)
  expect_identical(find_formatter("n_pct")(x), "1,037 (68.7%)")
})

test_that("mean_sd formats row 9 of the formatter table, 30.7 (4.8)", {
  x <- data.frame(mean = 30.66, sd = 4.81)
  expect_identical(find_formatter("mean_sd")(x), "30.7 (4.8)")
})

test_that("the house formatters take a data.table, the column digits, and give NA for a row with NA, one string per row", {
  x <- data.table::data.table(
    est = c(0.8, NA, -0.0004),
    lo = c(0.46, 0.1, -0.01),
    hi = c(1.16, 0.2, 0.01),
    digits = c(1, 2, 2)
  )
  expect_identical(
    find_formatter("ci")(x),
    c("0.8 (95% CI 0.5 to 1.2)", NA, "0.00 (95% CI −0.01 to 0.01)")
  )
  expect_identical(
    find_formatter("mean_sd")(data.frame(mean = 30.66, sd = 4.81, digits = 2)),
    "30.66 (4.81)"
  )
  expect_identical(
    find_formatter("effect")(data.frame(
      est = 1,
      lo = 0,
      hi = 2,
      p = 0.5,
      unit = NA_character_
    )),
    "(1.00, 95% CI 0.00 to 2.00, p = 0.500)"
  )
  expect_error(
    find_formatter("ci")(data.frame(est = 1, lo = 0)),
    "the formatter ci needs the column hi, and the value has the columns est, lo",
    fixed = TRUE
  )
  expect_error(
    find_formatter("p")(data.frame(p = "0.04")),
    "the column p of the formatter p MUST be numeric",
    fixed = TRUE
  )
  expect_error(
    find_formatter("ci")(data.frame(est = 1, lo = 0, hi = 2, digits = 1.5)),
    "the column digits of the formatter ci MUST hold whole numbers of 0 or more",
    fixed = TRUE
  )
})

test_that("block() returns a pbn_block and rejects a bad fn, format or items, also NULL items", {
  b <- block(function(results) 1, "ci", c("strobe:13a", "agrema:6.1"))
  expect_s3_class(b, "pbn_block")
  expect_identical(b$format, "ci")
  expect_identical(b$items, c("strobe:13a", "agrema:6.1"))
  expect_identical(block(function(results) 1, "p")$items, character(0))
  expect_error(block("f", "ci"), "`fn` must be a function of results.")
  for (bad in list(NULL, NA_character_, "", c("ci", "p"), 1)) {
    expect_error(
      block(function(results) 1, bad),
      "`format` must be one string, the name of a formatter.",
      fixed = TRUE,
      info = deparse(bad)
    )
  }
  for (bad in list(
    NULL,
    factor("strobe:1a"),
    list("strobe:1a"),
    c(a = "strobe:1a")
  )) {
    expect_error(
      block(function(results) 1, "ci", bad),
      "`items` must be a character vector of \"<id>:<item>\" strings, without names.",
      fixed = TRUE,
      info = deparse(bad)
    )
  }
  expect_error(
    block(function(results) 1, "ci", c("strobe:1a", "Strobe:1b")),
    "`items` must be \"<id>:<item>\" strings, such as \"strobe:13a\", and \"Strobe:1b\" is not.",
    fixed = TRUE
  )
  expect_error(
    block(function(results) 1, "ci", c("strobe:1a", NA)),
    "and NA is not.",
    fixed = TRUE
  )
  expect_error(
    block(function(results) 1, "ci", c("strobe:1a", "strobe:1a")),
    "`items` must name each item once, and \"strobe:1a\" comes more than once.",
    fixed = TRUE
  )
  expect_identical(
    parse_tags(c("strobe:13a", "agrema:6.1")),
    data.frame(id = c("strobe", "agrema"), item = c("13a", "6.1"))
  )
})

test_that("raw_value_problem() names the column, the row and the first cell with a formatted number, and passes labels and numbers", {
  ok <- data.frame(
    group = c("all", "age 18-25", "BMI"),
    n = c(1, 2, 3),
    level = factor(c("a", "b", "c"))
  )
  expect_null(raw_value_problem(ok))
  cells <- c("0.80", "1,037", "68%", "3 (", "n = 3", "p<0.001")
  for (cell in cells) {
    x <- data.frame(group = c("all", cell), n = 1:2)
    expect_identical(
      raw_value_problem(x),
      paste0(
        "holds a formatted number in the column group, row 2: \"",
        cell,
        "\". A block of pbn::block() returns numbers, and its formatter ",
        "formats them"
      ),
      info = cell
    )
  }
  expect_match(
    raw_value_problem(data.frame(f = factor("p = 0.04"))),
    "column f, row 1",
    fixed = TRUE
  )
  expect_match(
    raw_value_problem(c("0.80")),
    "returned an object of the class character",
    fixed = TRUE
  )
})

test_that("raw_value_problem() passes a comparison in a label, and stops on n, N or p before =, < or >", {
  for (cell in c("Education < 16 years", "Age >= 35", "> 5")) {
    expect_null(
      raw_value_problem(data.frame(group = c("all", cell), n = 1:2)),
      info = cell
    )
  }
  for (cell in c("n = 3", "N=12", "p < 0.001", "p=0.04", "p < 1", "P=1")) {
    expect_identical(
      raw_value_problem(data.frame(group = c("all", cell), n = 1:2)),
      paste0(
        "holds a formatted number in the column group, row 2: \"",
        cell,
        "\". A block of pbn::block() returns numbers, and its formatter ",
        "formats them"
      ),
      info = cell
    )
  }
})

test_that("find_formatter() takes the project formatter of the environment first, then the house one, and stops on an unknown name", {
  env <- new.env()
  env$ci <- function(x) return("own")
  expect_identical(find_formatter("ci", env)(data.frame()), "own")
  expect_identical(find_formatter("p", env), house_formatters()$p)
  expect_identical(find_formatter("ci"), house_formatters()$ci)
  expect_error(
    find_formatter("nope", env),
    "There is no formatter nope. The house formatters are ci, p, effect, n_pct, mean_sd.",
    fixed = TRUE
  )
})

test_that("txt() returns the project formatter's string when formatters.R defines ci, in the session and from the files", {
  p <- local_project()
  write_new_blocks(
    p,
    numeric_blocks,
    c(
      "ci <- function(x) {",
      "  return(paste0(\"own \", x$est))",
      "}"
    )
  )
  run <- save_run(p, only = c("real", "known"))
  expect_false(inherits(run$err, "error"))
  expect_identical(txt("Effect"), "own 0.8")
  expect_identical(txt("Sample", row = 2), "12 (0.8%)")
  # Without a session, txt() reads blocks.R and formatters.R as files.
  local_clean_state()
  expect_identical(txt("Effect", version = "v01", home = p$home), "own 0.8")
  withr::with_dir(p$home, {
    expect_identical(txt("Sample", version = "v01"), "1,037 (68.7%)")
  })
})

test_that("txt() formats at each call, and stops on a missing block, row, column or blocks.qs2", {
  p <- local_project()
  write_new_blocks(
    p,
    c(
      paste0(numeric_blocks, c(rep("", 8), ",")),
      "  \"Table\" = pbn::block(",
      "    function(results) data.frame(n = 1:2),",
      "    format = \"wide\"",
      "  )"
    ),
    "wide <- function(x) return(data.frame(a = paste0(\"n\", x$n), b = x$n))"
  )
  expect_false(inherits(save_run(p, only = c("real", "known"))$err, "error"))
  expect_identical(txt("Effect"), "0.80 (95% CI 0.46 to 1.16)")
  expect_identical(txt("Table", row = 2, col = "a"), "n2")
  expect_identical(txt("Table", row = 1, col = 1), "n1")
  # A new formatter shows at the next call, with no new blocks.qs2.
  write_lines(
    p$home,
    "v01_functions/formatters.R",
    "wide <- function(x) return(data.frame(a = paste0(\"m\", x$n)))"
  )
  expect_identical(txt("Table", row = 2), "m2")
  expect_error(txt("Nope"), "There is no block \"Nope\" in v01.", fixed = TRUE)
  expect_error(
    txt("Effect", row = 2),
    "The block \"Effect\" has 1 row, so it has no row 2.",
    fixed = TRUE
  )
  write_lines(
    p$home,
    "v01_functions/formatters.R",
    "wide <- function(x) return(data.frame(a = paste0(\"m\", x$n), b = x$n))"
  )
  expect_error(
    txt("Table"),
    "The formatter of the block \"Table\" returns 2 columns. Give the column as col.",
    fixed = TRUE
  )
  expect_error(
    txt("Table", col = "b"),
    "The cell of row 1 of the block \"Table\" is not a string.",
    fixed = TRUE
  )
  expect_error(
    txt("Table", col = "z"),
    "The formatted value of the block \"Table\" has no column z.",
    fixed = TRUE
  )
  unlink(file.path(p$home, "v01_results", "real", "blocks.qs2"))
  expect_error(
    txt("Effect"),
    "blocks.qs2. Write it with pbn::update_blocks()",
    fixed = TRUE
  )
})

test_that("the NAMESPACE file exports block, txt and update_blocks", {
  ns <- readLines(system.file("NAMESPACE", package = "pbn"))
  for (name in c("block", "txt", "update_blocks")) {
    expect_true(paste0("export(", name, ")") %in% ns, info = name)
  }
})
