# The styles of table1(): the loader, the checks of a style file, and the
# text of value_pretty and p_pretty. The style is
# fixtures/fixture_2026_12_01.yml.

# 20 hand-built rows, 10 in group a and 10 in group b.
# x: a has 3 ones and 7 zeros; b has 6 ones, 2 zeros and 2 NA.
# z: a is (1:10) / 3; b is 11..20.
d20 <- data.table::data.table(
  g = rep(c("a", "b"), each = 10),
  x = c(rep(1, 3), rep(0, 7), rep(1, 6), rep(0, 2), NA, NA),
  z = c((1:10) / 3, 11:20)
)
vars20 <- data.table::data.table(var = c("x", "z"), type = c("bin", "cont"))

# The same rows with by as a factor: the order b, a and the unused level c.
d20f <- data.table::copy(d20)
d20f$g <- factor(d20f$g, levels = c("b", "a", "c"))

# 25000 rows. y: a has 12347 ones of 20000; b has 1234 ones of 5000.
dbig <- data.table::data.table(
  g = rep(c("a", "b"), c(20000, 5000)),
  y = c(rep(1, 12347), rep(0, 7653), rep(1, 1234), rep(0, 3766))
)
varsbig <- data.table::data.table(var = "y", type = "bin")

fixture_style <- test_path("fixtures", "fixture_2026_12_01.yml")

# The fixture style with `change` applied, written to a temporary file that
# lives as long as the calling test.
style_with <- function(change, env = parent.frame()) {
  s <- yaml::read_yaml(fixture_style)
  s <- change(s)
  f <- withr::local_tempfile(fileext = ".yml", .local_envir = env)
  yaml::write_yaml(s, f)
  return(f)
}

# The message of the error of `expr`, or "no error". A test that compares
# this message with expect_identical() records one failure for a wrong or a
# missing error, and goes on to the next case.
error_message <- function(expr) {
  return(tryCatch(
    {
      force(expr)
      "no error"
    },
    error = conditionMessage
  ))
}

test_that("a styled bin row prints events and pct, and the missing count when it is above 0", {
  t1 <- table1(d20, by = "g", vars = vars20, style = fixture_style)
  r <- t1[t1$var == "x", ]
  # All: 9 of 18 = 50%, 2 missing. a: 3 of 10 = 30%. b: 6 of 8 = 75%,
  # 2 missing.
  expect_identical(
    r$value_pretty,
    c("9 (50.0); 2 missing", "3 (30.0)", "6 (75.0); 2 missing")
  )
})

test_that("a styled cont row rounds mean and sd to their decimals", {
  t1 <- table1(d20, by = "g", vars = vars20, style = fixture_style)
  r <- t1[t1$var == "z", ]
  # a: mean 55 / 30 = 1.833, sd sqrt(55 / 6) / 3 = 1.009.
  # b: mean 15.5, sd sqrt(55 / 6) = 3.028.
  # All: mean (55 / 3 + 155) / 20 = 8.667, sd 7.347.
  expect_identical(r$value_pretty, c("8.7 (7.35)", "1.8 (1.01)", "15.5 (3.03)"))
  expect_identical(r$p_pretty, rep(NA_character_, 3))
})

test_that("a p below p.lower prints as P <0.001", {
  style <- read_table1_style(fixture_style)
  expect_identical(
    format_p(c(0.0004, 0.00099, 0.001), style),
    c("P <0.001", "P <0.001", "P =0.001")
  )
  # The chi-square p of dbig is below 1e-300.
  t1 <- table1(dbig, by = "g", vars = varsbig, style = fixture_style)
  expect_identical(t1$p_pretty, rep("P <0.001", 3))
})

test_that("a p above p.upper prints as P >0.999", {
  # The thresholds compare the p before rounding, so 0.9994 is above 0.999.
  style <- read_table1_style(fixture_style)
  expect_identical(
    format_p(c(0.9996, 0.9994, 0.999, 0.9989), style),
    c("P >0.999", "P >0.999", "P =0.999", "P =0.999")
  )
})

test_that("a p between the thresholds prints exact to three decimals", {
  style <- read_table1_style(fixture_style)
  expect_identical(
    format_p(c(0.0049, 0.0501, NA), style),
    c("P =0.005", "P =0.050", NA)
  )
  # Fisher's exact test of x in d20: a = (7 zeros, 3 ones),
  # b = (2 zeros, 6 ones), p = 0.1534.
  t1 <- table1(d20, by = "g", vars = vars20, style = fixture_style)
  expect_identical(t1$p_pretty[t1$var == "x"], rep("P =0.153", 3))
})

test_that("a style with no thresholds and the template {p} prints the number only", {
  f <- style_with(function(s) {
    s$p$template$value <- "{p}"
    s$p$lower["value"] <- list(NULL)
    s$p$upper["value"] <- list(NULL)
    return(s)
  })
  style <- read_table1_style(f)
  expect_identical(
    format_p(c(0.0004, 0.0049, 0.9996), style),
    c("0.000", "0.005", "1.000")
  )
})

test_that("the thousands mark applies from 10,000 and not below", {
  t1 <- table1(dbig, by = "g", vars = varsbig, style = fixture_style)
  # All: 13581 of 25000 = 54.324%. a: 12347 of 20000 = 61.735%.
  # b: 1234 of 5000 = 24.68%.
  expect_identical(
    t1$value_pretty,
    c("13,581 (54.3)", "12,347 (61.7)", "1234 (24.7)")
  )
  th <- read_table1_style(fixture_style)$thousands
  expect_identical(
    format_number(c(9999, 10000, 12347, -12347), 0, th),
    c("9999", "10,000", "12,347", "-12,347")
  )
  # The rounded value decides: 9999.96 rounds to 10000.0.
  expect_identical(
    format_number(c(9999.94, 9999.96, 1234567.891), 1, th),
    c("9999.9", "10,000.0", "1,234,567.9")
  )
  th$mark$value <- ""
  expect_identical(format_number(12347, 0, th), "12347")
})

test_that("missing.show never adds no count, and always adds a count of 0 too", {
  never <- style_with(function(s) {
    s$missing$show$value <- "never"
    return(s)
  })
  t1 <- table1(d20, by = "g", vars = vars20, style = never)
  expect_identical(
    t1$value_pretty[t1$var == "x"],
    c("9 (50.0)", "3 (30.0)", "6 (75.0)")
  )
  always <- style_with(function(s) {
    s$missing$show$value <- "always"
    return(s)
  })
  t1 <- table1(d20, by = "g", vars = vars20, style = always)
  expect_identical(
    t1$value_pretty[t1$var == "x"],
    c("9 (50.0); 2 missing", "3 (30.0); 0 missing", "6 (75.0); 2 missing")
  )
})

test_that("with style = NULL, table1 is identical to pbn 2026.11.30", {
  # fixtures/table1_2026_11_30.qs2 holds the output of the installed pbn
  # 2026.11.30 for d20f and dbig. qs2 drops the self-reference of a
  # data.table, and setalloccol() restores it.
  frozen <- qs2::qs_read(test_path("fixtures", "table1_2026_11_30.qs2"))
  small <- data.table::setalloccol(frozen$small)
  big <- data.table::setalloccol(frozen$big)
  expect_identical(table1(d20f, by = "g", vars = vars20), small)
  expect_identical(table1(d20f, by = "g", vars = vars20, style = NULL), small)
  expect_identical(table1(dbig, by = "g", vars = varsbig, style = NULL), big)
})

test_that("a style adds value_pretty and p_pretty and changes no other column", {
  plain <- table1(d20f, by = "g", vars = vars20)
  styled <- table1(d20f, by = "g", vars = vars20, style = fixture_style)
  expect_identical(names(styled), c(names(plain), "value_pretty", "p_pretty"))
  expect_identical(as.list(styled)[names(plain)], as.list(plain))
  expect_identical(
    styled$value_pretty,
    c(
      "9 (50.0); 2 missing",
      "6 (75.0); 2 missing",
      "3 (30.0)",
      NA,
      "8.7 (7.35)",
      "15.5 (3.03)",
      "1.8 (1.01)",
      NA
    )
  )
  expect_identical(
    attr(styled, "footnotes"),
    "Values are n (percent) or mean (SD)."
  )
})

test_that("table1_styles lists the style files of the style folder, sorted", {
  # pbn 2026.12.2 installs one style.
  expect_identical(table1_styles(), "ijoa_brief_report_2026_11_20")
  dir <- withr::local_tempdir()
  file.create(file.path(
    dir,
    c("b_2026_01_01.yml", "a_2026_01_02.yml", "Bad_2026_01_01.yml", "a.yml")
  ))
  local_mocked_bindings(table1_style_dir = function() return(dir))
  expect_identical(table1_styles(), c("a_2026_01_02", "b_2026_01_01"))
  local_mocked_bindings(table1_style_dir = function() return(""))
  expect_identical(table1_styles(), character(0))
})

test_that("a style name loads the installed style of that name", {
  local_mocked_bindings(
    table1_style_dir = function() return(test_path("fixtures"))
  )
  expect_identical(table1_styles(), "fixture_2026_12_01")
  by_name <- table1(d20, by = "g", vars = vars20, style = "fixture_2026_12_01")
  by_path <- table1(d20, by = "g", vars = vars20, style = fixture_style)
  expect_identical(by_name$value_pretty, by_path$value_pretty)
  expect_identical(
    attr(read_table1_style("fixture_2026_12_01"), "file"),
    file.path(test_path("fixtures"), "fixture_2026_12_01.yml")
  )
})

test_that("a bad style name is an error that names it", {
  # expect_error() without a pattern catches any error, and the message is
  # then compared exactly. So an error from a later step fails the
  # comparison, and does not pass through as a test error.
  err <- expect_error(
    table1(d20, by = "g", vars = vars20, style = "nope_2026_01_01")
  )
  expect_identical(
    conditionMessage(err),
    paste0(
      "pbn has no Table 1 style named 'nope_2026_01_01'. The installed ",
      "styles are: ijoa_brief_report_2026_11_20"
    )
  )
  local_mocked_bindings(
    table1_style_dir = function() return(test_path("fixtures"))
  )
  err <- expect_error(
    table1(d20, by = "g", vars = vars20, style = "nope_2026_01_01")
  )
  expect_identical(
    conditionMessage(err),
    paste0(
      "pbn has no Table 1 style named 'nope_2026_01_01'. The installed ",
      "styles are: fixture_2026_12_01"
    )
  )
  expect_error(
    table1(d20, by = "g", vars = vars20, style = "nope.yml"),
    "The style file nope.yml does not exist.",
    fixed = TRUE
  )
  expect_error(
    table1(d20, by = "g", vars = vars20, style = c("a", "b")),
    "`style` must be one string",
    fixed = TRUE
  )
})

test_that("an unknown field is an error that names the field and the file", {
  f <- style_with(function(s) {
    s$bin$colour <- list(value = "red", source = "editorial choice")
    return(s)
  })
  expect_error(
    table1(d20, by = "g", vars = vars20, style = f),
    paste0("The style file ", f, " has an unknown field: bin.colour."),
    fixed = TRUE
  )
  f <- style_with(function(s) {
    s$colours <- list(value = "red", source = "editorial choice")
    return(s)
  })
  expect_error(
    read_table1_style(f),
    paste0("The style file ", f, " has an unknown field: colours."),
    fixed = TRUE
  )
  f <- style_with(function(s) {
    s$p$digits$note <- "three"
    return(s)
  })
  expect_error(
    read_table1_style(f),
    paste0("The style file ", f, " has an unknown field: p.digits.note."),
    fixed = TRUE
  )
})

test_that("every field of the schema is checked: an ill-typed value, a missing field and a missing source are errors that name it", {
  # The message for a YAML list as the value, for each of the 15 fields.
  string <- "must be one quoted string."
  digits <- "must be a whole number from 0 to 10."
  threshold <- "must be null, or a number above 0 and below 1."
  ill <- list(
    bin.template = string,
    bin.pct_digits = digits,
    cont.template = string,
    cont.mean_digits = digits,
    cont.sd_digits = digits,
    p.digits = digits,
    p.template = string,
    p.lower = threshold,
    p.below_template = string,
    p.upper = threshold,
    p.above_template = string,
    thousands.mark = string,
    thousands.from = "must be a number above 0.",
    missing.show = "must be one of: never, if_any, always.",
    missing.template = string
  )
  fields <- unlist(lapply(names(style_schema), function(section) {
    return(paste0(section, ".", names(style_schema[[section]])))
  }))
  expect_identical(sort(fields), sort(names(ill)))
  for (section in names(style_schema)) {
    for (name in names(style_schema[[section]])) {
      field <- paste0(section, ".", name)
      f <- style_with(function(s) {
        s[[section]][[name]]$value <- list("a", "b")
        return(s)
      })
      expect_identical(
        error_message(read_table1_style(f)),
        paste0("The field ", field, " of the style file ", f, " ", ill[[field]])
      )
      f <- style_with(function(s) {
        s[[section]][[name]] <- NULL
        return(s)
      })
      expect_identical(
        error_message(read_table1_style(f)),
        paste0("The style file ", f, " lacks the field ", field, ".")
      )
      f <- style_with(function(s) {
        s[[section]][[name]]$source <- NULL
        return(s)
      })
      expect_identical(
        error_message(read_table1_style(f)),
        paste0("The style file ", f, " lacks the field ", field, ".source.")
      )
    }
  }
  f <- style_with(function(s) {
    s$footnotes <- NULL
    return(s)
  })
  expect_identical(
    error_message(read_table1_style(f)),
    paste0("The style file ", f, " lacks the field footnotes.")
  )
})

test_that("each check of a value gives its own message", {
  cases <- list(
    list("bin", "pct_digits", 1.5, "must be a whole number from 0 to 10."),
    list(
      "bin",
      "template",
      "{events} ({median})",
      "holds {median}. It can hold only {events}, {n}, {pct}."
    ),
    list("p", "lower", 1.5, "must be null, or a number above 0 and below 1."),
    list("p", "lower", 0.0005, "must have no more decimals than p.digits."),
    list("thousands", "from", -1, "must be a number above 0."),
    list("thousands", "mark", 1, "must be one quoted string."),
    list(
      "missing",
      "show",
      "sometimes",
      "must be one of: never, if_any, always."
    )
  )
  for (case in cases) {
    f <- style_with(function(s) {
      s[[case[[1]]]][[case[[2]]]]$value <- case[[3]]
      return(s)
    })
    expect_identical(
      error_message(read_table1_style(f)),
      paste0(
        "The field ",
        case[[1]],
        ".",
        case[[2]],
        " of the style file ",
        f,
        " ",
        case[[4]]
      )
    )
  }
  f <- style_with(function(s) {
    s$p$lower$value <- 0.5
    s$p$upper$value <- 0.4
    return(s)
  })
  expect_identical(
    error_message(read_table1_style(f)),
    paste0(
      "The field p.lower of the style file ",
      f,
      " must be below p.upper."
    )
  )
  f <- style_with(function(s) {
    s$bin$template$source <- " "
    return(s)
  })
  expect_identical(
    error_message(read_table1_style(f)),
    paste0(
      "The field bin.template of the style file ",
      f,
      " needs a source: one string that is not empty."
    )
  )
  f <- style_with(function(s) {
    s$footnotes[[1]]$value <- 3
    return(s)
  })
  expect_identical(
    error_message(read_table1_style(f)),
    paste0(
      "The field footnotes[1] of the style file ",
      f,
      " must have a value that is one quoted string."
    )
  )
  f <- style_with(function(s) {
    s$bin$pct_digits <- 1
    return(s)
  })
  expect_identical(
    error_message(read_table1_style(f)),
    paste0(
      "The field bin.pct_digits of the style file ",
      f,
      " must be a map with the keys value and source."
    )
  )
})

test_that("pbn installs the IJOA brief report style, and it formats the fixture to the strings of its YAML", {
  ijoa <- "ijoa_brief_report_2026_11_20"
  expect_true(ijoa %in% table1_styles())
  expect_identical(
    attr(read_table1_style(ijoa), "file"),
    file.path(system.file("table1", package = "pbn"), paste0(ijoa, ".yml"))
  )
  # The YAML inst/table1/ijoa_brief_report_2026_11_20.yml gives:
  # bin.template "{events} ({pct}%)" with pct_digits 1; cont.template
  # "{mean} ({sd})" with mean_digits 1 and sd_digits 1; missing.show if_any
  # with "; {missing} missing"; p.digits 3, p.template "P ={p}", "P <0.001"
  # below 0.001 and "P >0.999" above 0.999; the mark "," from 10000.
  t1 <- table1(d20, by = "g", vars = vars20, style = ijoa)
  x <- t1[t1$var == "x", ]
  # The 0/1 cell. All: 9 of 18 = 50%, 2 missing. a: 3 of 10 = 30%.
  # b: 6 of 8 = 75%, 2 missing.
  expect_identical(
    x$value_pretty,
    c("9 (50.0%); 2 missing", "3 (30.0%)", "6 (75.0%); 2 missing")
  )
  # Fisher's exact p of x is 0.1534, so three decimals give 0.153.
  expect_identical(x$p_pretty, rep("P =0.153", 3))
  z <- t1[t1$var == "z", ]
  # The numeric cell. All: mean 8.667, sd 7.347. a: mean 1.833, sd 1.009.
  # b: mean 15.5, sd 3.028. One decimal each.
  expect_identical(z$value_pretty, c("8.7 (7.3)", "1.8 (1.0)", "15.5 (3.0)"))
  expect_identical(z$p_pretty, rep(NA_character_, 3))
  # The thousands rule. All: 13581 of 25000 = 54.324%. a: 12347 of 20000 =
  # 61.735%. b: 1234 of 5000 = 24.68%, and 1234 is below 10000. The
  # chi-square p of dbig is below 1e-300, so below 0.001.
  big <- table1(dbig, by = "g", vars = varsbig, style = ijoa)
  expect_identical(
    big$value_pretty,
    c("13,581 (54.3%)", "12,347 (61.7%)", "1234 (24.7%)")
  )
  expect_identical(big$p_pretty, rep("P <0.001", 3))
  # 5 of 10 in each group: every expected count is 5, so the chi-square
  # test applies, its statistic is 0 and p is 1, above 0.999.
  same <- data.table::data.table(
    g = rep(c("a", "b"), each = 10),
    y = rep(c(1, 0), times = 10)
  )
  eq <- table1(same, by = "g", vars = varsbig, style = ijoa)
  expect_identical(eq$value_pretty, c("10 (50.0%)", "5 (50.0%)", "5 (50.0%)"))
  expect_identical(eq$p_pretty, rep("P >0.999", 3))
  expect_identical(
    attr(t1, "footnotes"),
    c(
      "Values are n (%) or mean (SD).",
      "SD, standard deviation.",
      paste0(
        "P values compare the groups with Pearson's chi-square test, or ",
        "with Fisher's exact test when an expected count is below 5."
      )
    )
  )
})
