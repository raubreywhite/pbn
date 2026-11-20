# The statistics that table1() adds in pbn 2026.12.3: the median and
# quartiles of a "cont" variable, the "cat" type, and the rows and templates
# that a style adds for them. The style is fixtures/fixture_full.yml.

# 12 hand-built rows, 5 in group a and 7 in group b.
# z: a is 1, 2, 4, 7, 11 (odd n); b is 10, 20, ..., 60 and NA (even n).
# k: a is lo, lo, mid, hi, NA; b is lo, mid, mid, hi, hi, hi, NA. The factor
#    order lo, mid, hi is not the sorted order hi, lo, mid.
d12 <- data.table::data.table(
  g = rep(c("a", "b"), c(5, 7)),
  z = c(1, 2, 4, 7, 11, 10, 20, 30, 40, 50, 60, NA),
  k = factor(
    c("lo", "lo", "mid", "hi", NA, "lo", "mid", "mid", "hi", "hi", "hi", NA),
    levels = c("lo", "mid", "hi")
  )
)
vars12 <- data.table::data.table(var = c("z", "k"), type = c("cont", "cat"))

# 60 rows, 30 in each group. c3: a is 10 x, 10 y, 10 z; b is 5 x, 10 y,
# 15 z. Every expected count is 7.5 or more.
d60 <- data.table::data.table(
  g = rep(c("a", "b"), each = 30),
  c3 = c(
    rep(c("x", "y", "z"), c(10, 10, 10)),
    rep(c("x", "y", "z"), c(5, 10, 15))
  )
)
vars60 <- data.table::data.table(var = "c3", type = "cat")

full_style <- test_path("fixtures", "fixture_full.yml")

# The full style with `change` applied, written to a temporary file that
# lives as long as the calling test.
full_with <- function(change, env = parent.frame()) {
  s <- yaml::read_yaml(full_style)
  s <- change(s)
  f <- withr::local_tempfile(fileext = ".yml", .local_envir = env)
  yaml::write_yaml(s, f)
  return(f)
}

error_text <- function(expr) {
  return(tryCatch(
    {
      force(expr)
      "no error"
    },
    error = conditionMessage
  ))
}

test_that("a cont row carries the median and quartiles of the non-missing values, quantile type 7", {
  t1 <- table1(d12, by = "g", vars = vars12)
  r <- t1[t1$var == "z", ]
  expect_identical(r$grp, c("All", "a", "b"))
  expect_identical(r$n, c(11L, 5L, 6L))
  # a, n = 5: the quartiles are the 2nd, 3rd and 4th values.
  # b, n = 6: type 7 puts Q1 at 1 + 0.25 * 5 = 2.25, so 20 + 0.25 * 10.
  # Type 6 would give 17.5 and 52.5.
  # All, n = 11: 1 2 4 7 10 11 20 30 40 50 60. Q1 is at 3.5, so
  # (4 + 7) / 2. Type 6 would give 4.
  expect_identical(r$median, c(11, 4, 35))
  expect_identical(r$q1, c(5.5, 2, 22.5))
  expect_identical(r$q3, c(35, 7, 47.5))
  expect_identical(r$level, rep(NA_character_, 3))
})

test_that("a bin row has no level, median or quartiles", {
  d <- data.table::data.table(
    g = rep(c("a", "b"), each = 2),
    x = c(0, 1, 1, NA)
  )
  t1 <- table1(
    d,
    by = "g",
    vars = data.table::data.table(var = "x", type = "bin")
  )
  expect_identical(t1$level, rep(NA_character_, 3))
  expect_identical(t1$median, rep(NA_real_, 3))
  expect_identical(t1$q1, rep(NA_real_, 3))
  expect_identical(t1$q3, rep(NA_real_, 3))
})

test_that("a cat variable has one row per level and group, in the factor order", {
  t1 <- table1(d12, by = "g", vars = vars12)
  r <- t1[t1$var == "k", ]
  expect_identical(r$level, rep(c("lo", "mid", "hi"), each = 3))
  expect_identical(r$grp, rep(c("All", "a", "b"), 3))
  # n is the non-missing count of the group, the same for each level.
  expect_identical(r$n, rep(c(10L, 4L, 6L), 3))
  expect_identical(r$missing, rep(c(2L, 1L, 1L), 3))
  expect_identical(r$events, c(3L, 2L, 1L, 3L, 1L, 2L, 4L, 1L, 3L))
  # pct is 100 * events / n, with the non-missing n.
  expect_equal(r$pct, c(30, 50, 100 / 6, 30, 25, 200 / 6, 40, 25, 50))
  expect_true(all(is.na(r$mean)))
  expect_true(all(is.na(r$median)))
  expect_identical(
    names(t1),
    c(
      "var",
      "grp",
      "n",
      "missing",
      "mean",
      "sd",
      "events",
      "pct",
      "p",
      "test",
      "level",
      "median",
      "q1",
      "q3",
      "label"
    )
  )
})

test_that("the levels of a cat variable that is not a factor are sorted", {
  d <- data.table::data.table(
    g = rep(c("a", "b"), each = 3),
    s = c("b", "a", "c", "c", "a", NA),
    q = c(10, 9, 2, 9, 10, 2)
  )
  vars <- data.table::data.table(var = c("s", "q"), type = c("cat", "cat"))
  t1 <- table1(d, by = "g", vars = vars)
  expect_identical(unique(t1$level[t1$var == "s"]), c("a", "b", "c"))
  # A number sorts as a number, so 9 comes before 10.
  expect_identical(unique(t1$level[t1$var == "q"]), c("2", "9", "10"))
})

test_that("a cat variable uses Fisher's exact test when an expected count is below 5", {
  t1 <- table1(d12, by = "g", vars = vars12)
  r <- t1[t1$var == "k", ]
  # The table a = (2, 1, 1), b = (1, 2, 3) has the expected counts 1.2 to
  # 2.4. The p of Fisher's exact test is 26/35.
  expect_equal(r$p, rep(26 / 35, 9))
  expect_identical(r$test, rep("fisher", 9))
})

test_that("a cat variable uses the chi-square test over its levels when every expected count is 5 or more", {
  t1 <- table1(d60, by = "g", vars = vars60)
  # Expected counts: 7.5, 10, 12.5 in each group. X2 = 2 * 2.5^2 / 7.5 +
  # 2 * 2.5^2 / 12.5 = 8 / 3 on 2 degrees of freedom, so p = exp(-4 / 3).
  expect_equal(t1$p, rep(exp(-4 / 3), 9))
  expect_identical(t1$test, rep("chisq", 9))
})

test_that("an unused level of a cat factor has a row of 0 and does not change the test", {
  d <- data.table::copy(d60)
  d$c3 <- factor(d$c3, levels = c("x", "y", "z", "w"))
  t1 <- table1(d, by = "g", vars = vars60)
  w <- t1[t1$level == "w", ]
  expect_identical(w$events, c(0L, 0L, 0L))
  expect_equal(w$pct, c(0, 0, 0))
  expect_equal(t1$p, rep(exp(-4 / 3), 12))
  expect_identical(t1$test, rep("chisq", 12))
})

test_that("a style formats a cont row with the median and quartiles", {
  t1 <- table1(d12, by = "g", vars = vars12, style = full_style)
  r <- t1[t1$var == "z" & t1$row == "value", ]
  expect_identical(
    r$value_pretty,
    c("11.0 (5.5-35.0)", "4.0 (2.0-7.0)", "35.0 (22.5-47.5)")
  )
  # The mean and sd keys still work, in one template with the median.
  f <- full_with(function(s) {
    s$cont$template$value <- "{mean} ({sd}); {median} [{q1}, {q3}]"
    return(s)
  })
  t1 <- table1(d12, by = "g", vars = vars12, style = f)
  r <- t1[t1$var == "z" & t1$row == "value", ]
  # a: mean 5, sd sqrt(66 / 4) = 4.06. b: mean 35, sd sqrt(350) = 18.71.
  expect_identical(
    r$value_pretty[2:3],
    c("5.0 (4.06); 4.0 [2.0, 7.0]", "35.0 (18.71); 35.0 [22.5, 47.5]")
  )
})

test_that("a style gives a cat variable a heading row that carries its P, then one row per level", {
  t1 <- table1(d12, by = "g", vars = vars12, style = full_style)
  r <- t1[t1$var == "k", ]
  expect_identical(
    r$row,
    rep(c("heading", "value", "value", "value", "missing"), each = 3)
  )
  expect_identical(r$level, rep(c(NA, "lo", "mid", "hi", NA), each = 3))
  expect_identical(r$value_pretty[1:3], rep("by level", 3))
  # 26 / 35 = 0.743. The P is on the heading row only.
  expect_identical(r$p_pretty, c(rep("P =0.743", 3), rep(NA, 12)))
  expect_identical(
    r$value_pretty[4:12],
    c(
      "3/10 (30.0%)",
      "2/4 (50.0%)",
      "1/6 (16.7%)",
      "3/10 (30.0%)",
      "1/4 (25.0%)",
      "2/6 (33.3%)",
      "4/10 (40.0%)",
      "1/4 (25.0%)",
      "3/6 (50.0%)"
    )
  )
})

test_that("the Missing row gives the missing count over all rows of the group", {
  t1 <- table1(d12, by = "g", vars = vars12, style = full_style)
  # k: All 2 of 12 = 16.7%, a 1 of 5 = 20%, b 1 of 7 = 14.3%. The level
  # rows divide by the non-missing n: lo in a is 2 of 4 = 50%.
  k <- t1[t1$var == "k" & t1$row == "missing", ]
  expect_identical(k$value_pretty, c("2 (16.7%)", "1 (20.0%)", "1 (14.3%)"))
  expect_identical(k$grp, c("All", "a", "b"))
  expect_identical(k$n, c(10L, 4L, 6L))
  expect_identical(k$missing, c(2L, 1L, 1L))
  # The column pct of a Missing row holds the percent that its cell shows.
  expect_equal(k$pct, c(200 / 12, 20, 100 / 7))
  # z: All 1 of 12, a 0 of 5, b 1 of 7. The row is there for each group,
  # because one group has a missing value.
  z <- t1[t1$var == "z", ]
  expect_identical(z$row, rep(c("value", "missing"), each = 3))
  expect_identical(z$value_pretty[4:6], c("1 (8.3%)", "0 (0.0%)", "1 (14.3%)"))
  expect_identical(z$p_pretty, rep(NA_character_, 6))
  # No cell gets missing.template with a row choice.
  expect_false(any(grepl("missing", t1$value_pretty, fixed = TRUE)))
})

test_that("missing.show row_if_any gives no Missing row to a variable with no missing value, and row_always does", {
  t1 <- table1(d60, by = "g", vars = vars60, style = full_style)
  expect_identical(t1$row, rep(c("heading", "value"), c(3, 9)))
  always <- full_with(function(s) {
    s$missing$show$value <- "row_always"
    return(s)
  })
  t1 <- table1(d60, by = "g", vars = vars60, style = always)
  expect_identical(t1$row, rep(c("heading", "value", "missing"), c(3, 9, 3)))
  expect_identical(t1$value_pretty[13:15], rep("0 (0.0%)", 3))
})

test_that("the cat and missing row fields have their own digits", {
  f <- full_with(function(s) {
    s$cat$pct_digits$value <- 2
    s$missing$row_pct_digits$value <- 0
    s$cont$median_digits$value <- 2
    return(s)
  })
  t1 <- table1(d12, by = "g", vars = vars12, style = f)
  expect_identical(
    t1$value_pretty[t1$var == "k" & t1$level %in% "lo"],
    c("3/10 (30.00%)", "2/4 (50.00%)", "1/6 (16.67%)")
  )
  expect_identical(
    t1$value_pretty[t1$var == "k" & t1$row == "missing"],
    c("2 (17%)", "1 (20%)", "1 (14%)")
  )
  expect_identical(
    t1$value_pretty[t1$var == "z" & t1$row == "value"][3],
    "35.00 (22.50-47.50)"
  )
})

test_that("bin and cont rows of a styled table get the row type value", {
  d <- data.table::data.table(
    g = rep(c("a", "b"), each = 2),
    x = c(0, 1, 1, 0),
    z = 1:4
  )
  vars <- data.table::data.table(var = c("x", "z"), type = c("bin", "cont"))
  t1 <- table1(d, by = "g", vars = vars, style = full_style)
  expect_identical(t1$row, rep("value", 6))
  expect_identical(t1$p_pretty[1:3], rep("P >0.999", 3))
})

test_that("a style without a cat section cannot format a cat variable", {
  err <- error_text(table1(
    d12,
    by = "g",
    vars = vars12,
    style = test_path("fixtures", "fixture_2026_12_01.yml")
  ))
  expect_identical(
    err,
    paste0(
      "The style file ",
      test_path("fixtures", "fixture_2026_12_01.yml"),
      " has no section cat, so it cannot format the \"cat\" variable k."
    )
  )
})

test_that("a new field is needed only where the style uses it", {
  f <- full_with(function(s) {
    s$cont$median_digits <- NULL
    return(s)
  })
  expect_identical(
    error_text(read_table1_style(f)),
    paste0(
      "The field cont.template of the style file ",
      f,
      " holds {median}, so the style needs the field cont.median_digits."
    )
  )
  f <- full_with(function(s) {
    s$missing$row_template <- NULL
    return(s)
  })
  expect_identical(
    error_text(read_table1_style(f)),
    paste0(
      "The field missing.show of the style file ",
      f,
      " is row_if_any, so the style needs the field missing.row_template."
    )
  )
  f <- full_with(function(s) {
    s$missing$show$value <- "row_always"
    s$missing$row_pct_digits <- NULL
    return(s)
  })
  expect_identical(
    error_text(read_table1_style(f)),
    paste0(
      "The field missing.row_template of the style file ",
      f,
      " holds {pct}, so the style needs the field missing.row_pct_digits."
    )
  )
  f <- full_with(function(s) {
    s$cat$heading_template$value <- "{n}"
    return(s)
  })
  expect_identical(
    error_text(read_table1_style(f)),
    paste0(
      "The field cat.heading_template of the style file ",
      f,
      " holds {n}. It can hold no placeholder."
    )
  )
  f <- full_with(function(s) {
    s$cat <- NULL
    s$cont$median_digits <- NULL
    s$cont$template$value <- "{mean} ({sd})"
    s$missing$show$value <- "if_any"
    s$missing$row_template <- NULL
    s$missing$row_pct_digits <- NULL
    return(s)
  })
  expect_identical(error_text(read_table1_style(f)), "no error")
})

test_that("the type of a variable must be bin, cont or cat", {
  expect_identical(
    error_text(table1(
      d12,
      by = "g",
      vars = data.table::data.table(var = "z", type = "num")
    )),
    "The type must be \"bin\", \"cont\" or \"cat\". It is not for: z"
  )
})

test_that("a style file of pbn 2026.12.2 stays valid, without the new fields", {
  # fixtures/released/ holds the file of the style that pbn 2026.12.2
  # released and pbn 2026.12.4 withdrew, byte for byte.
  expect_identical(
    error_text(read_table1_style(test_path(
      "fixtures",
      "released",
      "ijoa_brief_report_2026_11_20.yml"
    ))),
    "no error"
  )
  expect_identical(
    error_text(read_table1_style(test_path(
      "fixtures",
      "fixture_2026_12_01.yml"
    ))),
    "no error"
  )
})

test_that("a Missing row may print the count only, without row_pct_digits", {
  for (show in c("row_if_any", "row_always")) {
    f <- full_with(function(s) {
      s$missing$show$value <- show
      s$missing$row_template$value <- "{missing}"
      s$missing$row_pct_digits <- NULL
      return(s)
    })
    expect_identical(error_text(read_table1_style(f)), "no error")
    t1 <- table1(d12, by = "g", vars = vars12, style = f)
    expect_identical(
      t1$value_pretty[t1$var == "k" & t1$row == "missing"],
      c("2", "1", "1")
    )
  }
})

test_that("a Missing row template with {pct} still needs row_pct_digits", {
  f <- full_with(function(s) {
    s$missing$row_template$value <- "{missing} of all, {pct}"
    s$missing$row_pct_digits <- NULL
    return(s)
  })
  expect_identical(
    error_text(read_table1_style(f)),
    paste0(
      "The field missing.row_template of the style file ",
      f,
      " holds {pct}, so the style needs the field missing.row_pct_digits."
    )
  )
  # With missing.show never, the style adds no Missing row and needs no
  # missing.row_pct_digits.
  f <- full_with(function(s) {
    s$missing$show$value <- "never"
    s$missing$row_pct_digits <- NULL
    return(s)
  })
  expect_identical(error_text(read_table1_style(f)), "no error")
})

test_that("missing.row_template sets the text of the Missing row", {
  f <- full_with(function(s) {
    s$missing$row_template$value <- "missing: {missing}, {pct} percent"
    return(s)
  })
  t1 <- table1(d12, by = "g", vars = vars12, style = f)
  expect_identical(
    t1$value_pretty[t1$var == "k" & t1$row == "missing"],
    c(
      "missing: 2, 16.7 percent",
      "missing: 1, 20.0 percent",
      "missing: 1, 14.3 percent"
    )
  )
})

test_that("a cat variable that is all NA has a row per group, and a style gives it a heading row and a Missing row of 100%", {
  # 5 rows, 3 in group a and 2 in group b. u is NA in each row and is not
  # a factor, so it has no level.
  d <- data.table::data.table(
    g = c("a", "a", "a", "b", "b"),
    u = rep(NA_character_, 5)
  )
  vars <- data.table::data.table(var = "u", type = "cat")
  t1 <- table1(d, by = "g", vars = vars)
  expect_identical(t1$grp, c("All", "a", "b"))
  expect_identical(t1$level, rep(NA_character_, 3))
  expect_identical(t1$n, c(0L, 0L, 0L))
  expect_identical(t1$missing, c(5L, 3L, 2L))
  expect_identical(t1$events, rep(NA_integer_, 3))
  expect_identical(t1$pct, rep(NA_real_, 3))
  expect_identical(t1$p, rep(NA_real_, 3))
  styled <- table1(d, by = "g", vars = vars, style = full_style)
  expect_identical(styled$row, rep(c("heading", "missing"), each = 3))
  expect_identical(
    styled$value_pretty,
    c(
      rep("by level", 3),
      "5 (100.0%)",
      "3 (100.0%)",
      "2 (100.0%)"
    )
  )
  expect_identical(styled$p_pretty, rep(NA_character_, 6))
  expect_equal(styled$pct, c(NA, NA, NA, 100, 100, 100))
  # With row_always too, and with no Missing row a heading row only.
  never <- full_with(function(s) {
    s$missing$show$value <- "never"
    return(s)
  })
  styled <- table1(d, by = "g", vars = vars, style = never)
  expect_identical(styled$row, rep("heading", 3))
})
