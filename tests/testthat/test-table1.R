# 40 hand-built rows, 20 in group a and 20 in group b.
# x1: a has 12 ones and 8 zeros; b has 6 ones, 12 zeros and 2 NA.
# x2: a has 1 one and 19 zeros; b has 5 ones and 15 zeros.
# z:  a is 1..20; b is 11..30.
d40 <- data.table::data.table(
  g = rep(c("a", "b"), each = 20),
  x1 = c(rep(1, 12), rep(0, 8), rep(1, 6), rep(0, 12), NA, NA),
  x2 = c(1, rep(0, 19), rep(1, 5), rep(0, 15)),
  z = c(1:20, 11:30)
)
vars40 <- data.table::data.table(
  var = c("x1", "x2", "z"),
  type = c("bin", "bin", "cont")
)

test_that("table1 has one row per variable and group, All first", {
  t1 <- table1(d40, by = "g", vars = vars40)
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
      "q3"
    )
  )
  expect_identical(t1$var, rep(c("x1", "x2", "z"), each = 3))
  expect_identical(t1$grp, rep(c("All", "a", "b"), 3))
})

test_that("table1 counts n, missing, events and pct of a bin variable", {
  t1 <- table1(d40, by = "g", vars = vars40)
  r <- t1[t1$var == "x1", ]
  expect_identical(r$n, c(38L, 20L, 18L))
  expect_identical(r$missing, c(2L, 0L, 2L))
  expect_identical(r$events, c(18L, 12L, 6L))
  expect_equal(r$pct, c(100 * 18 / 38, 60, 100 * 6 / 18))
  expect_true(all(is.na(r$mean)))
  expect_true(all(is.na(r$sd)))
})

test_that("table1 matches a hand calculation for a chi-square variable", {
  t1 <- table1(d40, by = "g", vars = vars40)
  r <- t1[t1$var == "x1", ]
  # 2x2 table: a = (8 zeros, 12 ones), b = (12 zeros, 6 ones), N = 38.
  # Every expected count is 8.5 or more. Without continuity correction:
  # X2 = N (ad - bc)^2 / (r1 r2 c1 c2) = 38 * 96^2 / (20 * 18 * 20 * 18).
  hand <- stats::pchisq(
    38 * 96^2 / (20 * 18 * 20 * 18),
    df = 1,
    lower.tail = FALSE
  )
  direct <- stats::chisq.test(
    matrix(c(8, 12, 12, 6), nrow = 2, byrow = TRUE),
    correct = FALSE
  )$p.value
  expect_equal(hand, direct)
  expect_equal(r$p, rep(hand, 3))
  expect_identical(r$test, rep("chisq", 3))
})

test_that("table1 uses Fisher's exact test when an expected count is below 5", {
  t1 <- table1(d40, by = "g", vars = vars40)
  r <- t1[t1$var == "x2", ]
  # 2x2 table: a = (19 zeros, 1 one), b = (15 zeros, 5 ones). The expected
  # count of ones is 20 * 6 / 40 = 3 in each group.
  direct <- stats::fisher.test(
    matrix(c(19, 1, 15, 5), nrow = 2, byrow = TRUE)
  )$p.value
  expect_equal(r$p, rep(direct, 3))
  expect_identical(r$test, rep("fisher", 3))
  expect_identical(r$events, c(6L, 1L, 5L))
  expect_equal(r$pct, c(15, 5, 25))
})

test_that("table1 gives mean and sd of a cont variable, and no test", {
  t1 <- table1(d40, by = "g", vars = vars40)
  r <- t1[t1$var == "z", ]
  # var(1..20) = 20 * 21 / 12 = 35, and 11..30 is a shift of 1..20.
  # All: sum of squares about the mean 15.5 is 11940 - 40 * 15.5^2 = 2330.
  expect_equal(r$mean, c(15.5, 10.5, 20.5))
  expect_equal(r$sd, c(sqrt(2330 / 39), sqrt(35), sqrt(35)))
  expect_identical(r$n, c(40L, 20L, 20L))
  expect_true(all(is.na(r$p)))
  expect_true(all(is.na(r$test)))
  expect_true(all(is.na(r$events)))
  expect_true(all(is.na(r$pct)))
})

test_that("table1 keeps the factor levels of by in their order", {
  d <- data.table::copy(d40)
  d$g <- factor(d$g, levels = c("b", "a"))
  t1 <- table1(d, by = "g", vars = vars40[1, ])
  expect_identical(t1$grp, c("All", "b", "a"))
  expect_identical(t1$events, c(18L, 6L, 12L))
})

test_that("table1 stops on a bin variable with a value other than 0, 1 or NA", {
  d <- data.table::copy(d40)
  d$x3 <- c(rep(0, 39), 2)
  expect_error(
    table1(
      d,
      by = "g",
      vars = data.table::data.table(var = "x3", type = "bin")
    ),
    "Variable 'x3' has type \"bin\" but holds values other than 0, 1 and NA: 2",
    fixed = TRUE
  )
})

test_that("table1 stops on a level of by named All", {
  d <- data.table::copy(d40)
  d$g[d$g == "a"] <- "All"
  expect_error(
    table1(d, by = "g", vars = vars40),
    "A level of `by` is \"All\"",
    fixed = TRUE
  )
  d$g <- factor(d$g, levels = c("All", "b"))
  expect_error(
    table1(d, by = "g", vars = vars40),
    "A level of `by` is \"All\"",
    fixed = TRUE
  )
})

test_that("table1 leaves an unused factor level out of the test", {
  d <- data.table::copy(d40)
  d$g <- factor(d$g, levels = c("a", "b", "c"))
  t1 <- table1(d, by = "g", vars = vars40)
  r <- t1[t1$var == "x1", ]
  expect_identical(r$grp, c("All", "a", "b", "c"))
  expect_identical(r$n, c(38L, 20L, 18L, 0L))
  # The same 2x2 table as the chi-square test above.
  hand <- stats::pchisq(
    38 * 96^2 / (20 * 18 * 20 * 18),
    df = 1,
    lower.tail = FALSE
  )
  expect_equal(r$p, rep(hand, 4))
  expect_identical(r$test, rep("chisq", 4))
})

test_that("table1 gives no test when only one group has rows", {
  d <- data.table::copy(d40[d40$g == "a", ])
  d$g <- factor(d$g, levels = c("a", "b"))
  t1 <- table1(d, by = "g", vars = vars40)
  r <- t1[t1$var == "x1", ]
  expect_identical(r$grp, c("All", "a", "b"))
  expect_identical(r$n, c(20L, 20L, 0L))
  expect_true(all(is.na(r$p)))
  expect_true(all(is.na(r$test)))
})
