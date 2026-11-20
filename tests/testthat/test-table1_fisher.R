# Fisher's exact test in table1() when the default workspace of
# stats::fisher.test() is too small.

# One row per count: `counts` is a matrix of groups by levels.
rows_of <- function(counts, groups, levels) {
  cells <- expand.grid(g = seq_along(groups), x = seq_along(levels))
  n <- as.vector(counts)
  return(data.table::data.table(
    g = factor(rep(groups[cells$g], n), levels = groups),
    x = factor(rep(levels[cells$x], n), levels = levels)
  ))
}

# Liao 2026 Table 1, Race: the 3 group columns INR >1.5 (N = 41), No FFP
# (N = 309) and FFP (N = 180), 530 women.
race_levels <- c(
  "Asian",
  "Black",
  "Native American",
  "Other",
  "White",
  "Declined",
  "Unavailable"
)
race_counts <- rbind(
  c(6, 5, 0, 5, 23, 1, 1),
  c(47, 50, 1, 27, 173, 3, 8),
  c(24, 39, 0, 19, 91, 1, 6)
)
race <- rows_of(race_counts, c("INR >1.5", "No FFP", "FFP"), race_levels)
vars_x <- data.table::data.table(var = "x", type = "cat")

test_that("the Liao race table of 3 groups gets the exact P of a workspace of 2e7", {
  # stats::fisher.test(race_counts) stops with FEXACT error 6, and
  # stats::fisher.test(race_counts, workspace = 2e7)$p.value is
  # 0.79993913867292177.
  t1 <- table1(race, by = "g", vars = vars_x)
  expect_equal(t1$p, rep(0.79993913867292177, 28), tolerance = 1e-12)
  expect_identical(t1$test, rep("fisher", 28))
})

test_that("the Liao race table of No FFP against FFP keeps the P of the default workspace", {
  two <- race[race$g != "INR >1.5", ]
  two$g <- droplevels(two$g)
  t1 <- table1(two, by = "g", vars = vars_x)
  # stats::fisher.test(race_counts[2:3, ])$p.value is 0.6644898596834673.
  # A workspace of 2e7 gives 0.66448985968346419, so the identity shows
  # that the default workspace gave this P.
  expect_identical(t1$p, rep(0.6644898596834673, 21))
  expect_identical(round(t1$p[1], 4), 0.6645)
  expect_identical(t1$test, rep("fisher", 21))
})

# 3 groups by 14 levels, 600 rows. Fisher's exact test stops with FEXACT
# error 7 with a workspace of 2e7.
big_counts <- matrix(
  c(
    14,
    9,
    21,
    22,
    16,
    17,
    18,
    12,
    16,
    23,
    9,
    9,
    17,
    17,
    9,
    17,
    17,
    16,
    14,
    11,
    14,
    16,
    20,
    18,
    17,
    16,
    20,
    16,
    24,
    11,
    11,
    15,
    15,
    18,
    14,
    12,
    9,
    18,
    11,
    0,
    1,
    0
  ),
  nrow = 3
)
big <- rows_of(big_counts, c("a", "b", "c"), sprintf("l%02d", 1:14))

test_that("a Fisher test that stops with a workspace of 2e7 gives P NA, the test fisher_failed and a warning", {
  expect_warning(
    t1 <- table1(big, by = "g", vars = vars_x),
    "Fisher's exact test of variable 'x' stops"
  )
  expect_identical(t1$p, rep(NA_real_, 56))
  expect_identical(t1$test, rep("fisher_failed", 56))
})
