# The column label of table1(), and the section rows of a styled output,
# which pbn 2026.12.4 added. The proof of a style compares the printed row
# labels with this column.

# inputs start
# 12 hand-built rows, 5 in group a and 7 in group b, and the unused level c
# of g. x is bin, z is cont, k is cat with the factor order lo, mid, hi, and
# u is a cat variable that is all NA.
dl <- data.table::data.table(
  g = factor(rep(c("a", "b"), c(5, 7)), levels = c("a", "b", "c")),
  x = c(1, 0, 0, 1, NA, 1, 1, 0, 0, 1, 1, 0),
  z = c(1, 2, 4, 7, 11, 10, 20, 30, 40, 50, 60, NA),
  k = factor(
    c("lo", "lo", "mid", "hi", NA, "lo", "mid", "mid", "hi", "hi", "hi", NA),
    levels = c("lo", "mid", "hi")
  ),
  u = rep(NA_character_, 12)
)
varsl <- data.table::data.table(
  var = c("x", "z", "k", "u"),
  type = c("bin", "cont", "cat", "cat")
)
varsl_bc <- varsl[1:2, ]
# inputs end

full_style <- test_path("fixtures", "fixture_full.yml")
old_style <- test_path("fixtures", "fixture_2026_12_01.yml")

# `x` without the column label.
without_label <- function(x) {
  x <- data.table::copy(x)
  data.table::set(x, j = "label", value = NULL)
  return(x)
}

test_that("each column and row of pbn 2026.12.3 is identical, and label is the one new column", {
  # fixtures/table1_2026_12_03.qs2 holds the output of pbn 2026.12.3, built
  # from git 9630182 into a throwaway library, for the inputs above. Its
  # styled outputs used fixture_full.yml of that commit, which has no
  # cat.level_prefix and no missing.row_label.
  frozen <- qs2::qs_read(test_path("fixtures", "table1_2026_12_03.qs2"))
  now <- list(
    plain = table1(dl, by = "g", vars = varsl),
    plain_null = table1(dl, by = "g", vars = varsl, style = NULL),
    full = table1(dl, by = "g", vars = varsl, style = full_style),
    old = table1(dl, by = "g", vars = varsl_bc, style = old_style)
  )
  expect_identical(names(now), names(frozen))
  for (k in names(frozen)) {
    expect_identical(names(now[[k]]), append(names(frozen[[k]]), "label", 14))
    expect_identical(without_label(now[[k]]), frozen[[k]], label = k)
  }
})

test_that("without a style, label is the label of vars, or the name, and a level row has its level", {
  t1 <- table1(dl, by = "g", vars = varsl)
  # x and z: 4 groups each; k: 3 levels of 4 groups; u: 4 rows with no
  # level.
  expect_identical(
    t1$label,
    c(
      rep(c("x", "z"), each = 4),
      rep(c("lo", "mid", "hi"), each = 4),
      rep("u", 4)
    )
  )
  v <- data.table::copy(varsl)
  v$label <- c("Smoker", NA, "Knee", "Unknown")
  v$section <- c("Mother", "Mother", NA, "Child")
  t1 <- table1(dl, by = "g", vars = v)
  expect_identical(
    t1$label,
    c(
      rep(c("Smoker", "z"), each = 4),
      rep(c("lo", "mid", "hi"), each = 4),
      rep("Unknown", 4)
    )
  )
  # Without a style there are no section rows.
  expect_false("section" %in% t1$row)
})

test_that("with a style, a heading row has the vars label, a level row cat.level_prefix and the level, and a Missing row missing.row_label", {
  v <- data.table::copy(varsl)
  v$label <- c("Smoker", "Age", "Knee", "Unknown")
  t1 <- table1(dl, by = "g", vars = v, style = full_style)
  k <- t1[t1$var == "k" & t1$grp == "All", ]
  expect_identical(k$row, c("heading", "value", "value", "value", "missing"))
  # fixture_full.yml: cat.level_prefix "- ", missing.row_label "Missing".
  expect_identical(k$label, c("Knee", "- lo", "- mid", "- hi", "Missing"))
  x <- t1[t1$var == "x" & t1$grp == "All", ]
  expect_identical(x$row, c("value", "missing"))
  expect_identical(x$label, c("Smoker", "Missing"))
  # Without the two fields, a level row has its level and a Missing row NA.
  s <- yaml::read_yaml(full_style)
  s$cat$level_prefix <- NULL
  s$missing$row_label <- NULL
  f <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(s, f)
  t1 <- table1(dl, by = "g", vars = v, style = f)
  k <- t1[t1$var == "k" & t1$grp == "All", ]
  expect_identical(k$label, c("Knee", "lo", "mid", "hi", NA))
})

test_that("with a style and a section column, one section row per group comes before the first variable of each section", {
  v <- data.table::copy(varsl)
  v$section <- c("Mother", "Mother", "", "Child")
  t1 <- table1(dl, by = "g", vars = v, style = full_style)
  groups <- c("All", "a", "b", "c")
  # Mother before x; none before z, which is in the same section; none
  # before k, which has no section; Child before u.
  sec <- which(t1$row == "section")
  expect_identical(sec, c(1:4, 41:44))
  expect_identical(t1$label[sec], rep(c("Mother", "Child"), each = 4))
  expect_identical(t1$grp[sec], rep(groups, 2))
  expect_identical(t1$var[sec], rep(NA_character_, 8))
  expect_identical(t1$n[sec], rep(NA_integer_, 8))
  expect_identical(t1$value_pretty[sec], rep(NA_character_, 8))
  expect_identical(t1$p_pretty[sec], rep(NA_character_, 8))
  expect_identical(t1$var[5], "x")
  expect_identical(t1$var[sec[5] + 4], "u")
  # The other rows are those of the output without the column section.
  t0 <- table1(dl, by = "g", vars = varsl, style = full_style)
  keep <- t1$row != "section"
  expect_identical(
    lapply(as.list(t1), function(v) return(v[keep])),
    lapply(as.list(t0), function(v) return(v))
  )
  expect_identical(attr(t1, "footnotes"), attr(t0, "footnotes"))
})

test_that("the columns label and section of vars must be text", {
  v <- data.table::copy(varsl)
  v$label <- 1:4
  expect_error(
    table1(dl, by = "g", vars = v),
    "The column label of `vars` must be text, with NA where it does not apply.",
    fixed = TRUE
  )
})
