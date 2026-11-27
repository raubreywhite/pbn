test_that("first_difference() takes two doubles as the same within a relative tolerance of sqrt(.Machine$double.eps), and nothing else", {
  est <- 0.1234567890123
  a <- list(
    fit = data.frame(term = c("a", "b"), est = c(est, -2.5), n = 1:2),
    p = 0.03
  )
  b <- a
  b$fit$est <- a$fit$est * (1 + 1e-10)
  b$p <- a$p * (1 - 1e-10)
  expect_null(first_difference(a, b, "results.qs2"))
  # A value near zero is compared on the absolute scale.
  expect_null(first_difference(c(x = 0), c(x = 1e-20), "v"))
  expect_null(first_difference(-1e-20, 1e-20, "v"))

  b$fit$est[2] <- -2.5 * (1 + 1e-6)
  expect_identical(
    first_difference(a, b, "results.qs2"),
    paste0(
      "results.qs2$fit$est[2] is -2.5 in the project and ",
      format(-2.5 * (1 + 1e-6), digits = 15),
      " in the file of pbn::to_one_file()."
    )
  )
  expect_match(first_difference(1e-6, 2e-6, "v"), "^v\\[1\\] is ")
  expect_match(first_difference(c(1, NA), c(1, 0), "v"), "^v\\[2\\] is NA ")
  expect_match(first_difference(NaN, NA_real_, "v"), "^v\\[1\\] is ")
  expect_match(first_difference(1, Inf, "v"), "^v\\[1\\] is ")
  # No tolerance for integers, characters or logicals.
  expect_match(first_difference(1L, 2L, "v"), "^v\\[1\\] is ")
  expect_match(first_difference("a", "b", "v"), "^v\\[1\\] is ")

  # Values the same within the tolerance, and other attributes.
  x <- structure(est, unit = "a")
  y <- structure(est * (1 + 1e-10), unit = "b")
  expect_identical(first_difference(x, y, "v"), "v has other attributes.")
  d <- data.frame(est = est)
  e <- data.frame(est = est * (1 + 1e-10), row.names = "r1")
  expect_identical(first_difference(d, e, "v"), "v has other attributes.")
  expect_identical(
    first_difference(x, structure(est * (1 + 1e-10), unit = "a"), "v"),
    NULL
  )
  # A numeric attribute and a double with a class MUST be identical.
  expect_identical(
    first_difference(structure(1, w = est), structure(1, w = est * (1 + 1e-10)), "v"),
    "v has other attributes."
  )
  t0 <- as.POSIXct("2026-11-27 08:00:00", tz = "UTC")
  expect_match(first_difference(t0, t0 + 1, "v"), "^v\\[1\\] is ")
  expect_match(first_difference(as.Date(20000.1), as.Date(20000.1 + 1e-10), "v"), "^v\\[1\\] is ")
})
