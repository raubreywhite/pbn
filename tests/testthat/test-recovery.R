# expect_recovers() and expect_biased(). The calibration tests run many
# repeated simulation studies of a correct normal estimator: each study is
# one group of `by`. Every number of the tests comes from the theory of the
# estimator, with its own constants, and not from the functions of pbn.

z_test <- stats::qnorm(1 - 0.0001 / 2)
z80 <- stats::qnorm(0.8)

# K studies of n simulations. Each simulation is the mean of m draws of a
# normal with SD 1 and mean `bias`, with the SE s / sqrt(m) times
# `se_factor`, and the interval mean +/- q * s / sqrt(m). With the default
# q, the interval holds the truth 0 in 95% of the simulations.
studies <- function(K, n, m, bias = 0, se_factor = 1, q = qt(0.975, m - 1)) {
  N <- K * n
  est <- stats::rnorm(N, bias, 1 / sqrt(m))
  s <- sqrt(stats::rchisq(N, m - 1) / (m - 1))
  return(data.frame(
    study = rep(seq_len(K), each = n),
    sim = rep(seq_len(n), K),
    estimate = est,
    se = s / sqrt(m) * se_factor,
    lower = est - q * s / sqrt(m),
    upper = est + q * s / sqrt(m)
  ))
}

# One study with a correct estimator, 300 simulations.
one_study <- function(n = 300, seed = 1) {
  withr::local_seed(seed)
  x <- studies(1, n, 20)
  x$study <- NULL
  return(x)
}

kind_of <- function(K) {
  return(rep(c("bias", "se", "coverage"), K))
}

test_that("z is the two-sided normal quantile of 0.0001, and each row prints it", {
  expect_equal(recovery_z, 3.890592, tolerance = 1e-6)
  out <- expect_recovers(one_study(), truth = 0)
  expect_true(all(grepl("(3.89 MCSE)", out$expected, fixed = TRUE)))
})

test_that("the Monte Carlo SE of each measure is the SD of the measure over repeated simulation studies", {
  # m = 3 gives se^2 a large variance, so the term var(se^2) of the SE of the
  # relative error matters: it is 1 / (4 n) against 1 / (2 (n - 1)).
  withr::local_seed(11)
  K <- 2000L
  n <- 300
  x <- studies(K, n, 3)
  parts <- split(x, x$study)
  st <- t(vapply(
    parts,
    function(g) {
      b <- bias_stats(g$estimate, 0)
      s <- se_stats(g$estimate, g$se)
      cv <- coverage_stats(g$lower, g$upper, 0, 0.95)
      return(c(b$bias, b$mcse, s$rel, s$mcse, cv$cover))
    },
    numeric(5)
  ))
  # With K = 2000, an SD has a relative SE of about 1 / sqrt(2 K) = 0.016.
  expect_equal(sd(st[, 1]) / mean(st[, 2]), 1, tolerance = 0.06)
  expect_equal(sd(st[, 3]) / mean(st[, 4]), 1, tolerance = 0.06)
  expect_equal(sd(st[, 5]) / sqrt(0.95 * 0.05 / n), 1, tolerance = 0.06)
  # Without the term var(se^2), the SE of the relative error is 18% too small.
  without <- mean(sqrt(1 / (2 * (n - 1))) * (1 + st[, 3]))
  expect_gt(sd(st[, 3]) / without, 1.12)
})

test_that("a correct analysis passes all three assertions in almost every repeated study", {
  withr::local_seed(1)
  K <- 2000L
  out <- expect_recovers(studies(K, 300, 20), truth = 0, by = "study")
  expect_identical(nrow(out), 3L * K)
  pass_all <- tapply(out$pass, rep(seq_len(K), each = 3), all)
  # The probability of a false alarm of one study is about 0.0006 at 300
  # simulations, so 1.2 of 2000 studies. 8 or more has a probability under
  # 0.0001. A z of 1.96 gives about 300.
  expect_lte(sum(!pass_all), 8)
})

test_that("a bias of the can-detect size fails the bias assertion in about 80% of the studies", {
  withr::local_seed(2)
  K <- 2000L
  n <- 300
  d <- (z_test + z80) / sqrt(20) / sqrt(n)
  out <- expect_recovers(studies(K, n, 20, bias = d), truth = 0, by = "study")
  fails <- mean(!out$pass[kind_of(K) == "bias"])
  # The SE of a share of 0.8 over 2000 studies is 0.009.
  expect_gt(fails, 0.74)
  expect_lt(fails, 0.86)
  # The rows report the same can-detect size, from the SD of each study.
  rows <- out$expected[kind_of(K) == "bias"]
  shown <- as.numeric(sub(".*can detect \\|bias\\| of ", "", rows))
  expect_equal(mean(shown), d, tolerance = 0.01)
})

test_that("SEs too small by the detectable ratio fail the SE assertion in about 80% of the studies", {
  withr::local_seed(3)
  K <- 2000L
  n <- 300
  m <- 20
  s0 <- sqrt(2 / (m - 1) / (4 * n) + 1 / (2 * (n - 1)))
  ratio <- 1 / ((1 + z_test * s0) * (1 + z80 * s0))
  x <- studies(K, n, m, se_factor = ratio)
  out <- expect_recovers(x, truth = 0, by = "study")
  fails <- mean(!out$pass[kind_of(K) == "se"])
  expect_gt(fails, 0.74)
  expect_lt(fails, 0.86)
  # The bias and the coverage do not depend on se.
  expect_lte(sum(!out$pass[kind_of(K) != "se"]), 8)
  # The rows report the same detectable ratio, from each study.
  rows <- out$expected[kind_of(K) == "se"]
  shown <- as.numeric(sub(".*can detect ModSE/EmpSE of ([0-9.]+) or .*", "\\1", rows))
  expect_equal(mean(shown), ratio, tolerance = 0.01)
})

test_that("under-coverage of the detectable size fails the coverage assertion in about 80% of the studies", {
  withr::local_seed(4)
  K <- 2000L
  n <- 300
  lo <- 0.95 - z_test * sqrt(0.95 * 0.05 / n)
  # The coverage p with p + z80 * sqrt(p (1 - p) / n) = lo.
  p <- stats::uniroot(
    function(p) return(p + z80 * sqrt(p * (1 - p) / n) - lo),
    c(0.5, lo),
    tol = 1e-10
  )$root
  x <- studies(K, n, 20, q = qt((1 + p) / 2, 19))
  out <- expect_recovers(x, truth = 0, by = "study")
  fails <- mean(!out$pass[kind_of(K) == "coverage"])
  expect_gt(fails, 0.74)
  expect_lt(fails, 0.86)
  rows <- out$expected[kind_of(K) == "coverage"]
  shown <- as.numeric(sub(".*can detect coverage of ", "", rows))
  expect_equal(mean(shown), p, tolerance = 0.002)
})

test_that("expect_biased passes a control with a bias of the can-detect size in about 80% of the studies, and fails an unbiased one", {
  withr::local_seed(5)
  K <- 2000L
  n <- 300
  d <- (z_test + z80) / sqrt(20) / sqrt(n)
  biased <- expect_biased(studies(K, n, 20, bias = d), truth = 0, by = "study")
  expect_identical(nrow(biased), K)
  expect_gt(mean(biased$pass), 0.74)
  expect_lt(mean(biased$pass), 0.86)
  unbiased <- expect_biased(studies(K, n, 20), truth = 0, by = "study")
  expect_lte(sum(unbiased$pass), 8)
  failed <- unbiased$observed[!unbiased$pass][1]
  expect_match(
    failed,
    "the control did not show the bias it was built to show",
    fixed = TRUE
  )
})

test_that("each row shows the observed value, the limit, the number of simulations and can detect", {
  x <- one_study()
  out <- expect_recovers(x, truth = 0)
  expect_identical(names(out), c("assertion", "expected", "observed", "pass"))
  expect_identical(
    out$assertion,
    c(
      "x: bias of estimate",
      "x: model SE of estimate against its SD",
      "x: coverage of the 95% interval"
    )
  )
  expect_true(all(out$pass))
  expect_true(all(grepl("can detect", out$expected, fixed = TRUE)))
  expect_true(all(endsWith(out$observed, "; 300 of 300 simulations")))
  b <- bias_stats(x$estimate, 0)
  expect_identical(
    out$expected[1],
    paste0(
      "|bias| <= ",
      trimws(formatC(z_test * b$mcse, digits = 3, format = "fg")),
      " (3.89 MCSE); can detect |bias| of ",
      trimws(formatC((z_test + z80) * b$mcse, digits = 3, format = "fg"))
    )
  )
  expect_identical(
    out$observed[1],
    paste0(
      trimws(formatC(b$bias, digits = 3, format = "fg")),
      "; 300 of 300 simulations"
    )
  )
  half <- z_test * sqrt(0.95 * 0.05 / 300)
  expect_match(
    out$expected[3],
    sprintf("^%.3f to %.3f \\(3.89 MCSE\\)", 0.95 - half, 0.95 + half)
  )
})

test_that("an assertion fails with too few simulations to check below 100, and below 288 for the coverage at 0.95", {
  small <- expect_recovers(one_study(10), truth = 0)
  expect_false(any(small$pass))
  expect_true(all(grepl("too few simulations to check", small$observed)))
  # The row still shows how little 10 simulations can detect.
  expect_true(all(grepl("can detect", small$expected, fixed = TRUE)))

  at_99 <- expect_recovers(one_study(99), truth = 0)
  expect_identical(
    grepl("too few simulations to check", at_99$observed),
    c(TRUE, TRUE, TRUE)
  )
  at_100 <- expect_recovers(one_study(100), truth = 0)
  expect_identical(
    grepl("too few simulations to check", at_100$observed),
    c(FALSE, FALSE, TRUE)
  )
  expect_match(at_100$observed[3], "at least 288 needed", fixed = TRUE)
  at_287 <- expect_recovers(one_study(287), truth = 0)
  expect_match(at_287$observed[3], "too few simulations to check", fixed = TRUE)
  at_288 <- expect_recovers(one_study(288), truth = 0)
  expect_true(all(at_288$pass))
  # At 288, the upper limit of the coverage is just below 1.
  expect_match(at_288$expected[3], "^0.900 to 1.000 ")
  # At the level 0.9, the coverage needs 137.
  x <- one_study(137)
  x$lower <- x$estimate - qt(0.95, 19) * x$se
  x$upper <- x$estimate + qt(0.95, 19) * x$se
  expect_true(all(expect_recovers(x, truth = 0, level = 0.9)$pass))
  x <- x[x$sim <= 136, ]
  expect_match(
    expect_recovers(x, truth = 0, level = 0.9)$observed[3],
    "too few simulations to check, at least 137 needed",
    fixed = TRUE
  )

  few <- expect_biased(data.frame(sim = 1:99, estimate = 1:99), truth = 0)
  expect_false(few$pass)
  expect_match(few$observed, "too few simulations to check", fixed = TRUE)
})

# The text after the observed value: the counts and the problems.
after_value <- function(observed) {
  return(sub("^[^;]*; ", "", observed))
}

test_that("each assertion counts the simulations that miss what it needs, and more than 5% fails it", {
  x <- one_study(320)
  x$estimate[1:16] <- NA
  ok <- expect_recovers(x, truth = 0)
  expect_true(all(ok$pass))
  expect_identical(
    after_value(ok$observed),
    c(
      "304 of 320 simulations, 16 missing",
      "304 of 320 simulations, 16 missing",
      "320 of 320 simulations"
    )
  )
  x$se[17] <- Inf
  x$lower[18:33] <- NA
  out <- expect_recovers(x, truth = 0)
  expect_identical(out$pass, c(TRUE, FALSE, TRUE))
  expect_identical(
    after_value(out$observed),
    c(
      "304 of 320 simulations, 16 missing",
      "303 of 320 simulations, 17 missing; more than 5% of the simulations missing",
      "304 of 320 simulations, 16 missing"
    )
  )
  # A missing se does not take a simulation out of the bias.
  x <- one_study(320)
  x$se[1:17] <- NA
  out <- expect_recovers(x, truth = 0)
  expect_identical(out$pass, c(TRUE, FALSE, TRUE))
  expect_identical(after_value(out$observed[1]), "320 of 320 simulations")
  # expect_biased reads the estimate only.
  x$estimate[1:16] <- NA
  b <- expect_biased(transform(x, estimate = estimate + 1), truth = 0)
  expect_true(b$pass)
  expect_identical(after_value(b$observed), "304 of 320 simulations, 16 missing")
})

test_that("an se of 0 or less, a lower above its upper, and equal estimates fail their assertion with a count", {
  x <- one_study()
  x$se[1] <- -x$se[1]
  x$se[2] <- 0
  out <- expect_recovers(x, truth = 0)
  expect_identical(out$pass, c(TRUE, FALSE, TRUE))
  expect_match(out$observed[2], "; 300 of 300 simulations; 2 simulations with se <= 0$")

  x <- one_study()
  x[3, c("lower", "upper")] <- x[3, c("upper", "lower")]
  out <- expect_recovers(x, truth = 0)
  expect_identical(out$pass, c(TRUE, TRUE, FALSE))
  expect_match(out$observed[3], "; 300 of 300 simulations; 1 simulation with lower > upper$")

  x <- one_study()
  x$estimate <- 0
  out <- expect_recovers(x, truth = 0)
  expect_identical(out$pass, c(FALSE, FALSE, TRUE))
  expect_true(all(grepl(
    "; all estimates are equal, so the Monte Carlo SE is 0",
    out$observed[1:2],
    fixed = TRUE
  )))
  expect_false(any(grepl("Inf|NaN", unlist(out[1:2, 2:3]))))
  b <- expect_biased(transform(x, estimate = 1), truth = 0)
  expect_false(b$pass)
  expect_match(b$observed, "all estimates are equal", fixed = TRUE)

  # With fewer than 2 usable rows, the SE row still counts the invalid se.
  one <- data.frame(sim = 1, estimate = 0.1, se = -0.2, lower = 0, upper = 1)
  out <- expect_recovers(one, truth = 0)
  expect_false(out$pass[2])
  expect_match(out$observed[2], "; 1 simulation with se <= 0", fixed = TRUE)
})

test_that("by gives three assertions for each group, with one truth or a truth for each group", {
  withr::local_seed(7)
  a <- studies(1, 300, 20)
  b <- studies(1, 300, 20, bias = 1)
  x <- rbind(cbind(term = "a", a), cbind(term = "b", b))
  x$study <- NULL
  truth <- data.frame(term = c("b", "a"), truth = c(1, 0))
  out <- expect_recovers(x, truth = truth, by = "term")
  expect_identical(nrow(out), 6L)
  expect_true(all(out$pass))
  expect_identical(
    sub(":.*", "", out$assertion),
    rep(c("x, term = a", "x, term = b"), each = 3)
  )
  one <- expect_recovers(x, truth = 0, by = "term")
  expect_identical(one$pass, c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))
  control <- expect_biased(x, truth = 0, by = "term")
  expect_identical(control$pass, c(FALSE, TRUE))
})

test_that("expect_recovers and expect_biased stop on input they cannot check", {
  x <- one_study()
  stops <- list(
    "`x` must be a data.frame" = list(x = as.list(x)),
    "`x` has no column sim" = list(x = x[names(x) != "sim"]),
    "`x` has no column se" = list(x = x[names(x) != "se"]),
    "must be numeric: estimate" = list(x = transform(x, estimate = "a")),
    "more than one row for one simulation" = list(x = rbind(x, x)),
    "`truth` must be one number." = list(x = x, truth = c(0, 1)),
    "`truth` must be one number, or a data.frame" = list(
      x = cbind(x, term = "a"),
      truth = "a",
      by = "term"
    ),
    "`truth` has no row for these groups of `x`: b" = list(
      x = rbind(cbind(x, term = "a"), cbind(x, term = "b")),
      truth = data.frame(term = "a", truth = 0),
      by = "term"
    ),
    "`truth` has rows for groups that are not in `x`: c" = list(
      x = cbind(x, term = "a"),
      truth = data.frame(term = c("a", "c"), truth = 0),
      by = "term"
    ),
    "`truth` has more than one row for one group" = list(
      x = cbind(x, term = "a"),
      truth = data.frame(term = c("a", "a"), truth = 0),
      by = "term"
    ),
    "The column term of `x` has a missing value" = list(
      x = rbind(cbind(x, term = "a"), cbind(x, term = NA)),
      by = "term"
    ),
    "The column term of `truth` has a missing value" = list(
      x = cbind(x, term = "a"),
      truth = data.frame(term = c("a", NA), truth = 0),
      by = "term"
    ),
    "`level` must be one number between 0 and 1" = list(
      x = x,
      level = 95
    )
  )
  for (msg in names(stops)) {
    args <- stops[[msg]]
    if (is.null(args$truth)) {
      args$truth <- 0
    }
    expect_error(do.call(expect_recovers, args), msg, fixed = TRUE)
  }
  expect_error(
    expect_biased(x[names(x) != "sim"], truth = 0),
    "`x` has no column sim",
    fixed = TRUE
  )
  # A data.table works as a data.frame.
  expect_identical(
    expect_recovers(data.table::as.data.table(x), truth = 0)$pass,
    expect_recovers(x, truth = 0)$pass
  )
})

test_that("an expect_ function built from expect_recovers fails a scenario with 3 simulations in save_results", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/expect_null.R",
    c(
      "expect_null <- function(results) {",
      "  m <- results$models[results$models$term == \"gb\", ]",
      "  m$se <- 1",
      "  m$lower <- m$estimate - 2",
      "  m$upper <- m$estimate + 2",
      "  pbn::expect_recovers(m, truth = 0, by = \"outcome\")",
      "}"
    )
  )
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  printed <- utils::capture.output(
    expect_error(save_results(r, plan, sc), "These assertions failed")
  )
  check <- utils::read.csv(file.path(
    p$home,
    "v01_results",
    "null",
    "check.csv"
  ))
  expect_identical(nrow(check), 6L)
  expect_false(any(check$pass))
  expect_true(all(grepl("too few simulations to check", check$observed)))
  expect_true(any(grepl("too few simulations to check", printed)))
})
