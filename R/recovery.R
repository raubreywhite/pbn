# The recovery checks of a fake scenario: expect_recovers() and
# expect_biased(). The Monte Carlo standard errors and their checks by
# simulation are in tests/testthat/test-recovery.R.

# One z for every assertion: the two-sided normal quantile of 0.0001.
recovery_z <- stats::qnorm(1 - 0.0001 / 2)
# The normal quantile of 0.8. A check finds an error of the size "can detect"
# with a probability of 80%.
power_z <- stats::qnorm(0.8)
# The fewest usable simulations for a check of the bias or the standard error.
min_sims <- 100L
# The largest share of simulations with a missing value that a check allows.
max_missing <- 0.05

#' Check that an analysis recovers the truth of a fake scenario
#'
#' `expect_recovers()` checks the bias, the standard error and the coverage of
#' an estimate over the simulations of a fake scenario. It returns the
#' assertions in the format of an `expect_<scenario>()` function.
#'
#' @param x The stacked results of one analysis kind, for example
#'   `results$models`. It is a data.frame with the column `sim` that
#'   [save_results()] adds. It has one row for each simulation, or for each
#'   simulation and group of `by`.
#' @param truth The true value of the estimate. One number for every group,
#'   or a data.frame with the columns of `by` and a column `truth`, with one
#'   row for each group.
#' @param estimate,se,lower,upper The names of the columns of `x` that hold
#'   the estimate, its standard error and the limits of its confidence
#'   interval.
#' @param by `NULL`, or the names of the columns of `x` that split it into
#'   groups, for example `"term"`. Each group gets its own three assertions.
#' @param level The level of the confidence interval of `lower` and `upper`.
#' @details
#' The checks work on the scale of the columns. Give a log odds ratio, its
#' standard error and its limits on the log scale, with a truth on the log
#' scale. Do not give odds ratios. On the scale of the odds ratio, the
#' estimate is biased, and the standard error is that of the log.
#'
#' For each group and assertion, `n` is the number of simulations with the
#' values that the assertion needs. The bias needs a finite `estimate`, the
#' standard error a finite `estimate` and `se`, and the coverage a finite
#' `lower` and `upper`. The empirical SE is `sd(estimate)` over these `n`
#' simulations. `z` is 3.89, the two-sided normal quantile of 0.0001. The
#' three assertions are:
#' 1. Bias: `mean(estimate) - truth`. Its Monte Carlo SE is
#'    `sd(estimate) / sqrt(n)`. The assertion passes when
#'    `|bias| <= z * MCSE`.
#' 2. Standard error: the relative error `ModSE / EmpSE - 1` of the model SE
#'    `ModSE = sqrt(mean(se^2))` against the empirical SE `EmpSE`. Its Monte
#'    Carlo SE is `(ModSE / EmpSE) * sqrt(var(se^2) / (4 * n * ModSE^4) +
#'    1 / (2 * (n - 1)))`. The assertion passes when
#'    `|ModSE / EmpSE - 1| <= z * MCSE`.
#' 3. Coverage: the share of simulations where `lower <= truth <= upper`. The
#'    assertion passes when the share is within `z * sqrt(level * (1 - level)
#'    / n)` of `level`.
#'
#' Morris, White and Crowther (2019), Table 6, define these three performance
#' measures and give a Monte Carlo SE for each. The SE of the relative error
#' is a delta-method approximation. It assumes that the estimate is normal,
#' and that `se` is independent of the estimate. The SE of the coverage uses
#' `level`, not the observed share. Under a correct analysis the true share
#' is `level`, and the observed share is 1 in many small studies, which gives
#' an SE of 0.
#'
#' Each row shows the observed value, the limit and the number of
#' simulations. It also shows "can detect": the smallest error that the
#' assertion finds with a probability of 80% at this number of simulations.
#' For the bias, this is `(z + 0.84) * sd(estimate) / sqrt(n)`. For the
#' standard error, it is the ratio `ModSE / EmpSE` below and above 1. For the
#' coverage, it is the true coverage below `level`. A large "can detect"
#' value means that the check passes almost anything.
#' @section The fewest simulations:
#' An assertion fails with "too few simulations to check" when `n` is below:
#' - 100 for the bias and the standard error. The bias uses `sd(estimate)`,
#'   so its false-alarm probability is that of a t distribution. At 100
#'   simulations it is 0.00018. At 30 it is 0.00054.
#' - for the coverage, the smallest `n` that puts both limits inside 0 and 1,
#'   and at least 100. The limits are
#'   `level +/- z * sqrt(level * (1 - level) / n)`. This is 288 for the level
#'   0.95, and 137 for 0.9.
#'
#' An assertion also fails when more than 5% of the simulations of the
#' group miss a value that it needs. Each row
#' shows its own number of missing simulations, also when it passes.
#' Simulations where the model failed are not missing at random, so a pass
#' with missing simulations is weaker.
#'
#' An assertion fails on a value that cannot be right, and its row gives the
#' count:
#' - the standard error, on an `se` of 0 or less.
#' - the coverage, on a `lower` above its `upper`.
#' - the bias and the standard error, when all estimates are equal. The
#'   Monte Carlo SE is then 0.
#' @section The false alarms:
#' A false alarm is a failed assertion for a correct analysis. It stops the
#' run, and the same seeds give it again in the next run. So `z` is large.
#' For each assertion the probability is about 0.0001. For one call with
#' `g` groups and 3 assertions in each group, it is about
#' `1 - (1 - 0.0001)^(3 * g)`:
#'
#' | Groups | Assertions | False alarm of the call |
#' |---|---|---|
#' | 1 | 3 | 0.0003 |
#' | 6 | 18 | 0.0018 |
#' | 20 | 60 | 0.0060 |
#'
#' The real probabilities are a little larger, because the bias is a t
#' statistic and the coverage is a binomial count. At 300 simulations, the
#' bias has 0.00012 and the coverage at the level 0.95 has 0.00028. There is
#' no option to change `z`.
#' @references Morris TP, White IR, Crowther MJ. Using simulation studies to
#'   evaluate statistical methods. Statistics in Medicine. 2019;38:2074-2102.
#'   \doi{10.1002/sim.8086}
#' @return A data.frame with the columns `assertion`, `expected`, `observed`
#'   and `pass`, and three rows for each group. Give it to [rbind()] with the
#'   other assertions of the `expect_<scenario>()` function.
#' @examples
#' set.seed(1)
#' x <- data.frame(sim = 1:300, estimate = rnorm(300, 0.5, 0.2), se = 0.2)
#' x$lower <- x$estimate - 1.96 * x$se
#' x$upper <- x$estimate + 1.96 * x$se
#' pbn::expect_recovers(x, truth = 0.5)
#' @family checks of fake scenarios
#' @seealso [expect_biased()] for a control analysis, and `vignette("pbn")`.
#' @export
expect_recovers <- function(
  x,
  truth,
  estimate = "estimate",
  se = "se",
  lower = "lower",
  upper = "upper",
  by = NULL,
  level = 0.95
) {
  label <- recovery_label(substitute(x))
  check_level(level)
  columns <- c(estimate = estimate, se = se, lower = lower, upper = upper)
  groups <- recovery_groups(x, truth, columns, by)
  rows <- lapply(groups, function(g) {
    v <- g$values
    prefix <- paste0(label, g$label, ": ")
    return(rbind(
      bias_row(
        usable(v, "estimate"),
        g$truth,
        paste0(prefix, "bias of ", estimate)
      ),
      se_row(
        usable(v, c("estimate", "se")),
        paste0(prefix, "model SE of ", estimate, " against its SD")
      ),
      coverage_row(
        usable(v, c("lower", "upper")),
        g$truth,
        level,
        paste0(prefix, "coverage of the ", 100 * level, "% interval")
      )
    ))
  })
  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  return(out)
}

#' Check that a control analysis shows its bias
#'
#' `expect_biased()` checks that the estimate of a control analysis is biased
#' over the simulations of a fake scenario. Use it for an analysis that MUST
#' be wrong, for example a crude model in a scenario with a confounder.
#'
#' @inheritParams expect_recovers
#' @param estimate The name of the column of `x` that holds the estimate.
#' @details
#' A control analysis shows that the fake scenario can find the error it was
#' built for. When a crude model is not biased, the confounder of the
#' scenario is too weak, and the recovery of the adjusted model shows
#' nothing.
#'
#' For each group, `n` is the number of simulations with a finite
#' `estimate`. The bias is `mean(estimate) - truth`, and its Monte Carlo SE is
#' `sd(estimate) / sqrt(n)`. The assertion passes when `|bias| > z * MCSE`,
#' with the `z` of [expect_recovers()], 3.89. Otherwise it fails, and the row
#' says that the control did not show the bias it was built to show. The row
#' shows the "can detect" value of [expect_recovers()]: the smallest bias that
#' the assertion finds with a probability of 80%.
#'
#' Below 100 simulations, the assertion fails with "too few simulations to
#' check". It also fails when more than 5% of the simulations have a missing
#' or infinite estimate, and when all estimates are equal.
#' @return A data.frame with the columns `assertion`, `expected`, `observed`
#'   and `pass`, and one row for each group.
#' @examples
#' set.seed(1)
#' x <- data.frame(sim = 1:300, estimate = rnorm(300, 0.9, 0.2))
#' pbn::expect_biased(x, truth = 0.5)
#' @family checks of fake scenarios
#' @seealso `vignette("pbn")`, which checks a crude model in a scenario with a
#'   confounder.
#' @export
expect_biased <- function(x, truth, estimate = "estimate", by = NULL) {
  label <- recovery_label(substitute(x))
  groups <- recovery_groups(x, truth, c(estimate = estimate), by)
  rows <- lapply(groups, function(g) {
    v <- usable(g$values, "estimate")
    return(biased_row(
      v,
      g$truth,
      paste0(label, g$label, ": bias of ", estimate, ", a control")
    ))
  })
  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  return(out)
}

check_level <- function(level) {
  ok <- is.numeric(level) && length(level) == 1 && !is.na(level)
  if (!ok || level <= 0 || level >= 1) {
    stop("`level` must be one number between 0 and 1.", call. = FALSE)
  }
  return(invisible(level))
}

# The expression of `x` as text for the assertions, for example
# "results$models". A value, for example from do.call(), gives "x".
recovery_label <- function(expr) {
  if (is.symbol(expr) || is.call(expr)) {
    return(deparse1(expr))
  }
  return("x")
}

# The groups of `x`: for each, its label, its truth, its number of rows, and
# the values of the `columns` as a named list. Stops on input that the
# checks cannot read.
recovery_groups <- function(x, truth, columns, by) {
  x <- check_recovery_input(x, columns, by)
  key <- group_key(x, by)
  if (anyDuplicated(paste(x$sim, key, sep = "\r")) > 0) {
    stop(
      "`x` has more than one row for one simulation",
      if (!is.null(by)) " and group",
      ". Give `by` the columns that tell these rows apart.",
      call. = FALSE
    )
  }
  keys <- unique(key)
  truths <- group_truths(truth, by, keys)
  index <- split(seq_len(nrow(x)), factor(key, levels = keys))
  out <- lapply(seq_along(keys), function(i) {
    rows <- index[[i]]
    label <- ""
    if (!is.null(by)) {
      at <- vapply(by, function(b) return(as.character(x[[b]][rows[1]])), "")
      label <- paste0(", ", paste(by, "=", at, collapse = ", "))
    }
    values <- lapply(columns, function(col) return(x[[col]][rows]))
    names(values) <- names(columns)
    return(list(label = label, truth = truths[i], values = values))
  })
  return(out)
}

# `x` as a data.frame. Stops when it is not stacked results with the
# numeric `columns` and the columns `by`.
check_recovery_input <- function(x, columns, by) {
  if (!is.data.frame(x)) {
    stop(
      "`x` must be a data.frame of stacked results, for example ",
      "results$models.",
      call. = FALSE
    )
  }
  x <- as.data.frame(x)
  if (!"sim" %in% names(x)) {
    stop(
      "`x` has no column sim. Give it the stacked results of a scenario ",
      "with more than 1 simulation.",
      call. = FALSE
    )
  }
  if (!is.null(by) && (!is.character(by) || anyNA(by) || "sim" %in% by)) {
    stop(
      "`by` must be NULL or names of columns of `x` other than sim.",
      call. = FALSE
    )
  }
  absent <- setdiff(c(columns, by), names(x))
  if (length(absent) > 0) {
    stop(
      "`x` has no column ",
      paste(absent, collapse = ", "),
      ". Its columns are: ",
      paste(names(x), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  check_no_missing_group(x, by, "`x`")
  numeric_ok <- vapply(
    columns,
    function(col) return(is.numeric(x[[col]])),
    TRUE
  )
  if (!all(numeric_ok)) {
    stop(
      "These columns of `x` must be numeric: ",
      paste(columns[!numeric_ok], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(x)
}

# Stops when a column `by` of `x` has a missing value, which names no group.
check_no_missing_group <- function(x, by, what) {
  for (b in by) {
    if (anyNA(x[[b]])) {
      stop(
        "The column ",
        b,
        " of ",
        what,
        " has a missing value, so it names no group. Give each row a value.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# One key for each row of `x`, from the columns `by`. One key for all rows
# when `by` is NULL.
group_key <- function(x, by) {
  if (is.null(by)) {
    return(rep("", nrow(x)))
  }
  return(do.call(paste, c(lapply(x[by], as.character), sep = "\r")))
}

# The truth of each group of `keys`.
group_truths <- function(truth, by, keys) {
  if (is.numeric(truth) && length(truth) == 1 && is.finite(truth)) {
    return(rep(truth, length(keys)))
  }
  if (!is.data.frame(truth) || is.null(by)) {
    stop(
      "`truth` must be one number",
      if (!is.null(by)) ", or a data.frame with the columns of `by` and truth",
      ".",
      call. = FALSE
    )
  }
  truth <- check_truth_frame(truth, by)
  truth_key <- group_key(truth, by)
  show <- function(k) return(gsub("\r", ", ", k, fixed = TRUE))
  no_truth <- setdiff(keys, truth_key)
  if (length(no_truth) > 0) {
    stop(
      "`truth` has no row for these groups of `x`: ",
      paste(show(no_truth), collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  no_group <- setdiff(truth_key, keys)
  if (length(no_group) > 0) {
    stop(
      "`truth` has rows for groups that are not in `x`: ",
      paste(show(no_group), collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  return(truth$truth[match(keys, truth_key)])
}

# `truth` as a data.frame. Stops when it does not have the columns `by` and
# truth, one row for each group and a finite truth on each row.
check_truth_frame <- function(truth, by) {
  truth <- as.data.frame(truth)
  absent <- setdiff(c(by, "truth"), names(truth))
  if (length(absent) > 0) {
    stop(
      "`truth` has no column ",
      paste(absent, collapse = ", "),
      ". It needs the columns of `by` and truth.",
      call. = FALSE
    )
  }
  if (!is.numeric(truth$truth) || !all(is.finite(truth$truth))) {
    stop("The column truth of `truth` must hold finite numbers.", call. = FALSE)
  }
  check_no_missing_group(truth, by, "`truth`")
  if (anyDuplicated(group_key(truth, by)) > 0) {
    stop("`truth` has more than one row for one group.", call. = FALSE)
  }
  return(truth)
}

# The values `need` of the simulations where each of them is finite, with
# `n_all`, the number of simulations of the group.
usable <- function(values, need) {
  ok <- Reduce(`&`, lapply(values[need], is.finite))
  out <- lapply(values[need], function(v) return(v[ok]))
  out$n_all <- length(ok)
  out$n <- sum(ok)
  return(out)
}

# The bias of `est` against `truth`, its Monte Carlo SE, and the smallest
# bias that a check finds with a probability of 80%.
bias_stats <- function(est, truth) {
  n <- length(est)
  emp <- stats::sd(est)
  mcse <- emp / sqrt(n)
  return(list(
    bias = mean(est) - truth,
    mcse = mcse,
    detect = (recovery_z + power_z) * mcse
  ))
}

# The relative error ModSE / EmpSE - 1, its Monte Carlo SE, and the ratios
# ModSE / EmpSE below and above 1 that a check finds with a probability of
# 80%.
se_stats <- function(est, se) {
  n <- length(est)
  emp <- stats::sd(est)
  v <- se^2
  mod <- sqrt(mean(v))
  ratio <- mod / emp
  # The relative SD of ModSE / EmpSE: the delta method for sqrt(mean(se^2)),
  # and for sd(estimate) of a normal estimate, independent of each other.
  s0 <- sqrt(stats::var(v) / (4 * n * mod^4) + 1 / (2 * (n - 1)))
  low <- 1 / ((1 + recovery_z * s0) * (1 + power_z * s0))
  high <- if (recovery_z * s0 < 1 && power_z * s0 < 1) {
    1 / ((1 - recovery_z * s0) * (1 - power_z * s0))
  } else {
    Inf
  }
  return(list(
    mod = mod,
    emp = emp,
    rel = ratio - 1,
    mcse = ratio * s0,
    detect_low = low,
    detect_high = high
  ))
}

# The coverage of the intervals, its limits around `level`, and the true
# coverage below `level` that a check finds with a probability of 80%.
coverage_stats <- function(lower, upper, truth, level) {
  n <- length(lower)
  half <- recovery_z * sqrt(level * (1 - level) / n)
  lo <- level - half
  # The coverage p where p + power_z * sqrt(p * (1 - p) / n) = lo: the
  # smaller root of (1 + k) p^2 - (2 lo + k) p + lo^2 = 0, k = power_z^2 / n.
  k <- power_z^2 / n
  b <- 2 * lo + k
  detect <- (b - sqrt(b^2 - 4 * (1 + k) * lo^2)) / (2 * (1 + k))
  return(list(
    cover = mean(lower <= truth & truth <= upper),
    lo = lo,
    hi = level + half,
    detect = detect
  ))
}

# The fewest usable simulations for a check of the coverage at `level`.
min_sims_coverage <- function(level) {
  need <- recovery_z^2 * level * (1 - level) / min(level, 1 - level)^2
  return(max(min_sims, as.integer(ceiling(need - 1e-9))))
}

# The number of usable simulations of `v`, of all, and of the missing.
count_text <- function(v) {
  missing <- v$n_all - v$n
  return(paste0(
    v$n,
    " of ",
    v$n_all,
    " simulations",
    if (missing > 0) paste0(", ", missing, " missing")
  ))
}

# The reason that an assertion that needs `need` usable simulations cannot
# pass, or NULL.
gate_text <- function(v, need) {
  if (v$n < need) {
    return(paste0("too few simulations to check, at least ", need, " needed"))
  }
  if ((v$n_all - v$n) / v$n_all > max_missing) {
    return(paste0(
      "more than ",
      100 * max_missing,
      "% of the simulations missing"
    ))
  }
  return(NULL)
}

fmt <- function(x) {
  return(trimws(formatC(x, digits = 3, format = "fg")))
}

# The problem of estimates that are all equal, or NULL.
equal_problem <- function(est) {
  if (stats::sd(est) > 0) {
    return(NULL)
  }
  return("all estimates are equal, so the Monte Carlo SE is 0")
}

# The problem of `bad` values that cannot be right, or NULL.
count_problem <- function(bad, what) {
  k <- sum(bad)
  if (k == 0) {
    return(NULL)
  }
  return(paste0(k, if (k == 1) " simulation" else " simulations", " with ", what))
}

z_text <- function() {
  return(paste0(" (", sprintf("%.2f", recovery_z), " MCSE)"))
}

assertion <- function(assertion, expected, observed, pass) {
  return(data.frame(
    assertion = assertion,
    expected = expected,
    observed = observed,
    pass = pass
  ))
}

# One assertion, with the observed text, the gate, the problems and the pass
# of the limit. The gate and each problem fail the assertion whatever the
# limit says.
gated <- function(name, expected, value, v, need, pass, problems = NULL) {
  problems <- c(gate_text(v, need), problems)
  observed <- paste(c(value, count_text(v), problems), collapse = "; ")
  return(assertion(name, expected, observed, isTRUE(pass) && is.null(problems)))
}

bias_row <- function(v, truth, name) {
  if (v$n < 2) {
    return(gated(name, "a bias within the limit", "NA", v, min_sims, FALSE))
  }
  s <- bias_stats(v$estimate, truth)
  limit <- recovery_z * s$mcse
  expected <- paste0(
    "|bias| <= ",
    fmt(limit),
    z_text(),
    "; can detect |bias| of ",
    fmt(s$detect)
  )
  pass <- abs(s$bias) <= limit
  problem <- equal_problem(v$estimate)
  return(gated(name, expected, fmt(s$bias), v, min_sims, pass, problem))
}

se_row <- function(v, name) {
  invalid <- count_problem(v$se <= 0, "se <= 0")
  if (v$n < 2) {
    return(gated(
      name,
      "a relative error within the limit",
      "NA",
      v,
      min_sims,
      FALSE,
      invalid
    ))
  }
  problems <- c(equal_problem(v$estimate), invalid)
  if (!is.null(equal_problem(v$estimate))) {
    value <- paste0("EmpSE 0, ModSE ", fmt(sqrt(mean(v$se^2))))
    return(gated(name, "estimates that vary", value, v, min_sims, FALSE, problems))
  }
  s <- se_stats(v$estimate, v$se)
  limit <- recovery_z * s$mcse
  expected <- paste0(
    "|relative error| <= ",
    sprintf("%.1f%%", 100 * limit),
    z_text(),
    "; can detect ModSE/EmpSE of ",
    fmt(s$detect_low),
    " or ",
    fmt(s$detect_high)
  )
  value <- paste0(
    sprintf("%+.1f%%", 100 * s$rel),
    " (ModSE ",
    fmt(s$mod),
    ", EmpSE ",
    fmt(s$emp),
    ")"
  )
  pass <- abs(s$rel) <= limit
  return(gated(name, expected, value, v, min_sims, pass, problems))
}

coverage_row <- function(v, truth, level, name) {
  need <- min_sims_coverage(level)
  if (v$n < 1) {
    return(gated(name, "a coverage within the limits", "NA", v, need, FALSE))
  }
  s <- coverage_stats(v$lower, v$upper, truth, level)
  expected <- paste0(
    sprintf("%.3f to %.3f", s$lo, s$hi),
    z_text(),
    "; can detect coverage of ",
    sprintf("%.3f", s$detect)
  )
  pass <- s$cover >= s$lo && s$cover <= s$hi
  problem <- count_problem(v$lower > v$upper, "lower > upper")
  value <- sprintf("%.3f", s$cover)
  return(gated(name, expected, value, v, need, pass, problem))
}

biased_row <- function(v, truth, name) {
  if (v$n < 2) {
    return(gated(name, "a bias beyond the limit", "NA", v, min_sims, FALSE))
  }
  s <- bias_stats(v$estimate, truth)
  limit <- recovery_z * s$mcse
  expected <- paste0(
    "|bias| > ",
    fmt(limit),
    z_text(),
    "; can detect |bias| of ",
    fmt(s$detect)
  )
  pass <- abs(s$bias) > limit
  problem <- equal_problem(v$estimate)
  out <- gated(name, expected, fmt(s$bias), v, min_sims, pass, problem)
  if (!pass && is.null(gate_text(v, min_sims)) && is.null(problem)) {
    out$observed <- paste0(
      out$observed,
      "; the control did not show the bias it was built to show"
    )
  }
  return(out)
}
