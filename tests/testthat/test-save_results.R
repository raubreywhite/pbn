# The results contract of save_results(): the names that the project gives,
# the name manifest for pbn, and the argument d.

# Each kind of element that a result MAY be.
many_results <- function() {
  return(list(
    p_interaction = 0.03,
    n = c(women = 21L, men = 19L),
    cor = matrix(
      c(1, 0.4, 0.4, 1),
      2,
      dimnames = list(c("a", "b"), c("a", "b"))
    ),
    by_outcome = list(
      pain = list(
        adjusted = list(
          estimates = data.frame(term = c("x", "z"), estimate = c(1.2, 0.8)),
          fit = data.frame(n = 40L, aic = 101.5)
        )
      ),
      fatigue = list(adjusted = list(estimates = data.frame(term = "x")))
    ),
    exclusions = data.frame(
      step = character(0),
      n_before = numeric(0),
      n_excluded = numeric(0),
      n_after = numeric(0)
    ),
    empty = data.frame(a = numeric(0), b = character(0)),
    # An unnamed list is allowed in a scenario with 1 simulation.
    unnamed = list(1, "a", data.frame(c = 2)),
    nothing = NULL
  ))
}

read_saved <- function(p) {
  return(qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2")))
}

test_that("save_results saves the results under the names of the call, and read_results gives each back identical", {
  p <- local_project()
  init(p)
  saved <- many_results()
  do.call(save_results, c(saved, list(d = NULL)))
  r <- read_results("v01", home = p$home)
  expect_identical(names(r), c("manifest", names(saved)))
  expect_identical(r[-1], saved)
  expect_true(is.null(r$nothing) && "nothing" %in% names(r))
  expect_identical(check_results(r)$status, rep("PASS", 7))
})

test_that("save_results stops on the name manifest, an unnamed result, a repeated or non-syntactic name, and no result", {
  p <- local_project()
  init(p)
  e <- data.frame(term = "x", estimate = 1)
  expect_error(
    save_results(manifest = list(), estimates = e, d = NULL),
    "`manifest` is the name of the element that pbn writes. Give that result another name.",
    fixed = TRUE
  )
  expect_error(
    save_results(estimates = e, e, d = NULL),
    "In this call, these elements have no name: 2.",
    fixed = TRUE
  )
  expect_error(
    save_results(e, d = NULL),
    "In this call, these elements have no name: 1.",
    fixed = TRUE
  )
  expect_error(
    save_results(estimates = e, estimates = e, d = NULL),
    "In this call, more than once: estimates.",
    fixed = TRUE
  )
  expect_error(
    save_results(`my estimates` = e, d = NULL),
    "In this call, these names are not syntactic: \"my estimates\".",
    fixed = TRUE
  )
  expect_error(
    save_results(d = NULL),
    "save_results() needs at least one named result, for example `estimates = e`.",
    fixed = TRUE
  )
  expect_identical(results_files(p), character(0))
})

test_that("save_results without d stops with the message; d = NULL saves with n_rows_data NA", {
  p <- local_project()
  init(p)
  expect_error(
    save_results(estimates = data.frame(term = "x", estimate = 1)),
    paste(
      "save_results() needs `d`. Pass the analysis data as `d = d` so pbn",
      "can refuse a table with one row for each row of the data, or pass",
      "`d = NULL` if the run has no single data table."
    ),
    fixed = TRUE
  )
  expect_identical(results_files(p), character(0))

  save_results(estimates = data.frame(term = "x", estimate = 1), d = NULL)
  r <- read_saved(p)
  expect_identical(r$manifest$n_rows_data, NA_integer_)
  expect_identical(
    check_results(r)$detail[4],
    "n_rows_data is NA, so nothing to check"
  )
})

test_that("save_results with d = d records n_rows_data, and a nested data.frame with that many rows stops it with its path", {
  p <- local_project()
  init(p)
  d <- utils::read.csv(file.path(p$data_default, "d.csv"))
  expect_identical(nrow(d), 10L)
  save_results(n_groups = 2L, d = d)
  expect_identical(read_saved(p)$manifest$n_rows_data, 10L)

  unlink(file.path(p$home, "v01_results", "real", "results.qs2"))
  expect_error(
    save_results(
      by_group = list(a = list(rows = list(people = d))),
      d = d
    ),
    paste0(
      "no_row_per_person: x$by_group$a$rows$people (a data.frame with 10 ",
      "rows) has one value for each row of the data"
    ),
    fixed = TRUE
  )
  expect_identical(results_files(p), character(0))
})

test_that("save_results stops on fitted values, one for each row of the data", {
  p <- local_project()
  init(p)
  d <- utils::read.csv(file.path(p$data_default, "d.csv"))
  fit <- stats::lm(y ~ x, data = d)
  expect_error(
    save_results(fitted = stats::fitted(fit), d = d),
    paste(
      "no_row_per_person: x$fitted (length 10) has one value for each row",
      "of the data. Save a summary of it instead."
    ),
    fixed = TRUE
  )
  expect_identical(results_files(p), character(0))
  save_results(mean_fitted = mean(stats::fitted(fit)), d = d)
  expect_identical(read_saved(p)$mean_fitted, mean(stats::fitted(fit)))
})

test_that("save_results stops on a fitted model and names its path and class", {
  p <- local_project()
  init(p)
  fit <- stats::lm(y ~ x, data = data.frame(x = 1:5, y = c(2, 4, 5, 4, 5)))
  expect_error(
    save_results(models = list(a = list(b = list(fit = fit))), d = NULL),
    "allowed_objects: not allowed: x$models$a$b$fit (class lm)",
    fixed = TRUE
  )
  expect_identical(results_files(p), character(0))
})

test_that("save_results saves without exclusions, and stops on exclusions with wrong counts", {
  p <- local_project()
  init(p)
  expect_error(
    save_results(
      exclusions = data.frame(
        step = "a",
        n_before = 10,
        n_excluded = 1,
        n_after = 10
      ),
      d = NULL
    ),
    "exclusions_table: n_before - n_excluded != n_after on these rows: 1",
    fixed = TRUE
  )
  expect_identical(results_files(p), character(0))
  save_results(flow = data.frame(stage = "screened", n = 50), d = NULL)
  expect_identical(
    check_results(read_saved(p))$detail[2],
    "there is no element exclusions, so nothing to check"
  )
})
