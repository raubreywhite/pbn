valid_results <- function() {
  return(list(
    manifest = list(version = "v01", n_rows_data = 40L),
    exclusions = data.frame(
      step = c("age 18 or more", "no missing outcome"),
      n_before = c(50, 45),
      n_excluded = c(5, 5),
      n_after = c(45, 40)
    ),
    estimates = data.frame(term = c("a", "b"), estimate = c(0.1, 0.2)),
    tables = list(counts = data.frame(grp = c("a", "b"), n = c(20, 20))),
    extra = list(note = "synthetic")
  ))
}

failing <- function(x) {
  found <- check_results(x)
  return(found$rule[found$status == "FAIL"])
}

test_that("check_results passes a valid object on every rule", {
  found <- check_results(valid_results())
  expect_identical(
    found$rule,
    c(
      "top_level_names",
      "exclusions_table",
      "no_fitted_models",
      "no_row_per_person"
    )
  )
  expect_identical(found$status, rep("PASS", 4))
  expect_identical(names(found), c("rule", "status", "detail"))
})

test_that("check_results passes an exclusions table with zero rows", {
  x <- valid_results()
  x$exclusions <- x$exclusions[0, ]
  expect_identical(failing(x), character(0))
})

test_that("top_level_names fails on an extra or a missing name", {
  x <- valid_results()
  x$plots <- list()
  expect_identical(failing(x), "top_level_names")

  x <- valid_results()
  x$extra <- NULL
  expect_identical(failing(x), "top_level_names")
})

test_that("exclusions_table fails on a missing column", {
  x <- valid_results()
  x$exclusions$n_after <- NULL
  expect_identical(failing(x), "exclusions_table")
})

test_that("exclusions_table fails when the counts do not add up", {
  x <- valid_results()
  x$exclusions$n_after[2] <- 41
  expect_identical(failing(x), "exclusions_table")
  expect_match(
    check_results(x)$detail[2],
    "n_before - n_excluded != n_after on these rows: 2",
    fixed = TRUE
  )
})

test_that("exclusions_table fails when exclusions is not a data.frame", {
  x <- valid_results()
  x$exclusions <- list(step = "a")
  expect_identical(failing(x), "exclusions_table")
})

test_that("no_fitted_models fails on a model anywhere in the object", {
  fit_data <- data.frame(u = 1:5, w = c(2, 4, 5, 4, 5))
  x <- valid_results()
  x$extra$deep <- list(fit = stats::lm(w ~ u, data = fit_data))
  expect_identical(failing(x), "no_fitted_models")
  expect_match(check_results(x)$detail[3], "x$extra$deep$fit", fixed = TRUE)

  x <- valid_results()
  x$tables$fit <- stats::glm(w ~ u, data = fit_data)
  expect_identical(failing(x), "no_fitted_models")
})

test_that("no_row_per_person fails on a data.frame with n_rows_data rows", {
  x <- valid_results()
  x$tables$people <- data.frame(id = 1:40)
  expect_identical(failing(x), "no_row_per_person")
  expect_match(check_results(x)$detail[4], "x$tables$people", fixed = TRUE)
})

test_that("no_row_per_person checks nothing when n_rows_data is NA", {
  x <- valid_results()
  x$tables$people <- data.frame(id = 1:40)
  x$manifest$n_rows_data <- NA_integer_
  expect_identical(failing(x), character(0))
})
