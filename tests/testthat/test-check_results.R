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
      "no_row_per_person",
      "manifest_scenario"
    )
  )
  expect_identical(found$status, rep("PASS", 5))
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

test_that("manifest_scenario fails when the manifest has no scenario, sim or sims", {
  for (el in c("scenario", "sim", "sims")) {
    x <- valid_results()
    x$manifest[[el]] <- NULL
    expect_identical(failing(x), "manifest_scenario", info = el)
    expect_match(
      check_results(x)$detail[5],
      paste("manifest has no", el),
      fixed = TRUE,
      info = el
    )
  }
})

test_that("manifest_scenario fails when scenario is not one string", {
  for (bad in list(1, c("real", "null"), NA_character_, character(0))) {
    x <- valid_results()
    x$manifest$scenario <- bad
    expect_identical(failing(x), "manifest_scenario")
  }
})

test_that("manifest_scenario passes a made-up scenario and a stacked manifest", {
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$sim <- NA_integer_
  x$manifest$sims <- 3L
  expect_identical(failing(x), character(0))
})

test_that("a stacked object with a sim column passes every rule", {
  x <- valid_results()
  x$manifest$stacked <- TRUE
  x$manifest$sim <- NA_integer_
  x$manifest$sims <- 2L
  x$manifest$n_rows_data <- NA_integer_
  x$exclusions <- rbind(
    data.frame(sim = 1L, x$exclusions),
    data.frame(sim = 2L, x$exclusions)
  )
  expect_identical(failing(x), character(0))

  # The same table without the stacked manifest fails.
  x$manifest$stacked <- NULL
  expect_identical(failing(x), "exclusions_table")
  expect_match(
    check_results(x)$detail[2],
    "the columns are sim, step",
    fixed = TRUE
  )
})

test_that("a stacked object without the sim column fails exclusions_table", {
  x <- valid_results()
  x$manifest$stacked <- TRUE
  expect_identical(failing(x), "exclusions_table")
})
