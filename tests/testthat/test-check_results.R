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
      "manifest_scenario",
      "manifest_data",
      "manifest_source"
    )
  )
  expect_identical(found$status, rep("PASS", 7))
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

test_that("manifest_scenario passes a fake scenario and a stacked manifest", {
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$data_md5 <- NULL
  x$manifest$fake_md5 <- "0123456789abcdef0123456789abcdef"
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

# A valid results object of the fake scenario null: fake_md5 in the place of
# data_md5.
fake_results <- function() {
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$data_md5 <- NULL
  x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
  return(x)
}

test_that("manifest_data passes data_md5 for real and fake_md5 for a fake scenario", {
  expect_identical(failing(valid_results()), character(0))
  expect_identical(failing(fake_results()), character(0))
  # A real data folder with no file gives an empty data_md5.
  x <- valid_results()
  x$manifest$data_md5 <- stats::setNames(character(0), character(0))
  expect_identical(failing(x), character(0))
  # A stacked fake manifest.
  x <- fake_results()
  x$manifest$stacked <- TRUE
  x$manifest$sim <- NA_integer_
  x$manifest$sims <- 2L
  x$exclusions <- data.frame(sim = 1L, x$exclusions)
  expect_identical(failing(x), character(0))
})

test_that("manifest_data fails a manifest with neither or both of data_md5 and fake_md5", {
  x <- valid_results()
  x$manifest$data_md5 <- NULL
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    "manifest has neither data_md5 nor fake_md5"
  )

  x <- fake_results()
  x$manifest$fake_md5 <- NULL
  expect_identical(failing(x), "manifest_data")

  for (scenario in c("real", "null")) {
    x <- fake_results()
    x$manifest$scenario <- scenario
    x$manifest$data_md5 <- c(d.csv = "0123456789abcdef0123456789abcdef")
    expect_identical(failing(x), "manifest_data", info = scenario)
    expect_identical(
      check_results(x)$detail[6],
      "manifest has both data_md5 and fake_md5",
      info = scenario
    )
  }
})

test_that("manifest_data fails fake_md5 for real and data_md5 for a fake scenario", {
  x <- fake_results()
  x$manifest$scenario <- "real"
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    "manifest has fake_md5, but the scenario \"real\" needs data_md5"
  )

  # The manifest of a fake scenario that pbn 2026.11.18 wrote.
  x <- valid_results()
  x$manifest$scenario <- "null"
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    "manifest has data_md5, but the fake scenario \"null\" needs fake_md5"
  )
  # An empty data_md5 is there too.
  x$manifest$data_md5 <- character(0)
  x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
  expect_identical(failing(x), "manifest_data")
})

test_that("manifest_data fails a fake_md5 that is not one md5", {
  bad <- list(
    na = NA_character_,
    upper = "FEDCBA9876543210FEDCBA9876543210",
    short = "fedcba9876543210fedcba987654321",
    not_hex = "fedcba9876543210fedcba987654321g",
    two = rep("fedcba9876543210fedcba9876543210", 2),
    number = 1,
    empty = character(0)
  )
  for (nm in names(bad)) {
    x <- fake_results()
    x$manifest$fake_md5 <- bad[[nm]]
    expect_identical(failing(x), "manifest_data", info = nm)
    expect_identical(
      check_results(x)$detail[6],
      "manifest$fake_md5 is not one md5: a string of 32 lower-case hex digits",
      info = nm
    )
  }
})

test_that("manifest_data fails a data_md5 that is not a named vector of md5 values", {
  md5 <- "0123456789abcdef0123456789abcdef"
  bad <- list(
    no_names = md5,
    no_names_empty = character(0),
    na = c(d.csv = NA_character_),
    upper = c(d.csv = toupper(md5)),
    empty_name = stats::setNames(md5, ""),
    na_name = stats::setNames(md5, NA_character_),
    number = c(d.csv = 1)
  )
  for (nm in names(bad)) {
    x <- valid_results()
    x$manifest$data_md5 <- bad[[nm]]
    expect_identical(failing(x), "manifest_data", info = nm)
    expect_match(
      check_results(x)$detail[6],
      "manifest$data_md5 is not a named character vector of md5 values",
      fixed = TRUE,
      info = nm
    )
  }
  # Two files pass.
  x <- valid_results()
  x$manifest$data_md5 <- c(
    a.csv = md5,
    b.sav = "fedcba9876543210fedcba9876543210"
  )
  expect_identical(failing(x), character(0))
})

test_that("manifest_source passes a named source_md5 that names the run script", {
  expect_identical(failing(valid_results()), character(0))
  expect_identical(failing(fake_results()), character(0))
  x <- valid_results()
  x$manifest$source_md5 <- c(v01_run.R = "0123456789abcdef0123456789abcdef")
  expect_identical(failing(x), character(0))
  # Without a valid version, any vNN_run.R names the run script.
  x$manifest$version <- NULL
  expect_identical(failing(x), character(0))
  expect_identical(
    check_results(x)$detail[7],
    "the manifest has source_md5, with the md5 of the run script"
  )
})

test_that("manifest_source fails a missing, malformed or incomplete source_md5", {
  run <- c(v01_run.R = "0123456789abcdef0123456789abcdef")
  cases <- list(
    missing = list(
      value = NULL,
      detail = paste(
        "manifest has no source_md5. An older pbn wrote this file:",
        "run the scenarios again"
      )
    ),
    unnamed = list(value = unname(run)),
    empty = list(value = stats::setNames(character(0), character(0))),
    upper_case = list(
      value = c(v01_run.R = "0123456789ABCDEF0123456789ABCDEF")
    ),
    short = list(value = c(v01_run.R = "0123")),
    not_character = list(value = c(v01_run.R = 1)),
    na_name = list(value = stats::setNames(unname(run), NA)),
    no_run_script = list(
      value = c("v01_functions/_cfg.R" = "0123456789abcdef0123456789abcdef"),
      detail = "manifest$source_md5 does not name the run script vNN_run.R"
    ),
    other_version = list(
      value = c(v02_run.R = "0123456789abcdef0123456789abcdef"),
      detail = "manifest$source_md5 does not name the run script vNN_run.R"
    )
  )
  malformed <- paste(
    "manifest$source_md5 is not a named character vector of md5 values,",
    "each a string of 32 lower-case hex digits"
  )
  for (nm in names(cases)) {
    x <- valid_results()
    x$manifest["source_md5"] <- list(cases[[nm]]$value)
    if (is.null(cases[[nm]]$value)) {
      x$manifest$source_md5 <- NULL
    }
    expect_identical(failing(x), "manifest_source", info = nm)
    detail <- if (is.null(cases[[nm]]$detail)) malformed else cases[[nm]]$detail
    expect_identical(check_results(x)$detail[7], detail, info = nm)
  }
})
