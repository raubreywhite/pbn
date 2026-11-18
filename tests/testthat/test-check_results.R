failing <- function(x) {
  found <- check_results(x)
  return(found$rule[found$status == "FAIL"])
}

test_that("check_results passes a valid object on every rule", {
  found <- check_results(valid_results())
  expect_identical(
    found$rule,
    c(
      "element_names",
      "exclusions_table",
      "allowed_objects",
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

test_that("element_names passes any unique syntactic names besides manifest", {
  x <- valid_results()
  x <- c(x["manifest"], list(p_value = 0.03, counts = c(a = 1L, b = 2L)))
  expect_identical(failing(x), character(0))
  expect_identical(
    check_results(x)$detail[1],
    "manifest, and each other element with a unique syntactic name"
  )
})

test_that("element_names fails a missing or second manifest, no other element, a missing, non-syntactic or repeated name", {
  m <- valid_results()["manifest"]
  cases <- list(
    no_manifest = list(
      x = list(estimates = data.frame(a = 1)),
      detail = "manifest MUST be there exactly once"
    ),
    two_manifests = list(
      x = c(m, m, list(a = 1)),
      detail = "manifest MUST be there exactly once; more than once: manifest"
    ),
    manifest_only = list(
      x = m,
      detail = "there is no element other than manifest"
    ),
    unnamed = list(
      x = c(m, list(a = 1, 2)),
      detail = "these elements have no name: 3"
    ),
    not_syntactic = list(
      x = c(m, list(`my table` = data.frame(a = 1), `if` = 1)),
      detail = "these names are not syntactic: \"my table\", \"if\""
    ),
    twice = list(
      x = c(m, list(a = 1, a = 2)),
      detail = "more than once: a"
    ),
    not_a_list = list(
      x = data.frame(a = 1),
      detail = "the object is not a list"
    )
  )
  for (nm in names(cases)) {
    found <- check_results(cases[[nm]]$x)
    expect_true("element_names" %in% failing(cases[[nm]]$x), info = nm)
    expect_identical(found$detail[1], cases[[nm]]$detail, info = nm)
  }
})

test_that("exclusions_table passes and says so when there is no element exclusions", {
  x <- valid_results()
  x$exclusions <- NULL
  expect_identical(failing(x), character(0))
  expect_identical(
    check_results(x)$detail[2],
    "there is no element exclusions, so nothing to check"
  )
  # Any other data.frame MAY have any columns.
  x$flow <- data.frame(stage = "screened", n = 50)
  expect_identical(failing(x), character(0))
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

test_that("allowed_objects fails each disallowed object with its path and class", {
  fit_data <- data.frame(u = 1:5, w = c(2, 4, 5, 4, 5))
  bad <- list(
    lm = list(value = stats::lm(w ~ u, data = fit_data), class = "lm"),
    glm = list(value = stats::glm(w ~ u, data = fit_data), class = "glm/lm"),
    fn = list(value = function(x) return(x), class = "function"),
    env = list(value = new.env(), class = "environment"),
    classed_list = list(
      value = structure(list(a = 1), class = "my_summary"),
      class = "my_summary"
    ),
    formula = list(value = w ~ u, class = "formula"),
    # asS4() sets the S4 bit, as on an object of an S4 class that contains
    # numeric. Without the bit, this vector passes.
    s4 = list(
      value = asS4(structure(c(1, 2), class = "fake_s4")),
      class = "fake_s4"
    ),
    # An object of an S4 class, such as a merMod fit, has the type S4.
    s4_type = list(
      value = methods::setClass(
        "pbn_fake_fit",
        representation(beta = "numeric"),
        where = new.env()
      )(),
      class = "pbn_fake_fit"
    )
  )
  for (nm in names(bad)) {
    x <- valid_results()
    x$extra$item <- bad[[nm]]$value
    expect_identical(failing(x), "allowed_objects", info = nm)
    expect_match(
      check_results(x)$detail[3],
      paste0("not allowed: x$extra$item (class ", bad[[nm]]$class, ")"),
      fixed = TRUE,
      info = nm
    )
    advice <- if (nm %in% c("lm", "glm")) {
      "save its table of coefficients as a data.frame"
    } else if (nm == "formula") {
      "save it as text with deparse()"
    } else {
      "convert it to a data.frame or a plain list"
    }
    expect_identical(
      check_results(x)$detail[3],
      paste0(
        "not allowed: x$extra$item (class ",
        bad[[nm]]$class,
        "): ",
        advice
      ),
      info = nm
    )
  }
})

test_that("allowed_objects gives the advice for each common object", {
  cases <- list(
    htest = list(
      value = stats::t.test(c(1, 2, 4, 3)),
      advice = "(class htest): save data.frame(statistic = , p_value = , ...) of the test"
    ),
    posixlt = list(
      value = as.POSIXlt("2026-01-01", tz = "UTC"),
      advice = "(class POSIXlt/POSIXt): convert it with as.POSIXct()"
    ),
    version = list(
      value = package_version("1.2.3"),
      advice = "(class package_version/numeric_version): convert it with as.character()"
    ),
    # The geometry column of an sf table: a list with the class sfc.
    geometry = list(
      value = structure(list(c(1, 2)), class = c("sfc_POINT", "sfc")),
      advice = "(class sfc_POINT/sfc): drop the geometry with sf::st_drop_geometry()"
    ),
    other = list(
      value = structure(list(a = 1), class = "my_summary"),
      advice = "(class my_summary): convert it to a data.frame or a plain list"
    )
  )
  for (nm in names(cases)) {
    x <- valid_results()
    x$extra$item <- cases[[nm]]$value
    expect_identical(
      check_results(x)$detail[3],
      paste("not allowed: x$extra$item", cases[[nm]]$advice),
      info = nm
    )
  }
})

test_that("allowed_objects walks attributes: a terms attribute, a function and an environment in an attribute fail", {
  fit_data <- data.frame(u = 1:5, w = c(2, 4, 5, 4, 5))
  x <- valid_results()
  x$extra$frame <- stats::model.frame(w ~ u, data = fit_data)
  x$extra$v <- structure(1, f = function() return(1))
  x$extra$w <- structure("a", meta = list(where = new.env()))
  expect_identical(failing(x), "allowed_objects")
  expect_identical(
    check_results(x)$detail[3],
    paste0(
      "not allowed: attr(x$extra$frame, \"terms\") (class terms/formula): ",
      "save it as text with deparse(); attr(x$extra$v, \"f\") (class ",
      "function): convert it to a data.frame or a plain list; ",
      "attr(x$extra$w, \"meta\")$where (class environment): convert it to a ",
      "data.frame or a plain list"
    )
  )
})

test_that("allowed_objects fails a model three levels deep and in a list column, and names each path", {
  fit_data <- data.frame(u = 1:5, w = c(2, 4, 5, 4, 5))
  x <- valid_results()
  x$models <- list(
    outcome_a = list(adjusted = list(fit = stats::lm(w ~ u, data = fit_data)))
  )
  x$tables$fits <- data.table::data.table(
    outcome = "b",
    fit = list(stats::lm(w ~ u, data = fit_data))
  )
  expect_identical(failing(x), "allowed_objects")
  expect_identical(
    check_results(x)$detail[3],
    paste0(
      "not allowed: x$tables$fits$fit[[1]] (class lm): save its table of ",
      "coefficients as a data.frame; x$models$outcome_a$adjusted$fit (class ",
      "lm): save its table of coefficients as a data.frame"
    )
  )
})

test_that("allowed_objects fails an R6 object", {
  skip_if_not_installed("R6")
  counter <- R6::R6Class("counter", public = list(n = 0))
  x <- valid_results()
  x$extra$counter <- counter$new()
  expect_identical(failing(x), "allowed_objects")
  expect_match(
    check_results(x)$detail[3],
    "x$extra$counter (class counter/R6)",
    fixed = TRUE
  )
})

test_that("allowed_objects passes each allowed object", {
  x <- valid_results()
  x <- c(
    x,
    list(
      # A key and an index give the attributes sorted and index, and each
      # data.table has .internal.selfref, an external pointer.
      a_data_table = data.table::setindex(
        data.table::setkey(data.table::data.table(a = 2:1, b = 3:4), a),
        b
      ),
      # A column read from an SPSS file carries label and format.spss.
      a_labelled_frame = data.frame(
        score = structure(c(1, 2), label = "Score", format.spss = "F8.2")
      ),
      a_tibble = structure(
        data.frame(a = 1:2),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      a_zero_row_frame = data.frame(a = numeric(0)),
      a_scalar = 0.03,
      a_named_vector = c(n = 40L, n_women = 21L),
      an_empty_vector = character(0),
      a_matrix = matrix(1:4, 2, dimnames = list(c("a", "b"), c("c", "d"))),
      an_array = array(1:8, c(2, 2, 2)),
      a_factor = factor(c("a", "b")),
      an_ordered_factor = factor(c("low", "high"), ordered = TRUE),
      a_date = as.Date("2026-01-01"),
      a_time = as.POSIXct("2026-01-01 12:00:00", tz = "UTC"),
      a_difftime = as.difftime(3, units = "days"),
      a_table = table(c("a", "b", "b")),
      a_noquote = noquote("a"),
      an_integer64 = structure(0, class = "integer64"),
      a_classed_vector = structure(c(1, 2), class = c("fit.vector", "numeric")),
      an_asis_list = I(list(1, "a")),
      a_null = NULL,
      a_nested_list = list(list(a = list(b = data.frame(c = 1))), "d")
    )
  )
  expect_identical(failing(x), character(0))
})

test_that("no_row_per_person fails on a data.frame with n_rows_data rows", {
  x <- valid_results()
  x$tables$people <- data.frame(id = 1:40)
  expect_identical(failing(x), "no_row_per_person")
  expect_match(check_results(x)$detail[4], "x$tables$people", fixed = TRUE)
})

test_that("no_row_per_person fails a data.frame with n_rows_data rows three levels deep, and names its path", {
  x <- valid_results()
  x$by_outcome <- list(a = list(raw = list(people = data.frame(id = 1:40))))
  expect_identical(failing(x), "no_row_per_person")
  expect_identical(
    check_results(x)$detail[4],
    paste(
      "x$by_outcome$a$raw$people (a data.frame with 40 rows) has one value",
      "for each row of the data. Save a summary of it instead. If it is an",
      "aggregate that has this size by chance, change its shape so that the",
      "size differs, for example to a long data.frame with an explicit",
      "grouping column, and say so in a comment"
    )
  )
  # 39 rows pass.
  x$by_outcome$a$raw$people <- data.frame(id = 1:39)
  expect_identical(failing(x), character(0))
})

test_that("no_row_per_person fails a vector, a factor, a date, a matrix and an array with n_rows_data values in one dimension, at any depth", {
  x <- valid_results()
  x$fitted <- stats::setNames(seq(0.1, 4, by = 0.1), 1:40)
  x$by_group <- list(a = list(sex = factor(rep(c("f", "m"), 20))))
  x$visits <- data.frame(id = 1:2)
  x$visits$dates <- list(as.Date("2026-01-01") + 0:39, as.Date("2026-01-01"))
  x$design <- matrix(0, 40, 2)
  x$cube <- array(0, c(2, 3, 40))
  expect_identical(failing(x), "no_row_per_person")
  expect_match(
    check_results(x)$detail[4],
    paste0(
      "^x\\$fitted \\(length 40\\), x\\$by_group\\$a\\$sex \\(length 40\\), ",
      "x\\$visits\\$dates\\[\\[1\\]\\] \\(length 40\\), x\\$design \\(dimensions ",
      "40x2\\), x\\$cube \\(dimensions 2x3x40\\) has one value for each row"
    )
  )
  # 39 values pass, and so do the vectors of the manifest.
  x <- valid_results()
  x$fitted <- seq_len(39)
  x$design <- matrix(0, 39, 2)
  x$manifest$packages <- stats::setNames(rep("1.0", 40), paste0("p", 1:40))
  expect_identical(failing(x), character(0))
})

test_that("no_row_per_person checks nothing when n_rows_data is 0 or 1, and checks from 2", {
  for (n in 0:1) {
    x <- valid_results()
    x$manifest$n_rows_data <- n
    x$one <- 0.5
    x$table_of_one <- data.frame(a = 1)
    x$empty <- numeric(0)
    expect_identical(failing(x), character(0), info = n)
    expect_identical(
      check_results(x)$detail[4],
      paste0("n_rows_data is ", n, ", less than 2, so nothing to check"),
      info = n
    )
  }
  x <- valid_results()
  x$manifest$n_rows_data <- 2L
  x$tables$counts <- NULL
  x$estimates <- NULL
  x$exclusions <- NULL
  x$pair <- c(0.1, 0.2)
  expect_identical(failing(x), "no_row_per_person")
  expect_match(check_results(x)$detail[4], "x$pair (length 2)", fixed = TRUE)
})

test_that("no_row_per_person checks nothing when n_rows_data is NA", {
  x <- valid_results()
  x$tables$people <- data.frame(id = 1:40)
  x$manifest$n_rows_data <- NA_integer_
  expect_identical(failing(x), character(0))
  expect_identical(
    check_results(x)$detail[4],
    "n_rows_data is NA, so nothing to check"
  )
})

test_that("no_row_per_person checks each count of a named n_rows_data, one for each data element", {
  x <- valid_results()
  x$manifest$n_rows_data <- c(d = 40L, pop = 12L, lookup = NA)
  expect_identical(failing(x), character(0))
  expect_identical(
    check_results(x)$detail[4],
    paste(
      "no data.frame has 40 or 12 rows, and no vector or matrix has 40 or 12",
      "values in one dimension"
    )
  )
  # A table with the rows of the second data element fails.
  x$tables$pop <- data.frame(id = 1:12)
  expect_identical(failing(x), "no_row_per_person")
  expect_match(
    check_results(x)$detail[4],
    "x$tables$pop (a data.frame with 12 rows)",
    fixed = TRUE
  )
  # Only NA, or only counts below 2, checks nothing.
  x$manifest$n_rows_data <- c(d = NA_integer_, pop = NA_integer_)
  expect_identical(failing(x), character(0))
  x$manifest$n_rows_data <- c(d = 1L, pop = NA_integer_)
  expect_identical(
    check_results(x)$detail[4],
    "n_rows_data is 1, less than 2, so nothing to check"
  )
  x$manifest$n_rows_data <- stats::setNames(integer(0), character(0))
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
  x$manifest$data_files <- NULL
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

test_that("exclusions_table passes the column data of a cohort, also stacked, and fails another extra column", {
  x <- valid_results()
  x$exclusions <- data.frame(data = "d", x$exclusions)
  expect_identical(failing(x), character(0))
  x$manifest$stacked <- TRUE
  x$exclusions <- data.frame(sim = 1L, x$exclusions)
  expect_identical(failing(x), character(0))

  y <- valid_results()
  y$exclusions <- data.frame(cohort = "d", y$exclusions)
  expect_identical(failing(y), "exclusions_table")
  expect_match(
    check_results(y)$detail[2],
    "the columns are cohort, step",
    fixed = TRUE
  )
})

test_that("a stacked object without the sim column fails exclusions_table", {
  x <- valid_results()
  x$manifest$stacked <- TRUE
  expect_identical(failing(x), "exclusions_table")
})

# A valid results object of the fake scenario null: fake_md5 in the place of
# data_files.
fake_results <- function() {
  x <- valid_results()
  x$manifest$scenario <- "null"
  x$manifest$data_files <- NULL
  x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
  return(x)
}

test_that("manifest_data passes data_files for real and fake_md5 for a fake scenario", {
  expect_identical(failing(valid_results()), character(0))
  expect_identical(failing(fake_results()), character(0))
  # Data folders with no file give data_files with 0 rows.
  x <- valid_results()
  x$manifest$data_files <- x$manifest$data_files[0, ]
  expect_identical(failing(x), character(0))
  # A stacked fake manifest.
  x <- fake_results()
  x$manifest$stacked <- TRUE
  x$manifest$sim <- NA_integer_
  x$manifest$sims <- 2L
  x$exclusions <- data.frame(sim = 1L, x$exclusions)
  expect_identical(failing(x), character(0))
})

test_that("manifest_data fails a manifest with neither or both of data_files and fake_md5, and the data_md5 of an older pbn", {
  x <- valid_results()
  x$manifest$data_files <- NULL
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    "manifest has neither data_files nor fake_md5"
  )

  x <- fake_results()
  x$manifest$fake_md5 <- NULL
  expect_identical(failing(x), "manifest_data")

  for (scenario in c("real", "null")) {
    x <- fake_results()
    x$manifest$scenario <- scenario
    x$manifest$data_files <- valid_results()$manifest$data_files
    expect_identical(failing(x), "manifest_data", info = scenario)
    expect_identical(
      check_results(x)$detail[6],
      "manifest has both data_files and fake_md5",
      info = scenario
    )
  }
  # The md5 of the data files, as an older pbn wrote them.
  x <- valid_results()
  x$manifest$data_files <- NULL
  x$manifest$data_md5 <- c(d.csv = "0123456789abcdef0123456789abcdef")
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    paste(
      "manifest has data_md5, the md5 of the data files that an older pbn",
      "wrote. pbn now writes data_files: run the scenario again"
    )
  )
})

test_that("manifest_data fails fake_md5 for real and data_files for a fake scenario", {
  x <- fake_results()
  x$manifest$scenario <- "real"
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    "manifest has fake_md5, but the scenario \"real\" needs data_files"
  )
  x <- valid_results()
  x$manifest$scenario <- "null"
  expect_identical(failing(x), "manifest_data")
  expect_identical(
    check_results(x)$detail[6],
    "manifest has data_files, but the fake scenario \"null\" needs fake_md5"
  )
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

test_that("manifest_data fails data_files that is not a fingerprint of files", {
  good <- data.frame(
    file = c("main/a.csv", "lookup/sub/b.sav"),
    size = c(120, 0),
    mtime = c(1.8e9, 1.8e9 + 0.25)
  )
  bad <- list(
    vector = c(d.csv = "0123456789abcdef0123456789abcdef"),
    no_mtime = good[, c("file", "size")],
    other_order = good[, c("file", "mtime", "size")],
    na_file = transform(good, file = c(NA, "x")),
    empty_file = transform(good, file = c("", "x")),
    twice = transform(good, file = c("main/a.csv", "main/a.csv")),
    negative = transform(good, size = c(-1, 0)),
    na_size = transform(good, size = c(NA, 0)),
    text_size = transform(good, size = c("1", "0")),
    na_mtime = transform(good, mtime = c(NA, 1))
  )
  for (nm in names(bad)) {
    x <- valid_results()
    x$manifest$data_files <- bad[[nm]]
    expect_identical(failing(x), "manifest_data", info = nm)
    expect_match(
      check_results(x)$detail[6],
      "manifest$data_files is not a data.frame with the columns file, size",
      fixed = TRUE,
      info = nm
    )
  }
  x <- valid_results()
  x$manifest$data_files <- good
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

test_that("no_row_per_person also checks the rows before the exclusions of a cohort", {
  x <- valid_results()
  x$manifest$n_rows_before_exclusions <- c(d = 50L)
  x$raw <- data.frame(i = seq_len(49))
  expect_identical(failing(x), character(0))
  x$raw <- data.frame(i = seq_len(50))
  expect_identical(failing(x), "no_row_per_person")
  expect_match(
    check_results(x)$detail[4],
    "x$raw (a data.frame with 50 rows) has one value for each row",
    fixed = TRUE
  )
})

test_that("element_names passes a manifest alone when the manifest puts kinds in results_gitexclude.qs2, and fails it otherwise", {
  x <- valid_results()["manifest"]
  x$manifest$analyses <- data.frame(
    analysis = "models",
    control = FALSE,
    file = "results.qs2"
  )
  expect_identical(failing(x), "element_names")
  x$manifest$analyses$file <- "results_gitexclude.qs2"
  expect_identical(failing(x), character(0))
})

test_that("no_row_per_person checks the rows after the exclusions when n_rows_data is NA", {
  x <- valid_results()
  x$manifest$n_rows_data <- c(d = NA_integer_)
  x$manifest$n_rows_before_exclusions <- c(d = 100L)
  x$manifest$n_rows_after_exclusions <- c(d = 90L)
  x$person <- data.frame(i = seq_len(89))
  expect_identical(failing(x), character(0))
  x$person <- data.frame(i = seq_len(90))
  expect_identical(failing(x), "no_row_per_person")
  expect_match(
    check_results(x)$detail[4],
    "x$person (a data.frame with 90 rows)",
    fixed = TRUE
  )
})

test_that("no_row_per_person does not walk the exclusions table of a cohort, and still checks each analysis kind", {
  x <- valid_results()
  x$manifest$n_rows_data <- c(d = 2L)
  x$exclusions <- data.frame(
    data = "d",
    step = c("age under 18", "age over 26"),
    n_before = c(8, 6),
    n_excluded = c(2, 4),
    n_after = c(6, 2)
  )
  x$estimates <- data.frame(term = "a", estimate = 0.1)
  x$tables <- list(counts = data.frame(grp = "a", n = 2))
  expect_identical(failing(x), character(0))
  x$person <- data.frame(i = 1:2)
  expect_identical(failing(x), "no_row_per_person")
  expect_match(
    check_results(x)$detail[4],
    "x$person (a data.frame with 2 rows)",
    fixed = TRUE
  )
})

# The simulations 1 to `sims` of the fake scenario null, each with 400 rows
# of data and a 1-row table of estimates, stacked by stack_results().
# `person` names the simulations whose table has one row for each person,
# and `draws` the simulations with a vector of 400 draws.
stacked_null <- function(sims = 400, person = integer(0), draws = integer(0)) {
  runs <- lapply(seq_len(sims), function(k) {
    x <- valid_results()
    x$manifest$scenario <- "null"
    x$manifest$data_files <- NULL
    x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
    x$manifest$sim <- k
    x$manifest$sims <- sims
    x$manifest$n_rows_data <- c(d = 400L)
    x$estimates <- data.frame(term = "a", estimate = k)
    if (k %in% person) {
      x$estimates <- data.frame(term = "a", estimate = seq_len(400))
    }
    x$extra$draws <- if (k %in% draws) seq_len(400) else 1:3
    return(x)
  })
  return(stack_results(runs, "null"))
}

test_that("no_row_per_person checks a stacked object for each simulation: 400 simulations of a 1-row table pass, and a table with a row for each person in one simulation fails", {
  x <- stacked_null()
  expect_identical(nrow(x$estimates), 400L)
  expect_identical(
    x$manifest$n_rows_by_sim[1:2, ],
    data.frame(
      sim = 1:2,
      data = "d",
      n_rows_data = 400L,
      n_rows_before_exclusions = NA_integer_,
      n_rows_after_exclusions = NA_integer_
    )
  )
  expect_identical(failing(x), character(0))
  expect_identical(
    check_results(x)$detail[4],
    paste(
      "in each of the 400 simulations with 2 or more rows of data, no",
      "data.frame has the rows of that data, and no vector or matrix has",
      "that many values in one dimension"
    )
  )

  y <- stacked_null(person = 3L, draws = 7L)
  expect_identical(failing(y), "no_row_per_person")
  expect_match(
    check_results(y)$detail[4],
    paste0(
      "x$estimates in simulation 3 (a data.frame with 400 rows), ",
      "x$extra$draws in simulation 7 (length 400) has one value for each ",
      "row of the data"
    ),
    fixed = TRUE
  )
})

test_that("no_row_per_person checks nothing on a stacked object without n_rows_by_sim, as pbn 2026.11.22 wrote it", {
  y <- stacked_null(sims = 3, person = 2L)
  y$manifest$n_rows_by_sim <- NULL
  expect_identical(failing(y), character(0))
  expect_identical(
    check_results(y)$detail[4],
    "n_rows_data is NA, so nothing to check"
  )
  y <- stacked_null(sims = 3, person = 2L)
  y$manifest$n_rows_by_sim$n_rows_data <- 1L
  expect_identical(
    check_results(y)$detail[4],
    "n_rows_data is NA, 0 or 1 in each simulation, so nothing to check"
  )
})

test_that("a list of data.frames with 2 or more numbers of rows gives the rows of each in n_rows_lists, and no_row_per_person checks each of them", {
  counted <- count_rows(
    list(
      imp = list(data.frame(x = 1:3), data.frame(x = 1:5)),
      same = list(data.frame(x = 1:4), data.frame(x = 1:4)),
      d = data.frame(x = 1:7),
      hash = "abc"
    ),
    list()
  )
  expect_identical(
    unclass(counted)[1:3],
    c(imp = NA_integer_, same = 4L, d = 7L)
  )
  expect_identical(attr(counted, "parts"), list(imp = c(3L, 5L)))
  expect_identical(n_rows_lists(counted), list(imp = c(3L, 5L)))
  expect_identical(
    n_rows_lists(count_rows(list(d = data.frame(x = 1)), list())),
    stats::setNames(list(), character(0))
  )
  x <- valid_results()
  x$manifest$n_rows_data <- c(imp = NA_integer_)
  x$manifest$n_rows_lists <- list(imp = c(3L, 5L))
  x$estimates <- data.frame(id = 1:5)
  found <- rule_no_row_per_person(x)
  expect_identical(found$status, "FAIL")
  expect_match(found$detail, "^x\\$estimates \\(", fixed = FALSE)
  x$estimates <- data.frame(id = 1:4)
  expect_identical(rule_no_row_per_person(x)$status, "PASS")
  x$estimates <- data.frame(id = 1:3)
  expect_identical(rule_no_row_per_person(x)$status, "FAIL")
  # Stacked results get one row of n_rows_by_sim for each data.frame.
  runs <- lapply(1:2, function(k) {
    return(list(
      manifest = list(
        n_rows_data = c(imp = NA_integer_),
        n_rows_lists = list(imp = c(3L, 5L + k))
      )
    ))
  })
  by_sim <- rows_by_sim(runs)
  expect_identical(by_sim$data, rep(c("imp", "imp[[1]]", "imp[[2]]"), 2))
  expect_identical(by_sim$n_rows_data, c(NA, 3L, 6L, NA, 3L, 7L))
})
