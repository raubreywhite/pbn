cfg <- function(...) {
  table <- data.frame(
    scenario = c("real", "known", "null"),
    sims = c(1, 1, 3),
    n_boot = c(200, 5, 5)
  )
  changes <- list(...)
  for (nm in names(changes)) {
    table[[nm]] <- changes[[nm]]
  }
  return(list(scenarios = table))
}

test_that("scenarios returns the checked table as a plain data.frame, or the rows of only", {
  out <- scenarios(cfg())
  expect_identical(
    out,
    data.frame(
      scenario = c("real", "known", "null"),
      sims = c(1L, 1L, 3L),
      n_boot = c(200, 5, 5)
    )
  )
  expect_identical(class(out), "data.frame")
  one <- scenarios(cfg(), only = "null")
  expect_identical(
    one,
    data.frame(scenario = "null", sims = 3L, n_boot = 5)
  )
  # The order of the table, not of only.
  expect_identical(
    scenarios(cfg(), only = c("null", "real"))$scenario,
    c("real", "null")
  )
  # A data.table comes back as a plain data.frame.
  dt <- list(scenarios = data.table::as.data.table(cfg()$scenarios))
  expect_identical(scenarios(dt), out)
})

test_that("scenarios stops on each broken rule of CFG$scenarios and on an unknown name in only", {
  stops <- list(
    "must be a data.frame with the columns scenario and sims" = list(
      scenarios = data.frame(scenario = "real")
    ),
    "must have a scenario named \"real\"" = cfg(scenario = c("a", "b", "c")),
    "each scenario must be a name of lower-case letters" = cfg(
      scenario = c("real", "Known", "null")
    ),
    "each scenario must be there once" = cfg(
      scenario = c("real", "null", "null")
    ),
    "sims must be a whole number of 1 or more" = cfg(sims = c(1, 1.5, 3)),
    "sims must be a whole number of 1 or more " = cfg(sims = c(1, 0, 3)),
    "the setting column n_boot has NA" = cfg(n_boot = c(1, NA, 3)),
    "the setting column name needs another name" = cfg(name = "x"),
    "the setting column analysis needs another name" = cfg(analysis = "x"),
    "the column output must be TRUE or FALSE on each row" = cfg(
      output = c(FALSE, NA, TRUE)
    ),
    "the column output must be TRUE or FALSE on each row " = cfg(
      output = c("no", "yes", "yes")
    ),
    "the column output must be FALSE for \"real\"" = cfg(
      output = c(TRUE, TRUE, FALSE)
    )
  )
  for (msg in names(stops)) {
    expect_error(
      scenarios(stops[[msg]]),
      trimws(msg),
      fixed = TRUE,
      info = msg
    )
  }
  listed <- cfg()
  listed$scenarios$n_boot <- list(1, 2, 3)
  expect_error(scenarios(listed), "must be an atomic vector", fixed = TRUE)
  expect_error(scenarios(list()), "must be a data.frame")
  expect_error(
    scenarios(cfg(), only = c("null", "absent")),
    "These scenarios are not in CFG$scenarios: absent",
    fixed = TRUE
  )
  expect_error(scenarios(cfg(), only = character(0)), "`only` must be")
})

test_that("scenarios keeps the column output after scenario and sims, and it is not a setting", {
  out <- scenarios(cfg(output = c(FALSE, TRUE, FALSE)))
  expect_identical(names(out), c("scenario", "sims", "output", "n_boot"))
  expect_identical(out$output, c(FALSE, TRUE, FALSE))
  expect_identical(
    scenarios(cfg(output = c(FALSE, TRUE, FALSE)), only = "known")$output,
    TRUE
  )
  expect_identical(setting_columns(out), "n_boot")
  expect_identical(settings_of(out, "known"), list(n_boot = 5))
})

test_that("end_unless_real returns invisibly when real is among the scenarios", {
  expect_invisible(end_unless_real(scenarios(cfg())))
  expect_null(end_unless_real(scenarios(cfg(), only = "real")))
})

test_that("end_unless_real in an interactive session stops with its own condition and keeps the session", {
  testthat::local_mocked_bindings(is_interactive = function() return(TRUE))
  cond <- tryCatch(
    end_unless_real(scenarios(cfg(), only = "null")),
    condition = function(c) return(c)
  )
  expect_s3_class(cond, "pbn_end_unless_real")
  expect_s3_class(cond, "error")
  expect_match(
    conditionMessage(cond),
    "The output part of the script needs the real results.",
    fixed = TRUE
  )
})

test_that("end_unless_real under Rscript quits with status 0, and no line after it runs", {
  skip_if_pbn_not_installed()
  root <- withr::local_tempdir()
  script <- file.path(root, "end.R")
  writeLines(
    c(
      "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = 1))",
      "args <- commandArgs(trailingOnly = TRUE)",
      "scenarios <- pbn::scenarios(CFG, only = args)",
      "cat('before\\n')",
      "pbn::end_unless_real(scenarios)",
      "cat('after\\n')",
      "writeLines('after', 'after.txt')"
    ),
    script
  )
  run <- function(only) {
    return(callr::rscript(
      script,
      cmdargs = only,
      wd = root,
      show = FALSE,
      stderr = "2>&1",
      fail_on_status = FALSE
    ))
  }
  fake <- run("null")
  expect_identical(fake$status, 0L)
  expect_match(fake$stdout, "before", fixed = TRUE)
  expect_match(
    fake$stdout,
    "pbn: The run had no scenario \"real\", so it ends here.",
    fixed = TRUE
  )
  expect_false(grepl("after", fake$stdout, fixed = TRUE))
  expect_false(file.exists(file.path(root, "after.txt")))

  real <- run(c("real", "null"))
  expect_identical(real$status, 0L)
  expect_match(real$stdout, "after", fixed = TRUE)
  expect_true(file.exists(file.path(root, "after.txt")))
})

test_that("end_unless_real in an interactive R process leaves the process alive", {
  skip_if_pbn_not_installed()
  session <- callr::r_session$new()
  withr::defer(session$close())
  out <- session$run(function() {
    options(interactive = TRUE)
    cond <- tryCatch(
      {
        # The binding in the namespace, as an interactive session has it.
        utils::assignInNamespace(
          "is_interactive",
          function() return(TRUE),
          "pbn"
        )
        pbn::end_unless_real(
          data.frame(scenario = "null", sims = 1L)
        )
        "no condition"
      },
      error = function(e) return(class(e))
    )
    return(list(cond = cond, alive = TRUE))
  })
  expect_true("pbn_end_unless_real" %in% out$cond)
  expect_true(out$alive)
  expect_identical(session$run(function() return(1 + 1)), 2)
})

test_that("a setting column MAY have NA for real only: real then has no such setting in its argsets and its manifest, and NA for a fake scenario stops", {
  cfg <- list(
    scenarios = data.frame(
      scenario = c("real", "null"),
      sims = c(1, 2),
      b3 = c(NA, 0.5)
    )
  )
  sc <- scenarios(cfg)
  expect_identical(sc$b3, c(NA, 0.5))
  expect_identical(settings_of(sc, "real"), list())
  expect_identical(settings_of(sc, "null"), list(b3 = 0.5))
  bad <- cfg
  bad$scenarios$b3 <- c(1, NA)
  expect_error(
    scenarios(bad),
    paste0(
      "CFG$scenarios: the setting column b3 has NA for the scenario null. ",
      "Each fake scenario needs a value. Only \"real\" MAY have NA."
    ),
    fixed = TRUE
  )
  p <- local_project()
  lines <- cfg_lines()
  lines[
    lines == "    n_boot = c(200, 5, 5)"
  ] <- "    n_boot = c(200, 5, 5), b3 = c(NA, 1, 2)"
  write_lines(p$home, "v01_functions/_cfg.R", lines)
  run <- save_run(p, only = c("real", "known"))
  expect_false(inherits(run$err, "error"))
  real <- qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2"))
  known <- qs2::qs_read(file.path(
    p$home,
    "v01_results",
    "known",
    "results.qs2"
  ))
  expect_identical(real$manifest$settings, list(n_boot = 200))
  expect_identical(known$manifest$settings, list(n_boot = 5, b3 = 1))
  argsets <- lapply(run$plan$analyses, function(a) return(a$argset))
  real_args <- argsets[startsWith(names(argsets), "real__")]
  expect_false(any(vapply(
    real_args,
    function(a) return("b3" %in% names(a)),
    logical(1)
  )))
  known_args <- argsets[startsWith(names(argsets), "known__")]
  expect_true(all(vapply(
    known_args,
    function(a) return(identical(a$b3, 1)),
    logical(1)
  )))
})
