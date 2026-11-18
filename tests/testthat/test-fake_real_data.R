# initialize_project(fake_real_data = TRUE): the folders of this computer
# hold fake files in the formats of the real data.

test_that("the manifest of real holds fake_real_data, TRUE with the flag and FALSE without, and a fake scenario has no such field", {
  p <- local_project()
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  real <- qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2"))
  known <- qs2::qs_read(file.path(
    p$home,
    "v01_results",
    "known",
    "results.qs2"
  ))
  expect_true(real$manifest$fake_real_data)
  expect_false("fake_real_data" %in% names(known$manifest))
  expect_identical(
    read_results("v01", home = p$home)$manifest$fake_real_data,
    TRUE
  )
  save_run(p, only = c("real", "known"))
  real <- qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2"))
  expect_false(real$manifest$fake_real_data)
  expect_error(
    initialize_project("v01", p$home, p$data_default, fake_real_data = NA),
    "`fake_real_data` must be TRUE or FALSE.",
    fixed = TRUE
  )
})

test_that("manifest_data fails a real manifest without fake_real_data, as from an older pbn, and one that is not TRUE or FALSE", {
  x <- valid_results()
  expect_identical(rule_manifest_data(x)$status, "PASS")
  x$manifest$fake_real_data <- NULL
  found <- rule_manifest_data(x)
  expect_identical(found$status, "FAIL")
  expect_identical(
    found$detail,
    "manifest has no fake_real_data. An older pbn wrote this file: run the scenario again"
  )
  x$manifest$fake_real_data <- "yes"
  expect_identical(
    rule_manifest_data(x)$detail,
    "manifest$fake_real_data is not TRUE or FALSE"
  )
  # A fake scenario needs no such field.
  y <- valid_results()
  y$manifest$scenario <- "null"
  y$manifest$data_files <- NULL
  y$manifest$fake_real_data <- NULL
  y$manifest$fake_md5 <- "00112233445566778899aabbccddeeff"
  expect_identical(rule_manifest_data(y)$status, "PASS")
})

test_that("run.md says that real ran on fake real data in the sections Run, Data and History", {
  p <- local_project()
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  md <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_true(any(
    md ==
      paste0(
        "- The scenario real ran on fake real data: the folders of this ",
        "computer hold fake files in the formats of the real data."
      )
  ))
  expect_true(any(grepl("^\\| d +\\| real \\(fake real data\\) +\\|", md)))
  expect_true(any(grepl("^\\| d +\\| known +\\|", md)))
  history <- md[seq(match("## History", md), length(md))]
  expect_true(any(grepl(
    "| All 2 assertions passed, fake real data |",
    history,
    fixed = TRUE
  )))
  save_run(p, only = c("real", "known"))
  md <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_false(any(grepl(
    "fake real data",
    md[seq_len(match("## History", md))],
    fixed = TRUE
  )))
  # The exclusions of real get the same label.
  log <- list(
    fake_real_data = TRUE,
    scenarios = data.frame(scenario = "real", sims = 1L),
    run = list(
      exclusions = list(
        real = list(
          d = data.frame(
            data = "d",
            step = "age 18 or more",
            n_before = 10L,
            n_excluded = 1L,
            n_after = 9L
          )
        )
      )
    )
  )
  expect_identical(md_exclusions(log)[3], "### real (fake real data)")
  log$fake_real_data <- FALSE
  expect_identical(md_exclusions(log)[3], "### real")
})

test_that("make_outputs on fake real data writes real as fake output, stamps its figure, and deletes the real output of an earlier run", {
  skip_if_not_installed("ggplot2")
  p <- local_project()
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  write_lines(
    p$home,
    "v01_results/real/output/old.txt",
    "from a run on real data"
  )
  stamped <- new.env()
  fn <- function(results, dir) {
    q <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y))
    stamped[[results$manifest$scenario]] <- !identical(stamp_fake(q), q)
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "known"), sims = 1)
  ))
  printed <- utils::capture.output(out <- make_outputs(fn, sc))
  expect_identical(
    out$file,
    "v01_results/real/output_fake_data/fake_data_figure.png"
  )
  expect_true(stamped$real)
  expect_false(dir.exists(file.path(p$home, "v01_results", "real", "output")))
  expect_identical(
    printed[1],
    paste0(
      "pbn: deleted v01_results/real/output/, because the real results come ",
      "from fake real data, so no output of real data of an earlier run stays."
    )
  )
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "fake_data_names"], "PASS")
  # Without the flag, real is real output again.
  save_run(p, only = c("real", "known"))
  utils::capture.output(out <- make_outputs(fn, sc))
  expect_identical(out$file, "v01_results/real/output/figure.png")
  expect_false(stamped$real)
})

test_that("show_blocks and show_block print FAKE REAL DATA for real results from fake real data, and not otherwise", {
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(\"Lines\" = function(results) c(\"a\", \"b\"))"
    )
  )
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  out <- utils::capture.output(show_blocks())
  expect_identical(out[2], "FAKE REAL DATA")
  out <- utils::capture.output(show_block(1))
  expect_identical(
    out[1:3],
    c("[1] Lines", "FAKE REAL DATA", "page 1 of 1, rows 1 to 2 of 2")
  )
  out <- utils::capture.output(show_block(1, rows = 1, page = 2))
  expect_identical(out[2], "FAKE REAL DATA")
  save_run(p, only = c("real", "known"))
  expect_false(any(utils::capture.output(show_blocks()) == "FAKE REAL DATA"))
  expect_identical(
    utils::capture.output(show_block(1))[2],
    "page 1 of 1, rows 1 to 2 of 2"
  )
})

test_that("check_project warns in real_results_are_fake while the real results come from fake real data, and passes otherwise", {
  p <- local_project()
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  found <- project_findings(p$home)
  row <- found[found$rule == "real_results_are_fake", ]
  expect_identical(row$status, "WARN")
  expect_identical(row$path, "v01_results/real/results.qs2")
  expect_identical(failed_rules(found), character(0))
  save_run(p, only = c("real", "known"))
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "real_results_are_fake"], "PASS")
})

test_that("run_script_initialises accepts fake_real_data, and the file of to_one_file never holds it", {
  p <- local_project()
  run <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(run)
  run[i] <- sub("))$", "), fake_real_data = TRUE)", run[i])
  writeLines(run, file.path(p$home, "v01_run.R"))
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "run_script_initialises"], "PASS")
  init(p, fake_real_data = TRUE)
  utils::capture.output(file <- to_one_file("v01", "N:/x", c(main = "N:/d")))
  gen <- parse(file, keep.source = FALSE)
  call <- Filter(
    function(e) {
      return(is.call(e) && identical(e[[1]], quote(pbn::initialize_project)))
    },
    as.list(gen)
  )[[1]]
  expect_false("fake_real_data" %in% names(call))
  expect_false(any(grepl("fake_real_data", readLines(file), fixed = TRUE)))
  one <- eval(call$one_file)
  expect_error(
    initialize_project(
      "v01",
      file.path(p$root),
      p$data_default,
      one_file = one,
      fake_real_data = TRUE
    ),
    "The file of pbn::to_one_file() runs on real data, so it takes no fake_real_data = TRUE.",
    fixed = TRUE
  )
})

test_that("check_one_file passes a project on fake real data: the fresh run has no flag, and the blocks print the same without the line FAKE REAL DATA", {
  skip_if_pbn_not_installed()
  p <- local_project()
  add_one_file_files(p)
  run <- readLines(file.path(p$home, "v01_run.R"))
  i <- init_line(run)
  run[i] <- sub("))$", "), fake_real_data = TRUE)", run[i])
  writeLines(run, file.path(p$home, "v01_run.R"))
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  real <- qs2::qs_read(file.path(p$home, "v01_results", "real", "results.qs2"))
  expect_true(real$manifest$fake_real_data)
  init(p, fake_real_data = TRUE)
  out <- utils::capture.output(ok <- check_one_file("v01"))
  expect_match(
    out,
    "^PASS: .* manifest\\$fake_real_data\\. Each page of the 2 blocks prints the same for both\\.$"
  )
  expect_true(ok)
})

test_that("make_outputs and stamp_fake take fake real data from the manifest of the real results, not from the R session", {
  skip_if_not_installed("ggplot2")
  p <- local_project()
  stamped <- new.env()
  fn <- function(results, dir) {
    q <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y))
    stamped[[results$manifest$scenario]] <- !identical(stamp_fake(q), q)
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  sc <- scenarios(list(
    scenarios = data.frame(scenario = c("real", "known"), sims = 1)
  ))
  # Results from fake real data, and a session without the flag.
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  init(p)
  utils::capture.output(out <- make_outputs(fn, sc))
  expect_identical(
    out$file,
    "v01_results/real/output_fake_data/fake_data_figure.png"
  )
  expect_true(stamped$real)
  # Results from real data, and a session with the flag.
  save_run(p, only = c("real", "known"))
  write_lines(
    p$home,
    "v01_results/real/output_fake_data/fake_data_old.png",
    "x"
  )
  init(p, fake_real_data = TRUE)
  printed <- utils::capture.output(out <- make_outputs(fn, sc))
  expect_identical(out$file, "v01_results/real/output/figure.png")
  expect_false(stamped$real)
  expect_false(dir.exists(file.path(
    p$home,
    "v01_results",
    "real",
    "output_fake_data"
  )))
  expect_identical(
    printed[1],
    paste0(
      "pbn: deleted v01_results/real/output_fake_data/, because the real ",
      "results come from real data, so no output of fake real data of an ",
      "earlier run stays."
    )
  )
  # The blocks read the manifest too.
  write_lines(
    p$home,
    "v01_functions/blocks.R",
    "blocks <- list(\"Lines\" = function(results) c(\"a\", \"b\"))"
  )
  init(p, fake_real_data = TRUE)
  expect_false(any(utils::capture.output(show_block(1)) == "FAKE REAL DATA"))
  save_run(p, only = c("real", "known"), fake_real_data = TRUE)
  init(p)
  expect_identical(utils::capture.output(show_block(1))[2], "FAKE REAL DATA")
})
