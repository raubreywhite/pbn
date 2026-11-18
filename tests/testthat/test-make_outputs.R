# make_outputs(), stamp_fake() and the column output of CFG$scenarios, on
# the synthetic project of helper-project.R, run in this R session.

# The config of local_project() with the column output.
cfg_with_output <- function(output = "c(FALSE, TRUE, TRUE)") {
  lines <- cfg_lines()
  i <- which(lines == "    sims = c(1, 1, 3),")
  lines[i] <- paste0("    sims = c(1, 1, 3), output = ", output, ",")
  return(lines)
}

# Runs and saves the scenarios `only` of the project `p`, and returns the
# scenarios of CFG.
run_and_save <- function(p, only = NULL, env = parent.frame()) {
  sourced <- init(p, env = env)
  sc <- if (is.null(only)) {
    scenarios(sourced$CFG)
  } else {
    scenarios(sourced$CFG, only = only)
  }
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  return(scenarios(sourced$CFG))
}

# An output function that writes a figure and a table, and records the
# scenario and the number of rows of results$models in `seen`.
recording_output <- function(seen) {
  return(function(results, dir) {
    s <- results$manifest$scenario
    seen[[s]] <- nrow(results$models)
    writeLines(s, file.path(dir, "figure.png"))
    dir.create(file.path(dir, "tables"))
    utils::write.csv(results$models, file.path(dir, "tables", "models.csv"))
    return(invisible(NULL))
  })
}

test_that("make_outputs writes real into output/, and each fake scenario with output = TRUE into output_fake_data/ with the prefix fake_data_", {
  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sc <- run_and_save(p)
  expect_identical(sc$output, c(FALSE, TRUE, TRUE))
  results <- file.path(p$home, "v01_results")
  before <- file_states(results)
  write_lines(p$home, "v01_results/real/output/old.txt", "from an earlier run")

  seen <- new.env()
  printed <- utils::capture.output(
    out <- make_outputs(recording_output(seen), sc)
  )
  expect_identical(
    out,
    data.frame(
      scenario = rep(c("real", "known", "null"), each = 2),
      file = c(
        "v01_results/real/output/figure.png",
        "v01_results/real/output/tables/models.csv",
        "v01_results/known/output_fake_data/fake_data_figure.png",
        "v01_results/known/output_fake_data/tables/fake_data_models.csv",
        "v01_results/null/output_fake_data/fake_data_figure.png",
        "v01_results/null/output_fake_data/tables/fake_data_models.csv"
      )
    )
  )
  expect_true(all(file.exists(file.path(p$home, out$file))))
  # The earlier file is gone, and pbn wrote nothing outside the folders but
  # the run log, which gets the section of make_outputs().
  expect_false(file.exists(file.path(results, "real", "output", "old.txt")))
  after <- file_states(results)
  outside <- !grepl("/output(_fake_data)?/", after$file) &
    after$file != "run.md"
  kept <- after[outside, ]
  rownames(kept) <- NULL
  expect_identical(kept, before[before$file != "run.md", ])
  # null gets its 3 simulations stacked, the others their 1 simulation.
  expect_identical(
    as.list(seen)[c("real", "known", "null")],
    list(
      real = 6L,
      known = 6L,
      null = 18L
    )
  )
  expect_identical(
    printed,
    c(
      "pbn: the output of the scenario \"real\" is in v01_results/real/output/, which pbn emptied first: 2 files.",
      "pbn: the output of the scenario \"known\" is in v01_results/known/output_fake_data/, which pbn emptied first: 2 files.",
      "pbn: the output of the scenario \"null\" is in v01_results/null/output_fake_data/, which pbn emptied first: 2 files.",
      "pbn: wrote the run log v01_results/run.md."
    )
  )
  found <- project_findings(p$home)
  expect_identical(found$status[found$rule == "fake_data_names"], "PASS")
})

test_that("make_outputs writes nothing for a fake scenario without output = TRUE, and keeps a name that has the prefix", {
  p <- local_project()
  sc <- run_and_save(p)
  expect_null(sc$output)
  utils::capture.output(out <- make_outputs(recording_output(new.env()), sc))
  expect_identical(unique(out$scenario), "real")
  expect_false(dir.exists(file.path(
    p$home,
    "v01_results",
    "known",
    "output_fake_data"
  )))

  write_lines(
    p$home,
    "v01_functions/_cfg.R",
    cfg_with_output("c(FALSE, TRUE, FALSE)")
  )
  sc <- run_and_save(p)
  fn <- function(results, dir) {
    writeLines("x", file.path(dir, "fake_data_numbers.txt"))
    return(invisible(NULL))
  }
  utils::capture.output(out <- make_outputs(fn, sc[sc$scenario != "real", ]))
  expect_identical(
    out$file,
    "v01_results/known/output_fake_data/fake_data_numbers.txt"
  )
})

test_that("make_outputs skips a scenario that was not run, or whose checks failed, with one line each", {
  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sc <- run_and_save(p, only = "real")
  seen <- new.env()
  printed <- utils::capture.output(make_outputs(recording_output(seen), sc))
  expect_identical(
    printed[2:3],
    paste0(
      "pbn: no output for the scenario \"",
      c("known", "null"),
      "\", because it has no results.qs2. Run the scenario first."
    )
  )
  expect_identical(ls(seen), "real")

  sc <- run_and_save(p, only = c("real", "known"))
  # save_results() deleted the output of real. An output folder made after
  # it goes with the line of the skip.
  expect_false(dir.exists(file.path(p$home, "v01_results", "real", "output")))
  write_lines(p$home, "v01_results/real/output/figure.png", "x")
  write_lines(
    p$home,
    "v01_results/real/check.csv",
    c("assertion,expected,observed,pass", "a,1,2,FALSE")
  )
  printed <- utils::capture.output(make_outputs(recording_output(seen), sc))
  # The output of the earlier call is deleted with the line.
  expect_identical(
    printed[1],
    paste0(
      "pbn: no output for the scenario \"real\", because its check.csv ",
      "holds an assertion that did not pass. pbn deleted ",
      "v01_results/real/output/, so no earlier output stays."
    )
  )
  expect_false(dir.exists(file.path(p$home, "v01_results", "real", "output")))
  known_dir <- file.path(p$home, "v01_results", "known", "output_fake_data")
  expect_true(dir.exists(known_dir))
  write_lines(
    p$home,
    "v01_results/known/check.csv",
    c("assertion,expected,observed,pass", "a,1,2,FALSE")
  )
  printed <- utils::capture.output(make_outputs(recording_output(seen), sc))
  expect_match(
    printed[2],
    "pbn deleted v01_results/known/output_fake_data/",
    fixed = TRUE
  )
  expect_false(dir.exists(known_dir))
  # Without an earlier output folder, the line names no deletion.
  printed <- utils::capture.output(make_outputs(recording_output(seen), sc))
  expect_identical(
    printed[1],
    paste0(
      "pbn: no output for the scenario \"real\", because its check.csv ",
      "holds an assertion that did not pass."
    )
  )
})

test_that("make_outputs deletes the old output of a chosen scenario without results, and leaves a scenario that is not chosen alone", {
  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sc <- run_and_save(p, only = "real")
  old_known <- "v01_results/known/output_fake_data/fake_data_old.png"
  old_null <- "v01_results/null/output_fake_data/fake_data_old.png"
  write_lines(p$home, old_known, "old")
  write_lines(p$home, old_null, "old")
  # null is not chosen in this call, so its folder stays.
  chosen <- sc[sc$scenario != "null", ]
  printed <- utils::capture.output(make_outputs(
    recording_output(new.env()),
    chosen
  ))
  expect_identical(
    printed[2],
    paste0(
      "pbn: no output for the scenario \"known\", because it has no ",
      "results.qs2. Run the scenario first. pbn deleted ",
      "v01_results/known/output_fake_data/, so no earlier output stays."
    )
  )
  expect_false(dir.exists(file.path(p$home, dirname(old_known))))
  expect_true(file.exists(file.path(p$home, old_null)))
})

test_that("make_outputs stops when the call for a fake scenario writes outside its folder, and deletes the added files", {
  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sc <- run_and_save(p)
  results <- file.path(p$home, "v01_results")
  real_png <- file.path(results, "real", "output", "figure.png")
  stray <- file.path(results, "null", "stray.txt")
  fn <- function(results, dir) {
    s <- results$manifest$scenario
    writeLines(s, file.path(dir, "figure.png"))
    if (s == "known") {
      # A hard-coded path: the real folder, and a new file.
      writeLines("fake", real_png)
      writeLines("fake", stray)
      unlink(file.path(dirname(dir), "check.csv"))
    }
    return(invisible(NULL))
  }
  printed <- NULL
  err <- tryCatch(
    printed <- utils::capture.output(make_outputs(fn, sc)),
    error = function(e) return(conditionMessage(e))
  )
  expect_identical(
    err,
    paste0(
      "The output function wrote outside v01_results/known/output_fake_data/ ",
      "for the scenario \"known\". Added, and deleted by pbn: ",
      "v01_results/null/stray.txt. Changed: ",
      "v01_results/real/output/figure.png. Removed: ",
      "v01_results/known/check.csv. pbn did not restore the changed or the ",
      "removed files. Write into `dir` only."
    )
  )
  expect_false(file.exists(stray))
  expect_identical(readLines(real_png), "fake")

  # A fake output function that writes into its folder only passes, and so
  # does a real one that writes elsewhere: the real call is not checked.
  sc <- run_and_save(p)
  fn <- function(results, dir) {
    writeLines("x", file.path(dir, "figure.png"))
    if (results$manifest$scenario == "real") {
      writeLines("x", file.path(dirname(dir), "note.txt"))
    }
    return(invisible(NULL))
  }
  utils::capture.output(out <- make_outputs(fn, sc))
  expect_identical(nrow(out), 3L)
})

test_that("a rewrite of a recent file with the same size and the same mtime is found by its md5", {
  root <- file.path(withr::local_tempdir(), "v01_results")
  dir.create(file.path(root, "real"), recursive = TRUE)
  f <- file.path(root, "real", "figure.png")
  writeLines("real", f)
  before <- hash_recent(results_listing(root, "known/output_fake_data"), root)
  expect_false(is.na(before$md5))
  writeLines("fake", f)
  # The same tick of the clock: the mtime of the listing is that of now.
  before$mtime <- as.numeric(file.mtime(f))
  expect_error(
    check_outside(root, before, "known/output_fake_data", "x", "known"),
    "Changed: v01_results/real/figure.png.",
    fixed = TRUE
  )
  # An unchanged file passes.
  before <- hash_recent(results_listing(root, "known/output_fake_data"), root)
  expect_null(check_outside(
    root,
    before,
    "known/output_fake_data",
    "x",
    "known"
  ))
})

test_that("results_listing lists each file outside the skipped folder, with its size and its mtime", {
  root <- withr::local_tempdir()
  for (i in 1:20) {
    dir.create(file.path(root, sprintf("sim_%04d", i)))
    for (j in 1:100) {
      writeLines(
        "x",
        file.path(root, sprintf("sim_%04d", i), paste0(j, ".qs2"))
      )
    }
  }
  listing <- results_listing(root, "sim_0001")
  expect_identical(nrow(listing), 1900L)
  expect_identical(names(listing), c("file", "size", "mtime"))
})

test_that("make_outputs stops with the scenario named when the output function fails, and on a clash of names", {
  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sc <- run_and_save(p)
  fn <- function(results, dir) {
    if (results$manifest$scenario == "null") {
      stop("no column called effect")
    }
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  expect_error(
    utils::capture.output(make_outputs(fn, sc)),
    "The output function failed for the scenario \"null\": no column called effect",
    fixed = TRUE
  )
  expect_true(file.exists(file.path(
    p$home,
    "v01_results/known/output_fake_data/fake_data_figure.png"
  )))
  # stamp_fake() is not left on for a fake scenario.
  expect_null(output_state$fake)

  clash <- function(results, dir) {
    writeLines("x", file.path(dir, "a.png"))
    writeLines("x", file.path(dir, "fake_data_a.png"))
    return(invisible(NULL))
  }
  expect_error(
    utils::capture.output(make_outputs(clash, sc[sc$scenario == "known", ])),
    "with and without the prefix fake_data_: fake_data_a.png",
    fixed = TRUE
  )
  expect_error(make_outputs("fn", sc), "`fn` must be a function")
  local_clean_state()
  expect_error(
    make_outputs(fn, sc),
    "Call pbn::initialize_project() before pbn::make_outputs().",
    fixed = TRUE
  )
})

test_that("stamp_fake adds the label for a fake scenario only, and stops without ggplot2 only when it must stamp", {
  skip_if_not_installed("ggplot2")
  q <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y))
  expect_identical(stamp_fake(q), q)
  expect_identical(stamp_fake("not a figure"), "not a figure")

  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sc <- run_and_save(p)
  seen <- new.env()
  fn <- function(results, dir) {
    seen[[results$manifest$scenario]] <- stamp_fake(q)
    return(invisible(NULL))
  }
  utils::capture.output(make_outputs(fn, sc))
  expect_identical(seen$real, q)
  for (s in c("known", "null")) {
    layers <- seen[[s]]$layers
    expect_length(layers, length(q$layers) + 1)
    grob <- layers[[length(layers)]]$geom_params$grob
    expect_identical(grob$label, "FAKE DATA")
    expect_identical(grob$rot, 30)
  }
  expect_identical(stamp_fake(q), q)

  bad <- function(results, dir) {
    stamp_fake("not a figure")
    return(invisible(NULL))
  }
  expect_error(
    utils::capture.output(make_outputs(bad, sc[sc$scenario == "known", ])),
    "`q` must be a ggplot",
    fixed = TRUE
  )
  local_mocked_bindings(has_ggplot2 = function() return(FALSE))
  utils::capture.output(make_outputs(fn, sc[sc$scenario == "real", ]))
  expect_identical(seen$real, q)
  expect_error(
    utils::capture.output(make_outputs(fn, sc[sc$scenario == "known", ])),
    "pbn::stamp_fake() needs the package ggplot2",
    fixed = TRUE
  )
})

# The panels of the gtable `g`, its labels "FAKE DATA", and the insets of
# the label: the rows and columns each covers, and the fills of its
# rectangles. Panels of a nested gtable count too.
figure_parts <- function(g) {
  panels <- 0L
  labels <- 0L
  walk <- function(x) {
    if (inherits(x, "gtable")) {
      drawn <- !vapply(x$grobs, inherits, logical(1), what = "zeroGrob")
      panels <<- panels + sum(grepl("^panel-", x$layout$name) & drawn)
    }
    if (inherits(x, "text") && identical(x$label, "FAKE DATA")) {
      labels <<- labels + 1L
    }
    for (k in c("children", "grobs")) {
      for (child in x[[k]]) {
        walk(child)
      }
    }
    return(invisible(NULL))
  }
  walk(g)
  fills <- character(0)
  walk_fill <- function(x) {
    if (inherits(x, "rect") || inherits(x, "roundrect")) {
      fills <<- c(fills, as.character(x$gp$fill))
    }
    for (k in c("children", "grobs")) {
      for (child in x[[k]]) {
        walk_fill(child)
      }
    }
    return(invisible(NULL))
  }
  inset <- grepl("^inset", g$layout$name)
  for (i in which(inset)) {
    walk_fill(g$grobs[[i]])
  }
  # The rows and columns of each plot of the top gtable, from its
  # background, against those of the inset.
  plots <- g$layout[grepl("^background-", g$layout$name), ]
  covers <- any(inset) &&
    all(g$layout$t[inset] <= min(plots$t)) &&
    all(g$layout$l[inset] <= min(plots$l)) &&
    all(g$layout$b[inset] >= max(plots$b)) &&
    all(g$layout$r[inset] >= max(plots$r))
  return(list(
    panels = panels,
    labels = labels,
    covers = covers,
    opaque = sum(!is.na(fills) & fills != "transparent")
  ))
}

test_that("stamp_fake puts one label over the whole figure of a patchwork, and leaves each panel visible", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  plot_xy <- function(y, colour = NULL) {
    d <- data.frame(x = 1:3, y = y, g = c("a", "b", "a"))
    q <- ggplot2::ggplot(d, ggplot2::aes(x, y))
    if (is.null(colour)) {
      return(q + ggplot2::geom_point())
    }
    return(q + ggplot2::geom_point(ggplot2::aes(colour = g)))
  }
  a <- plot_xy(1:3)
  b <- plot_xy(3:1)
  c3 <- plot_xy(c(2, 1, 3), colour = TRUE)
  # With the namespace of patchwork loaded, / and | combine ggplots.
  shapes <- list(
    "a / b, heights, title" = a /
      b +
      patchwork::plot_layout(heights = c(1, 1.5)) +
      patchwork::plot_annotation(title = "A title"),
    "a | b, tags" = (a | b) + patchwork::plot_annotation(tag_levels = "A"),
    "(a | b) / c, guides and axis titles collected, caption" = (a | c3) /
      c3 +
      patchwork::plot_layout(guides = "collect", axis_titles = "collect") +
      patchwork::plot_annotation(caption = "A caption", tag_levels = "A"),
    "wrap_plots(list, ncol = 2)" = patchwork::wrap_plots(
      list(a, b, c3, a),
      ncol = 2
    )
  )
  output_state$fake <- TRUE
  on.exit(rm(list = ls(output_state), envir = output_state), add = TRUE)
  # A device of no file keeps Rplots.pdf out of the folder of the tests.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (shape in names(shapes)) {
    q <- shapes[[shape]]
    stamped <- stamp_fake(q)
    expect_s3_class(stamped, "patchwork")
    before <- figure_parts(patchwork::patchworkGrob(q))
    after <- figure_parts(patchwork::patchworkGrob(stamped))
    # Each panel is drawn, one label lies over every plot, and nothing of
    # the label hides a panel.
    expect_identical(after$panels, before$panels, label = shape)
    expect_identical(after$labels, 1L, label = shape)
    expect_true(after$covers, label = shape)
    expect_identical(after$opaque, 0L, label = shape)
  }
  expect_identical(
    figure_parts(patchwork::patchworkGrob(shapes[[1]]))$panels,
    2L
  )
  # A grob of another package stops, with the way to stamp.
  expect_error(
    stamp_fake(grid::rectGrob()),
    "`q` must be a ggplot or a patchwork. Stamp each ggplot before you combine it with another package.",
    fixed = TRUE
  )
  local_mocked_bindings(has_patchwork = function() return(FALSE))
  expect_error(
    stamp_fake(shapes[[1]]),
    "needs the package patchwork",
    fixed = TRUE
  )
})

test_that("output is not a setting: it is not in the argsets, the manifest settings or the cache key", {
  p <- local_project()
  write_lines(p$home, "v01_functions/_cfg.R", cfg_with_output())
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  argset <- plan$get_argset("known__sim_0001__models__1")
  expect_false("output" %in% names(argset))
  expect_identical(argset$n_boot, 5)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  m <- read_results("v01", "known", home = p$home)$manifest
  expect_identical(m$settings, list(n_boot = 5))

  skip_if_not_installed("cohort")
  q <- local_cohort_project()
  first <- cohort_run(q)
  expect_identical(first$out, cache_lines(q, rep("there is no cache file", 3)))
  lines <- cohort_cfg_lines(q$counter)
  lines <- sub(
    "sims = c(1, 2),",
    "sims = c(1, 2), output = c(FALSE, TRUE),",
    lines,
    fixed = TRUE
  )
  write_lines(q$home, "v01_functions/_cfg.R", lines)
  second <- cohort_run(q)
  expect_identical(second$scenarios$output, c(FALSE, TRUE))
  expect_identical(second$out, cache_lines(q, rep(NA, 3)))
})
