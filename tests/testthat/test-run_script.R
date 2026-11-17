# The run script of the synthetic project in new R sessions, as a project
# runs it. org sources the functions folder into the global environment.

# Every file under v01_results/ with its md5.
tree_md5 <- function(p) {
  files <- results_files(p)
  return(stats::setNames(
    md5_of(file.path(p$home, "v01_results", files)),
    files
  ))
}

# Every results object under v01_results/, without source_md5, which holds
# the md5 of the run script, and without packages, the loaded packages.
# Each element of the list holds the object and the names of its packages.
results_without_source <- function(p) {
  files <- grep("\\.qs2$", results_files(p), value = TRUE)
  out <- lapply(files, function(f) {
    x <- qs2::qs_read(file.path(p$home, "v01_results", f))
    packages <- names(x$manifest$packages)
    x$manifest$source_md5 <- NULL
    x$manifest$packages <- NULL
    return(list(x = x, packages = packages))
  })
  return(stats::setNames(out, files))
}

test_that("the target run script runs end to end, writes exactly the expected tree, and check_project passes", {
  skip_if_pbn_not_installed()
  p <- local_project()
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  expect_identical(results_files(p), expected_tree)
  expect_match(res$stdout, "Assertions of the scenario \"known\"", fixed = TRUE)
  expect_match(res$stdout, "Assertions of the scenario \"null\"", fixed = TRUE)
  expect_false(file.exists(file.path(
    p$home,
    "v01_results",
    "real",
    "check.csv"
  )))
  expect_identical(
    readLines(file.path(p$home, "v01_results", "real", "output.txt")),
    "output"
  )
  # The control kind is in the fake scenarios only.
  expect_false("models_crude" %in% names(read_results("v01", home = p$home)))
  expect_true(
    "models_crude" %in% names(read_results("v01", "known", home = p$home))
  )
  found <- project_findings(p$home)
  expect_identical(found$status, rep("PASS", nrow(found)))
})

test_that("the data function runs exactly once for each scenario and simulation, with 1 and with 2 workers", {
  skip_if_pbn_not_installed()
  for (workers in 1:2) {
    p <- local_project()
    counter <- file.path(p$root, "calls.txt")
    write_lines(p$home, "v01_functions/_cfg.R", cfg_lines(counter))
    write_lines(
      p$home,
      "v01_run.R",
      run_script_lines("v01", p$data_default, workers = workers)
    )
    res <- run_script(p$home)
    expect_identical(res$status, 0L, info = res$stdout)
    calls <- readLines(counter)
    expect_length(calls, 5)
    # 5 scenarios and simulations, with 4 to 6 analyses each.
    expect_identical(
      sort(calls),
      c("known 1", "null 1", "null 2", "null 3", "real 1"),
      info = paste("workers", workers)
    )
  }
})

test_that("the script run twice in new R sessions gives byte-identical results.qs2 and check.csv", {
  skip_if_pbn_not_installed()
  p <- local_project()
  first_run <- run_script(p$home)
  expect_identical(first_run$status, 0L, info = first_run$stdout)
  first <- tree_md5(p)
  unlink(file.path(p$home, "v01_results"), recursive = TRUE)
  second_run <- run_script(p$home)
  expect_identical(second_run$status, 0L, info = second_run$stdout)
  expect_identical(names(first), expected_tree)
  expect_identical(tree_md5(p), first)
})

test_that("scenarios(CFG, only = \"null\") writes only under null/, leaves real/ byte-identical with its mtime, and ends before the output part", {
  skip_if_pbn_not_installed()
  p <- local_project()
  expect_identical(run_script(p$home)$status, 0L)
  real <- file_states(file.path(p$home, "v01_results", "real"))
  known <- file_states(file.path(p$home, "v01_results", "known"))
  null_before <- file_states(file.path(p$home, "v01_results", "null"))
  unlink(file.path(p$home, "v01_results", "real", "output.txt"))
  real <- real[real$file != "output.txt", ]
  rownames(real) <- NULL
  Sys.sleep(1.1)

  write_lines(
    p$home,
    "v01_run.R",
    run_script_lines("v01", p$data_default, only = "\"null\"")
  )
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  expect_match(
    res$stdout,
    "pbn: The run had no scenario \"real\", so it ends here.",
    fixed = TRUE
  )
  expect_identical(file_states(file.path(p$home, "v01_results", "real")), real)
  expect_identical(
    file_states(file.path(p$home, "v01_results", "known")),
    known
  )
  null_after <- file_states(file.path(p$home, "v01_results", "null"))
  expect_identical(null_after$file, null_before$file)
  expect_true(all(null_after$mtime > null_before$mtime))
  # The output part did not run.
  expect_false(file.exists(file.path(
    p$home,
    "v01_results",
    "real",
    "output.txt"
  )))
})

test_that("workers = 2 gives results identical to workers = 1, with the functions of the folder in the global environment", {
  skip_if_pbn_not_installed()
  run_with <- function(workers) {
    p <- local_project(env = parent.frame())
    r_file <- file.path(p$root, "r.qs2")
    write_lines(
      p$home,
      "v01_run.R",
      run_script_lines(
        "v01",
        p$data_default,
        workers = workers,
        extra = paste0("qs2::qs_save(r, \"", r_file, "\")")
      )
    )
    res <- run_script(p$home)
    expect_identical(res$status, 0L, info = res$stdout)
    return(list(
      r = qs2::qs_read(r_file),
      results = results_without_source(p),
      check = md5_of(file.path(
        p$home,
        "v01_results",
        c("known", "null"),
        "check.csv"
      ))
    ))
  }
  one <- run_with(1)
  two <- run_with(2)
  # The stamp names the plan of each R session, so it differs.
  expect_false(identical(attr(two$r, "pbn_run"), attr(one$r, "pbn_run")))
  attr(one$r, "pbn_run") <- NULL
  attr(two$r, "pbn_run") <- NULL
  expect_identical(two$r, one$r)
  expect_length(one$r, 28)
  expect_identical(two$check, one$check)
  expect_identical(
    names(two$results),
    grep("[.]qs2$", expected_tree, value = TRUE)
  )
  for (f in names(one$results)) {
    expect_identical(two$results[[f]]$x, one$results[[f]]$x, info = f)
    # The manifest names the packages loaded in the main R session and on
    # the workers. With 2 workers the main session loads doParallel. plnr
    # calls digest on the workers only, and the manifest names it too.
    expect_identical(
      setdiff(two$results[[f]]$packages, one$results[[f]]$packages),
      "doParallel",
      info = f
    )
    expect_identical(
      setdiff(one$results[[f]]$packages, two$results[[f]]$packages),
      character(0),
      info = f
    )
  }
})

test_that("a failing analysis in the run script lists every failure in full and saves nothing, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  outputs <- list()
  for (workers in 1:2) {
    p <- local_project()
    write_lines(
      p$home,
      "v01_functions/analyses.R",
      c(
        analysis_lines(),
        "make_table1 <- function(data, argset) {",
        "  if (argset$sim == 2) stop(\"planted: \", strrep(\"y\", 1100), \" end\")",
        "  return(pbn::table1(data$d, by = \"g\", vars = CFG$vars))",
        "}"
      )
    )
    write_lines(
      p$home,
      "v01_run.R",
      run_script_lines("v01", p$data_default, workers = workers)
    )
    res <- run_script(p$home)
    expect_identical(res$status, 1L, info = res$stdout)
    expect_false(dir.exists(file.path(p$home, "v01_results")))
    expect_match(
      res$stdout,
      paste0(
        "pbn: the analysis null__sim_0002__table1__1 failed:\n",
        "Error: planted: ",
        strrep("y", 1100),
        " end"
      ),
      fixed = TRUE
    )
    expect_match(
      res$stdout,
      "1 of 28 analyses failed. pbn printed each failed analysis",
      fixed = TRUE
    )
    outputs[[workers]] <- grep(
      "^pbn: the analysis",
      strsplit(res$stdout, "\n")[[1]],
      value = TRUE
    )
  }
  expect_identical(outputs[[2]], outputs[[1]])
})

test_that("with library(data.table) in the run script, an action with := and .() runs on 2 workers", {
  skip_if_pbn_not_installed()
  p <- local_project()
  write_lines(
    p$home,
    "v01_functions/counts.R",
    c(
      "count_groups <- function(data, argset) {",
      "  d <- as.data.table(data$d)",
      "  d[, one := 1]",
      "  d[, .(n = sum(one)), keyby = g]",
      "}"
    )
  )
  lines <- run_script_lines("v01", p$data_default, workers = 2)
  at <- grep("^r <- pbn::run_plan", lines)
  lines <- append(
    lines,
    "pbn::add_analyses(p, scenarios, name = \"counts\", fn = count_groups)",
    after = at - 1
  )
  write_lines(p$home, "v01_run.R", c("library(data.table)", lines))
  res <- run_script(p$home)
  expect_identical(res$status, 0L, info = res$stdout)
  counts <- read_results("v01", "null", home = p$home)$counts
  expect_identical(names(counts), c("sim", "g", "n"))
  expect_identical(counts$n, rep(20, 6))
})
