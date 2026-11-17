# render_run(): the run log as a PDF, with Quarto.

test_that("render_run writes run.pdf beside run.md, and leaves no file of Quarto in the results folder", {
  skip_if(!nzchar(Sys.which("quarto")), "Quarto is not on the PATH, and render_run() needs it")
  p <- local_project()
  sourced <- init(p)
  sc <- scenarios(sourced$CFG)
  plan <- build_plan(sourced, sc)
  r <- run_plan(plan)
  utils::capture.output(save_results(r, plan, sc))
  before <- results_files(p)
  # The PDF comes to the results folder as a temporary file, which pbn
  # renames, so an interruption never leaves a part of run.pdf.
  seen <- new.env()
  local_mocked_bindings(rename_file = function(from, to) {
    seen$from <- from
    seen$to <- to
    seen$target_before <- file.exists(to)
    return(file.rename(from, to))
  })
  out <- utils::capture.output(pdf <- render_run("v01", home = p$home))
  expect_identical(seen$to, pdf)
  expect_identical(dirname(seen$from), dirname(pdf))
  expect_true(endsWith(seen$from, ".pdf.tmp"))
  expect_false(seen$target_before)
  expect_identical(pdf, file.path(p$home, "v01_results", "run.pdf"))
  expect_identical(readBin(pdf, "raw", 4), charToRaw("%PDF"))
  expect_identical(results_files(p), sort(c(before, "run.pdf")))
  expect_identical(out, paste0("pbn: wrote ", pdf, "."))
  expect_false(dir.exists(file.path(p$home, "v01_results", ".quarto")))
  expect_identical(failed_rules(project_findings(p$home)), character(0))
})

test_that("render_run stops with one sentence when Quarto is missing, and when there is no run.md", {
  p <- local_project()
  expect_error(
    render_run("v01", home = p$home),
    paste0(
      "There is no run log ",
      file.path(p$home, "v01_results", "run.md"),
      ". pbn::save_results() writes it."
    ),
    fixed = TRUE
  )
  write_lines(p$home, "v01_results/run.md", "# Run log")
  local_mocked_bindings(quarto_path = function() {
    return(Sys.which("pbn-no-such-program-for-the-test"))
  })
  expect_error(
    render_run("v01", home = p$home),
    paste0(
      "pbn::render_run() needs Quarto: install it from https://quarto.org, ",
      "so that the program quarto is on the PATH."
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(p$home, "v01_results", "run.pdf")))
  expect_error(render_run("v1", home = p$home), "`version` must be")
})
