#' Render the run log to a PDF
#'
#' `render_run()` renders the run log `<version>_results/run.md` to
#' `<version>_results/run.pdf` with Quarto and its Typst format. A run never
#' calls it, so a missing Quarto cannot fail a run.
#'
#' @param version The version, for example `"v01"`.
#' @param home The project folder. The default is the working directory.
#' @details
#' [save_results()] writes `run.md`. `render_run()` needs the program
#' `quarto` on the `PATH`. Quarto holds Typst, so the PDF needs no LaTeX. It
#' stops when `run.md` does not exist, when Quarto is not installed, or when
#' Quarto fails.
#'
#' `render_run()` renders a copy of `run.md` in a temporary folder, with an
#' A4 page, a 9 point font and narrow margins. So no file of Quarto stays in
#' the results folder, and `run.md` stays as [save_results()] wrote it. It
#' then copies `run.pdf` to a temporary file in `<version>_results/`, and
#' renames it to `run.pdf`. So an interruption never leaves a part of the
#' PDF.
#'
#' `run.pdf` is generated output, and `run.md` is the form for git. Keep
#' `run.pdf` out of git, for example with the line `run.pdf` in the
#' `.gitignore` of the project. pbn does not write that line.
#' @return Invisibly, the path of `run.pdf`.
#' @examples
#' home <- file.path(tempdir(), "pbn_render_example")
#' dir.create(file.path(home, "v01_results"), recursive = TRUE)
#' writeLines(
#'   c("# Run log", "", "| Scenario | Simulations |", "|---|---:|", "| real | 1 |"),
#'   file.path(home, "v01_results", "run.md")
#' )
#' if (nzchar(Sys.which("quarto"))) {
#'   pbn::render_run("v01", home = home)
#' }
#' unlink(home, recursive = TRUE)
#' @family results
#' @seealso `vignette("pbn")`, which shows the run log of a small fake
#'   project.
#' @export
render_run <- function(version, home = ".") {
  check_version(version)
  if (!is_one_string(home) || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  folder <- file.path(home, paste0(version, "_results"))
  log <- file.path(folder, "run.md")
  if (!file.exists(log)) {
    stop(
      "There is no run log ",
      log,
      ". pbn::save_results() writes it.",
      call. = FALSE
    )
  }
  quarto <- quarto_path()
  if (!nzchar(quarto)) {
    stop(
      "pbn::render_run() needs Quarto: install it from https://quarto.org, ",
      "so that the program quarto is on the PATH.",
      call. = FALSE
    )
  }
  tmp <- tempfile("pbn_render_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write_lf_lines(
    c(pdf_header, readLines(log, warn = FALSE)),
    file.path(tmp, "run.md")
  )
  out <- suppressWarnings(system2(
    quarto,
    c("render", shQuote(file.path(tmp, "run.md")), "--to", "typst"),
    stdout = TRUE,
    stderr = TRUE
  ))
  made <- file.path(tmp, "run.pdf")
  if (!file.exists(made)) {
    stop(
      "Quarto did not write the PDF. Its output:\n",
      paste(out, collapse = "\n"),
      call. = FALSE
    )
  }
  # A copy to a temporary file in the results folder, renamed into place,
  # so that an interruption never leaves a part of run.pdf.
  pdf <- file.path(folder, "run.pdf")
  part <- tempfile("run_", tmpdir = folder, fileext = ".pdf.tmp")
  on.exit(unlink(part), add = TRUE)
  if (!file.copy(made, part) || !rename_file(part, pdf)) {
    stop("pbn could not write ", pdf, call. = FALSE)
  }
  cat("pbn: wrote ", pdf, ".\n", sep = "")
  return(invisible(pdf))
}

# The YAML header of the copy of run.md that render_run() renders: an A4
# page, a 9 point font and narrow margins, so that the tables fit the page.
# A text between two |, such as |bias|, stays on one line: the Typst rule
# puts it in a box, and Typst breaks no line inside a box. Tables get an 8
# point font and a narrow inset left and right, so that a wide table such
# as the history fits the page.
pdf_header <- c(
  "---",
  "format:",
  "  typst:",
  "    papersize: a4",
  "    fontsize: 9pt",
  "    margin:",
  "      x: 1.5cm",
  "      y: 1.5cm",
  "    include-in-header:",
  "      text: |",
  "        #show regex(\"[|][A-Za-z ]+[|]\"): it => box(it)",
  "        #set table(inset: (x: 3pt, y: 6pt))",
  "        #show table: set text(size: 8pt)",
  "---",
  ""
)

# The path of the program quarto, or "" when it is not on the PATH. In one
# place, so that the tests can point it at a program that does not exist.
quarto_path <- function() {
  return(Sys.which("quarto"))
}
