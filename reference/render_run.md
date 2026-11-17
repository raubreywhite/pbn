# Render the run log to a PDF

`render_run()` renders the run log `<version>_results/run.md` to
`<version>_results/run.pdf` with Quarto and its Typst format. A run
never calls it, so a missing Quarto cannot fail a run.

## Usage

``` r
render_run(version, home = ".")
```

## Arguments

- version:

  The version, for example `"v01"`.

- home:

  The project folder. The default is the working directory.

## Value

Invisibly, the path of `run.pdf`.

## Details

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes `run.md`. `render_run()` needs the program `quarto` on the
`PATH`. Quarto holds Typst, so the PDF needs no LaTeX. It stops when
`run.md` does not exist, when Quarto is not installed, or when Quarto
fails.

`render_run()` renders a copy of `run.md` in a temporary folder, with an
A4 page, a 9 point font and narrow margins. So no file of Quarto stays
in the results folder, and `run.md` stays as
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
wrote it. It then copies `run.pdf` to a temporary file in
`<version>_results/`, and renames it to `run.pdf`. So an interruption
never leaves a part of the PDF.

`run.pdf` is generated output, and `run.md` is the form for git. Keep
`run.pdf` out of git, for example with the line `run.pdf` in the
`.gitignore` of the project. pbn does not write that line.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
shows the run log of a small fake project.

Other results:
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md),
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_render_example")
dir.create(file.path(home, "v01_results"), recursive = TRUE)
writeLines(
  c("# Run log", "", "| Scenario | Simulations |", "|---|---:|", "| real | 1 |"),
  file.path(home, "v01_results", "run.md")
)
if (nzchar(Sys.which("quarto"))) {
  pbn::render_run("v01", home = home)
}
unlink(home, recursive = TRUE)
```
