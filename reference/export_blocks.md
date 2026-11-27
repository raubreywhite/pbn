# Take the blocks off a server as text, and read them back

`export_blocks()` prints the raw value of each block as R code, in pages
with a checksum. `import_blocks()` reads the captured pages into
`<version>_results/real/blocks.qs2`, and `read_blocks()` returns those
values for a manuscript.

## Usage

``` r
export_blocks(version = NULL)

import_blocks(version, home = ".")

read_blocks(version, home = ".")
```

## Arguments

- version:

  The version, for example `"v01"`. For `export_blocks()`, the default
  `NULL` is the version of the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

- home:

  The project folder. The default `"."` is the working folder.

## Value

`export_blocks()`: invisibly, the printed lines. `import_blocks()`:
invisibly, the path of the `blocks.qs2` that it wrote. `read_blocks()`:
a named list with the raw value of each block.

## Details

### export_blocks()

`export_blocks()` runs on the server, after the file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
It needs
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
in this R session, and it reads the blocks and the real results as
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
does. For each block, it keeps the raw value, the value that
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes to `blocks.qs2`. It does not call the formatter of a
[`block()`](https://www.rwhite.no/pbn/reference/block.md). It turns a
data.table into a data.frame, because R cannot read the pointer of a
data.table back. It stops on a block that stops, and the error names the
block.

The text of block `k` is the output of base R
[`dput()`](https://rdrr.io/r/base/dput.html) for a list. The list holds
one element: the raw value, with the name of the block. The `control` of
[`dput()`](https://rdrr.io/r/base/dput.html) is
`c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")`.
With `"digits17"`, each number has 17 significant digits, so the number
after the import is the same double.

Each page has at most 35 lines: a header line and at most 34 lines of
text. The header line is
`pbn-blocks <version> run <run date> block <k>/<n> page <p>/<P> md5 <m>`.
The run date is that of
[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md).
`<m>` is the first 8 hex characters of
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) of the text
lines of the page. pbn takes them without trailing white space, joined
by `"\n"`, with no final newline. An empty line follows each page.

When the real results have `fake_real_data` `TRUE` in their manifest,
each header line ends with the token ` fake`. Examples are a run of a
one-file project on this computer, see
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
The pages of real data have no token.

### import_blocks()

Copy or type the pages from the screen of the server into
`<version>_results/real/export_blocks.txt`. `import_blocks()` reads that
file. It removes the trailing white space of each line and each empty
line, and it ignores the lines before the first header line. The lines
after a header line, up to the next header line, are the text of that
page.

`import_blocks()` stops and writes nothing when:

- the file has no header line,

- a header line names another version, or the pages name more than one
  run date or more than one count of blocks,

- some header lines end with the token ` fake` and some do not,

- the md5 of the text of a page differs from its header, and the error
  names the block and the page,

- a page is missing, and the error names the block and the page,

- a page comes twice with other text. A page that comes twice with the
  same text counts once.

- R cannot parse the text of a block, or the text is not a list of one
  named value.

It rebuilds each value with
`eval(parse(text = text), envir = baseenv())`. So import only the pages
that `export_blocks()` printed, because `import_blocks()` runs the file
as R code. It writes the named list of the values to
`<version>_results/real/blocks.qs2` and overwrites that file. It writes
there in a one-file project too, where the scenario `real` of this
computer is in `fake/`, see
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
When the pages end with the token ` fake`, the list has the attribute
`pbn_fake = TRUE`.

### read_blocks()

`read_blocks()` returns `<version>_results/real/blocks.qs2` when it
exists. Else it returns the `blocks.qs2` of the scenario `real` of this
computer, and it gives a warning that the numbers are fake. In a
one-file project that file is `<version>_results/fake/blocks.qs2`. In a
project that is not one-file it is `real/blocks.qs2` itself, so
`read_blocks()` stops.
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md) and
[`make_checklists()`](https://www.rwhite.no/pbn/reference/make_checklists.md)
read the blocks in the same way. The rule `built_reads_source` of
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
says which functions a manuscript in `manuscript/built/` MUST use.

The values are fake when they come from another file than
`real/blocks.qs2`, or when `real/blocks.qs2` has `pbn_fake = TRUE`. For
fake values in a document that knitr renders, such as a `.qmd` or an
`.Rmd`, `read_blocks()`,
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md) and
[`make_checklists()`](https://www.rwhite.no/pbn/reference/make_checklists.md)
add a knitr document hook. The hook puts the line `**FAKE DATA**` once
at the top of the output, after the YAML header. A document that reads
only real values gets no such line.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), section
"Running on an air-gapped server".

Other one file:
[`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md),
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md),
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
