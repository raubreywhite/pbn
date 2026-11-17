# Show the numbers of the manuscript, one block at a time

`show_blocks()` prints the index of the blocks of the project, and
`show_block()` prints one page of one block. They are for a screen whose
numbers leave a server only as a screenshot.

## Usage

``` r
show_blocks(version = NULL, home = NULL)

show_block(k, page = 1, rows = 35, version = NULL, home = NULL)
```

## Arguments

- version:

  The version, for example `"v01"`. The default `NULL` is the version of
  the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

- home:

  The project folder. The default `NULL` is the folder of the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

- k:

  The block, by its number in `blocks` or by its name.

- page:

  The page of the block, a whole number from 1.

- rows:

  The rows of one page, a whole number from 1. The default 35 fits one
  screenshot.

## Value

`show_blocks()`: invisibly, a data.frame with the columns `block`,
`name`, `rows`, `pages` and `error`. `show_block()`: invisibly, the
whole value of the block function.

## Details

The project declares the blocks in its functions folder, as one object
`blocks`. It is a named list of functions of `results`, in the order of
the manuscript. The name of a block is its place in the manuscript, for
example `"Table 1"` or `"Results para 1: exclusions"`. Each function
gets the real results as
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
returns them. It returns a data.frame, or a character vector that prints
as one line for each element. The function formats the numbers for the
manuscript, and `show_block()` prints the table as the function returns
it.

    blocks <- list(
      "Table 1" = function(results) results$table1,
      "Results para 1: exclusions" = function(results) results$exclusions
    )

The results include the kinds of `gitexclude`, so a block MAY show rows
for each person, for example to find a fault. What a person copies or
screenshots from the screen is the responsibility of that person.

Both functions need
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
in this R session, because the blocks come from the functions folder
that it sourced. Without it they stop. They read the real results from
the disk at each call, with
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md).
So they work in the project folder, and on a server after the file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
stops on real results whose `check.csv` holds a failed assertion, and so
do they.

`show_blocks()` calls each block function once. For each block, it
prints the number, the name, the rows and the pages. For a function that
stops, it prints the error in the place of the rows, and goes on.

`show_block()` prints these lines:

1.  the number and the name of the block,

2.  the page, for example `page 2 of 4, rows 36 to 70 of 140`,

3.  the rows of the page.

It stops on a block function that stops, and the error names the block.
During the call, the option `width` is 270 and the row names are off. No
option of R or of data.table cuts a row or a column, and a wide table
wraps its columns. Both functions record the options before a block
function runs, and set them back at the end of the call. So an option
that a block function sets or adds does not stay.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), section
"Running on an air-gapped server".

Other one file:
[`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md),
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
