# The formatted string of one block, for the manuscript

`txt()` returns one string of the formatted value of a block, from the
real `blocks.qs2` of the version. A manuscript calls it inline, for
example `` `r pbn::txt("Results para 1: effect", version = "v02")` ``.

## Usage

``` r
txt(name, row = 1, col = NULL, version = NULL, home = NULL)
```

## Arguments

- name:

  The name of the block in `blocks`, one string.

- row:

  The row of the formatted value, a whole number from 1.

- col:

  The column of the formatted value when the formatter returns a
  data.frame: a name or a number. `NULL` is right only for a character
  vector or a data.frame with one column.

- version:

  The version, for example `"v01"`. The default `NULL` is the version of
  the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

- home:

  The project folder. The default `NULL` is the folder of the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

## Value

One string.

## Details

`txt()` reads the raw value as
[`read_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md)
does: `<home>/<version>_results/real/blocks.qs2` when it exists, which
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md),
[`update_blocks()`](https://www.rwhite.no/pbn/reference/update_blocks.md)
and
[`import_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md)
write. In a one-file project without that file, it reads `fake/` with a
warning, see the section The folder fake/ of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
It formats the value at each call, so a change of a formatter needs no
new `blocks.qs2`.

After
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
of `version` and `home` in this R session, `txt()` takes the block from
that session. Else it reads `<version>_functions/blocks.R` without
running it. It then needs a call
[`pbn::block()`](https://www.rwhite.no/pbn/reference/block.md) with a
string `format` for the block `name`, and it evaluates only the function
assignments of `<version>_functions/formatters.R`. Their parent
environment holds the top-level assignments of the other `.R` files of
the functions folder, such as `CFG`, in the order that
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
sources them. A top-level call that is not an assignment, such as
[`cat()`](https://rdrr.io/r/base/cat.html), does not run. The default
`home` is then the working folder. The default `version` needs
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).

`txt()` stops when the block, the row, the column or `blocks.qs2` is
missing. It also stops when the block is not a
[`pbn::block()`](https://www.rwhite.no/pbn/reference/block.md), or when
the cell is not a string. It stops on a raw value that breaks the rule
of [`block()`](https://www.rwhite.no/pbn/reference/block.md).

## See also

[`block()`](https://www.rwhite.no/pbn/reference/block.md),
[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md).
