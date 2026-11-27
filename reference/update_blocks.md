# Write blocks.qs2 again from the saved results

`update_blocks()` evaluates the blocks on the saved results of each
scenario of the version, and writes `blocks.qs2` of each one. It runs no
analysis.

## Usage

``` r
update_blocks(version = NULL, home = NULL)
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

## Value

Invisibly, the names of the scenarios that it wrote.

## Details

It does what step 7 of
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
does, for each folder `<version>_results/<scenario>/` that holds a
`results.qs2`. In a one-file project, the folder of `"real"` is `fake/`,
and `update_blocks()` does not change `real/`, the results of the
server. See the section The folder fake/ of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
It creates `blocks.qs2` where it is absent. Call it after a change of
the raw value of a block, or to create the files without a run. A change
of a formatter needs no call, because
[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md) and
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md) format at each
call. `blocks.qs2` holds the raw value of each block.

It needs
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
of `version` in this R session. It stops as
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
does on a block that fails or that breaks the rule of
[`block()`](https://www.rwhite.no/pbn/reference/block.md). Then it
writes no file.

## See also

[`block()`](https://www.rwhite.no/pbn/reference/block.md),
[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md).
