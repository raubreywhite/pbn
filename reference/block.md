# Declare one block of the manuscript, with its formatter and its items

`block()` declares one element of the list `blocks` in
`<version>_functions/blocks.R`. The function of the block returns
numbers, and the formatter turns them into the text of the manuscript.

## Usage

``` r
block(fn, format, items = character(0))
```

## Arguments

- fn:

  A function of `results`. It returns the raw value of the block: a
  data.frame or a data.table of numbers and labels.

- format:

  The name of the formatter, one string. It is the name of a house
  formatter, or of a function in `<version>_functions/formatters.R`.

- items:

  The checklist items that the block answers, as a character vector of
  `"<id>:<item>"` strings, for example `c("strobe:13a", "strobe:14a")`.
  The default is no item.

## Value

An object of the class `pbn_block`: a list with `fn`, `format` and
`items`.

## Details

The rule set `house_2026_11_26` and the newer sets need `block()` for
each element of `blocks`:

    blocks <- list(
      "Results para 1: effect" = pbn::block(
        function(results) results$effect,
        format = "effect",
        items = c("strobe:17")
      )
    )

The raw value MUST hold numbers as numbers. A character or factor column
holds labels only. No cell of such a column MAY match `[0-9][.,][0-9]`,
`[0-9]%`, `[0-9] \(` or `\b[nNpP] ?[=<>] ?[0-9]`. Each of these patterns
is a formatted number, such as `0.80`, `68.7%`, `3 (` and `n = 3`. A
comparison in a label, such as `Education < 16 years`, passes.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md),
[`update_blocks()`](https://www.rwhite.no/pbn/reference/update_blocks.md),
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md),
[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md) and
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md) stop on such a
cell. The error names the block, the column and the first cell that
matches. The check reads the value only, so a block MAY call any
function, for example [`paste0()`](https://rdrr.io/r/base/paste.html) to
make a name.

The formatted value of a block is `formatter(value)`. A formatter is a
function of one argument, the raw value. It returns a character vector
with one string for each row, or a data.frame or a data.table. Its
columns MAY be of mixed types. pbn has five house formatters. Each of
them uses the minus sign U+2212 for a negative number:

|  |  |  |
|----|----|----|
| name | columns | example |
| `ci` | `est`, `lo`, `hi`; `digits` (default 2) | `0.80 (95% CI 0.46 to 1.16)` |
| `p` | `p` | `p < 0.001`, `p = 0.047` |
| `effect` | `est`, `lo`, `hi`, `p`; `unit`, `digits` (default 2) | `(0.80 points, 95% CI 0.46 to 1.16, p < 0.001)` |
| `n_pct` | `n`, `total`; `digits` (default 1) | `1,037 (68.7%)` |
| `mean_sd` | `mean`, `sd`; `digits` (default 1) | `30.7 (4.8)` |

`p` gives `p < 0.001` below 0.001, and else 3 decimals. A row with `NA`
in a column that the formatter needs gives `NA`.

A project formatter is a function in `<version>_functions/formatters.R`.
It wins over a house formatter of the same name. That file MUST hold
only assignments of a name to `function(...)`. A project formatter sees
`CFG` and the functions of the functions folder, in
[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md), in
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md) and in the file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
pbn leaves it out of `manifest$source_md5` under the same conditions as
`blocks.R`, see
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
So a change of a formatter keeps the results current. The rule
`blocks_list` of
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
fails a use of a name of `formatters.R` in a file other than `blocks.R`
and `formatters.R`.

In `blocks.R`, `format` MUST be one string, and `items` MUST be a call
`c("<id>:<item>", ...)` of strings, so that
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
reads them without running code. An `<id>` holds lower-case letters,
digits and `-`. An `<item>` holds no space and no `:`.

## See also

[`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md),
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md),
[`update_blocks()`](https://www.rwhite.no/pbn/reference/update_blocks.md).
