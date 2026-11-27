# Write the filled-in checklist of each reporting guideline

`make_checklists()` writes one Markdown table for each checklist file
`guidelines/<x>.yml`, with the status of each item in the manuscript. An
item gets `numbers found` only when each formatted string of each block
that names it occurs in the manuscript text.

## Usage

``` r
make_checklists(version = NULL, manuscript = NULL, home = NULL)
```

## Arguments

- version:

  The version, for example `"v02"`. The default is the version of the
  last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

- manuscript:

  The path of one `.md` or `.docx` file, relative to the working folder.
  The default `NULL` is each `.md` file in `manuscript/built/<version>/`
  of `home`, without the files `checklist_*.md`.

- home:

  The project folder. The default is the folder of the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  of `version` in this R session, and else the working folder.

## Value

The table of all checklists, invisibly. It is a data.frame with one row
per item and the columns Guideline (the key `id`), Section, Item,
Recommendation, Reported in and Status. Its cells are the text without
the Markdown escapes of the files.

## Details

The items come from `guidelines/*.yml`, after the key `design` of each
file, as
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
reads them. A block names its items in the argument `items` of
[`block()`](https://www.rwhite.no/pbn/reference/block.md) in
`<version>_functions/blocks.R`, which `make_checklists()` reads without
running code.

The formatted strings of a block are its formatted value, as
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md) gives it. That is
the raw value of the real `blocks.qs2` of the version, through the
formatter of the block. So a change of a formatter needs no
[`update_blocks()`](https://www.rwhite.no/pbn/reference/update_blocks.md).
A formatter that returns a character vector gives each element. A
formatter that returns a data.frame gives each cell of each row, row by
row, with each cell as
[`as.character()`](https://rdrr.io/r/base/character.html) gives it. An
`NA` string is never found.

The text of a `.md` file is its lines. A heading is a line that starts
with 1 to 6 `#` and a space, outside a fenced code block. The text of a
`.docx` file is in `word/document.xml`, which
[`utils::unzip()`](https://rdrr.io/r/utils/unzip.html) reads. Each
paragraph `<w:p>` is one line, the contents of its `<w:t>` runs joined
with no space. A heading is a paragraph whose style `w:pStyle` starts
with `Heading`. The text before the first heading of a file is under the
name of the file.

The search normalises the text and each string in 7 steps, in this
order. It changes `<br>`, `<br/>` and `<br />` to a space. It removes
each other HTML tag, which is `<`, a letter, then letters and digits,
and optional attributes after a space, then `>` or `/>`. It decodes
`&lt;`, `&gt;`, `&nbsp;` and `&amp;`. It removes the Markdown backslash
before `*`, `<`, `>`, `_`, `[` and `]`. It changes U+00A0 and U+2009 to
a space. It changes each run of white space to one space, and it trims
the ends. It keeps the minus sign U+2212 and the case of each letter. So
the Markdown `p \< 0.001` matches the string `p < 0.001`.

A string is found when it occurs in the text of all files, joined line
by line. The column Reported in names each heading whose section holds
one of the strings of the item, joined by `; `.

For each checklist, `make_checklists()` writes
`manuscript/built/<version>/checklist_<id>.md` in `home`, and it creates
that folder when it is absent. It writes no other file. The table has
the columns Section, Item, Recommendation, Reported in and Status. The
Recommendation is the full key `text` of the item. The Status is one of:

- `numbers found`: a result item, and each string of each of its blocks
  occurs in the text. A reader still confirms that the text answers the
  item.

- `numbers missing: <string>`: a result item, and `<string>` is the
  first string that the text does not hold. The blocks are in the order
  of `blocks`.

- `no block`: a result item that no block names.

- `to fill`: a prose or an author item.

The files `checklist_*.md` are never part of the text. A `manuscript`
path whose name matches `checklist_*.md` is an error.

`make_checklists()` stops in each of these cases:

- a block of [`block()`](https://www.rwhite.no/pbn/reference/block.md)
  has no string `format` or no literal `items`.

- a block that names a result item is not in the real `blocks.qs2`, or
  that file is missing.

- a checklist file breaks the schema of
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).

## See also

[`block()`](https://www.rwhite.no/pbn/reference/block.md),
[`txt()`](https://www.rwhite.no/pbn/reference/txt.md),
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).
