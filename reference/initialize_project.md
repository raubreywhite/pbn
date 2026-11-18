# Start a project version

`initialize_project()` sets the paths of a project version and sources
its functions folder. It MUST be the first call in every `vNN_run.R`.

## Usage

``` r
initialize_project(
  version,
  home,
  folders,
  env = .GlobalEnv,
  one_file = NULL,
  fake_real_data = FALSE,
  ...
)
```

## Arguments

- version:

  The version, written as `"v"` and two digits, for example `"v01"`.

- home:

  The project folder. It holds `<version>_run.R`, `<version>_functions/`
  and the file of the rules `pbn.yml`.

- folders:

  The folders outside the project that the project reads, as a named
  character vector, for example
  `c(main = "../data", lookup = "../codes")`. One folder MAY have no
  name, and it then gets the name `main`. Each name MUST be a syntactic
  R name, once. A folder MAY be absent, for example on a computer
  without the real data. Only
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  for the scenario `"real"` then stops.

- env:

  The environment that the functions folder is sourced into.

- one_file:

  `NULL`. The file of
  [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  gives a value here. Do not write it by hand. See the section One file.

- fake_real_data:

  `TRUE` when the folders of `folders` on this computer hold fake files
  in the formats of the real data, and `FALSE`, the default, otherwise.
  See the section Fake real data.

- ...:

  Nothing. A call with the removed argument `data_raw` stops, and the
  error says how to use `folders`.

## Value

Invisibly, a list with `version`, `home`, `results` and `folders`.
`results` is the results folder, and `folders` the named paths of the
folders.

## Details

`initialize_project()` does these steps in this order:

1.  It reads the rules of
    [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
    from `home/pbn.yml`. It stops on each error of `pbn.yml` that stops
    [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md),
    also when the file is missing.

2.  It computes the md5 of `<version>_run.R` and of each file in
    `<version>_functions/`, before it sources them.
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    stores them in `manifest$source_md5`, without
    `<version>_functions/blocks.R`.

3.  It lists each file under each folder of `folders` that exists, at
    any depth, with its size and its modification time. This is the
    fingerprint of the data. pbn never reads the content of a data file,
    so a file of 60 GB costs what a small file costs.
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    stores the fingerprint in `manifest$data_files` of the scenario
    `"real"`. A file that the run writes later is not in the list. The
    section The fingerprint of the data gives the rules.

4.  It calls
    [`org::initialize_project()`](https://www.rwhite.no/org/reference/initialize_project.html)
    with `folders_to_be_sourced` set to `<version>_functions`.
    `max_loc_per_file` is the `limit` of the rule `loc_per_file`, 1000
    in the house set. When the rule is `warn` or `off`, it is `Inf`,
    because the check of org stops the run on a file with more lines.
    pbn gives org no folder of `folders`, so
    [`org::project`](https://www.rwhite.no/org/reference/project.html)
    holds `home`, `computer_id` and `env` only.

Project code finds a folder with
[`folder()`](https://www.rwhite.no/pbn/reference/folder.md), for example
`file.path(pbn::folder("main"), "d.csv")`. Call
[`folder()`](https://www.rwhite.no/pbn/reference/folder.md) inside a
function: a top-level call in a file of the functions folder runs before
`initialize_project()` sets the folders, and it stops.

The results folder is `<home>/<version>_results`.
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
creates it. `initialize_project()` creates no folder, also no folder
with today's date. It stops when `<version>_functions/` does not exist.

`initialize_project()` reads no scenario, no simulation, no command-line
argument and no environment variable, and it sets no seed.
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)
chooses the scenarios, and
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) and
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
set the seeds.

## The fingerprint of the data

The fingerprint is a data.frame with one row for each file, sorted by
`file` in the C locale:

- `file`, the name `<folder name>/<path in the folder>` with `/`, for
  example `main/sub/d.csv`. It never holds the path of the folder.

- `size`, the size in bytes.

- `mtime`, the modification time in whole seconds since 1970-01-01 UTC,
  with the fraction of a second cut off. A copy of a file can lose part
  of that fraction, for example on Windows, and whole seconds stay the
  same.

Hidden files count. A link to a file gives the size and the time of its
target, and [`list.files()`](https://rdrr.io/r/base/list.files.html)
follows a link to a folder. A broken link stops `initialize_project()`.
It also stops when a folder of `folders` is inside another or is the
same folder. It stops when a folder holds or is inside
`<version>_results/` or `<version>_cache_gitexclude/`, because pbn
writes there in each run. A folder elsewhere inside `home` is allowed.

A file that changed in place, and kept its size and the second of its
modification time, is not seen. A copy that does not keep the
modification times looks like changed data, so the cache computes the
data again. `file.copy(..., copy.date = TRUE)` keeps them.

## Fake real data

The real data of a project MAY be on a server only. Its folders on this
computer MAY then hold fake files in the formats of the real data. Then
the code that reads the real data runs here, and
[`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
has results to compare. Give `fake_real_data = TRUE` in the run script
of such a computer. It is about the scenario `"real"` only:

- The manifest of the real results gets `fake_real_data = TRUE`. It is
  `FALSE` without the flag. A fake scenario has no such field.

- The run log says that the scenario real ran on fake real data, in the
  sections Run and History. The sections Data and Exclusions name the
  scenario `real (fake real data)`.

- [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  writes the output of `"real"` as fake output, into
  `real/output_fake_data/` with the prefix `fake_data_`, and
  [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md)
  marks its figures. It deletes an old `real/output/`.

- [`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  and
  [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  print `FAKE REAL DATA`.

The argument decides only what
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
writes into the manifest.
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md),
[`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md),
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
and [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
read `fake_real_data` from the manifest of the real results on the disk.
So an R session without the argument still marks such results, and an R
session with it never marks results from real data.

- The rule `real_results_are_fake` of
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  warns while such real results are on the disk.

[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads such results as usual. The file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
never gives `fake_real_data`, because the server holds the real data.
`fake_real_data = TRUE` together with `one_file` stops.

## One file

The file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
calls `initialize_project()` with `one_file`, a list with these
elements:

- `project`, the name of the project folder on the computer that made
  the file. The run log names the project with it.

- `source_md5`, the md5 of the run script and of each file of the
  functions folder on that computer. The manifest stores it.

- `code`, the code of the functions folder as one quoted block, in the
  order that org sources the files.

With `one_file`, `initialize_project()` reads no `pbn.yml`, needs no
functions folder, and checks no code lines. It evaluates `code` in `env`
in the place of the source of the functions folder. A use of the cache
then does not check the `data_*.R` files on the disk, because the code
is in the file.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other project setup:
[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md),
[`folder()`](https://www.rwhite.no/pbn/reference/folder.md),
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_initialize_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines("extends: house_2026_11_17", file.path(home, "pbn.yml"))
writeLines(
  "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 3)))",
  file.path(home, "v01_functions", "_cfg.R")
)

env <- new.env()
p <- pbn::initialize_project(
  version = "v01",
  home = home,
  folders = c(main = file.path(home, "data")),
  env = env
)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/RtmpCwSRWy/pbn_initialize_example/v01_functions into 
p$results
#> [1] "/tmp/RtmpCwSRWy/pbn_initialize_example/v01_results"
pbn::folder("main")
#> [1] "/tmp/RtmpCwSRWy/pbn_initialize_example/data"
env$CFG$scenarios
#>   scenario sims
#> 1     real    1
#> 2     null    3

unlink(home, recursive = TRUE)
```
