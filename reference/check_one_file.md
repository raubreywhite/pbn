# Check the file of to_one_file() on this computer

`check_one_file()` runs the file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md) in
a new R session, on copies of the folders of the project. It prints PASS
when the real results are those of the project, or else the first
difference.

## Usage

``` r
check_one_file(version)
```

## Arguments

- version:

  The version, for example `"v01"`. It MUST be the version of the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

## Value

Invisibly, `TRUE` on PASS and `FALSE` on FAIL.

## Details

Run the project as normal first, so that `<version>_results/real/` holds
its real results, and call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
again in this R session. Without real results, `check_one_file()` runs
the checks of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
that need no results, and stops. So the file of a project MAY be checked
for ASCII, for a parse and for a left-out function before the first full
run.

`check_one_file()` then does these steps:

1.  It compares `manifest$source_md5` of the real results with the md5
    of the files of the project now, without
    `<version>_functions/blocks.R`. When they differ, the results of the
    project are older than its code. It prints FAIL with the changed
    files and says to run `<version>_run.R` again. It runs nothing.

2.  It makes a temporary folder with an empty project folder, and a copy
    of each folder of the project with all its files. The empty project
    folder stands for the server, where there is no functions folder and
    no `pbn.yml`.

3.  It writes the file of
    [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
    with these paths, and runs it with `Rscript` in a new R session.

4.  It compares `results.qs2` and `results_gitexclude.qs2` of the real
    results of the run with those of the project, element by element.

5.  It calls each block of
    [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
    on the real results of the project and on those of the run, in one
    more new R session. That session attaches no package of the run
    script. It compares what
    [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
    prints for each page of each block, as text, without the line
    `FAKE REAL DATA`. A block that stops on either results, or that
    prints other text, is a FAIL that names the block.

Four fields of the manifest MAY differ, and the comparison leaves them
out:

- `run_date`, the date of the run.

- `packages`, the loaded packages, because the new R session loads other
  packages than the R session of the normal run.

- the column `from_cache` of `data_cache`, because the copy has no
  cache.

- `fake_real_data`, because the file never runs on fake real data. See
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).

The comparison also leaves `<version>_functions/blocks.R` out of
`source_md5`, also for results of an older pbn that hold it. Every other
element MUST be identical, also `source_md5`, `data_files` and
`cache_key`. So the results do not depend on the place of the project
and of its folders. The copies keep the modification times of the files,
with `file.copy(copy.date = TRUE)`. That is why `data_files` and the
cache key agree. pbn reads no content of a data file, so a copy with new
times looks like changed data. The times are whole seconds, so a copy
that loses a part of a second, as on Windows, still agrees.

When the run of the file stops with an error, `check_one_file()` prints
FAIL with the exit status and the last 20 lines of the output of the
run, and compares nothing. On PASS, it deletes the temporary folder. On
FAIL, it keeps it, and the text names the folder with the file and the
output of its run.

The check copies each folder, so it needs the disk space of the real
data once more. These are its limits:

- It cannot check what differs on the server, for example the versions
  of R and of the packages there.

- It runs the file with `Rscript`, and on the server a person pastes it
  into an interactive R session. So it does not cover code that depends
  on [`interactive()`](https://rdrr.io/r/base/interactive.html).

- It compares the blocks on the real data of this computer. A block that
  stops only on other data, for example on an empty table, passes here.

- pbn reads no content of a data file. A data file that changed in place
  and kept its size and its modification time is not seen.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), section
"Running on an air-gapped server".

Other one file:
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md),
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
