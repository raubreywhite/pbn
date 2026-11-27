# Check the file of to_one_file() on this computer

`check_one_file()` runs the file `<version>_one_file.R` of the project
in a new R session, on copies of the folders of the project. It prints
PASS when the real results are those of the project, or else the first
difference.

## Usage

``` r
check_one_file(...)
```

## Arguments

- ...:

  Nothing. A call with an argument stops. `check_one_file()` takes the
  version and the paths of the server from the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  of this R session.

## Value

Invisibly, `TRUE` on PASS and `FALSE` on FAIL.

## Details

Run the project as normal first, so that `<version>_results/real/` holds
its real results. In a one-file project, that folder is
`<version>_results/fake/`, see the section The folder fake/ of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
The run of the file writes `real/` of its temporary project folder, as
on the server. Write the file with
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md),
and call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
again in this R session, with `one_file_home` and `one_file_folders`.

`check_one_file()` then does these steps:

1.  It makes the lines of
    [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
    in memory, with the code of the project and the paths of the server
    of the last
    [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
    It compares them with `<version>_one_file.R` in the project, line by
    line. This R session is the one source of the paths, so a change of
    `one_file_home` in the run script also makes the file older. Only
    the line of the header with the version of pbn and the date does not
    count. When a line differs, the file is older than the code of the
    project. It prints FAIL with the number of the first line that
    differs and the line in both files, each cut to 80 characters. It
    says to run
    [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
    again. A file of an older pbn, with the argument `one_file`, also
    gets this FAIL, and the text says so. It prints FAIL also when the
    project has no such file.

2.  Without real results, it stops here. So the file of a project MAY be
    checked for ASCII, for a parse and for a left-out function before
    the first full run.

3.  It compares `manifest$source_md5` of the real results with the md5
    of the files of the project now, without the files that the manifest
    leaves out, see `source_md5` of
    [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
    When they differ, the results of the project are older than its
    code. It prints FAIL with the changed files and says to run
    `<version>_run.R` again. It runs nothing.

4.  It reads `manifest$failed_fake_checks` of the real results. When it
    names a fake scenario with a failed assertion, it prints FAIL with
    the scenario and its `check.csv`, as
    [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
    does. It runs nothing.

5.  It makes a temporary folder with an empty project folder, and a copy
    of each folder of the project with all its files. The empty project
    folder stands for the server, where there is no functions folder and
    no `pbn.yml`.

6.  It writes a copy of the file of the project to the temporary folder.
    In the copy, each line with a path of the server gets the temporary
    path. These are the lines where
    [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
    with the temporary paths writes another line. It runs the copy with
    `Rscript` in a new R session. So the check runs the file that goes
    into git.

7.  It compares `results.qs2` and `results_gitexclude.qs2` of the real
    results of the run with those of the project, element by element.

8.  It calls each block of
    [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
    on the real results of the project and on those of the run, in one
    more new R session. That session attaches no package of the run
    script. It compares what
    [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
    prints for each page of each block, as text, without the line
    `FAKE REAL DATA`. A block that stops on either results, or that
    prints other text, is a FAIL that names the block.

Five fields of the manifest MAY differ, and the comparison leaves them
out:

- `run_date`, the date of the run.

- `packages`, the loaded packages, because the new R session loads other
  packages than the R session of the normal run.

- the column `from_cache` of `data_cache`, because the copy has no
  cache.

- `fake_real_data`, because the file never runs on fake real data. See
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).

- `failed_fake_checks`, because the file runs no fake scenario. Step 4
  checks it on the real results of the project.

The comparison also leaves `<version>_functions/blocks.R` and
`<version>_functions/formatters.R` out of `source_md5`, also for results
of an older pbn that hold them. Every other element MUST be the same,
also `source_md5`, `data_files` and `cache_key`. Two doubles are the
same when they differ by at most `sqrt(.Machine$double.eps)`, about
1.5e-8, of the larger absolute value. That is the default tolerance of
[`all.equal()`](https://rdrr.io/r/base/all.equal.html). Below that size,
the limit is that absolute difference. The same model in two R sessions
MAY differ in the last digits, and no printed number shows it. Every
other value MUST be identical, also a double with a class, such as a
`Date` or a `POSIXct`. The attributes MUST be identical too, in any
order. So the results do not depend on the place of the project and of
its folders. The copies keep the modification times of the files, with
`file.copy(copy.date = TRUE)`. That is why `data_files` and the cache
key agree. pbn reads no content of a data file, so a copy with new times
looks like changed data. The times are whole seconds, so a copy that
loses a part of a second, as on Windows, still agrees.

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
[`export_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md),
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md),
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
