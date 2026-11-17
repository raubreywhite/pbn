# Write the run of the real data as one file for a server

`to_one_file()` writes one R script that runs the scenario `"real"` of
the project on a server without a network. A person pastes the file into
R there and runs it from the top to the end.

## Usage

``` r
to_one_file(version, home, folders)
```

## Arguments

- version:

  The version, for example `"v01"`. It MUST be the version of the last
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  in this R session.

- home:

  The project folder on the server, one string, for example
  `"N:/durable/project"`. The results go to `<home>/<version>_results/`.

- folders:

  The folders of the project on the server, as a named character vector.
  It MUST name exactly the folders of `folders` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).

## Value

Invisibly, the path of the file. It prints the path, the number of
lines, the size in bytes and the length of the longest line.

## Details

Call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
for `version` first, in the project on this computer. `to_one_file()`
takes the project from that call. It stops when the run script or a file
of the functions folder changed after it.

The file holds these parts, in this order:

1.  A header with the version of pbn, the date, the project folder and
    the version. It also gives the md5 of each file in it and the
    workers.

2.  `setwd(home)`, so that the run script runs in its project folder, as
    `Rscript <version>_run.R` does.

3.  The run script as it is written, with two changes. Its call of
    [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
    gets the paths of the server and the argument `one_file`. `one_file`
    holds the code of each `.R` file of the functions folder, in the
    order that org sources them. The call `pbn::scenarios(CFG)` becomes
    `pbn::scenarios(CFG, only = "real")`.

4.  A comment that says what to type next, for example
    [`pbn::show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md).

The section One file of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
gives the rules of `one_file`. On the server, the run needs no `pbn.yml`
and no functions folder. pbn writes the results and the run log under
`home`, and, for a data function with `cache = TRUE`, the cache there
too. The manifest holds the md5 of the files of this computer. On this
computer,
[`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
checks that the file gives the real results of the project.

The functions `expect_<scenario>()` of the fake scenarios of
`CFG$scenarios` are not in the file, because the run has the real data
only. `expect_real()` stays. The fake data code stays, because it is
inside the data functions.

`to_one_file()` stops when, and writes no file:

- the run script has not exactly one top-level call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  or not exactly one call `pbn::scenarios(CFG)` with no other argument.
  It names what it found.

- a file in the file holds a byte that is not ASCII, or a NUL. The error
  names the file and the line. The file goes through a clipboard, which
  can change such a byte.

- `home`, a path of `folders` or the name of the project folder is not
  ASCII.

- `folders` misses a folder of the project, or names one that the
  project does not have. The error names it.

- code that stays in the file uses a name of an `expect_<scenario>()`
  that the file leaves out. The file would stop on the server. The error
  names the file and the line.

- the assignment of such a function shares a line with other code. That
  is text before it on its first line, or text other than a comment
  after it on its last line.

- the file does not parse.

The file has LF line ends. pbn does not cut long lines, and it prints
the length of the longest line. The file keeps the `workers` of
[`pbn::run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) of
the run script, and its header says how to change it. The paths go into
the file as they are given, so give `/` in a path for Windows too, for
example `"N:/durable/project"`.

The file is always `<version>_one_file.R` in the folder of the project
of the last
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
of this R session. The printed line names that folder.

These are limits of the file:

- The comments of the code go into the file as they are written. So a
  comment with a path of this computer reaches the server.

- The rule `no_absolute_paths` of
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  does not see a path with one name such as `"/data"`, or a path that
  [`paste0()`](https://rdrr.io/r/base/paste.html) or
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html) builds. Such a path
  in the code reaches the server too.

Put the file in git. It is what ran on the server.
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
knows its name, and its code rules do not read it, because it holds the
paths of the server by design.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), section
"Running on an air-gapped server".

Other one file:
[`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md),
[`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
