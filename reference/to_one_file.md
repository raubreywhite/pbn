# Write the run of the real data as one file for a server

`to_one_file()` writes one R script that runs the scenario `"real"` of
the project on a server without a network. A person pastes the file into
R there and runs it from the top to the end.

## Usage

``` r
to_one_file(...)
```

## Arguments

- ...:

  Nothing. A call with an argument stops, and the error names the
  arguments `one_file_home` and `one_file_folders` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).

## Value

Invisibly, the path of the file. It prints the path, the number of
lines, the size in bytes and the length of the longest line.

## Details

Call
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
first, in the project on this computer, with the paths of the server as
`one_file_home` and `one_file_folders`. `to_one_file()` takes the
version, the project and the paths from that call. Without the paths, it
stops and prints the two arguments to add. It stops when the run script
or a file of the functions folder changed after that call. It needs no
results. So you MAY generate and read the file before the first full
run. The results on the server go to
`<one_file_home>/<version>_results/`.

The file holds these parts, in this order:

1.  A header with the version of pbn, the date, the project folder and
    the version. It also gives the md5 of each file in it and the
    workers.

2.  `setwd(home)`, so that the run script runs in its project folder, as
    `Rscript <version>_run.R` does.

3.  The run script as it is written, with three changes. Its call of
    [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
    gets the paths of the server as `home` and `folders`, and the
    argument `generated`. `generated` holds the code of each `.R` file
    of the functions folder, in the order that org sources them. The
    call has no `one_file_home`, `one_file_folders` or `fake_real_data`.
    The call `pbn::scenarios(CFG)` becomes
    `pbn::scenarios(CFG, only = "real")`. A line with only a comment
    such as `# pbn::to_one_file()` is not in the file. The section The
    md5 of the run script of
    [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
    gives the rule.

4.  A comment that says what to type next, for example
    [`pbn::show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md).

The section One file of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
gives the rules of `generated`. On the server, the run needs no
`pbn.yml` and no functions folder. pbn writes the results and the run
log under `one_file_home`, and, for a data function with `cache = TRUE`,
the cache there too. The manifest holds the md5 of the files of this
computer. On this computer,
[`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
checks that the file gives the real results of the project.

The functions `expect_<scenario>()` of the fake scenarios of
`CFG$scenarios` are not in the file, because the run has the real data
only. The comment lines directly above such a function go with it, up to
a blank line or a line with code. `expect_real()` stays. The fake data
code stays, because it is inside the data functions.

The run script MAY give `fake_real_data = TRUE` for this computer. The
file never does, because the server holds the real data.

The file is ASCII, because it goes through a clipboard, which can change
a byte that is not ASCII. pbn reads each file of the project as UTF-8
and changes each character that is not ASCII on the parse data, token by
token. The files of the project stay as they are:

- In a string, the character becomes its escape, `\uXXXX`, or
  `\UXXXXXXXX` beyond the BMP, for example `"\u2265"` for the sign
  greater than or equal. Every other character of the string stays as it
  is written, also its quotes, its escapes and its line breaks. In a raw
  string `r"(...)"`, `\u` is text and not an escape. So pbn writes such
  a raw string again from its value. It becomes a normal string in
  double quotes on one line, with `\n` for each line break. R does not
  allow `\u` in a string with an escape `\x` or an octal escape. So pbn
  writes such a string from its value too. The code of the file is
  [`identical()`](https://rdrr.io/r/base/identical.html) to the code of
  the project. Such a string gets the encoding mark `"UTF-8"` where the
  string of the project MAY have `"unknown"`, and
  [`identical()`](https://rdrr.io/r/base/identical.html) treats the two
  as equal.

- In a comment, the character becomes the text `<U+XXXX>`.

- In a name, for example a name in backticks, `to_one_file()` stops,
  because R reads a name only as it is written.

`to_one_file()` stops when, and writes no file:

- the run script has not exactly one top-level call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  or not exactly one call `pbn::scenarios(CFG)` with no other argument.
  It names what it found.

- a file in the file holds a name that is not ASCII, a NUL, or a line
  that is not UTF-8. It also stops on a string that is not valid UTF-8
  after its escapes. The error names the file and the line.

- a path of the server or the name of the project folder is not ASCII.

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

- The comments of the code go into the file as they are written, with
  `<U+XXXX>` for a character that is not ASCII. So a comment with a path
  of this computer reaches the server.

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
