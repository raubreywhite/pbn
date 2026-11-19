# Freeze a version of a project

`freeze_version()` writes the file `<version>_frozen.md5` in `home`,
with the md5 of `<version>_run.R` and of each file under
`<version>_functions/`. The rule `frozen_versions_unchanged` of
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
then fails when one of these files changes, or when a file comes or
goes.

## Usage

``` r
freeze_version(version, home = ".")
```

## Arguments

- version:

  The version, `"v"` and two digits, for example `"v01"`.

- home:

  The project folder.

## Value

Invisibly, the path of the file, relative to `home`.

## Details

The file has one line for each file, `<md5> <path>`, in the format of
the command `md5sum`. Each path is relative to `home`, and the lines are
sorted by path in the C locale. The files under `<version>_functions/`
are all files at any depth, hidden files too.

The md5 of a file with the extension `.R` or `.r` is the md5 of its text
with LF line ends. Each CR LF counts as LF, because R parses the file
the same either way. So a checkout with CR LF line ends, for example on
Windows with the git setting `core.autocrlf = true`, gives the same md5
for such a file. The md5 of every other file is that of its bytes. When
each `.R` file has LF line ends, `md5sum -c v01_frozen.md5`, run in
`home`, also checks the files.

Freeze a version when its results go into a manuscript or to a reader. A
change after that goes into a new version. A second call of
`freeze_version()` writes the file again, so it records every change
since the first call as intended. It then prints the files whose md5 it
changes.

## See also

Other checks:
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md),
[`verify()`](https://www.rwhite.no/pbn/reference/verify.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_freeze_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines("x <- 1", file.path(home, "v01_run.R"))
writeLines("f <- function() return(1)", file.path(home, "v01_functions", "f.R"))
pbn::freeze_version("v01", home = home)
readLines(file.path(home, "v01_frozen.md5"))
#> [1] "6e5f7d320bb45e981dc6db489eb750ea  v01_functions/f.R"
#> [2] "7a520755d4be04115776fc6fbdff8e83  v01_run.R"        

unlink(home, recursive = TRUE)
```
