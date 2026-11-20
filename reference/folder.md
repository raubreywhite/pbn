# The path of a folder of the project

`folder()` gives the path of one folder of `folders` of
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
Project code reads each folder outside the project through it.

## Usage

``` r
folder(name = "main", ...)
```

## Arguments

- name:

  The name of the folder in `folders` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  The default `"main"` is also the name of one folder given without a
  name.

- ...:

  Parts of a path inside the folder, each one string, for example
  `pbn::folder("main", "sub", "d.csv")`. `folder()` joins them to the
  folder with `/`. A part MUST NOT start with `/`, `\\` or a drive such
  as `C:`. It MUST NOT hold `..`, so a path never leaves the folder.

## Value

The path of the folder, or of the path inside it, one string. A folder
that existed at
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
has its normalised path with `/`, and another folder has the path as
`folders` gave it.

## Details

`folder()` stops before
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
and on a name that the project does not declare. The error names the
declared folders. A folder that does not exist still has its path, so
the code of a fake scenario runs on a computer without the real data.

[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) gives
the folders to each worker, so `folder()` works in a data function and
in an analysis on a worker too.

The path of a folder is never in a results file or in a cache key. The
manifest names the files of a folder
`<folder name>/<path in the folder>`, so the same data in another place
gives the same results.

## See also

[`vignette("pbn")`](https://www.rwhite.no/pbn/articles/pbn.md), which
builds and runs a small fake project.

Other project setup:
[`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md),
[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`new_version()`](https://www.rwhite.no/pbn/reference/new_version.md),
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_folder_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines("extends: house_2026_11_20", file.path(home, "pbn.yml"))
writeLines("CFG <- list()", file.path(home, "v01_functions", "_cfg.R"))
pbn::initialize_project(
  "v01",
  home,
  folders = c(main = file.path(home, "data"), lookup = file.path(home, "codes")),
  env = new.env()
)
#> You are NOT sourcing into .GlobalEnv. All functions will be sourced into an environment that is returned from this function.
#> Sourcing all code inside /tmp/Rtmpr8qhGR/pbn_folder_example/v01_functions into 
pbn::folder("lookup")
#> [1] "/tmp/Rtmpr8qhGR/pbn_folder_example/codes"
pbn::folder("main", "sub", "d.csv")
#> [1] "/tmp/Rtmpr8qhGR/pbn_folder_example/data/sub/d.csv"

unlink(home, recursive = TRUE)
```
