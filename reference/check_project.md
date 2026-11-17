# Check a project against the house layout

`check_project()` checks a project folder against the house layout and
prints one line per finding. In a non-interactive R session it quits
with status 1 when any rule fails.

## Usage

``` r
check_project(path = ".")
```

## Arguments

- path:

  The project folder.

## Value

Invisibly, a data.frame of the findings with the columns `rule`,
`status`, `path` and `detail`.

## Details

A rule with no finding prints one PASS line. Each finding prints one
line: `PASS|WARN|FAIL <rule> <detail>`. A count line comes last.

Every rule reads one set of files and their folders:

- When `path` is the top folder of a git repository, the set is the
  output of `git ls-files --cached --others --exclude-standard`. These
  are the tracked files, and the untracked files that git does not
  ignore.

- Otherwise, the set is every file and folder on disk.

Both sets leave out `.DS_Store`, `.Rhistory`, `.RData`, the Word lock
files `~$*`, and everything in `.git/` or `.Rproj.user/`.

The rules:

- `lower_case_names` (FAIL): a file or folder name has a capital letter.
  Everything under `background/` is not checked. These are exempt:
  `README.md`, `CHANGELOG.md`, `AGENTS.md`, `CLAUDE.md`, `LICENSE`,
  `DESCRIPTION`, a top-level dotfile, a top-level `*.Rproj` file, file
  extensions, and the initials in
  `manuscript/manual/vNN_<INITIALS>.docx`.

- `run_script_initialises` (FAIL): a `vNN_run.R` at the top level has no
  call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  whose `version` argument is the string `"vNN"`. The detail names each
  argument of the call that
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  does not have.

- `version_items_complete` (FAIL): a `vNN_run.R` has no
  `vNN_functions/`, or the reverse.

- `results_contract` (FAIL): a `vNN_results/<scenario>/results.qs2` or a
  `vNN_results/<scenario>/sim_NNNN/results.qs2` fails
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
  or a `results.qs2` is directly in `vNN_results/`.

  The rule also reads each `results_gitexclude.qs2` beside such a file,
  also when git ignores it. It fails when the file does not hold exactly
  the analysis kinds that the manifest of `results.qs2` puts there.
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  does not check it.

- `gitexclude_not_tracked` (FAIL): git tracks a path with `gitexclude`
  in its name, from `git ls-files` in `path`. Such a path holds data for
  each person. When git is not installed or `path` is not in a git
  repository, the rule has nothing to check. It then prints one PASS
  line that says so.

- `scenario_checks` (FAIL): a `vNN_results/<scenario>/check.csv` has a
  value of `pass` that is not `TRUE`, holds no assertion, or has no
  column `pass` that R can read.
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes these files.

- `manual_manuscript_names` (FAIL): a file in `manuscript/manual/` does
  not match `^v[0-9]{2}_[A-Z]{2,4}[0-9]?\.docx$`.

- `manuscript_reads_real` (FAIL): an `.R`, `.qmd` or `.Rmd` file in
  `manuscript/built/` calls
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  or
  [`pbn::read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  with a `scenario` argument that is not the string `"real"`. A call
  without `scenario` passes. In `.qmd` and `.Rmd` files, the rule reads
  the R chunks that
  [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html) extracts
  and the inline R code.

- `no_local_copy_of_pbn_function` (FAIL): a top-level assignment in a
  `vNN_functions/*.R` file defines a function with the name of a pbn
  export.

- `loc_per_file` (FAIL): an `.R` file has more than 1000 code lines, as
  counted by
  [`org::loc_per_file()`](https://www.rwhite.no/org/reference/loc_per_file.html).

- `no_environment_variables` (FAIL): a run script or a file in a
  functions folder uses `Sys.getenv` or `Sys.setenv`.

- `unknown_path` (WARN): the house layout does not name a top-level file
  or folder. The layout names `vNN_run.R`, `vNN_functions/`,
  `vNN_results/`, `vNN_cache_gitexclude/`, `vNN_test/`,
  `vNN_check_functions.R` and `check_project.R`, among others. A
  top-level dotfile and a top-level `*.Rproj` file are never unknown.

The R parser, not a regular expression, finds the calls, the assignments
and the symbols.

The file `pbn_allow.txt` in the project folder MAY list exceptions.
Write one `rule<TAB>path` per line, with the path relative to the
project folder. An allowed finding prints as WARN, with `(allowed)` at
the end. An entry that matches no finding prints as a WARN of the rule
`pbn_allow`.

## Examples

``` r
home <- file.path(tempdir(), "pbn_check_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines(
  'pbn::initialize_project(version = "v01", home = ".", data_raw = "x")',
  file.path(home, "v01_run.R")
)
pbn::check_project(home)
#> PASS  lower_case_names  no capital letter in a name
#> PASS  run_script_initialises  every vNN_run.R calls pbn::initialize_project() with its own version
#> PASS  version_items_complete  every vNN_run.R has a vNN_functions/, and the reverse
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and vNN_results/<scenario>/sim_NNNN/results.qs2 passes check_results()
#> PASS  gitexclude_not_tracked  the project is not a git repository, so nothing to check
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  unknown_path  the house layout names every top-level path
#> 12 PASS, 0 WARN, 0 FAIL

unlink(home, recursive = TRUE)
```
