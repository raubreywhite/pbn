# Changelog

## pbn 2026.11.16

- First trial version: the house layout,
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md),
  the results contract,
  [`table1()`](https://www.rwhite.no/pbn/reference/table1.md), and
  [`run_version()`](https://www.rwhite.no/pbn/reference/run_version.md).
- A run with `data_raw=<path>` writes its results to
  `<version>_results/scenario/<basename of the normalised path>/`, so it
  never overwrites the real `results.qs2`.
- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  resolves `..`, `~` and a trailing slash in `data_raw=<path>` first.
  Before, `data_raw=..` wrote over the real `results.qs2`.
- A scenario run stops when its data folder is the default data folder,
  or is or holds the real results folder.
- A scenario run stops when its scenario folder holds the results of
  another data folder with the same basename.
- `manifest$data_md5` lists the files in the data folder at
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  so a file that the run writes later is not an input.
- `manifest$packages` always names `qs2`, and `manifest$run_date` is the
  date only, so an unchanged run on the same day writes the same bytes.
- [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) stops on a
  level of `by` named `"All"`. A level with no non-missing value is left
  out of the test. With fewer than two groups left, `p` and `test` are
  `NA`.
- In a git repository,
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  reads only the files that git does not ignore. It never reads
  `.DS_Store`, `.Rhistory`, `.RData`, `~$*` or `.Rproj.user/`.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  checks the `results.qs2` of every scenario folder, and `unknown_path`
  knows `vNN_check_functions.R`, `vNN_check_scenarios.R`, top-level
  dotfiles and `*.Rproj`.
