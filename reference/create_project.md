# Create a new project

`create_project()` writes the files of a new project at version `v01`
into the folder `home`. The project passes
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
at once, and [`verify()`](https://www.rwhite.no/pbn/reference/verify.md)
gives a PASS on the fake scenarios without the real data.

## Usage

``` r
create_project(home = ".", folders = c(main = "../data"))
```

## Arguments

- home:

  The project folder. It MUST exist.

- folders:

  The folders outside the project that the project reads, as for
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  The run script gets them as they are given.

## Value

Invisibly, the paths of the files that it wrote, relative to `home`.

## Details

`create_project()` writes these files:

- `pbn.yml`, with the line `extends: house_2026_11_20`.

- `v01_run.R`, the run script. Its data steps are `raw`, `clean` and
  `cohort`, and it has one analysis kind, `risk_difference`.

- `v01_functions/`, with `_cfg.R`, the data steps `data_raw.R`,
  `data_clean.R` and `data_cohort.R`, the analysis
  `fit_risk_difference.R`, the checks `expect_known.R` and the output
  function `output.R`.

- `README.md`, with a title from the name of `home`, a section How to
  run, and the arithmetic of the fake scenario.

- `CHANGELOG.md`, with the section `## v01`.

- `AGENTS.md`, a copy of
  `system.file("templates", "AGENTS.md", package = "pbn")`.

- `.gitignore`, with the line `*gitexclude*`.

- `check_project.R`, with the line
  [`pbn::check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).

`CFG$scenarios` has the scenario `real` and the fake scenario `known`.
`known` has 10 rows. The step cohort excludes 1, and the difference of
the risks is 0.35. `expect_known()` checks both numbers.

The run script calls
`pbn::scenarios(CFG, only = setdiff(CFG$scenarios$scenario, "real"))`.
So a run includes each fake scenario of `CFG$scenarios`, and never
`real`. That is why `verify("v01")` gives a PASS on a computer without
the real data. Its verdict line then says that the PASS covers the fake
scenarios only. `data_raw()` stops for `real` with the message to
replace the [`stop()`](https://rdrr.io/r/base/stop.html) with code that
reads the real data. When that code is there, remove `only` from the
call.

The files hold no comments. `README.md` says why, and `CHANGELOG.md`
says what changed.

`create_project()` writes no file that is already there. It stops before
it writes anything in each of these cases:

- one of its files exists. The error names each such file.

- the package cohort is not installed, because the step `cohort` needs
  it. The error gives the line that installs it:
  `pak::pak(c("raubreywhite/pbn", "raubreywhite/cohort"))`.

When a write stops part way, `create_project()` removes each file and
folder that the call created. So a second call can start again.

## See also

Other project setup:
[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md),
[`folder()`](https://www.rwhite.no/pbn/reference/folder.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`new_version()`](https://www.rwhite.no/pbn/reference/new_version.md),
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_create_example")
dir.create(home)
pbn::create_project(home = home)
#> create_project(): wrote 14 files in /tmp/Rtmp9Ef4aF/pbn_create_example. The project is at version v01. Run pbn::verify("v01") in that folder.
pbn::check_project(home)
#> Rules: house_2026_11_20, 0 overrides, 0 allowed paths (pbn.yml)
#> PASS  lower_case_names  no capital letter in a name
#> PASS  run_script_initialises  every vNN_run.R calls pbn::initialize_project() with its own version
#> PASS  version_items_complete  every vNN_run.R has a vNN_functions/, and the reverse
#> PASS  frozen_versions_unchanged  every version but the newest has a vNN_frozen.md5, and no file of a frozen version changed after pbn::freeze_version()
#> PASS  readme_and_changelog  README.md and CHANGELOG.md exist, and CHANGELOG.md has a section ## vNN for every version
#> PASS  changelog_with_code  the project is not a git repository, so nothing to compare
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and each vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2 on the disk passes check_results()
#> PASS  gitexclude_not_tracked  the project is not a git repository, so nothing to check
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  built_reads_own_version  every read_results() call in manuscript/built/vNN/ reads the version vNN
#> PASS  built_has_run  every folder manuscript/built/vNN/ has a vNN_run.R
#> PASS  manual_not_ahead  every manuscript/manual/vNN_<INITIALS>.docx is older than the newest run version
#> PASS  fake_data_names  every file in an output_fake_data/ folder, and no file elsewhere, has a name that starts with fake_data_
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  cyclomatic_complexity  no function of an .R file has a cyclomatic complexity above 15
#> PASS  comment_share  no run script or .R file of a functions folder has more than 25% comment lines
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  no_cross_version_reads  no string in a run script or a functions folder names another version or adhoc/
#> PASS  no_local_assign  no .R file calls local, assign, delayedAssign, assignInNamespace or assignInMyNamespace, or uses <<- or ->>
#> PASS  no_escape_hatches  no run script or functions folder calls suppressWarnings(), suppressMessages(), try(), tryCatch() or withCallingHandlers()
#> PASS  no_absolute_paths  no .R file has an absolute path or a file.path() from a string, outside the arguments of pbn::initialize_project()
#> PASS  blocks_list  blocks, when a functions folder has it, is a list of named functions in vNN_functions/blocks.R, and no other code uses a name of that file
#> PASS  real_results_are_fake  no real results on the disk come from fake real data
#> PASS  unknown_path  the house layout names every top-level path
#> 27 PASS, 0 WARN, 0 FAIL, 0 OFF

unlink(home, recursive = TRUE)
```
