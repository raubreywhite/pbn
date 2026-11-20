# Start a new version of a project

`new_version()` freezes the version `from` and copies its run script and
its functions folder to the version `to`. The project then passes the
rule `frozen_versions_unchanged` of
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md).

## Usage

``` r
new_version(from = "v01", to = NULL, home = ".")
```

## Arguments

- from:

  The version to copy, `"v"` and two digits, for example `"v01"`.

- to:

  The new version, or `NULL`, the default, for the version after `from`.
  For `"v01"` that is `"v02"`. After a manual manuscript
  `manuscript/manual/vNN_<INITIALS>.docx`, `to` is `v(NN+1)`, as the
  rule `manual_not_ahead` of
  [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  says.

- home:

  The project folder.

## Value

Invisibly, `to`.

## Details

`new_version()` does these steps in this order:

1.  It freezes `from` with
    [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md),
    when `<from>_frozen.md5` does not exist. When it exists and no file
    of `from` changed after it, `new_version()` leaves it as it is.

2.  It copies `<from>_run.R` to `<to>_run.R`, and each file under
    `<from>_functions/` to `<to>_functions/`.

3.  It changes the argument `version` of the call
    [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
    in `<to>_run.R` from `from` to `to`. It changes no other text of the
    file.

4.  It adds the section `## <to>` above the first section `## ` of
    `CHANGELOG.md`, with one line that names `from`. It creates
    `CHANGELOG.md` when the file does not exist. When the section is
    there already, it leaves the file as it is, and its message says so.

It stops before it writes anything in each of these cases:

- `<to>_run.R`, `<to>_functions/` or `<to>_frozen.md5` exists. The error
  names each one.

- `<from>_run.R` or `<from>_functions/` does not exist.

- `from` is not the newest version of the project. The error names the
  newest.

- `to` is not newer than each version of the project.

- `<from>_frozen.md5` exists, and a file of `from` changed, came or went
  after it. The error names each file. Put the files back, or record the
  change as intended with
  [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md).

- `<from>_run.R` has no call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  whose argument `version` is the string `from`.

When a step stops after `new_version()` wrote something, it removes each
file and folder that the call created, also `<from>_frozen.md5`. It
writes `CHANGELOG.md` back as it was. So a second call can start again.

## See also

Other project setup:
[`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md),
[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md),
[`folder()`](https://www.rwhite.no/pbn/reference/folder.md),
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
[`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_new_version_example")
dir.create(home)
pbn::create_project(home = home)
#> create_project(): wrote 14 files in /tmp/RtmpRxfTwP/pbn_new_version_example. The project is at version v01. Run pbn::verify("v01") in that folder.
pbn::new_version("v01", home = home)
#> new_version(): v01 is now frozen in v01_frozen.md5. Wrote v02_run.R and v02_functions/. Added the section ## v02 to CHANGELOG.md.
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
