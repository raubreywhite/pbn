# Check a project against the house layout

`check_project()` checks a project folder against the rules of its file
`pbn.yml` and prints one line per finding. In a non-interactive R
session it quits with status 1 when any rule fails.

## Usage

``` r
check_project(path = ".")
```

## Arguments

- path:

  The project folder. It MUST hold the file `pbn.yml`.

## Value

Invisibly, a data.frame of the findings with the columns `rule`,
`status`, `path` and `detail`.

## Details

The first line names the rule set of `pbn.yml`, and counts its changes
and its allowed paths. A rule with no finding prints one PASS line, and
a rule that is off prints one OFF line. Each finding prints one line:
`PASS|WARN|FAIL <rule> <detail>`. A count line comes last.

Every rule reads one list of files and their folders:

- When `path` is the top folder of a git repository, the list is the
  output of `git ls-files --cached --others --exclude-standard`. These
  are the tracked files, and the untracked files that git does not
  ignore.

- Otherwise, the list is every file and folder on disk.

Both lists leave out `.DS_Store`, `.Rhistory`, `.RData`, the Word lock
files `~$*`, and everything in `.git/` or `.Rproj.user/`.

The rules, with their level in the set `house_2026_11_17`:

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
  `vNN_results/<scenario>/sim_NNNN/results.qs2` of pbn 2026.11.22 fails
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md),
  or a `results.qs2` is directly in `vNN_results/`.

  The rule also reads each
  `vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2` on the disk,
  also when git ignores it, and fails a file that fails
  [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md).
  A clone of the repository has none, and the rule then needs none.

  The rule also reads each `results_gitexclude.qs2` beside a
  `results.qs2`, and each `sim_NNNN_gitexclude.qs2` beside a
  `sim_NNNN.qs2`, also when git ignores it. It fails when the file does
  not hold exactly the analysis kinds that the manifest of the results
  file puts there.
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

- `fake_data_names` (FAIL): a file in a folder `output_fake_data/` has a
  name without the prefix `fake_data_`, or a file with the prefix is in
  another folder.
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  writes these folders.

- `no_local_copy_of_pbn_function` (FAIL): a top-level assignment in a
  `vNN_functions/*.R` file defines a function with the name of a pbn
  export. The one exception is `get_data`: a project MAY name its data
  function `get_data(scenario, sim)`, and a run script calls the export
  as
  [`pbn::get_data()`](https://www.rwhite.no/pbn/reference/get_data.md).

- `loc_per_file` (FAIL): an `.R` file has more than `limit` code lines,
  as counted by
  [`org::loc_per_file()`](https://www.rwhite.no/org/reference/loc_per_file.html).
  The house `limit` is 1000.

- `cyclomatic_complexity` (FAIL): a function scores above `limit` in
  [`cyclocomp::cyclocomp()`](https://rdrr.io/pkg/cyclocomp/man/cyclocomp.html).
  The house `limit` is 15, the limit of the lint gate of the house
  packages. The rule reads the `.R` files of `loc_per_file`. It scores
  each function that a top-level assignment defines, and each function
  in a [`list()`](https://rdrr.io/r/base/list.html) of a top-level
  assignment, as `<list>$<name>`. A function inside a function counts
  toward the outer function. Top-level code that is not a function gets
  no score. The rule parses each file and runs no code of it. It skips a
  file that R cannot parse, which `loc_per_file` fails. The detail gives
  the file, the function and its score. A path under `allow` allows each
  function of one file.

- `no_environment_variables` (FAIL): a run script or a file in a
  functions folder uses `Sys.getenv` or `Sys.setenv`.

- `no_local_assign` (FAIL): an `.R` file uses a construct of `forbid` at
  any depth. The house `forbid` holds all seven constructs:
  [`local()`](https://rdrr.io/r/base/eval.html),
  [`assign()`](https://rdrr.io/r/base/assign.html),
  [`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html),
  [`assignInNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html),
  [`assignInMyNamespace()`](https://rdrr.io/r/utils/getFromNamespace.html),
  `<<-` and `->>`. Each of these creates or changes an object in a place
  other than the line where it stands. The rule reads the `.R` files of
  `loc_per_file`, parses each file and runs no code of it. A call with
  the prefix of its own package, `base::` or `utils::`, counts. A call
  of the same name from another package passes. A variable or an
  argument named `local` or `assign` passes, and so do `<-`, `->`, `=`
  and `:=`. The rule skips a file that R cannot parse. The detail gives
  the file, the line and the construct. A path under `allow` allows each
  finding of one file.

- `unknown_path` (WARN): the house layout does not name a top-level file
  or folder. The layout names `vNN_run.R`, `vNN_functions/`,
  `vNN_results/`, `vNN_cache_gitexclude/`, `vNN_test/`,
  `vNN_check_functions.R`, `check_project.R` and `pbn.yml`, among
  others. A top-level dotfile and a top-level `*.Rproj` file are never
  unknown. The rule also reads the files directly in `vNN_results/`.
  There it names the run log `run.md`, its PDF `run.pdf` and its errors
  file `run_errors_gitexclude.md`, and no rule needs them. It reads no
  deeper, so it never warns on the folders `output/` and
  `output_fake_data/` that
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  writes in `vNN_results/<scenario>/`, or on the folder
  `sims_gitexclude/` that
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes there.

The R parser, not a regular expression, finds the calls, the assignments
and the symbols.

## The file pbn.yml

Each project MUST have the file `pbn.yml` in its top folder `path`. pbn
reads no other file. A file `pbn.yaml` stops `check_project()` with the
advice to rename it. The smallest `pbn.yml` is one line:

    extends: house_2026_11_17

The key `extends` names a house rule set. A set is a dated file in the
folder `rules/` of pbn, and pbn never changes a set after its release. A
change to the house rules is a new set with a new date. pbn never
chooses a set for a project. So a new version of pbn does not change the
rules of a project. To read the set, run
`readLines(system.file("rules", "house_2026_11_17.yml", package = "pbn"))`.

The set gives each rule one of three levels:

- `fail`: a finding is FAIL, and `check_project.R` ends with status 1.

- `warn`: a finding is WARN.

- `off`: the rule does not run, and its one line is OFF.

The parameters of the rules are `limit` of `loc_per_file`, `limit` of
`cyclomatic_complexity` and `forbid` of `no_local_assign`. The list of
the rules above gives their house values.

The key `rules` MAY change the level of a rule, a parameter, or both. A
change merges into the entry of the set. So
`cyclomatic_complexity: {limit: 25}` keeps the level `fail`. The short
form `lower_case_names: warn` changes the level only. `forbid` MAY list
fewer constructs than the house set.

The key `allow` MAY list paths for each rule, relative to `path`. A path
allows the findings with exactly that path. A folder MAY end in `/`, and
it does not allow the files in it. An allowed finding is WARN, with
`(allowed)` at the end. A path that matches no finding is a WARN of the
rule `allow`.

Three rules are locked: `results_contract`, `gitexclude_not_tracked` and
`fake_data_names`. The first two keep data for each person out of git.
The third keeps the output of fake data apart from the output of real
data. `pbn.yml` cannot set a locked rule to `warn` or `off`, and cannot
allow a path for it. It cannot name a rule of
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
either.

This `pbn.yml` changes two rules and allows three paths:

    extends: house_2026_11_17
    rules:
      cyclomatic_complexity: {limit: 25}
      unknown_path: off
    allow:
      lower_case_names: [adhoc/Old_notes.txt, adhoc/Drafts/]
      no_local_assign:
        - adhoc/explore.R

`check_project()` and
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
stop before any rule runs on each of these, with a message that names
the key:

- there is no `pbn.yml`. The message gives the one line to write.

- there is a file `pbn.yaml`, or a file `pbn_allow.txt`. For
  `pbn_allow.txt`, the message gives its lines as the YAML of the key
  `allow`.

- `pbn.yml` is not valid YAML. This includes a key that is there twice
  and a tab. The message holds the message of the yaml package.

- there is no key `extends`, or it names a set that pbn does not have.

- a key other than `extends`, `rules` and `allow`.

- a name that is not a rule. The message names the nearest rule.

- a parameter that the rule does not have.

- a level other than `fail`, `warn` and `off`.

- a `limit` that is not a whole number of 1 or more.

- a construct in `forbid` that the rule does not know, or an empty
  `forbid`. To turn the rule off, set its level to `off`.

- a value under `allow` that is not a path, for example `off`. YAML
  reads `off`, `yes` and `no` as booleans, so a path with such a name
  needs quotes.

- a change or an allowed path of a locked rule.

## Examples

``` r
home <- file.path(tempdir(), "pbn_check_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines("extends: house_2026_11_17", file.path(home, "pbn.yml"))
writeLines(
  'pbn::initialize_project(version = "v01", home = ".", data_raw = "x")',
  file.path(home, "v01_run.R")
)
pbn::check_project(home)
#> Rules: house_2026_11_17, 0 overrides, 0 allowed paths (pbn.yml)
#> PASS  lower_case_names  no capital letter in a name
#> PASS  run_script_initialises  every vNN_run.R calls pbn::initialize_project() with its own version
#> PASS  version_items_complete  every vNN_run.R has a vNN_functions/, and the reverse
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and each vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2 on the disk passes check_results()
#> PASS  gitexclude_not_tracked  the project is not a git repository, so nothing to check
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  fake_data_names  every file in an output_fake_data/ folder, and no file elsewhere, has a name that starts with fake_data_
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  cyclomatic_complexity  no function of an .R file has a cyclomatic complexity above 15
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  no_local_assign  no .R file calls local, assign, delayedAssign, assignInNamespace or assignInMyNamespace, or uses <<- or ->>
#> PASS  unknown_path  the house layout names every top-level path
#> 15 PASS, 0 WARN, 0 FAIL, 0 OFF

unlink(home, recursive = TRUE)
```
