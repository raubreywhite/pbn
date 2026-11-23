#' Check a project against the house layout
#'
#' `check_project()` checks a project folder against the rules of its file
#' `pbn.yml` and prints one line per finding. In a non-interactive R session
#' it quits with status 1 when any rule fails.
#'
#' @param path The project folder. It MUST hold the file `pbn.yml`.
#' @details
#' The first line names the rule set of `pbn.yml`, and counts its changes
#' and its allowed paths. A rule with no finding prints one PASS line, and a
#' rule that is off prints one OFF line. Each finding prints one line:
#' `PASS|WARN|FAIL  <rule>  <detail>`. The detail of a WARN or a FAIL ends
#' with `Fix:` and the change that removes the finding. A count line comes
#' last.
#'
#' Every rule reads one list of files and their folders:
#' - When `path` is the top folder of a git repository, the list is the
#'   output of `git ls-files --cached --others --exclude-standard`. These are
#'   the tracked files, and the untracked files that git does not ignore.
#' - Otherwise, the list is every file and folder on disk.
#'
#' Both lists leave out `.DS_Store`, `.Rhistory`, `.RData`, the Word lock
#' files `~$*`, and everything in `.git/` or `.Rproj.user/`.
#'
#' The rules, with their level in the set `house_2026_11_20`, are below.
#' The set `house_2026_11_19` has the same rules at the same levels, without
#' `built_reads_own_version`, `built_has_run` and `manual_not_ahead`. The
#' set `house_2026_11_17` also does not have `frozen_versions_unchanged`,
#' `readme_and_changelog`, `changelog_with_code`, `comment_share`,
#' `no_cross_version_reads` and `no_escape_hatches`. A rule that the set of
#' the project does not have prints no line.
#' - `lower_case_names` (FAIL): a file or folder name has a capital letter.
#'   Everything under `background/` is not checked. These are exempt:
#'   `README.md`, `CHANGELOG.md`, `AGENTS.md`, `CLAUDE.md`, `LICENSE`,
#'   `DESCRIPTION`, a top-level dotfile, a top-level `*.Rproj` file, file
#'   extensions, and the initials in `manuscript/manual/vNN_<INITIALS>.docx`.
#' - `run_script_initialises` (FAIL): a `vNN_run.R` at the top level has no
#'   call `pbn::initialize_project()` whose `version` argument is the string
#'   `"vNN"`. A call with an argument that [initialize_project()] does not
#'   have does not count, and the detail names that argument.
#' - `version_items_complete` (FAIL): a `vNN_run.R` has no `vNN_functions/`,
#'   or the reverse.
#' - `frozen_versions_unchanged` (FAIL): one of these is true:
#'   - a version older than the newest has no `vNN_frozen.md5` of
#'     [freeze_version()]. A version is a `vNN_run.R` or a `vNN_functions/`
#'     at the top level, and the newest is the one with the largest `NN`.
#'     So a deleted `vNN_frozen.md5` of an older version fails. The newest
#'     version needs none. The fix names the call
#'     `pbn::freeze_version("vNN")`.
#'   - a file of a version changed, came or went after `freeze_version()`
#'     wrote its `vNN_frozen.md5`. The files are `vNN_run.R` and each file
#'     under `vNN_functions/`, as `freeze_version()` lists them from the
#'     disk. The detail names the file, with `changed`, `added` or
#'     `removed`. A `.R` file whose line ends change between LF and CR LF,
#'     and nothing else, does not count as changed. A change of the line
#'     ends of any other file counts, see [freeze_version()].
#'   - a line of `vNN_frozen.md5` is not `<md5>  <path>`.
#' - `readme_and_changelog` (FAIL): `README.md` or `CHANGELOG.md` is not at
#'   the top level, or `CHANGELOG.md` has no section for a version. A
#'   section is a line that starts with `## vNN`, with a space or nothing
#'   after it, for example `## v01` or `## v01 (2026-11-19)`. A version is a
#'   `vNN_run.R` or a `vNN_functions/` at the top level. From the set
#'   `house_2026_11_20`, a version is also the `vNN` of a file
#'   `manuscript/manual/vNN_<INITIALS>.docx` with a name that
#'   `manual_manuscript_names` passes. `README.md` says
#'   what the project is, how to run it and why its code is as it is.
#'   `CHANGELOG.md` says what changed in each version. The detail is
#'   `README.md: missing`, `CHANGELOG.md: missing` or
#'   `CHANGELOG.md: no section "## vNN"`.
#' - `changelog_with_code` (WARN): in a git repository, a `vNN_run.R` or a
#'   file under `vNN_functions/` differs from the last commit, and
#'   `CHANGELOG.md` does not. A change is staged or not staged, or the file
#'   is untracked and git does not ignore it. A deleted file counts too. In
#'   a repository without a commit, each staged file is a change. The rule
#'   reads the project folder `path` only, also when it is a folder inside a
#'   repository. When git is not installed or `path` is not in a git
#'   repository, the rule has nothing to compare. It then prints one PASS
#'   line that says so. The detail is
#'   `<file>: changed since the last commit, and CHANGELOG.md did not change`.
#' - `results_contract` (FAIL): a `vNN_results/<scenario>/results.qs2` or a
#'   `vNN_results/<scenario>/sim_NNNN/results.qs2` of pbn 2026.11.22 fails
#'   [check_results()], or a `results.qs2` is directly in `vNN_results/`.
#'
#'   The rule also reads each `vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2`
#'   on the disk, also when git ignores it, and fails a file that fails
#'   [check_results()]. A clone of the repository has none, and the rule then
#'   needs none.
#'
#'   The rule also reads each `results_gitexclude.qs2` beside a
#'   `results.qs2`, and each `sim_NNNN_gitexclude.qs2` beside a
#'   `sim_NNNN.qs2`, also when git ignores it. It fails when the file does
#'   not hold exactly the analysis kinds that the manifest of the results
#'   file puts there. [check_results()] does not check it.
#' - `gitexclude_not_tracked` (FAIL): git tracks a path with `gitexclude` in
#'   its name, from `git ls-files` in `path`. Such a path holds data for each
#'   person. When git is not installed or `path` is not in a git repository,
#'   the rule has nothing to check. It then prints one PASS line that says
#'   so.
#' - `scenario_checks` (FAIL): a `vNN_results/<scenario>/check.csv` has a
#'   value of `pass` that is not `TRUE`, holds no assertion, or has no column
#'   `pass` that R can read. [save_results()] writes these files.
#' - `manual_manuscript_names` (FAIL): a file in `manuscript/manual/` does not
#'   match `^v[0-9]{2}_[A-Z]{2,4}[0-9]?\.docx$`.
#' - `manuscript_reads_real` (FAIL): an `.R`, `.qmd` or `.Rmd` file in
#'   `manuscript/built/` calls `read_results()` or `pbn::read_results()` with
#'   a `scenario` argument that is not the string `"real"`. A call without
#'   `scenario` passes. In `.qmd` and `.Rmd` files, the rule reads the R
#'   chunks that [knitr::purl()] extracts and the inline R code.
#' - `built_reads_own_version` (FAIL): an `.R`, `.qmd` or `.Rmd` file in
#'   `manuscript/built/vNN/` calls `read_results()` or `pbn::read_results()`
#'   with a `version` argument that is not the string `"vNN"`. A variable
#'   and a call without `version` fail too. The rule reads the code as
#'   `manuscript_reads_real` does, and it fails a file that R cannot parse.
#'   It does not read a file in `manuscript/built/` outside a folder `vNN/`,
#'   or a file of another language, such as `.py`. The detail lists each
#'   call that fails.
#' - `built_has_run` (FAIL): a folder `manuscript/built/vNN/` has no
#'   `vNN_run.R` at the top level. In a git repository, an empty folder is
#'   not in the list of files, so the rule does not see it. The detail is
#'   `manuscript/built/vNN: there is no vNN_run.R`.
#' - `manual_not_ahead` (FAIL): a file
#'   `manuscript/manual/vNN_<INITIALS>.docx` has `NN` at or above `MM`, the
#'   largest version of a `vMM_run.R`. The run scripts and the manuscripts
#'   share one version counter. So a hand edit of version `vMM` is
#'   `v(MM+1)_<INITIALS>.docx`, and the next run version is `v(MM+2)`. The
#'   detail is
#'   `manual vNN is ahead of the newest run vMM: freeze vMM and start vPP with pbn::new_version("vMM", to = "vPP")`,
#'   with `PP` = `NN` + 1. The rule does not read a name that
#'   `manual_manuscript_names` fails. Without a `vNN_run.R`, the rule has
#'   nothing to compare. It then prints one PASS line that says so.
#' - `fake_data_names` (FAIL): a file in a folder `output_fake_data/` has a
#'   name without the prefix `fake_data_`, or a file with the prefix is in
#'   another folder. [make_outputs()] writes these folders.
#' - `no_local_copy_of_pbn_function` (FAIL): a top-level assignment in a
#'   `vNN_functions/*.R` file defines a function with the name of a pbn
#'   export. The one exception is `get_data`: a project MAY name its data
#'   function `get_data(scenario, sim)`, and a run script calls the export
#'   as `pbn::get_data()`. The set `house_2026_11_17` does not know the
#'   exports `create_project`, `freeze_version`, `new_version` and `verify`,
#'   so a local function with such a name passes there.
#' - `loc_per_file` (FAIL): an `.R` file has more than `limit` code lines, as
#'   counted by [org::loc_per_file()]. The house `limit` is 1000. The rule
#'   reads each `.R` file of the project but `vNN_one_file.R`.
#' - `cyclomatic_complexity` (FAIL): a function scores above `limit` in
#'   [cyclocomp::cyclocomp()]. The house `limit` is 15, the limit of the lint
#'   gate of the house packages. The rule reads the `.R` files of
#'   `loc_per_file`. It scores each
#'   function that a top-level assignment defines, and each function in a
#'   `list()` of a top-level assignment, as `<list>$<name>`. A function
#'   inside a function counts toward the outer function. Top-level code that
#'   is not a function gets no score. The rule parses each file and runs no
#'   code of it. It skips a file that R cannot parse, which `loc_per_file`
#'   fails. The detail gives the file, the function and its score. A path
#'   under `allow` allows each function of one file.
#' - `comment_share` (WARN): in `vNN_run.R` or an `.R` file at the top of
#'   `vNN_functions/`, the share of comment lines is more than `limit`. The
#'   share is the comment lines divided by the lines that are not blank. The
#'   house `limit` is 0.25, so a file of 20 such lines MAY have 5 comment
#'   lines. The parse data decides what is a comment, so a `#` in a string
#'   does not count. A line with code and a comment after it is a comment
#'   line. A section marker is not a comment line. A section marker is a
#'   comment with text and then four or more `=` or `-` at its end, as in
#'   `# PART 1 ====`. The rule skips a file that R cannot parse. The detail
#'   is
#'   `<file>: <n> of <m> lines that are not blank are comments, <share>, above the limit of <limit>`.
#'   An explanation of why the code is as it is goes into `README.md`, and
#'   what changed goes into `CHANGELOG.md`.
#' - `no_environment_variables` (FAIL): a run script or a file in a functions
#'   folder uses `Sys.getenv` or `Sys.setenv`.
#' - `no_cross_version_reads` (FAIL): a string constant of `vNN_run.R` or of
#'   an `.R` file at the top of `vNN_functions/` names another version or the
#'   folder `adhoc/`. The rule splits each string at `/` and `\`. A string
#'   fails when one of its parts is one of these:
#'   - `vMM`, with `MM` other than `NN`, as in `read_results("v01")` in
#'     `v02_run.R`.
#'   - `vMM_` and an item of the layout: `run.R`, `functions`, `results`,
#'     `cache_gitexclude`, `test`, `check_functions.R`, `one_file.R` or
#'     `frozen.md5`. An example is `"../v01_functions/get_data.R"`.
#'   - `adhoc`, as in `"adhoc/helpers.R"`.
#'
#'   These pass:
#'   - the version of the file and its items, and a comment.
#'   - a column name such as `"v01_age"`, and `"v1"` and `"v100"`.
#'   - a part with more words, such as `"an adhoc analysis of v01"`.
#'   - a string that `paste0()` or `sprintf()` builds. The rule misses it.
#'
#'   A column or a level with the name `v01` fails, so allow its file in
#'   `pbn.yml`. The rule skips a file that R cannot parse, and a string of
#'   more than 1000 bytes. The detail is
#'   `<file>:<line>: "<the string, cut at 40 characters>" names vMM`, or
#'   `names adhoc/`.
#' - `no_local_assign` (FAIL): an `.R` file uses a construct of `forbid` at
#'   any depth. The house `forbid` holds all seven constructs: `local()`,
#'   `assign()`, `delayedAssign()`, `assignInNamespace()`,
#'   `assignInMyNamespace()`, `<<-` and `->>`. Each of these creates or changes an
#'   object in a place other than the line where it stands. The rule reads
#'   the `.R` files of `loc_per_file`, parses each file and runs no code of
#'   it. A call with the prefix of its own package, `base::` or `utils::`,
#'   counts. A call of the same name from another package passes. A variable or an argument named `local` or `assign`
#'   passes, and so do `<-`, `->`, `=` and `:=`. The rule skips a file that R
#'   cannot parse. The detail gives the file, the line and the construct. A
#'   path under `allow` allows each finding of one file.
#' - `no_escape_hatches` (WARN): `vNN_run.R` or an `.R` file at the top of
#'   `vNN_functions/` calls `suppressWarnings()`, `suppressMessages()`,
#'   `try()`, `tryCatch()` or `withCallingHandlers()`, at any depth. Each of
#'   these can hide a warning or an error of the run. A call with the prefix
#'   `base::` counts. A call of the same name from another package passes. So
#'   do a word in a comment or a string, and a variable or an argument named
#'   `try`.
#'   The rule skips a file that R cannot parse. The detail gives the file,
#'   the line and the call. A path under `allow` allows each finding of one
#'   file. Give the reason in a comment in `pbn.yml`.
#' - `no_absolute_paths` (FAIL): a string constant of an `.R` file of
#'   `loc_per_file` is an absolute path, or a path starts from a string
#'   constant. The rule reads the strings from the
#'   parse data, so a comment does not count. The arguments of a call
#'   `pbn::initialize_project()` are exempt, because a project declares its
#'   paths there, also `one_file_home` and `one_file_folders`. A string is an absolute path when it
#'   starts with one of these:
#'   - `/`, a name of letters, digits, `.`, `_` or `-` that starts with a
#'     letter or `_`, and `/` again, as in `"/home/a/x.csv"` or `"/data/"`.
#'   - `~/` or `~\`, the home folder.
#'   - a drive letter, `:` and `/` or `\`, as in `"C:/x"`, `"C:\\x"` or
#'     `"N:/durable/x"`.
#'   - `\\` and a letter or a digit, the start of a UNC path such as
#'     `"\\\\server\\share"`.
#'
#'   These strings pass:
#'   - `"/"` and `"~"`.
#'   - the regular expressions `"^/"`, `"/$"` and `"/.*$"`.
#'   - a URL such as `"https://a.org/b"`.
#'   - the formats `"%s/%s"` and `"%Y/%m/%d"`, and the fraction `"1/2"`.
#'   - the formula `"y ~ x"`, and `"/month"`.
#'   - the relative paths `"data/x.csv"`, `"./x"` and `"../x"`. A path with
#'     `..` leaves the project folder, and it passes too.
#'   - a path with one name and no second `/`, such as `"/data"`, and a
#'     string of more than 1000 bytes. The rule misses these.
#'
#'   The rule also fails a call of `file.path()`, `base::file.path()` or
#'   `fs::path()` whose first argument is a string constant, for example
#'   `file.path("data", "x.csv")` or `file.path("/", "data")`. A path that
#'   starts from a constant is absolute, or it depends on the working
#'   directory. A path to data starts from `pbn::folder()`. A path for output
#'   starts from the folder that pbn gives the function, such as `dir` of
#'   [make_outputs()]. A call whose first argument is a name or a
#'   call passes, for example `file.path(dir, "fig2.png")` or
#'   `file.path(tempdir(), "x")`. A path that `paste0()` or `sprintf()`
#'   builds stays outside the rule.
#'
#'   The detail is `<file>:<line>: "<the string, cut at 40 characters>"`, or
#'   `<file>:<line>: file.path("<the first argument, cut at 40
#'   characters>", ...)`. A path under `allow` allows each finding of one
#'   file.
#' - `blocks_list` (FAIL): for each functions folder, one of these is true:
#'   - a file other than `vNN_functions/blocks.R` assigns `blocks` at its
#'     top level. pbn leaves `blocks.R` out of `manifest$source_md5`, so the
#'     blocks MUST be in that file.
#'   - more than one top-level assignment of `blocks.R` assigns it.
#'   - its value is not a call of `list()`, or the list is empty.
#'   - an element has no name, or a name comes twice.
#'   - an element is not a `function`, or the name of a function that a
#'     top-level assignment of the functions folder defines.
#'   - a top-level expression of `blocks.R` is not an assignment with `<-`
#'     or `=` of a name to `function(...)`, or of `blocks` to `list(...)`.
#'     Examples are `library(x)`, `options(...)`, `set.seed(1)`, an `if` and
#'     `fmt <- sprintf`. Such code runs when pbn sources the folder, so it
#'     can change the results.
#'   - a name that `blocks.R` assigns at its top level, and another file of
#'     the folder assigns too. The detail names both places.
#'   - another file of the folder, or `vNN_run.R`, uses a name that
#'     `blocks.R` assigns at its top level. Examples are a helper `fmt()`,
#'     and `CFG` after `CFG$labels <- ...` in `blocks.R`. The run would then
#'     depend on a file outside `source_md5`. A name after `$` or `@` and
#'     the name of an argument do not count.
#'
#'   The detail is `<file>:<line>: <what>`. A folder without `blocks` and
#'   without `blocks.R` passes. [initialize_project()] makes the same checks
#'   of `blocks.R`, and puts it back into `manifest$source_md5` when one
#'   fails, also when this rule is `warn` or `off`.
#' - `real_results_are_fake` (WARN): a file `vNN_results/real/results.qs2`
#'   holds `manifest$fake_real_data` `TRUE`. Its run had
#'   `fake_real_data = TRUE` of [initialize_project()], so its numbers are
#'   not those of the real data. It passes when no such file is in the
#'   list of files. In a git repository, a real results file that git
#'   ignores is not in the list, so the rule does not see it.
#' - `unknown_path` (WARN): the house layout does not name a top-level file or
#'   folder. The layout names `vNN_run.R`, `vNN_functions/`, `vNN_results/`,
#'   `vNN_cache_gitexclude/`, `vNN_test/`, `vNN_check_functions.R`,
#'   `vNN_one_file.R`, `check_project.R` and `pbn.yml`, among others. From
#'   the set `house_2026_11_19`, it also names `vNN_frozen.md5` of
#'   [freeze_version()]. A top-level dotfile and a top-level
#'   `*.Rproj` file are never unknown. The rule also reads the files
#'   directly in `vNN_results/`. There it names the run log `run.md`, its PDF
#'   `run.pdf` and its errors file `run_errors_gitexclude.md`, and no rule
#'   needs them. It reads no deeper, so it never
#'   warns on the folders `output/` and `output_fake_data/` that
#'   [make_outputs()] writes in `vNN_results/<scenario>/`, or on the folder
#'   `sims_gitexclude/` that [save_results()] writes there.
#'
#' The R parser, not a regular expression, finds the calls, the assignments
#' and the symbols.
#'
#' `vNN_one_file.R` at the top level is the file of [to_one_file()]. It holds
#' the paths of a server by design, and pbn writes it. So `loc_per_file`,
#' `cyclomatic_complexity`, `no_local_assign` and `no_absolute_paths` do not
#' read it, and no rule takes it for a run script. Put it in git: it is what
#' ran on the server.
#'
#' @section The file pbn.yml:
#' Each project MUST have the file `pbn.yml` in its top folder `path`. pbn
#' reads no other file. A file `pbn.yaml` stops `check_project()` with the
#' advice to rename it. The smallest `pbn.yml` is one line:
#'
#' ```yaml
#' extends: house_2026_11_20
#' ```
#'
#' The key `extends` names a house rule set. A set is a dated file in the
#' folder `rules/` of pbn, and pbn never changes a set after its release. A
#' change to the house rules is a new set with a new date. pbn never chooses
#' a set for a project. So a new version of pbn does not change the rules of
#' a project. To read the set, run
#' `readLines(system.file("rules", "house_2026_11_20.yml", package = "pbn"))`.
#'
#' The set gives each rule one of three levels:
#' - `fail`: a finding is FAIL, and `check_project.R` ends with status 1.
#' - `warn`: a finding is WARN.
#' - `off`: the rule does not run, and its one line is OFF.
#'
#' The parameters of the rules are `limit` of `loc_per_file`, `limit` of
#' `cyclomatic_complexity`, `limit` of `comment_share` and `forbid` of
#' `no_local_assign`. The list of the rules above gives their house values.
#'
#' The key `rules` MAY change the level of a rule, a parameter, or both. A
#' change merges into the entry of the set. So `cyclomatic_complexity:
#' {limit: 25}` keeps the level `fail`. The short form `lower_case_names:
#' warn` changes the level only. `forbid` MAY list fewer constructs than the
#' house set.
#'
#' The key `allow` MAY list paths for each rule, relative to `path`. A path
#' allows the findings with that path and the findings of each path under
#' it. So the folder `adhoc` allows `adhoc/Notes.txt` and
#' `adhoc/Old/Data.csv`, but not `adhoc2/x.R`. A final `/` changes nothing:
#' `adhoc` and `adhoc/` are the same entry. pbn does not look on the disk
#' for a file or a folder of that name. An allowed finding is WARN, with
#' `(allowed)` before its fix. A path that matches no finding is a WARN of the
#' rule `allow`.
#'
#' Three rules are locked: `results_contract`, `gitexclude_not_tracked` and
#' `fake_data_names`. The first two keep data for each person out of git. The
#' third keeps the output of fake data apart from the output of real data.
#' `pbn.yml` cannot set a locked rule to `warn` or `off`, and cannot allow a
#' path for it. It cannot name a rule of [check_results()] either.
#'
#' The key `ignore` MAY list folders at the top level, such as `archive/`,
#' that no rule reads: their files give no finding, not even a WARN. Use it
#' for old work that nothing runs. A final `/` changes nothing. An entry
#' MUST be one folder at the top level that the house layout does not name,
#' so it cannot be `manuscript/`, `background/`, `adhoc/` or a folder of a
#' version. A folder that does not exist is no error. The locked rules still
#' read an ignored folder, so a tracked file with `gitexclude` in its name
#' fails there too. The first line of the report counts the ignored
#' folders.
#'
#' This `pbn.yml` changes two rules, allows three paths and ignores one
#' folder:
#'
#' ```yaml
#' extends: house_2026_11_20
#' rules:
#'   cyclomatic_complexity: {limit: 25}
#'   unknown_path: off
#' allow:
#'   lower_case_names: [adhoc/Old_notes.txt, adhoc/Drafts/]
#'   no_local_assign:
#'     - adhoc/explore.R
#' ignore: [archive/]
#' ```
#'
#' `check_project()` and [initialize_project()] stop before any rule runs on
#' each of these, with a message that names the key:
#' - there is no `pbn.yml`. The message gives the one line to write.
#' - there is a file `pbn.yaml`, or a file `pbn_allow.txt`. For
#'   `pbn_allow.txt`, the message gives its lines as the YAML of the key
#'   `allow`.
#' - `pbn.yml` is not valid YAML. This includes a key that is there twice and
#'   a tab. The message holds the message of the yaml package.
#' - there is no key `extends`, or it names a set that pbn does not have.
#' - a key other than `extends`, `rules`, `allow` and `ignore`.
#' - a name that is not a rule. The message names the nearest rule.
#' - a rule that the set of `extends` does not have. The message names the
#'   oldest set with it.
#' - a parameter that the rule does not have.
#' - a level other than `fail`, `warn` and `off`.
#' - a `limit` that is not a whole number of 1 or more. For
#'   `comment_share`, a `limit` that is not a number from 0 to 1.
#' - a construct in `forbid` that the rule does not know, or an empty
#'   `forbid`. To turn the rule off, set its level to `off`.
#' - a value under `allow` that is not a path, for example `off`. YAML reads
#'   `off`, `yes` and `no` as booleans, so a path with such a name needs
#'   quotes.
#' - a change or an allowed path of a locked rule.
#' - an entry of `ignore` that is not one folder at the top level, is a name
#'   of the house layout, is there twice, or is a file.
#' @return Invisibly, a data.frame of the findings with the columns `rule`,
#'   `status`, `path` and `detail`.
#' @examples
#' home <- file.path(tempdir(), "pbn_check_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines("extends: house_2026_11_20", file.path(home, "pbn.yml"))
#' writeLines(
#'   'pbn::initialize_project(version = "v01", home = ".", folders = c(main = "x"))',
#'   file.path(home, "v01_run.R")
#' )
#' writeLines("# A fake project", file.path(home, "README.md"))
#' writeLines(c("# Changelog", "", "## v01"), file.path(home, "CHANGELOG.md"))
#' pbn::check_project(home)
#'
#' unlink(home, recursive = TRUE)
#' @family checks
#' @export
check_project <- function(path = ".") {
  lines <- project_report(path)
  cat(lines, sep = "\n")
  if (!interactive() && attr(lines, "counts")[["FAIL"]] > 0) {
    quit(save = "no", status = 1)
  }
  return(invisible(attr(lines, "findings")))
}

# The lines that check_project() prints: the line of the rules, one line for
# each finding and the count line. The findings and the counts are the
# attributes `findings` and `counts`.
project_report <- function(path, rules = read_rules(path)) {
  findings <- project_findings(path, rules)
  lines <- trimws(
    paste0(findings$status, "  ", findings$rule, "  ", findings$detail),
    which = "right"
  )
  counts <- vapply(
    c("PASS", "WARN", "FAIL", "OFF"),
    function(s) return(sum(findings$status == s)),
    integer(1)
  )
  out <- c(
    rules_line(rules),
    lines,
    paste0(counts, " ", names(counts), collapse = ", ")
  )
  attr(out, "findings") <- findings
  attr(out, "counts") <- counts
  return(out)
}

# The findings of every rule, at the level of the rules of the project and
# after the section allow of pbn.yml. check_project() prints these, and the
# tests read them without the quit(). `rules` is the value of read_rules().
project_findings <- function(path, rules = read_rules(path)) {
  if (!dir.exists(path)) {
    stop("The project folder does not exist: ", path, call. = FALSE)
  }
  defs <- project_rules()
  out <- list()
  # A rule that the set of the project does not have does not run, and
  # prints no line.
  for (rule in intersect(names(defs), names(rules$rules))) {
    out[[rule]] <- rule_findings(
      path,
      rule,
      defs[[rule]],
      rules$rules[[rule]],
      rules$set
    )
  }
  findings <- apply_allow(bind_rows(out), rules)
  fixes <- c(
    vapply(defs, function(d) return(d[[3]]), character(1)),
    allow = "remove the entry from pbn.yml"
  )
  return(add_fix(findings, fixes))
}

# The findings with the fix of their rule at the end of the detail of each
# WARN and FAIL, as "<detail>. Fix: <fix>.". `fixes` is the fix of each rule,
# named by the rule. A file that R cannot parse gets the fix of parse_fix. A
# version that frozen_versions_unchanged needs frozen gets the fix of
# frozen_missing_fix(), with its version.
add_fix <- function(findings, fixes) {
  open <- findings$status %in% c("WARN", "FAIL")
  detail <- sub("(?<!\\.)\\.$", "", findings$detail[open], perl = TRUE)
  fix <- unname(fixes[findings$rule[open]])
  fix[grepl(": R cannot parse it( \\(allowed\\))?$", detail)] <- parse_fix
  unfrozen <- findings$rule[open] == "frozen_versions_unchanged" &
    grepl(frozen_missing_pattern, detail)
  fix[unfrozen] <- vapply(
    detail[unfrozen],
    frozen_missing_fix,
    character(1),
    USE.NAMES = FALSE
  )
  findings$detail[open] <- paste0(detail, ". Fix: ", fix, ".", recycle0 = TRUE)
  return(findings)
}

# The fix of a finding on a file that R cannot parse, for each rule.
parse_fix <- "correct the syntax of the file"

# The findings of the rule `rule` with the definition `def` of
# project_rules() and the entry `entry` of the rules of the project. A rule
# that is off gives one OFF row. Each finding of a rule that runs gets the
# level of the rule as its status. A rule function with the argument `set`
# gets the name of the set of the project.
rule_findings <- function(path, rule, def, entry, set = NULL) {
  if (identical(entry$level, "off")) {
    return(project_row(rule, "OFF", "", "off in pbn.yml"))
  }
  args <- c(list(path), entry[setdiff(names(entry), c("level", "locked"))])
  if ("set" %in% names(formals(def[[1]]))) {
    args$set <- set
  }
  found <- do.call(def[[1]], args)
  if (nrow(found) == 0) {
    pass <- if (is.function(def[[2]])) def[[2]](entry) else def[[2]]
    return(project_row(rule, "PASS", "", pass))
  }
  open <- found$status != "PASS"
  found$status[open] <- toupper(entry$level)
  return(found)
}

# The findings after the section allow of pbn.yml. An allowed finding is a
# WARN with "(allowed)" at the end. An entry that matches no finding is a
# WARN of the rule allow.
apply_allow <- function(findings, rules) {
  allow <- rules$allow
  if (nrow(allow) == 0) {
    return(findings)
  }
  open <- !findings$status %in% c("PASS", "OFF")
  # covers[i, j]: the entry j covers the finding i, because the finding has
  # the path of the entry or a path under it.
  covers <- vapply(
    seq_len(nrow(allow)),
    function(j) {
      return(
        open &
          findings$rule == allow$rule[j] &
          (findings$path == allow$path[j] |
            startsWith(findings$path, paste0(allow$path[j], "/")))
      )
    },
    logical(nrow(findings))
  )
  covers <- matrix(covers, nrow = nrow(findings), ncol = nrow(allow))
  allowed <- rowSums(covers) > 0
  findings$status[allowed] <- "WARN"
  findings$detail[allowed] <- paste(findings$detail[allowed], "(allowed)")
  stale <- allow[colSums(covers) == 0, , drop = FALSE]
  off <- vapply(
    stale$rule,
    function(r) return(identical(rules$rules[[r]]$level, "off")),
    logical(1)
  )
  return(rbind(
    findings,
    project_row(
      "allow",
      "WARN",
      rep(rules_file, nrow(stale)),
      paste0(
        "pbn.yml: the entry allow: ",
        stale$rule,
        ": ",
        stale$written,
        " matches no finding",
        ifelse(off, ", because the rule is off", ""),
        recycle0 = TRUE
      )
    )
  ))
}
