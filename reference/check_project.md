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
`PASS|WARN|FAIL <rule> <detail>`. The detail of a WARN or a FAIL ends
with `Fix:` and the change that removes the finding. A count line comes
last.

Every rule reads one list of files and their folders:

- When `path` is the top folder of a git repository, the list is the
  output of `git ls-files --cached --others --exclude-standard`. These
  are the tracked files, and the untracked files that git does not
  ignore.

- Otherwise, the list is every file and folder on disk.

Both lists leave out `.DS_Store`, `.Rhistory`, `.RData`, the Word lock
files `~$*`, and everything in `.git/` or `.Rproj.user/`.

The rules, with their level in the set `house_2026_11_27`, are below.
The set `house_2026_11_26` has the same rules at the same levels,
without `display_numbering`. The set `house_2026_11_25` also does not
have `built_reads_source`, `manual_has_supplement`,
`manual_si_has_main`, `guidelines_valid` and `guidelines_covered`. The
set `house_2026_11_20` also does not have `no_plot_title`,
`no_plot_subtitle` and `no_plot_caption`. The set `house_2026_11_19`
also does not have `built_reads_own_version`, `built_has_run` and
`manual_not_ahead`. The set `house_2026_11_17` also does not have
`frozen_versions_unchanged`, `readme_and_changelog`,
`changelog_with_code`, `comment_share`, `no_cross_version_reads` and
`no_escape_hatches`. A rule that the set of the project does not have
prints no line.

- `lower_case_names` (FAIL): a file or folder name has a capital letter.
  Everything under `background/` is not checked. These are exempt:
  `README.md`, `CHANGELOG.md`, `AGENTS.md`, `CLAUDE.md`, `LICENSE`,
  `DESCRIPTION`, a top-level dotfile, a top-level `*.Rproj` file, file
  extensions, and the initials in
  `manuscript/manual/vNN_<INITIALS>.docx` and
  `manuscript/manual/vNN_<INITIALS>_<suffix>.docx`, and in the drafts of
  an agent with the same names in `manuscript/draft/`.

- `run_script_initialises` (FAIL): a `vNN_run.R` at the top level has no
  call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  whose `version` argument is the string `"vNN"`. A call with an
  argument that
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  does not have does not count, and the detail names that argument.

- `version_items_complete` (FAIL): a `vNN_run.R` has no
  `vNN_functions/`, or the reverse.

- `frozen_versions_unchanged` (FAIL): one of these is true:

  - a version older than the newest has no `vNN_frozen.md5` of
    [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md).
    A version is a `vNN_run.R` or a `vNN_functions/` at the top level,
    and the newest is the one with the largest `NN`. So a deleted
    `vNN_frozen.md5` of an older version fails. The newest version needs
    none. The fix names the call `pbn::freeze_version("vNN")`.

  - a file of a version changed, came or went after
    [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
    wrote its `vNN_frozen.md5`. The files are `vNN_run.R` and each file
    under `vNN_functions/`, as
    [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
    lists them from the disk. The detail names the file, with `changed`,
    `added` or `removed`. A `.R` file whose line ends change between LF
    and CR LF, and nothing else, does not count as changed. A change of
    the line ends of any other file counts, see
    [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md).

  - a line of `vNN_frozen.md5` is not `<md5> <path>`.

- `readme_and_changelog` (FAIL): `README.md` or `CHANGELOG.md` is not at
  the top level, or `CHANGELOG.md` has no section for a version. A
  section is a line that starts with `## vNN`, with a space or nothing
  after it, for example `## v01` or `## v01 (2026-11-19)`. A version is
  a `vNN_run.R` or a `vNN_functions/` at the top level. From the set
  `house_2026_11_20`, a version is also the `vNN` of a file
  `manuscript/manual/vNN_<INITIALS>.docx` with a name that
  `manual_manuscript_names` passes. `README.md` says what the project
  is, how to run it and why its code is as it is. `CHANGELOG.md` says
  what changed in each version. The detail is `README.md: missing`,
  `CHANGELOG.md: missing` or `CHANGELOG.md: no section "## vNN"`.

- `changelog_with_code` (WARN): in a git repository, a `vNN_run.R` or a
  file under `vNN_functions/` differs from the last commit, and
  `CHANGELOG.md` does not. A change is staged or not staged, or the file
  is untracked and git does not ignore it. A deleted file counts too. In
  a repository without a commit, each staged file is a change. The rule
  reads the project folder `path` only, also when it is a folder inside
  a repository. When git is not installed or `path` is not in a git
  repository, the rule has nothing to compare. It then prints one PASS
  line that says so. The detail is
  `<file>: changed since the last commit, and CHANGELOG.md did not change`.

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
  not match `^v[0-9]{2}_[A-Z]{2,4}[0-9]?(_[a-z0-9]+)?\.docx$`. The
  suffix is one word of lower-case letters and digits, for a second file
  of the same version, such as the supporting information
  `v02_AB_si.docx`.

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

- `built_reads_own_version` (FAIL): an `.R`, `.qmd` or `.Rmd` file in
  `manuscript/built/vNN/` calls
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  or
  [`pbn::read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  with a `version` argument that is not the string `"vNN"`. A variable
  and a call without `version` fail too. The rule reads the code as
  `manuscript_reads_real` does, and it fails a file that R cannot parse.
  It does not read a file in `manuscript/built/` outside a folder
  `vNN/`, or a file of another language, such as `.py`. The detail lists
  each call that fails. From the set `house_2026_11_26`, the rule also
  reads each [`txt()`](https://www.rwhite.no/pbn/reference/txt.md) call
  and each
  [`read_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md)
  call.

- `built_reads_source` (FAIL): a file in `manuscript/built/vNN/` does
  not read the numbers of its source. A version is one-file when its
  `vNN_run.R` gives `one_file_home` in a top-level call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  pbn parses the run script and runs nothing. In a one-file version, a
  file fails without a
  [`read_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md)
  or [`txt()`](https://www.rwhite.no/pbn/reference/txt.md) call, and on
  a
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  call. Each file there also fails while `vNN_results/real/blocks.qs2`
  does not exist, or has the attribute `pbn_fake` of
  [`import_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md),
  because the numbers are then fake. In another version, a file fails on
  a
  [`read_blocks()`](https://www.rwhite.no/pbn/reference/export_blocks.md)
  call. The rule reads the code as `manuscript_reads_real` does. It does
  not read a file outside a folder `vNN/`.

- `built_has_run` (FAIL): a folder `manuscript/built/vNN/` has no
  `vNN_run.R` at the top level. In a git repository, an empty folder is
  not in the list of files, so the rule does not see it. The detail is
  `manuscript/built/vNN: there is no vNN_run.R`.

- `manual_not_ahead` (FAIL): a file
  `manuscript/manual/vNN_<INITIALS>.docx` has `NN` at or above `MM`, the
  largest version of a `vMM_run.R`. The run scripts and the manuscripts
  share one version counter. So a hand edit of version `vMM` is
  `v(MM+1)_<INITIALS>.docx`, and the next run version is `v(MM+2)`. The
  detail is
  `manual vNN is ahead of the newest run vMM: freeze vMM and start vPP with pbn::new_version("vMM", to = "vPP")`,
  with `PP` = `NN` + 1. The rule does not read a name that
  `manual_manuscript_names` fails. Without a `vNN_run.R`, the rule has
  nothing to compare. It then prints one PASS line that says so.

- `manual_has_supplement` (FAIL): a main file
  `manuscript/manual/vNN_<INITIALS>.docx` has no supplement
  `manuscript/manual/vMM_<INITIALS>_si.docx` with `MM` at or below `NN`.
  The initials of the supplement MAY differ. The supplement suffix is
  exactly `si`, so a file with another suffix is neither a main file nor
  a supplement. The rule reads `manuscript/manual/` only, so a draft in
  `manuscript/draft/` does not count. A project without a supplement
  turns the rule off with `rules: manual_has_supplement: off` in
  `pbn.yml`.

- `manual_si_has_main` (FAIL): a supplement
  `manuscript/manual/vNN_<INITIALS>_si.docx` has no main file
  `manuscript/manual/vNN_<INITIALS>.docx` with the same version and the
  same initials. The detail names the main file to add.

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
  The set `house_2026_11_17` does not know the exports `create_project`,
  `freeze_version`, `new_version` and `verify`, so a local function with
  such a name passes there.

- `loc_per_file` (FAIL): an `.R` file has more than `limit` code lines,
  as counted by
  [`org::loc_per_file()`](https://www.rwhite.no/org/reference/loc_per_file.html).
  The house `limit` is 1000. The rule reads each `.R` file of the
  project but `vNN_one_file.R`.

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

- `comment_share` (WARN): in `vNN_run.R` or an `.R` file at the top of
  `vNN_functions/`, the share of comment lines is more than `limit`. The
  share is the comment lines divided by the lines that are not blank.
  The house `limit` is 0.25, so a file of 20 such lines MAY have 5
  comment lines. The parse data decides what is a comment, so a `#` in a
  string does not count. A line with code and a comment after it is a
  comment line. A section marker is not a comment line. A section marker
  is a comment with text and then four or more `=` or `-` at its end, as
  in `# PART 1 ====`. The rule skips a file that R cannot parse. The
  detail is
  `<file>: <n> of <m> lines that are not blank are comments, <share>, above the limit of <limit>`.
  An explanation of why the code is as it is goes into `README.md`, and
  what changed goes into `CHANGELOG.md`.

- `no_environment_variables` (FAIL): a run script or a file in a
  functions folder uses `Sys.getenv` or `Sys.setenv`.

- `no_cross_version_reads` (FAIL): a string constant of `vNN_run.R` or
  of an `.R` file at the top of `vNN_functions/` names another version
  or the folder `adhoc/`. The rule splits each string at `/` and `\`. A
  string fails when one of its parts is one of these:

  - `vMM`, with `MM` other than `NN`, as in `read_results("v01")` in
    `v02_run.R`.

  - `vMM_` and an item of the layout: `run.R`, `functions`, `results`,
    `cache_gitexclude`, `test`, `check_functions.R`, `one_file.R` or
    `frozen.md5`. An example is `"../v01_functions/get_data.R"`.

  - `adhoc`, as in `"adhoc/helpers.R"`.

  These pass:

  - the version of the file and its items, and a comment.

  - a column name such as `"v01_age"`, and `"v1"` and `"v100"`.

  - a part with more words, such as `"an adhoc analysis of v01"`.

  - a string that [`paste0()`](https://rdrr.io/r/base/paste.html) or
    [`sprintf()`](https://rdrr.io/r/base/sprintf.html) builds. The rule
    misses it.

  A column or a level with the name `v01` fails, so allow its file in
  `pbn.yml`. The rule skips a file that R cannot parse, and a string of
  more than 1000 bytes. The detail is
  `<file>:<line>: "<the string, cut at 40 characters>" names vMM`, or
  `names adhoc/`.

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

- `no_escape_hatches` (WARN): `vNN_run.R` or an `.R` file at the top of
  `vNN_functions/` calls
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html),
  [`suppressMessages()`](https://rdrr.io/r/base/message.html),
  [`try()`](https://rdrr.io/r/base/try.html),
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) or
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html), at
  any depth. Each of these can hide a warning or an error of the run. A
  call with the prefix `base::` counts. A call of the same name from
  another package passes. So do a word in a comment or a string, and a
  variable or an argument named `try`. The rule skips a file that R
  cannot parse. The detail gives the file, the line and the call. A path
  under `allow` allows each finding of one file. Give the reason in a
  comment in `pbn.yml`.

- `no_absolute_paths` (FAIL): a string constant of an `.R` file of
  `loc_per_file` is an absolute path, or a path starts from a string
  constant. The rule reads the strings from the parse data, so a comment
  does not count. The arguments of a call
  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  are exempt, because a project declares its paths there, also
  `one_file_home` and `one_file_folders`. A string is an absolute path
  when it starts with one of these:

  - `/`, a name of letters, digits, `.`, `_` or `-` that starts with a
    letter or `_`, and `/` again, as in `"/home/a/x.csv"` or `"/data/"`.

  - `~/` or `~\`, the home folder.

  - a drive letter, `:` and `/` or `\`, as in `"C:/x"`, `"C:\\x"` or
    `"N:/durable/x"`.

  - `\\` and a letter or a digit, the start of a UNC path such as
    `"\\\\server\\share"`.

  These strings pass:

  - `"/"` and `"~"`.

  - the regular expressions `"^/"`, `"/$"` and `"/.*$"`.

  - a URL such as `"https://a.org/b"`.

  - the formats `"%s/%s"` and `"%Y/%m/%d"`, and the fraction `"1/2"`.

  - the formula `"y ~ x"`, and `"/month"`.

  - the relative paths `"data/x.csv"`, `"./x"` and `"../x"`. A path with
    `..` leaves the project folder, and it passes too.

  - a path with one name and no second `/`, such as `"/data"`, and a
    string of more than 1000 bytes. The rule misses these.

  The rule also fails a call of
  [`file.path()`](https://rdrr.io/r/base/file.path.html),
  [`base::file.path()`](https://rdrr.io/r/base/file.path.html) or
  [`fs::path()`](https://fs.r-lib.org/reference/path.html) whose first
  argument is a string constant, for example
  `file.path("data", "x.csv")` or `file.path("/", "data")`. A path that
  starts from a constant is absolute, or it depends on the working
  directory. A path to data starts from
  [`pbn::folder()`](https://www.rwhite.no/pbn/reference/folder.md). A
  path for output starts from the folder that pbn gives the function,
  such as `dir` of
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md).
  A call whose first argument is a name or a call passes, for example
  `file.path(dir, "fig2.png")` or `file.path(tempdir(), "x")`. A path
  that [`paste0()`](https://rdrr.io/r/base/paste.html) or
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html) builds stays
  outside the rule.

  The detail is `<file>:<line>: "<the string, cut at 40 characters>"`,
  or
  `<file>:<line>: file.path("<the first argument, cut at 40 characters>", ...)`.
  A path under `allow` allows each finding of one file.

- `no_plot_title` (FAIL): an `.R` file of `loc_per_file` gives a figure
  a title. A figure has no title, no subtitle and no caption, because
  its legend belongs in the manuscript. The rule fails each of these:

  - a call `ggtitle()` with a value of its first argument `label`.

  - an argument `title` of `labs()`.

  - an argument `title` of `plot_annotation()`, by name or as its first
    argument.

  - an assignment to `<x>$labels$title`.

  A call MAY have the prefix `ggplot2::` for `ggtitle()` and `labs()`,
  and `patchwork::` for `plot_annotation()`. A call of the same name
  from another package passes. R matches the arguments of a call as it
  does in a run: by name, by a part of a name and by position. These
  pass:

  - the value `NULL`, as in `labs(title = NULL)`, which removes the
    title.

  - the other arguments, as in
    `labs(x = "Age", colour = "Group", tag = "A")` and
    `plot_annotation(tag_levels = "A")`.

  - a theme element, as in `theme(plot.title = element_text())`.

  - an argument `title` or `caption` of another function, as in
    `knitr::kable(d, caption = "x")`.

  - a call that R cannot match, as one with an argument that the
    function does not have, and a title that
    [`do.call()`](https://rdrr.io/r/base/do.call.html) or a `...`
    passes. The rule misses these.

  The rule parses each file and runs no code of it. It skips a file that
  R cannot parse. The detail is
  `<file>:<line>: <call>(<argument> = ...)`, for example
  `v01_functions/output.R:12: ggplot2::labs(title = ...)`, or
  `<file>:<line>: <target> <- ...`. A path under `allow` allows each
  finding of one file.

- `no_plot_subtitle` (FAIL): as `no_plot_title`, for a subtitle: the
  argument `subtitle` of `ggtitle()`, by name or as its second argument,
  the argument `subtitle` of `labs()` and of `plot_annotation()`, and an
  assignment to `<x>$labels$subtitle`.

- `no_plot_caption` (FAIL): as `no_plot_title`, for a caption: the
  argument `caption` of `labs()` and of `plot_annotation()`, and an
  assignment to `<x>$labels$caption`.

- `blocks_list` (FAIL): for each functions folder, one of these is true:

  - a file other than `vNN_functions/blocks.R` assigns `blocks` at its
    top level. pbn leaves `blocks.R` out of `manifest$source_md5`, so
    the blocks MUST be in that file.

  - more than one top-level assignment of `blocks.R` assigns it.

  - its value is not a call of
    [`list()`](https://rdrr.io/r/base/list.html), or the list is empty.

  - an element has no name, or a name comes twice.

  - an element is not a `function`, or the name of a function that a
    top-level assignment of the functions folder defines.

  - a top-level expression of `blocks.R` is not an assignment with `<-`
    or `=` of a name to `function(...)`, or of `blocks` to `list(...)`.
    Examples are [`library(x)`](https://rdrr.io/r/base/library.html),
    `options(...)`, `set.seed(1)`, an `if` and `fmt <- sprintf`. Such
    code runs when pbn sources the folder, so it can change the results.

  - a name that `blocks.R` assigns at its top level, and another file of
    the folder assigns too. The detail names both places.

  - another file of the folder, or `vNN_run.R`, uses a name that
    `blocks.R` assigns at its top level. Examples are a helper `fmt()`,
    and `CFG` after `CFG$labels <- ...` in `blocks.R`. The run would
    then depend on a file outside `source_md5`. A name after `$` or `@`
    and the name of an argument do not count.

  The detail is `<file>:<line>: <what>`. A folder without `blocks` and
  without `blocks.R` passes.
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  makes the same checks of `blocks.R`, and puts it back into
  `manifest$source_md5` when one fails, also when this rule is `warn` or
  `off`.

  From the set `house_2026_11_26`, the rule also fails each of these:

  - an element of `blocks` that is not a call
    `pbn::block(fn, format, items)`.

  - a `format` that is not one string, or that names no house formatter
    and no function of `formatters.R`.

  - an `items` that is not a literal `c("<id>:<item>", ...)` of strings.

  - a top-level expression of `vNN_functions/formatters.R` that does not
    assign a name to `function(...)`.

  - a name of `formatters.R` that a file other than `blocks.R` uses or
    assigns.

  The rule does not run the blocks, so it does not see their raw values.
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md),
  [`update_blocks()`](https://www.rwhite.no/pbn/reference/update_blocks.md),
  [`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md),
  [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  and [`txt()`](https://www.rwhite.no/pbn/reference/txt.md) check each
  raw value as [`block()`](https://www.rwhite.no/pbn/reference/block.md)
  says.

- `real_results_are_fake` (WARN): a file `vNN_results/real/results.qs2`
  holds `manifest$fake_real_data` `TRUE`. Its run had
  `fake_real_data = TRUE` of
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md),
  so its numbers are not those of the real data. It passes when no such
  file is in the list of files. In a git repository, a real results file
  that git ignores is not in the list, so the rule does not see it.

- `guidelines_valid` (FAIL): a file `guidelines/<x>.yml` breaks the
  checklist schema below. Each such file gives one FAIL, and its detail
  names its first 3 problems and counts the rest. The rule reads each
  `.yml` file directly in `guidelines/`. It does not read a file in a
  folder under `guidelines/`, or a file with another extension, such as
  `.yaml`.

- `guidelines_covered` (WARN): a result item of a valid file
  `guidelines/<x>.yml` has no block, or a block names an item that no
  valid file has. Each such item and each such tag gives one WARN. The
  rule reads the literal `items` of each
  [`block()`](https://www.rwhite.no/pbn/reference/block.md) in the
  `blocks.R` of the newest functions folder, without running code. A tag
  of a file that fails `guidelines_valid` gives no WARN.
  [`make_checklists()`](https://www.rwhite.no/pbn/reference/make_checklists.md)
  then checks the manuscript text against the formatted strings of the
  blocks.

- `display_numbering` (FAIL): the numbered figures and tables of the
  outputs, of the manuscript and of its citations do not agree. The rule
  reads the output folders `vNN_results/<scenario>/output/` and
  `vNN_results/<scenario>/output_fake_data/` of the newest run version.
  It reads each name there without the prefix `fake_data_`. Each name
  MUST be one of these:

  - `fig_N_<name>` or `fig_sN_<name>`, a figure, with any extension. `N`
    is a whole number from 1, and `s` marks a supplementary figure.

  - `tables.xlsx`, with one sheet `tab_N` or `tab_sN` for each numbered
    table.

  - `data_behind_figures.xlsx`, with the plotted data in sheets
    `fig_N_<name>` or `fig_sN_<name>`. A figure MAY have more than one
    sheet.

  - `fig_N_<name>_external.txt`, `fig_sN_<name>_external.txt`,
    `tab_N_<name>_external.txt` or `tab_sN_<name>_external.txt`, an item
    that the output function does not make, such as a flow chart.

  - `nonum_<name>`, anything else.

  A sheet `nonum_<name>` passes in both workbooks. A file `tab_N_<name>`
  that does not end with `_external.txt` fails, because tables go in
  `tables.xlsx`. Each figure file other than an `_external.txt` item
  MUST have a sheet of its number in `data_behind_figures.xlsx` of its
  folder. Each sheet `fig_N_` MUST have a figure file of its number
  there.

  The manuscript is the newest version `NN` of
  `manuscript/manual/vNN_<INITIALS>.docx`, then its `_si` files, and the
  files `manuscript/built/vNN/*.md`. Without a manuscript, the rule
  checks the names only. A citation is "Figure 1", "Fig. 1", "Table 1",
  "Figure S1" or "Supplementary Figure 1", which is Figure S1. A range
  such as "Tables 1-3" cites each number of it. A list needs the plural
  word, and it follows the Oxford comma. It has two items, as in
  "Figures 1 and 2", or three or more that end in ", and", as in
  "Figures 1, 2, and 3". A range MAY be an item. A list such as "Figures
  1, 2 and 3" fails, and its items still count as citations. A comma
  after one item ends the citation, so "Figures 1 and 2, 25 and 30
  persons" cites Figures 1 and 2 only. The text of a tracked deletion
  and of the old place of moved text in a `.docx` file does not count.
  There are four series: the figures, the tables, the supplementary
  figures and the supplementary tables. In each series, the first
  citations MUST come in the order 1, 2, 3, with no gap. A paragraph
  that starts with "Figure 2." or "Table S1:" is a caption. So is
  "**Figure 2.**" in Markdown emphasis, and "![](figures/fig.png)", the
  text of an image. The text MUST cite each captioned item outside its
  own caption.

  The numbered items of the outputs are the figure files, the sheets
  `tab_` of `tables.xlsx` and the `_external.txt` items of
  `vNN_results/real/output/`. Without that folder, they are the items of
  the first `output_fake_data/` folder of the version in name order,
  such as `fake/output_fake_data/` of a one-file project. Each numbered
  item MUST be cited or captioned in the manuscript. Each item of the
  manuscript MUST be a numbered item of the outputs. The PASS line names
  the folder.

- `unknown_path` (WARN): the house layout does not name a top-level file
  or folder. The layout names `vNN_run.R`, `vNN_functions/`,
  `vNN_results/`, `vNN_cache_gitexclude/`, `vNN_test/`,
  `vNN_check_functions.R`, `vNN_one_file.R`, `check_project.R` and
  `pbn.yml`, among others. From the set `house_2026_11_19`, it also
  names `vNN_frozen.md5` of
  [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md).
  From the set `house_2026_11_26`, it also names the folder
  `guidelines/`. A top-level dotfile and a top-level `*.Rproj` file are
  never unknown. The rule also reads the files directly in
  `vNN_results/`. There it names the run log `run.md`, its PDF `run.pdf`
  and its errors file `run_errors_gitexclude.md`, and no rule needs
  them. It reads no deeper, so it never warns on the folders `output/`
  and `output_fake_data/` that
  [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  writes in `vNN_results/<scenario>/`, or on the folder
  `sims_gitexclude/` that
  [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  writes there.

A checklist file `guidelines/<x>.yml` holds one reporting guideline, in
the checklist schema of the methods repository. Copy the file from
there. `guidelines_valid` fails the file unless each of these is true:

- The file is valid YAML, and a map.

- It has no key other than `id`, `title`, `version`, `source`,
  `citation`, `designs`, `design` and `items`.

- `id` and `title` are each one string. `id` has only the characters
  `a-z`, `0-9` and `-`, and no other file has the same `id`.

- `version`, `source` and `citation` MAY be missing. Each one that is
  there is one string.

- A key that is there has a value. `version:` with nothing after it, or
  `citation: ~`, fails. This holds for `designs` and `design` too, and
  for the `designs` of an entry.

- `designs` MAY be missing. When it is there, it is a YAML sequence of
  strings, such as `[cohort]`. One string, such as `designs: cohort`, or
  a map fails. The same holds for the `designs` of an entry.

- `design` MAY be missing. When it is there, it is one of `designs`.

- `items` is a YAML sequence with at least one entry. A map fails.

- Each entry has no key other than `item`, `section`, `topic`, `kind`,
  `designs` and `text`. Its `item`, `section`, `topic`, `kind` and
  `text` are each one string. Quote an item id such as `"2"`, because
  YAML reads `2` as a number.

- The `kind` of each entry is `result`, `prose` or `author`.

- The `designs` of an entry MAY be missing, and the entry then belongs
  to each design of the file. When it is there, the file has `designs`,
  and each value is one of them.

- An item id comes once per design, among the entries that `design`
  keeps. STROBE has three entries with the item id `15`, one for each
  design, so it passes.

A project copy MAY add the key `design`, such as `design: cohort`. pbn
then keeps the entries whose `designs` include it, and the entries
without `designs`.

The R parser, not a regular expression, finds the calls, the assignments
and the symbols.

`vNN_one_file.R` at the top level is the file of
[`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md).
It holds the paths of a server by design, and pbn writes it. So
`loc_per_file`, `cyclomatic_complexity`, `no_local_assign`,
`no_absolute_paths`, `no_plot_title`, `no_plot_subtitle` and
`no_plot_caption` do not read it, and no rule takes it for a run script.
Put it in git: it is what ran on the server.

## The file pbn.yml

Each project MUST have the file `pbn.yml` in its top folder `path`. pbn
reads no other file. A file `pbn.yaml` stops `check_project()` with the
advice to rename it. The smallest `pbn.yml` is one line:

    extends: house_2026_11_25

The key `extends` names a house rule set. A set is a dated file in the
folder `rules/` of pbn, and pbn never changes a set after its release. A
change to the house rules is a new set with a new date. pbn never
chooses a set for a project. So a new version of pbn does not change the
rules of a project. To read the set, run
`readLines(system.file("rules", "house_2026_11_25.yml", package = "pbn"))`.

The set gives each rule one of three levels:

- `fail`: a finding is FAIL, and `check_project.R` ends with status 1.

- `warn`: a finding is WARN.

- `off`: the rule does not run, and its one line is OFF.

The parameters of the rules are `limit` of `loc_per_file`, `limit` of
`cyclomatic_complexity`, `limit` of `comment_share` and `forbid` of
`no_local_assign`. The list of the rules above gives their house values.

The key `rules` MAY change the level of a rule, a parameter, or both. A
change merges into the entry of the set. So
`cyclomatic_complexity: {limit: 25}` keeps the level `fail`. The short
form `lower_case_names: warn` changes the level only. `forbid` MAY list
fewer constructs than the house set.

The key `allow` MAY list paths for each rule, relative to `path`. A path
allows the findings with that path and the findings of each path under
it. So the folder `adhoc` allows `adhoc/Notes.txt` and
`adhoc/Old/Data.csv`, but not `adhoc2/x.R`. A final `/` changes nothing:
`adhoc` and `adhoc/` are the same entry. pbn does not look on the disk
for a file or a folder of that name. An allowed finding is WARN, with
`(allowed)` before its fix. A path that matches no finding is a WARN of
the rule `allow`.

Three rules are locked: `results_contract`, `gitexclude_not_tracked` and
`fake_data_names`. The first two keep data for each person out of git.
The third keeps the output of fake data apart from the output of real
data. `pbn.yml` cannot set a locked rule to `warn` or `off`, and cannot
allow a path for it. It cannot name a rule of
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
either.

The key `ignore` MAY list folders at the top level, such as `archive/`,
that no rule reads: their files give no finding, not even a WARN. Use it
for old work that nothing runs. A final `/` changes nothing. An entry
MUST be one folder at the top level that the house layout of the set
does not name, so it cannot be `manuscript/`, `background/`, `adhoc/` or
a folder of a version. From the set `house_2026_11_26`, it cannot be
`guidelines/` either. A folder that does not exist is no error. The
locked rules still read an ignored folder, so a tracked file with
`gitexclude` in its name fails there too. The first line of the report
counts the ignored folders.

This `pbn.yml` changes two rules, allows three paths and ignores one
folder:

    extends: house_2026_11_25
    rules:
      cyclomatic_complexity: {limit: 25}
      unknown_path: off
    allow:
      lower_case_names: [adhoc/Old_notes.txt, adhoc/Drafts/]
      no_local_assign:
        - adhoc/explore.R
    ignore: [archive/]

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

- a key other than `extends`, `rules`, `allow` and `ignore`.

- a name that is not a rule. The message names the nearest rule.

- a rule that the set of `extends` does not have. The message names the
  oldest set with it.

- a parameter that the rule does not have.

- a level other than `fail`, `warn` and `off`.

- a `limit` that is not a whole number of 1 or more. For
  `comment_share`, a `limit` that is not a number from 0 to 1.

- a construct in `forbid` that the rule does not know, or an empty
  `forbid`. To turn the rule off, set its level to `off`.

- a value under `allow` that is not a path, for example `off`. YAML
  reads `off`, `yes` and `no` as booleans, so a path with such a name
  needs quotes.

- a change or an allowed path of a locked rule.

- an entry of `ignore` that is not one folder at the top level, is a
  name of the house layout, is there twice, or is a file.

## See also

Other checks:
[`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md),
[`verify()`](https://www.rwhite.no/pbn/reference/verify.md)

## Examples

``` r
home <- file.path(tempdir(), "pbn_check_example")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
writeLines("extends: house_2026_11_25", file.path(home, "pbn.yml"))
writeLines(
  'pbn::initialize_project(version = "v01", home = ".", folders = c(main = "x"))',
  file.path(home, "v01_run.R")
)
writeLines("# A fake project", file.path(home, "README.md"))
writeLines(c("# Changelog", "", "## v01"), file.path(home, "CHANGELOG.md"))
pbn::check_project(home)
#> Rules: house_2026_11_25, 0 overrides, 0 allowed paths (pbn.yml)
#> PASS  lower_case_names  no capital letter in a name
#> PASS  run_script_initialises  every vNN_run.R calls pbn::initialize_project() with its own version
#> PASS  version_items_complete  every vNN_run.R has a vNN_functions/, and the reverse
#> PASS  frozen_versions_unchanged  every version but the newest has a vNN_frozen.md5, and no file of a frozen version changed after pbn::freeze_version()
#> PASS  readme_and_changelog  README.md and CHANGELOG.md exist, and CHANGELOG.md has a section ## vNN for every version
#> PASS  changelog_with_code  the project is not a git repository, so nothing to compare
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and each vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2 on the disk passes check_results()
#> PASS  gitexclude_not_tracked  the project is not a git repository, so nothing to check
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx or vNN_<INITIALS>_<suffix>.docx
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
#> PASS  no_plot_title  no .R file gives a figure a title
#> PASS  no_plot_subtitle  no .R file gives a figure a subtitle
#> PASS  no_plot_caption  no .R file gives a figure a caption
#> PASS  blocks_list  blocks, when a functions folder has it, is a list of named functions in vNN_functions/blocks.R, and no other code uses a name of that file
#> PASS  real_results_are_fake  no real results on the disk come from fake real data
#> PASS  unknown_path  the house layout names every top-level path
#> 30 PASS, 0 WARN, 0 FAIL, 0 OFF

unlink(home, recursive = TRUE)
```
