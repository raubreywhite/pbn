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
#' `PASS|WARN|FAIL  <rule>  <detail>`. A count line comes last.
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
#' The rules, with their level in the set `house_2026_11_17`:
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
#' - `fake_data_names` (FAIL): a file in a folder `output_fake_data/` has a
#'   name without the prefix `fake_data_`, or a file with the prefix is in
#'   another folder. [make_outputs()] writes these folders.
#' - `no_local_copy_of_pbn_function` (FAIL): a top-level assignment in a
#'   `vNN_functions/*.R` file defines a function with the name of a pbn
#'   export. The one exception is `get_data`: a project MAY name its data
#'   function `get_data(scenario, sim)`, and a run script calls the export
#'   as `pbn::get_data()`.
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
#' - `no_environment_variables` (FAIL): a run script or a file in a functions
#'   folder uses `Sys.getenv` or `Sys.setenv`.
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
#' - `no_absolute_paths` (FAIL): a string constant of an `.R` file of
#'   `loc_per_file` is an absolute path, or a path starts from a string
#'   constant. The rule reads the strings from the
#'   parse data, so a comment does not count. The arguments of a call
#'   `pbn::initialize_project()` or `pbn::to_one_file()` are exempt, because
#'   a project declares its paths there. A string is an absolute path when it
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
#' - `blocks_list` (FAIL): the functions folder defines the object `blocks`
#'   of [show_block()], and one of these is true:
#'   - more than one top-level assignment assigns it.
#'   - its value is not a call of `list()`, or the list is empty.
#'   - an element has no name, or a name comes twice.
#'   - an element is not a `function`, or the name of a function that a
#'     top-level assignment of the functions folder defines.
#'
#'   A project without `blocks` passes.
#' - `unknown_path` (WARN): the house layout does not name a top-level file or
#'   folder. The layout names `vNN_run.R`, `vNN_functions/`, `vNN_results/`,
#'   `vNN_cache_gitexclude/`, `vNN_test/`, `vNN_check_functions.R`,
#'   `vNN_one_file.R`, `check_project.R` and `pbn.yml`, among others. A top-level dotfile and a top-level
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
#' extends: house_2026_11_17
#' ```
#'
#' The key `extends` names a house rule set. A set is a dated file in the
#' folder `rules/` of pbn, and pbn never changes a set after its release. A
#' change to the house rules is a new set with a new date. pbn never chooses
#' a set for a project. So a new version of pbn does not change the rules of
#' a project. To read the set, run
#' `readLines(system.file("rules", "house_2026_11_17.yml", package = "pbn"))`.
#'
#' The set gives each rule one of three levels:
#' - `fail`: a finding is FAIL, and `check_project.R` ends with status 1.
#' - `warn`: a finding is WARN.
#' - `off`: the rule does not run, and its one line is OFF.
#'
#' The parameters of the rules are `limit` of `loc_per_file`, `limit` of
#' `cyclomatic_complexity` and `forbid` of `no_local_assign`. The list of
#' the rules above gives their house values.
#'
#' The key `rules` MAY change the level of a rule, a parameter, or both. A
#' change merges into the entry of the set. So `cyclomatic_complexity:
#' {limit: 25}` keeps the level `fail`. The short form `lower_case_names:
#' warn` changes the level only. `forbid` MAY list fewer constructs than the
#' house set.
#'
#' The key `allow` MAY list paths for each rule, relative to `path`. A path
#' allows the findings with exactly that path. A folder MAY end in `/`, and it
#' does not allow the files in it. An allowed finding is WARN, with
#' `(allowed)` at the end. A path that matches no finding is a WARN of the
#' rule `allow`.
#'
#' Three rules are locked: `results_contract`, `gitexclude_not_tracked` and
#' `fake_data_names`. The first two keep data for each person out of git. The
#' third keeps the output of fake data apart from the output of real data.
#' `pbn.yml` cannot set a locked rule to `warn` or `off`, and cannot allow a
#' path for it. It cannot name a rule of [check_results()] either.
#'
#' This `pbn.yml` changes two rules and allows three paths:
#'
#' ```yaml
#' extends: house_2026_11_17
#' rules:
#'   cyclomatic_complexity: {limit: 25}
#'   unknown_path: off
#' allow:
#'   lower_case_names: [adhoc/Old_notes.txt, adhoc/Drafts/]
#'   no_local_assign:
#'     - adhoc/explore.R
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
#' - a key other than `extends`, `rules` and `allow`.
#' - a name that is not a rule. The message names the nearest rule.
#' - a parameter that the rule does not have.
#' - a level other than `fail`, `warn` and `off`.
#' - a `limit` that is not a whole number of 1 or more.
#' - a construct in `forbid` that the rule does not know, or an empty
#'   `forbid`. To turn the rule off, set its level to `off`.
#' - a value under `allow` that is not a path, for example `off`. YAML reads
#'   `off`, `yes` and `no` as booleans, so a path with such a name needs
#'   quotes.
#' - a change or an allowed path of a locked rule.
#' @return Invisibly, a data.frame of the findings with the columns `rule`,
#'   `status`, `path` and `detail`.
#' @examples
#' home <- file.path(tempdir(), "pbn_check_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines("extends: house_2026_11_17", file.path(home, "pbn.yml"))
#' writeLines(
#'   'pbn::initialize_project(version = "v01", home = ".", folders = c(main = "x"))',
#'   file.path(home, "v01_run.R")
#' )
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
  for (rule in names(defs)) {
    out[[rule]] <- rule_findings(path, rule, defs[[rule]], rules$rules[[rule]])
  }
  findings <- bind_rows(out)
  return(apply_allow(findings, rules))
}

# The findings of the rule `rule` with the definition `def` of
# project_rules() and the entry `entry` of the rules of the project. A rule
# that is off gives one OFF row. Each finding of a rule that runs gets the
# level of the rule as its status.
rule_findings <- function(path, rule, def, entry) {
  if (identical(entry$level, "off")) {
    return(project_row(rule, "OFF", "", "off in pbn.yml"))
  }
  params <- entry[setdiff(names(entry), c("level", "locked"))]
  found <- do.call(def[[1]], c(list(path), params))
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
  entries <- paste(allow$rule, allow$path, sep = "\t")
  finding_key <- paste(findings$rule, findings$path, sep = "\t")
  open <- !findings$status %in% c("PASS", "OFF")
  allowed <- open & finding_key %in% entries
  findings$status[allowed] <- "WARN"
  findings$detail[allowed] <- paste(findings$detail[allowed], "(allowed)")
  stale <- allow[!entries %in% finding_key[open], , drop = FALSE]
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
