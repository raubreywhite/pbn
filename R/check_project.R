#' Check a project against the house layout
#'
#' `check_project()` checks a project folder against the house layout and
#' prints one line per finding. In a non-interactive R session it quits with
#' status 1 when any rule fails.
#'
#' @param path The project folder.
#' @details
#' A rule with no finding prints one PASS line. Each finding prints one line:
#' `PASS|WARN|FAIL  <rule>  <detail>`. A count line comes last.
#'
#' Every rule reads one set of files and their folders:
#' - When `path` is the top folder of a git repository, the set is the output
#'   of `git ls-files --cached --others --exclude-standard`. These are the
#'   tracked files, and the untracked files that git does not ignore.
#' - Otherwise, the set is every file and folder on disk.
#'
#' Both sets leave out `.DS_Store`, `.Rhistory`, `.RData`, the Word lock
#' files `~$*`, and everything in `.git/` or `.Rproj.user/`.
#'
#' The rules:
#' - `lower_case_names` (FAIL): a file or folder name has a capital letter.
#'   Everything under `background/` is not checked. These are exempt:
#'   `README.md`, `CHANGELOG.md`, `AGENTS.md`, `CLAUDE.md`, `LICENSE`,
#'   `DESCRIPTION`, a top-level dotfile, a top-level `*.Rproj` file, file
#'   extensions, and the initials in `manuscript/manual/vNN_<INITIALS>.docx`.
#' - `run_script_initialises` (FAIL): a `vNN_run.R` at the top level has no
#'   call `pbn::initialize_project()` whose `version` argument is the string
#'   `"vNN"`. The detail names each argument of the call that
#'   [initialize_project()] does not have.
#' - `version_items_complete` (FAIL): a `vNN_run.R` has no `vNN_functions/`,
#'   or the reverse.
#' - `results_contract` (FAIL): a `vNN_results/<scenario>/results.qs2` or a
#'   `vNN_results/<scenario>/sim_NNNN/results.qs2` fails [check_results()],
#'   or a `results.qs2` is directly in `vNN_results/`.
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
#' - `no_local_copy_of_pbn_function` (FAIL): a top-level assignment in a
#'   `vNN_functions/*.R` file defines a function with the name of a pbn
#'   export.
#' - `loc_per_file` (FAIL): an `.R` file has more than 1000 code lines, as
#'   counted by [org::loc_per_file()].
#' - `no_environment_variables` (FAIL): a run script or a file in a functions
#'   folder uses `Sys.getenv` or `Sys.setenv`.
#' - `unknown_path` (WARN): the house layout does not name a top-level file or
#'   folder. The layout names `vNN_run.R`, `vNN_functions/`, `vNN_results/`,
#'   `vNN_test/`, `vNN_check_functions.R` and `check_project.R`, among
#'   others. A top-level dotfile and a top-level
#'   `*.Rproj` file are never unknown.
#'
#' The R parser, not a regular expression, finds the calls, the assignments
#' and the symbols.
#'
#' The file `pbn_allow.txt` in the project folder MAY list exceptions. Write
#' one `rule<TAB>path` per line, with the path relative to the project folder.
#' An allowed finding prints as WARN, with `(allowed)` at the end. An entry
#' that matches no finding prints as a WARN of the rule `pbn_allow`.
#' @return Invisibly, a data.frame of the findings with the columns `rule`,
#'   `status`, `path` and `detail`.
#' @examples
#' home <- file.path(tempdir(), "pbn_check_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines(
#'   'pbn::initialize_project(version = "v01", home = ".", data_raw = "x")',
#'   file.path(home, "v01_run.R")
#' )
#' pbn::check_project(home)
#'
#' unlink(home, recursive = TRUE)
#' @family checks
#' @export
check_project <- function(path = ".") {
  findings <- project_findings(path)
  lines <- trimws(
    paste0(findings$status, "  ", findings$rule, "  ", findings$detail),
    which = "right"
  )
  cat(lines, sep = "\n")
  counts <- vapply(
    c("PASS", "WARN", "FAIL"),
    function(s) return(sum(findings$status == s)),
    integer(1)
  )
  cat(paste0(counts, " ", names(counts), collapse = ", "), "\n", sep = "")
  if (!interactive() && counts[["FAIL"]] > 0) {
    quit(save = "no", status = 1)
  }
  return(invisible(findings))
}

# The findings of every rule, after the allowlist. check_project() prints
# these, and the tests read them without the quit().
project_findings <- function(path) {
  if (!dir.exists(path)) {
    stop("The project folder does not exist: ", path, call. = FALSE)
  }
  rules <- project_rules()
  out <- list()
  for (rule in names(rules)) {
    found <- rules[[rule]][[1]](path)
    if (nrow(found) == 0) {
      found <- project_row(rule, "PASS", "", rules[[rule]][[2]])
    }
    out[[rule]] <- found
  }
  findings <- bind_rows(out)
  return(apply_allowlist(findings, path))
}

apply_allowlist <- function(findings, path) {
  file <- file.path(path, "pbn_allow.txt")
  if (!file.exists(file)) {
    return(findings)
  }
  entries <- readLines(file, warn = FALSE)
  entries <- entries[nzchar(trimws(entries))]
  finding_key <- paste(findings$rule, findings$path, sep = "\t")
  open <- findings$status != "PASS"
  allowed <- open & finding_key %in% entries
  findings$status[allowed] <- "WARN"
  findings$detail[allowed] <- paste(findings$detail[allowed], "(allowed)")
  stale <- entries[!entries %in% finding_key[open]]
  return(rbind(
    findings,
    project_row(
      "pbn_allow",
      "WARN",
      rep("pbn_allow.txt", length(stale)),
      paste0(
        "pbn_allow.txt: the entry \"",
        sub("\t", "<TAB>", stale, fixed = TRUE),
        "\" matches no finding",
        recycle0 = TRUE
      )
    )
  ))
}
