#' Check a results object against the contract
#'
#' `check_results()` checks a results object against the contract of
#' [save_results()]. It reports each rule as PASS or FAIL, and it stops on
#' nothing.
#'
#' @param x The results object, normally from [read_results()].
#' @details
#' The rule `element_names` checks that the manifest is there. The rules
#' `manifest_scenario`, `manifest_data` and `manifest_source` check that it
#' is valid. The seven rules:
#' - `element_names`: `x` is a list with the element `manifest` exactly once
#'   and at least one other element. The other element MAY be missing when
#'   `manifest$analyses` puts a kind in `results_gitexclude.qs2`. Each
#'   element has a name. Each name is syntactic, so `make.names()` does not
#'   change it, and no name is there twice.
#' - `exclusions_table`: when `x` has an element `exclusions`, it is a
#'   data.frame with the columns `step`, `n_before`, `n_excluded` and
#'   `n_after`, and MAY have the column `data`. [save_results()] writes
#'   `data` for the exclusion table of a cohort. When `manifest$stacked` is
#'   `TRUE`, it also has the column `sim` that [save_results()] adds when it
#'   stacks. On every row,
#'   `n_before - n_excluded == n_after`. Without an element `exclusions`, the
#'   rule passes and says so.
#' - `allowed_objects`: each element at each depth is one of these:
#'   - a data.frame, of any class that inherits from data.frame. This
#'     includes a data.table and a tibble.
#'   - an atomic vector of any length and of any class, including a matrix,
#'     an array, a factor, a `Date` and a `POSIXct`.
#'   - `NULL`.
#'   - a list of such elements, named or not. The list has no class, or
#'     only classes from this set: `list`, `AsIs`, `table`, `noquote`,
#'     `difftime`, `Date`, `POSIXct`, `factor` and `integer64`.
#'
#'   Each other element fails, and the detail names its path, its class and
#'   a fix. This includes a function, an environment, an S4 object, an R6
#'   object, a formula and each other list with a class. A fitted model of
#'   the class `lm`, `glm`, `gam`, `merMod`, `brmsfit`, `coxph` or `lavaan`
#'   fails too. The detail has a fix for a model, a formula, an `htest`, a
#'   `POSIXlt`, a `package_version` and an `sf` geometry. For each other
#'   object, convert it to a data.frame or a plain list.
#'
#'   The walk goes into the columns of a data.frame, so a model in a list
#'   column fails too. It also goes into the attributes of each object, and
#'   it writes the path of an attribute as `attr(x$a, "terms")`. So the
#'   `terms` attribute of a model frame fails. The walk skips the attributes
#'   that R and data.table set: `names`, `dim`, `dimnames`, `row.names`,
#'   `class`, `levels`, `tzone`, `units`, `comment`, `sorted`, `index` and
#'   `.internal.selfref`.
#' - `no_row_per_person`: no element at any depth of the results has one
#'   value for each row of the data. `manifest$n_rows_data` holds one row
#'   count, or one for each data element. For a cohort,
#'   `manifest$n_rows_before_exclusions` and
#'   `manifest$n_rows_after_exclusions` hold the rows before and after the
#'   exclusions too. `manifest$n_rows_lists` holds the rows of each
#'   data.frame of a data element that is a list of data.frames with 2 or
#'   more numbers of rows. For each count `n` of 2 or more in any of the
#'   four, the rule fails each of these:
#'   - a data.frame with `n` rows.
#'   - an atomic vector of length `n`, also a factor, a `Date` or a
#'     `POSIXct`.
#'   - a matrix or an array with a dimension `n`.
#'
#'   The walk goes into lists, the columns and list columns of data.frames,
#'   and attributes. It does not go into `manifest` or `exclusions`. A count that is `NA`, 0
#'   or 1 checks nothing, and a `manifest$n_rows_data` without another count
#'   checks nothing too. A table with fewer rows than the data passes, and
#'   so does a vector with one value for each group.
#'
#'   For stacked results, the rule is about the rows of one simulation. It
#'   takes the results apart into the value of each simulation. It uses the
#'   column `sim` of each data.frame and the entries `sim_<kkkk>` of each
#'   other value. It checks each simulation against its own counts in
#'   `manifest$n_rows_by_sim`. So 400 simulations of a 1-row table, a
#'   stacked table of 400 rows, pass with 400 rows of data in each
#'   simulation. A table with one row for each person in one simulation
#'   fails. Stacked results without `manifest$n_rows_by_sim` get the rule of
#'   unstacked results. An older pbn wrote their counts as `NA`, so they
#'   check nothing.
#' - `manifest_scenario`: `manifest` has the elements `scenario`, `sim` and
#'   `sims`, and `scenario` is one string.
#' - `manifest_data`: `manifest` has exactly one of `data_files` and
#'   `fake_md5`. It has `data_files` when `scenario` is `"real"`, and
#'   `fake_md5` when `scenario` is another string. `fake_md5` MUST be one
#'   string of 32 lower-case hex digits. `data_files` MUST be a data.frame
#'   with the columns `file`, `size` and `mtime`. Each `file` is a name that
#'   comes once, each `size` a number of 0 or more, and each `mtime` a
#'   number. It has 0 rows when the data folders have no file. A manifest
#'   with `data_md5`, the md5 of the data files of an older pbn, fails this
#'   rule: run the scenario again. For `"real"`, `manifest$fake_real_data`
#'   MUST be `TRUE` or `FALSE`. A real manifest of an older pbn has no
#'   `fake_real_data`, so it fails this rule: run the scenario again.
#'
#'   For `"real"`, `manifest$failed_fake_checks` MUST be a character vector
#'   without attributes, also no names. It MAY be empty. Each element MUST
#'   match `^[a-z0-9_]+$`, MUST NOT be `NA` or `"real"`, and MUST come once.
#'   The element MUST be there when `manifest$pbn_version` is a version of
#'   2026.11.25 or newer. A manifest without it and without such a version
#'   passes, as the results of an older pbn do.
#' - `manifest_source`: `manifest$source_md5` is a named character vector of
#'   md5 values, each a string of 32 lower-case hex digits. It names the run
#'   script `<version>_run.R` of `manifest$version`, or a `vNN_run.R` when
#'   the manifest has no valid version. A results file that pbn 2026.11.20
#'   or older wrote has no `source_md5`, so it fails this rule.
#'
#' `check_results()` checks the object of `results.qs2` or of
#' `sim_<kkkk>.qs2`. The object of a file of `gitexclude` MAY hold data for
#' each person, and these rules do not apply to it. See the section Two
#' files of [save_results()].
#' @return A data.frame with one row per rule and the columns `rule`, `status`
#'   and `detail`. `status` is `"PASS"` or `"FAIL"`.
#' @examples
#' x <- list(
#'   manifest = list(
#'     scenario = "real",
#'     sim = 1L,
#'     sims = 1L,
#'     data_files = data.frame(file = "main/d.csv", size = 120, mtime = 1.8e9),
#'     fake_real_data = FALSE,
#'     source_md5 = c(v01_run.R = "fedcba9876543210fedcba9876543210"),
#'     n_rows_data = 100L
#'   ),
#'   exclusions = data.frame(
#'     step = "age 18 or more",
#'     n_before = 100,
#'     n_excluded = 4,
#'     n_after = 97
#'   ),
#'   estimates = data.frame(term = "x", estimate = 1.2),
#'   p_interaction = 0.04
#' )
#' pbn::check_results(x)
#' @family results
#' @export
check_results <- function(x) {
  return(rbind(
    rule_element_names(x),
    rule_exclusions_table(x),
    rule_allowed_objects(x),
    rule_no_row_per_person(x),
    rule_manifest_scenario(x),
    rule_manifest_data(x),
    rule_manifest_source(x)
  ))
}
