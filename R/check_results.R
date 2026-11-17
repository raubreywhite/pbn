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
#'   and at least one other element. Each element has a name. Each name is
#'   syntactic, so `make.names()` does not change it, and no name is there
#'   twice.
#' - `exclusions_table`: when `x` has an element `exclusions`, it is a
#'   data.frame with the columns `step`, `n_before`, `n_excluded` and
#'   `n_after`. When `manifest$stacked` is `TRUE`, it also has the column
#'   `sim` that [run_scenarios()] adds. On every row,
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
#'   value for each row of the data. When `n` is `manifest$n_rows_data`, the
#'   rule fails each of these:
#'   - a data.frame with `n` rows.
#'   - an atomic vector of length `n`, also a factor, a `Date` or a
#'     `POSIXct`.
#'   - a matrix or an array with a dimension `n`.
#'
#'   The walk goes into lists, the columns and list columns of data.frames,
#'   and attributes. It does not go into `manifest`. When `n_rows_data` is
#'   `NA`, 0 or 1, this rule checks nothing. A table with fewer rows than the
#'   data passes, and so does a vector with one value for each group.
#' - `manifest_scenario`: `manifest` has the elements `scenario`, `sim` and
#'   `sims`, and `scenario` is one string.
#' - `manifest_data`: `manifest` has exactly one of `data_md5` and
#'   `fake_md5`. It has `data_md5` when `scenario` is `"real"`, and
#'   `fake_md5` when `scenario` is another string. `fake_md5` MUST be one
#'   string of 32 lower-case hex digits. `data_md5` MUST be a character
#'   vector of such strings, named by file. It has length 0 when the data
#'   folder has no file. A results file that pbn 2026.11.18 or older wrote
#'   has `data_md5` for a fake scenario, so it fails this rule.
#' - `manifest_source`: `manifest$source_md5` is a named character vector of
#'   md5 values, each a string of 32 lower-case hex digits. It names the run
#'   script `<version>_run.R` of `manifest$version`, or a `vNN_run.R` when
#'   the manifest has no valid version. A results file that pbn 2026.11.20
#'   or older wrote has no `source_md5`, so it fails this rule.
#' @return A data.frame with one row per rule and the columns `rule`, `status`
#'   and `detail`. `status` is `"PASS"` or `"FAIL"`.
#' @examples
#' x <- list(
#'   manifest = list(
#'     scenario = "real",
#'     sim = 1L,
#'     sims = 1L,
#'     data_md5 = c(d.csv = "0123456789abcdef0123456789abcdef"),
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
