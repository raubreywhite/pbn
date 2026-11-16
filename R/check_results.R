#' Check a results object against the contract
#'
#' `check_results()` checks a results object against the contract of
#' [save_results()]. It reports each rule as PASS or FAIL, and it stops on
#' nothing.
#'
#' @param x The results object, normally from [read_results()].
#' @details
#' The four rules:
#' - `top_level_names`: the names are `manifest`, `exclusions`, `estimates`,
#'   `tables` and `extra`, each once, and there are no others.
#' - `exclusions_table`: `exclusions` is a data.frame with the columns `step`,
#'   `n_before`, `n_excluded` and `n_after`. On every row,
#'   `n_before - n_excluded == n_after`.
#' - `no_fitted_models`: no element anywhere in `x` inherits from `lm`, `glm`,
#'   `gam`, `merMod`, `brmsfit`, `coxph` or `lavaan`.
#' - `no_row_per_person`: no data.frame anywhere in `x` has
#'   `manifest$n_rows_data` rows. When `n_rows_data` is `NA`, this rule checks
#'   nothing.
#' @return A data.frame with one row per rule and the columns `rule`, `status`
#'   and `detail`. `status` is `"PASS"` or `"FAIL"`.
#' @examples
#' x <- list(
#'   manifest = list(n_rows_data = 100L),
#'   exclusions = data.frame(
#'     step = "age 18 or more",
#'     n_before = 100,
#'     n_excluded = 4,
#'     n_after = 97
#'   ),
#'   estimates = data.frame(term = "x", estimate = 1.2),
#'   tables = list(),
#'   extra = list()
#' )
#' pbn::check_results(x)
#' @family results
#' @export
check_results <- function(x) {
  return(rbind(
    rule_top_level_names(x),
    rule_exclusions_table(x),
    rule_no_fitted_models(x),
    rule_no_row_per_person(x)
  ))
}
