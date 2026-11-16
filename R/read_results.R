#' Read the results object of a project version
#'
#' `read_results()` reads `<home>/<version>_results/results.qs2`, the file
#' that [save_results()] writes.
#'
#' @param version The version, for example `"v01"`.
#' @param home The project folder.
#' @return The results object, a named list. See [save_results()].
#' @examples
#' # See the example of save_results().
#' @family results
#' @export
read_results <- function(version, home = ".") {
  check_version(version)
  file <- file.path(home, paste0(version, "_results"), "results.qs2")
  if (!file.exists(file)) {
    stop("There is no results file at ", file, call. = FALSE)
  }
  return(qs2::qs_read(file))
}
