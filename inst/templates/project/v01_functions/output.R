make_output <- function(results, dir) {
  utils::write.csv(
    results$risk_difference,
    file.path(dir, "risk_difference.csv"),
    row.names = FALSE
  )
}
