make_output <- function(results, dir) {
  utils::write.csv(
    results$risk_difference,
    file.path(dir, "nonum_risk_difference.csv"),
    row.names = FALSE
  )
}
