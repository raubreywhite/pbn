expect_known <- function(results) {
  observed <- c(
    results$exclusions$n_excluded,
    results$risk_difference$estimate
  )
  expected <- c(1, 0.35)
  return(data.frame(
    assertion = c("rows excluded", "difference of the risks, b minus a"),
    expected = expected,
    observed = observed,
    pass = abs(observed - expected) < 1e-9
  ))
}
