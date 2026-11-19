fit_risk_difference <- function(data, argset) {
  fit <- stats::lm(outcome ~ group, data = data$d)
  return(data.frame(
    term = "group b",
    estimate = stats::coef(fit)[["groupb"]],
    se = sqrt(stats::vcov(fit)["groupb", "groupb"])
  ))
}
