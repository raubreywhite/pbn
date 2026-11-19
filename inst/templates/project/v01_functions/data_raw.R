data_raw <- function(scenario, sim) {
  if (scenario == "real") {
    stop(
      "Replace this stop() in data_raw() with code that reads the real data, ",
      "for example data.table::fread(pbn::folder(\"main\", \"d.csv\")).",
      call. = FALSE
    )
  }
  return(data.table::data.table(
    Group = rep(c("a", "b"), each = 5),
    Outcome = c(1, 0, 0, 0, NA, 1, 1, 1, 0, 0)
  ))
}
