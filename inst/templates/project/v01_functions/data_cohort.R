data_cohort <- function(d) {
  cp <- cohort::CohortPipeline$new(d)
  cp$exclude_and_track("root", "missing outcome", "is.na(outcome)")
  return(cp)
}
