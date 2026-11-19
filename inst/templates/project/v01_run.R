pbn::initialize_project(version = "v01", home = ".", folders = {{folders}})
scenarios <- pbn::scenarios(CFG, only = setdiff(CFG$scenarios$scenario, "real"))

p <- plnr::Plan$new()
pbn::add_data(p, "d", raw = data_raw, clean = data_clean, cohort = data_cohort)
pbn::add_analyses(p, scenarios, name = "risk_difference", fn = fit_risk_difference)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::make_outputs(make_output, scenarios)
