# Package index

## Run a project version

- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  : Start a project version
- [`scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md) :
  Choose the scenarios of a run
- [`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md)
  : End the run script here unless the real data ran

## The plan

- [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) : Add
  data to a plan
- [`cohort_data()`](https://www.rwhite.no/pbn/reference/cohort_data.md)
  [`keep_exclusions()`](https://www.rwhite.no/pbn/reference/cohort_data.md)
  : Work on the data of a cohort and keep its exclusion table
- [`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
  : Add one analysis for each scenario and simulation to a plan
- [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) : Run
  a plan

## Results

- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  : Save the results of a run
- [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  : Read the results object of a project version
- [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  : Check a results object against the contract

## Checks of fake scenarios

- [`expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md)
  : Check that an analysis recovers the truth of a fake scenario
- [`expect_biased()`](https://www.rwhite.no/pbn/reference/expect_biased.md)
  : Check that a control analysis shows its bias

## Output

- [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  : Write the output of the real data and of chosen fake scenarios
- [`stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md) :
  Mark a figure of fake data

## Checks of the layout

- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  : Check a project against the house layout

## Shared helpers

- [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) : Compute
  the numbers of a descriptive table
