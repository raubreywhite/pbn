# Package index

## Start a project or a version

- [`create_project()`](https://www.rwhite.no/pbn/reference/create_project.md)
  : Create a new project
- [`new_version()`](https://www.rwhite.no/pbn/reference/new_version.md)
  : Start a new version of a project

## Run a project version

- [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  : Start a project version
- [`folder()`](https://www.rwhite.no/pbn/reference/folder.md) : The path
  of a folder of the project
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
- [`progress()`](https://www.rwhite.no/pbn/reference/progress.md) : Show
  the progress inside one analysis
- [`get_data()`](https://www.rwhite.no/pbn/reference/get_data.md)
  [`get_argset()`](https://www.rwhite.no/pbn/reference/get_data.md) :
  Get the data or the argset of one analysis at the console

## Results

- [`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
  : Save the results of a run
- [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  : Read the results object of a project version
- [`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
  : Check a results object against the contract
- [`render_run()`](https://www.rwhite.no/pbn/reference/render_run.md) :
  Render the run log to a PDF

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

## Running on an air-gapped server

- [`to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  : Write the run of the real data as one file for a server
- [`check_one_file()`](https://www.rwhite.no/pbn/reference/check_one_file.md)
  : Check the file of to_one_file() on this computer
- [`show_blocks()`](https://www.rwhite.no/pbn/reference/show_blocks.md)
  [`show_block()`](https://www.rwhite.no/pbn/reference/show_blocks.md) :
  Show the numbers of the manuscript, one block at a time

## Checks of the layout

- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  : Check a project against the house layout
- [`verify()`](https://www.rwhite.no/pbn/reference/verify.md) : Run a
  version and check the project
- [`freeze_version()`](https://www.rwhite.no/pbn/reference/freeze_version.md)
  : Freeze a version of a project

## Shared helpers

- [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) : Compute
  the numbers of a descriptive table
