# Get started with pbn

## What pbn is for

Most analysis projects have a layout of their own. A number in a
manuscript then often has no clear path back to the code and the data
that made it. Tests on fake data, where they exist, run apart from the
analysis. pbn gives each project one layout and one run script, and it
makes the real data one scenario among fake ones.

## The layout

Names are lower case, with a few exceptions such as `README.md` and the
initials in a manuscript file name.
[`?check_project`](https://www.rwhite.no/pbn/reference/check_project.md)
lists every exception. Each version of the analysis has a prefix, here
`v01_`. A project holds these items at the top level:

``` text
v01_run.R                 the run script of version v01
v01_functions/            the config and the functions that the run script sources
v01_results/<scenario>/   the results of each scenario
v01_test/                 optional testthat files for the functions
v01_check_functions.R     optional checks of the functions
check_project.R           one line: pbn::check_project()
run_scenarios.R           one line: pbn::run_scenarios("v01")
pbn_allow.txt             optional exceptions to check_project()
manuscript/manual/        manuscripts that a person edits
manuscript/built/vNN/     manuscripts that code builds from the results
background/               documents from outside the project, with names that pbn does not check
adhoc/                    work outside the run script
data_describe/            descriptions of the data
README.md                 what the project is and how to run it
CHANGELOG.md              the changes of each version
AGENTS.md                 the rules for agents that work in the project
.gitignore                the files that git leaves out
```

The rest of this article builds a small fake project in a temporary
folder, file by file, and runs it. The project compares a 0/1 outcome
between two groups, `a` and `b`.

``` r
root <- file.path(tempdir(), "pbn_get_started")
home <- file.path(root, "project")
dir.create(file.path(home, "v01_functions"), recursive = TRUE)
```

## The config

`v01_functions/_cfg.R` holds the config of version v01. `CFG$scenarios`
names each scenario and its number of simulations. The scenario `real`
MUST be there, and it uses the real data. Here the fake scenario `known`
has 1 simulation and the fake scenario `null` has 3. `CFG$vars` names
the variables of the descriptive table.

``` r
CFG <- list(
  scenarios = data.frame(
    scenario = c("real", "known", "null"),
    sims = c(1, 1, 3)
  ),
  vars = data.table::data.table(
    var = c("outcome", "x"),
    type = c("bin", "cont")
  )
)
```

Keep `_cfg.R` to top-level assignments.
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
and
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
source `v01_functions/` in a new R session to read `CFG$scenarios`, so
each other top-level call in the folder runs there too.

## A fake scenario

Each fake scenario needs two functions in `v01_functions/`.
`fake_<scenario>(dir)` writes the data files into the folder `dir`.
`expect_<scenario>(results)` gets the results of the scenario and
returns the assertions on them.

The scenario `known` plants the counts by hand. The comment holds the
arithmetic, so a reader can check each expected value without R.

``` r
# Group a has 11 rows: 2 with the outcome, 8 without, 1 missing.
# Group b has 11 rows: 6 with the outcome, 4 without, 1 missing.
# The run excludes the 2 rows with a missing outcome: 22 - 2 = 20 rows.
# Percentage with the outcome: 100 * (2 + 6) / 20 = 40.
# Odds ratio of b against a: (6 / 4) / (2 / 8) = 6.
fake_known <- function(dir) {
  d <- data.frame(
    id = 1:22,
    group = rep(c("a", "b"), each = 11),
    x = 31:52,
    outcome = c(rep(c(1, 0, NA), c(2, 8, 1)), rep(c(1, 0, NA), c(6, 4, 1)))
  )
  data.table::fwrite(d, file.path(dir, "d.csv"))
}

expect_known <- function(results) {
  e <- results$estimates
  t1 <- results$tables$table1
  observed <- c(
    results$exclusions$n_excluded,
    t1$events[t1$var == "outcome" & t1$grp == "b"],
    e$estimate[e$term == "pct_outcome"],
    e$estimate[e$term == "or_group_b"]
  )
  expected <- c(2, 6, 40, 6)
  data.frame(
    assertion = c(
      "rows excluded",
      "outcome events in group b",
      "percentage with the outcome",
      "odds ratio of b against a"
    ),
    expected = expected,
    observed = observed,
    pass = abs(observed - expected) < 1e-6
  )
}
```

The scenario `known` tests four outputs of the analysis against numbers
that a person computed by hand. It does not test how the analysis
behaves on data as large or as untidy as the real data.

`expect_<scenario>()` MUST return a data.frame with at least one row and
the columns `assertion`, `expected`, `observed` and `pass`.
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
counts an error, zero rows or another shape as one failed assertion.
`expect_real()` is optional.

The scenario `null` draws data where the outcome does not depend on the
group. pbn sets the seed to the number of the simulation before it calls
`fake_null()`. Each simulation thus has different data, and a second run
gives the same data again.

``` r
fake_null <- function(dir) {
  d <- data.frame(
    id = 1:400,
    group = sample(c("a", "b"), 400, replace = TRUE),
    x = round(stats::rnorm(400, 50, 10)),
    outcome = stats::rbinom(400, 1, 0.3)
  )
  data.table::fwrite(d, file.path(dir, "d.csv"))
}

# results holds the 3 simulations stacked, with a first column sim.
expect_null <- function(results) {
  e <- results$estimates
  sims_n <- length(unique(e$sim))
  log_or <- mean(log(e$estimate[e$term == "or_group_b"]))
  data.frame(
    assertion = c("simulations", "mean log odds ratio of b against a"),
    expected = c("3", "-0.5 to 0.5"),
    observed = c(sims_n, round(log_or, 3)),
    pass = c(sims_n == 3, abs(log_or) < 0.5)
  )
}
```

The scenario `null` tests that the mean log odds ratio over 3
simulations is within 0.5 of zero when the data has no difference
between the groups. It does not test that the analysis finds a
difference that is there. With 400 rows, one log odds ratio has a
standard error of about 0.22, so the mean of 3 has about 0.13. The bound
0.5 is thus about 4 standard errors.

## The run script

`v01_run.R` runs one simulation of one scenario. The same script runs on
each scenario, and only the data changes.

The real data of this fake project is a small csv file in the folder
`data_raw`, next to the project folder. In a real project, this folder
holds the real data, and it lives outside the repository.

``` r
data_raw <- file.path(root, "data_raw")
dir.create(data_raw)
set.seed(4)
d <- data.frame(id = 1:400, group = rep(c("a", "b"), 200), x = round(rnorm(400, 50, 10)))
d$outcome <- rbinom(400, 1, ifelse(d$group == "b", 0.4, 0.25))
d$outcome[sample(400, 12)] <- NA
data.table::fwrite(d, file.path(data_raw, "d.csv"))
```

The run script has these parts, in this order:

1.  One commented line `# pbn::use_scenario("<name>", sim = 1)` for each
    scenario. The section “Stepping through a scenario by hand” explains
    them.
2.  The call
    [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
    It sources `v01_functions/`, and it gives the data folder
    `p$data_raw` and the results folder `p$results` of the scenario.
3.  PART 1 reads the data, and PART 2 computes the results and saves
    them with
    [`pbn::save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
4.  [`pbn::end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
    ends the run of a fake scenario.
5.  PART 3 writes the output of the real results.

``` r
# Run one of these lines to step through a scenario by hand. The choice lasts for the R session.
# pbn::use_scenario("real", sim = 1)
# pbn::use_scenario("known", sim = 1)
# pbn::use_scenario("null", sim = 1)
p <- pbn::initialize_project(version = "v01", home = ".", data_raw = "../data_raw")

# PART 1 -- DATA CREATION ====
d0 <- data.table::fread(file.path(p$data_raw, "d.csv"))
d <- d0[!is.na(outcome)]

# PART 2 -- ANALYSIS ====
fit <- stats::glm(outcome ~ group, family = "binomial", data = d)
pbn::save_results(
  exclusions = data.frame(
    step = "outcome not missing",
    n_before = nrow(d0),
    n_excluded = nrow(d0) - nrow(d),
    n_after = nrow(d)
  ),
  estimates = data.frame(
    term = c("pct_outcome", "or_group_b"),
    estimate = c(100 * mean(d$outcome), exp(stats::coef(fit)[["groupb"]]))
  ),
  tables = list(table1 = pbn::table1(d, by = "group", vars = CFG$vars)),
  d = d
)
pbn::end_if_fake_scenario()

# PART 3 -- OUTPUT ====
r <- pbn::read_results("v01")
data.table::fwrite(r$tables$table1, file.path(p$results, "table1.csv"))
```

`data_raw` is relative to the project folder, because
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
runs the script there. pbn reads `data_raw` for the real data only.

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
checks the results object before it writes `results.qs2`. It stops on a
fitted model of a class that
[`?check_results`](https://www.rwhite.no/pbn/reference/check_results.md)
lists. It also stops on a table with as many rows as the data. `d = d`
gives pbn that number of rows. Without `d`, pbn does not make the second
check.

## Running everything

`run_scenarios.R` holds one line. In a real project, you run it with the
project folder as the working directory.

``` r
pbn::run_scenarios("v01")
```

Here the call gives the project folder as `home`.
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
runs `v01_run.R` once for each simulation of each scenario, each time in
a new R session. It then stacks the 3 simulations of `null`, and it
calls `expect_known()` and `expect_null()`.

``` r
pbn::run_scenarios("v01", home = home)
#> pbn: scenario real, simulation 1
#> pbn: scenario known, simulation 1
#> pbn: scenario null, simulation 1
#> pbn: scenario null, simulation 2
#> pbn: scenario null, simulation 3
#> Assertions of the scenario "known":
#>                    assertion expected observed pass
#>                rows excluded        2        2 TRUE
#>    outcome events in group b        6        6 TRUE
#>  percentage with the outcome       40       40 TRUE
#>    odds ratio of b against a        6        6 TRUE
#> Assertions of the scenario "null":
#>                           assertion    expected observed pass
#>                         simulations           3    3.000 TRUE
#>  mean log odds ratio of b against a -0.5 to 0.5    0.133 TRUE
```

Each scenario has a folder in `v01_results/`. The scenario `null` has
one folder for each simulation, and its stacked `results.qs2`. A fake
run keeps its data in `data/`.

``` r
list.files(file.path(home, "v01_results"), recursive = TRUE)
#>  [1] "known/check.csv"           "known/data/d.csv"         
#>  [3] "known/results.qs2"         "null/check.csv"           
#>  [5] "null/results.qs2"          "null/sim_0001/data/d.csv" 
#>  [7] "null/sim_0001/results.qs2" "null/sim_0002/data/d.csv" 
#>  [9] "null/sim_0002/results.qs2" "null/sim_0003/data/d.csv" 
#> [11] "null/sim_0003/results.qs2" "real/results.qs2"         
#> [13] "real/table1.csv"
```

[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
writes the assertions of each fake scenario to `check.csv`:

``` r
cat(readLines(file.path(home, "v01_results", "known", "check.csv")), sep = "\n")
#> "assertion","expected","observed","pass"
#> "rows excluded",2,2,TRUE
#> "outcome events in group b",6,6,TRUE
#> "percentage with the outcome",40,40,TRUE
#> "odds ratio of b against a",6,5.99999999999878,TRUE
```

## Reading results

[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads the `results.qs2` of one scenario. The default scenario is `real`.

``` r
r <- pbn::read_results("v01", home = home)
r$estimates
#>          term  estimate
#> 1 pct_outcome 29.123711
#> 2  or_group_b  1.747826
r$tables$table1
#>        var    grp     n missing     mean       sd events      pct          p
#>     <char> <char> <int>   <int>    <num>    <num>  <int>    <num>      <num>
#> 1: outcome    All   388       0       NA       NA    113 29.12371 0.01325468
#> 2: outcome      a   196       0       NA       NA     46 23.46939 0.01325468
#> 3: outcome      b   192       0       NA       NA     67 34.89583 0.01325468
#> 4:       x    All   388       0 49.76031 9.711698     NA       NA         NA
#> 5:       x      a   196       0 50.12245 9.768358     NA       NA         NA
#> 6:       x      b   192       0 49.39062 9.664991     NA       NA         NA
#>      test
#>    <char>
#> 1:  chisq
#> 2:  chisq
#> 3:  chisq
#> 4:   <NA>
#> 5:   <NA>
#> 6:   <NA>
```

With `sim`, it reads the results of that one simulation. Without `sim`,
it reads the results file of the scenario. For a scenario with one
simulation, that file is the results of the run. For a scenario with
more, it is the stacked results of all simulations, with a first column
`sim`.

``` r
pbn::read_results("v01", scenario = "null", sim = 2, home = home)$estimates
#>          term  estimate
#> 1 pct_outcome 30.250000
#> 2  or_group_b  1.323457
pbn::read_results("v01", scenario = "null", home = home)$estimates
#>   sim        term  estimate
#> 1   1 pct_outcome 31.750000
#> 2   1  or_group_b  1.087506
#> 3   2 pct_outcome 30.250000
#> 4   2  or_group_b  1.323457
#> 5   3 pct_outcome 27.250000
#> 6   3  or_group_b  1.035040
```

The manifest records where the results come from. It holds the versions
of R, pbn and the loaded packages, the date, the scenario and the
simulation. For the real data, `data_md5` holds the md5 of each data
file. For a fake scenario, `fake_md5` holds the md5 of the code of
`fake_<scenario>()`.

``` r
str(r$manifest)
#> List of 11
#>  $ pbn_version: chr "2026.11.20"
#>  $ r_version  : chr "R version 4.6.2 (2026-10-29)"
#>  $ packages   : Named chr [1:8] "0.2.0" "1.18.6.1" "1.8.1" "2026.11.18" ...
#>   ..- attr(*, "names")= chr [1:8] "charport" "data.table" "glue" "org" ...
#>  $ run_date   : chr "2026-11-16"
#>  $ version    : chr "v01"
#>  $ scenario   : chr "real"
#>  $ sim        : int 1
#>  $ sims       : int 1
#>  $ data_raw   : chr "/tmp/RtmpSo0Qmx/pbn_get_started/data_raw"
#>  $ data_md5   : Named chr "396527785293e765115178711b7aab75"
#>   ..- attr(*, "names")= chr "d.csv"
#>  $ n_rows_data: int 388
```

## Stepping through a scenario by hand

The commented lines at the top of `v01_run.R` are for a person who steps
through the script in an interactive R session. To step through a
scenario:

1.  Set the project folder as the working directory.
2.  Run one of the commented lines without the `#`.
3.  Run the script line by line.

``` r
pbn::use_scenario("null", sim = 1)
# Now run v01_run.R line by line.
pbn::use_scenario("real", sim = 1)
```

This chunk does not run when the article builds, because
[`use_scenario()`](https://www.rwhite.no/pbn/reference/use_scenario.md)
stops in a non-interactive R session.

The choice lasts for the R session. Each later
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
call uses it, also when you run the script again.
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
then prints one line, here
`pbn: scenario null, simulation 1 of 3 (fake)`. Change `sim` to step
through another simulation. `pbn::use_scenario("real", sim = 1)` goes
back to the real data.

The lines MUST stay commented in the file. An active line stops the run
under `Rscript`, and
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
fails it.

## Where a fake run ends

[`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
comes directly after
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
For a fake scenario under
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md),
it quits R with status 0, and
[`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
counts that as a success. PART 3 thus runs for the real data only, and
only the real scenario has `table1.csv`:

``` r
list.files(file.path(home, "v01_results"), pattern = "^table1[.]csv$", recursive = TRUE)
#> [1] "real/table1.csv"
```

In an interactive R session,
[`end_if_fake_scenario()`](https://www.rwhite.no/pbn/reference/end_if_fake_scenario.md)
stops with an error for a fake scenario, and the R session stays open.

## Checking the project

`check_project.R` holds one line.

``` r
pbn::check_project()
```

[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
checks a project folder against the layout. It prints one line for each
rule that passes, one line for each finding, and a count.

``` r
pbn::check_project(home)
#> PASS  lower_case_names  no capital letter in a name
#> PASS  run_script_initialises  every vNN_run.R calls pbn::initialize_project() with its own version
#> PASS  no_active_use_scenario  no vNN_run.R has an active call pbn::use_scenario()
#> PASS  use_scenario_lines  every vNN_run.R has one line # pbn::use_scenario("<name>", sim = 1) for each scenario in CFG$scenarios, above pbn::initialize_project()
#> PASS  version_items_complete  every vNN_run.R has a vNN_functions/, and the reverse
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and vNN_results/<scenario>/sim_NNNN/results.qs2 passes check_results()
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  unknown_path  the house layout names every top-level path
#> 13 PASS, 0 WARN, 0 FAIL
```

In a non-interactive R session,
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
quits R with status 1 when a rule fails. The build of this article is
such a session. So the article runs the failing check in a new R
session, with `callr`.

Add a file with a capital letter in its name, and run `check_project.R`:

``` r
dir.create(file.path(home, "adhoc"))
writeLines("notes", file.path(home, "adhoc", "Notes.txt"))
out <- callr::rscript(file.path(home, "check_project.R"), wd = home, show = FALSE, fail_on_status = FALSE)
out$status
#> [1] 1
cat(grep("FAIL", strsplit(out$stdout, "\n")[[1]], value = TRUE), sep = "\n")
#> FAIL  lower_case_names  adhoc/Notes.txt: the name has a capital letter
#> 12 PASS, 0 WARN, 1 FAIL
```

Remove the file again:

``` r
unlink(file.path(home, "adhoc"), recursive = TRUE)
```

## A check that goes red

Make one small error in the analysis: compute the percentage on the rows
before the exclusion. Keep a copy of the script first.

``` r
run <- file.path(home, "v01_run.R")
file.copy(run, file.path(root, "v01_run_copy.R"))
#> [1] TRUE
script <- readLines(run)
script <- sub("100 * mean(d$outcome)", "100 * sum(d$outcome) / nrow(d0)", script, fixed = TRUE)
writeLines(script, run)
grep("sum(d$outcome)", readLines(run), value = TRUE, fixed = TRUE)
#> [1] "    estimate = c(100 * sum(d$outcome) / nrow(d0), exp(stats::coef(fit)[[\"groupb\"]]))"
```

Run the scenario `known` only:

``` r
pbn::run_scenarios("v01", scenario = "known", home = home)
#> pbn: scenario known, simulation 1
#> Assertions of the scenario "known":
#>                    assertion expected observed  pass
#>                rows excluded        2  2.00000  TRUE
#>    outcome events in group b        6  6.00000  TRUE
#>  percentage with the outcome       40 36.36364 FALSE
#>    odds ratio of b against a        6  6.00000  TRUE
#> Error:
#> ! These assertions failed:
#>   known: percentage with the outcome (expected 40, observed 36.3636363636364)
```

The real data runs through the same lines of `v01_run.R` as the fake
data. A fake scenario with a known answer thus tests the analysis that
gives the real results. On the real data, the wrong denominator gives a
plausible percentage and no error. Only the planted count shows the
mistake.

Put the copy back, and run the scenario again. Its assertions pass.

``` r
file.copy(file.path(root, "v01_run_copy.R"), run, overwrite = TRUE)
pbn::run_scenarios("v01", scenario = "known", home = home)
```

## Manuscripts

`manuscript/manual/` holds the Word files that a person edits. Each name
MUST match `vNN_<INITIALS>.docx`, for example `v01_AB.docx`, with the
initials in upper case. `manuscript/built/vNN/` holds the manuscripts
that code builds from the results. A built manuscript MUST read the real
results only, and
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
fails a
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
call there with another scenario. A call without `scenario` reads the
real results:

``` r
r <- pbn::read_results("v01")
```

The default `home = "."` is the working directory, so this file MUST run
with the project folder as the working directory.

## What pbn does not do yet

- No function starts a new version, for example `v02_run.R` and
  `v02_functions/` from version v01.
- No function creates the folders and the files of a new project.
- No tool shows the tracked changes between two Word manuscripts.
- No function formats a table for a journal.
  [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) computes
  the numbers only, and it gives no p-value for a continuous variable.
- [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  runs the simulations one after another. A run that fails stops it, and
  the later runs do not start.
- When
  [`run_scenarios()`](https://www.rwhite.no/pbn/reference/run_scenarios.md)
  runs some scenarios only, the results of the other scenarios stay as
  they are, also when they are out of date.
- `fake_md5` is the md5 of the code of `fake_<scenario>()` only. A
  change in a function that it calls does not change `fake_md5`.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  runs project code when it sources `v01_functions/`. Its rule
  `use_scenario_lines` does not check the order of the lines.
