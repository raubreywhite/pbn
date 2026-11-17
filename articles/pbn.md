# Get started with pbn

## What pbn is for

Most analysis projects have a layout of their own. A number in a
manuscript then often has no clear path back to the code and the data
that made it. Tests on fake data, where they exist, run apart from the
analysis. pbn gives each project one layout and one run script. The run
script is a `plnr` plan, and the real data is one scenario among fake
ones.

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

The real data of this fake project is a small csv file in the folder
`data_raw`, next to the project folder. In a real project, this folder
holds the real data, and it is outside the repository.

``` r
data_raw <- file.path(root, "data_raw")
dir.create(data_raw)
set.seed(4)
d <- data.frame(id = 1:400, group = rep(c("a", "b"), 200), x = round(rnorm(400, 50, 10)))
d$outcome <- rbinom(400, 1, ifelse(d$group == "b", 0.4, 0.25))
d$outcome[sample(400, 12)] <- NA
data.table::fwrite(d, file.path(data_raw, "d.csv"))
```

## The config

`v01_functions/_cfg.R` holds the config of version v01. `CFG$scenarios`
names each scenario and its number of simulations. The scenario `real`
MUST be there, and it uses the real data. Here the fake scenario `known`
has 1 simulation and the fake scenario `null` has 3. Each other column
of `CFG$scenarios` is a setting of the scenario, and
[`?scenarios`](https://www.rwhite.no/pbn/reference/scenarios.md) gives
the rules.

``` r
CFG <- list(
  scenarios = data.frame(
    scenario = c("real", "known", "null"),
    sims = c(1, 1, 3)
  ),
  adjust = c("none", "x"),
  vars = data.table::data.table(
    var = c("outcome", "x"),
    type = c("bin", "cont")
  )
)
```

## The data function

`get_data(scenario, sim)` gives the data of one scenario and simulation.
For `real`, it reads the csv file in the folder that
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
gives as `org::project$data_raw`. For a fake scenario, it makes the
data. pbn sets the seed to `sim` before each call, so each simulation
has its own data, and a second run gives the same data.

``` r
# known: group a has 11 rows, 2 with the outcome, 8 without, 1 missing.
# Group b has 11 rows, 6 with the outcome, 4 without, 1 missing.
# Odds ratio of b against a: (6 / 4) / (2 / 8) = 6.
# null: the outcome does not depend on the group.
get_data <- function(scenario, sim) {
  switch(scenario,
    real = data.table::fread(file.path(org::project$data_raw, "d.csv")),
    known = data.table::data.table(
      id = 1:22,
      group = rep(c("a", "b"), each = 11),
      x = rep(c(31, 45, 38, 52, 40, 33, 47, 36, 50, 42, 44), 2),
      outcome = c(rep(c(1, 0, NA), c(2, 8, 1)), rep(c(1, 0, NA), c(6, 4, 1)))
    ),
    data.table::data.table(
      id = 1:400,
      group = sample(c("a", "b"), 400, replace = TRUE),
      x = round(stats::rnorm(400, 50, 10)),
      outcome = stats::rbinom(400, 1, 0.3)
    )
  )
}
```

The comment holds the arithmetic of `known`, so a reader can check each
expected value without R.

## The analyses

An action function takes `data` and `argset`, as in any `plnr` plan.
`data$d` is the data of the scenario and simulation. `argset` holds
`scenario`, `sim`, the extra dimensions and the settings.

``` r
make_table1 <- function(data, argset) {
  pbn::table1(data$d[!is.na(outcome)], by = "group", vars = CFG$vars)
}

fit_models <- function(data, argset) {
  if (plnr::is_run_directly()) {
    argset <- p$get_argset("null__sim_0003__models__2")
    data <- p$get_data("null__sim_0003__models__2")
  }
  d <- data$d[!is.na(outcome)]
  rhs <- if (argset$adjust == "none") "group" else "group + x"
  fit <- stats::glm(stats::as.formula(paste("outcome ~", rhs)), family = "binomial", data = d)
  data.frame(term = "or_group_b", estimate = exp(stats::coef(fit)[["groupb"]]))
}

# A negative control: with a permuted group, the true odds ratio is 1.
fit_permuted <- function(data, argset) {
  d <- data$d[!is.na(outcome)]
  d[, group := sample(group)]
  fit <- stats::glm(outcome ~ group, family = "binomial", data = d)
  data.frame(term = "or_group_b", estimate = exp(stats::coef(fit)[["groupb"]]))
}
```

## The assertions

Each fake scenario needs a function `expect_<scenario>(results)`. It
gets the results of the scenario, with one element for each analysis
kind, and returns the assertions on them. It MUST return a data.frame
with at least one row and the columns `assertion`, `expected`,
`observed` and `pass`. `expect_real()` is optional.

``` r
expect_known <- function(results) {
  t1 <- results$table1
  m <- results$models
  observed <- c(
    t1$events[t1$var == "outcome" & t1$grp == "b"],
    m$estimate[m$adjust == "none"]
  )
  expected <- c(6, 6)
  data.frame(
    assertion = c("outcome events in group b", "crude odds ratio of b against a"),
    expected = expected,
    observed = observed,
    pass = abs(observed - expected) < 1e-6
  )
}

# results holds the 3 simulations stacked, and each data.frame has a first
# column sim.
expect_null <- function(results) {
  m <- results$models
  p <- results$permuted
  log_or <- mean(log(m$estimate[m$adjust == "none"]))
  log_or_permuted <- mean(log(p$estimate))
  data.frame(
    assertion = c("simulations", "mean crude log odds ratio", "mean permuted log odds ratio"),
    expected = c("3", "-0.5 to 0.5", "-0.5 to 0.5"),
    observed = c(length(unique(m$sim)), round(log_or, 3), round(log_or_permuted, 3)),
    pass = c(length(unique(m$sim)) == 3, abs(log_or) < 0.5, abs(log_or_permuted) < 0.5)
  )
}
```

The scenario `known` tests two outputs against numbers that a person
computed by hand. It does not test how the analysis behaves on data as
large or as untidy as the real data. The scenario `null` tests that the
mean log odds ratio over 3 simulations is within 0.5 of zero when the
groups do not differ. It does not test that the analysis finds a
difference that is there. With 400 rows, one log odds ratio has a
standard error of about 0.22, so the mean of 3 has about 0.13.

## The run script

`v01_run.R` runs every scenario as one `plnr` plan:

1.  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
    sources `v01_functions/` and sets the paths.
2.  [`pbn::scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)
    checks `CFG$scenarios` and chooses the scenarios of the run.
3.  [`pbn::add_data()`](https://www.rwhite.no/pbn/reference/add_data.md)
    adds the data function, and
    [`pbn::add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
    adds each analysis kind.
4.  [`pbn::run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
    runs the plan, and
    [`pbn::save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    saves and checks the results.
5.  [`pbn::end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md)
    ends the script when the run had no real data.
6.  PART 3 writes the output of the real results.

``` r
pbn::initialize_project(version = "v01", home = ".", data_raw = "../data_raw")
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d", fn = function(scenario, sim) get_data(scenario, sim))
pbn::add_analyses(p, scenarios, name = "table1", fn = make_table1)
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, adjust = CFG$adjust)
pbn::add_analyses(p, scenarios, name = "permuted", fn = fit_permuted, control = TRUE)
r <- pbn::run_plan(p)
pbn::save_results(r, p, scenarios)

pbn::end_unless_real(scenarios)

# PART 3 -- OUTPUT ====
res <- pbn::read_results("v01")
data.table::fwrite(res$table1, file.path("v01_results", "real", "table1.csv"))
```

[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
adds one analysis for each scenario, simulation and value of the extra
dimensions. `models` has the extra dimension `adjust`, so it has two
analyses for each simulation. `permuted` has `control = TRUE`, so it
runs for the fake scenarios only and never appears in the real results.
`pbn::scenarios(CFG, only = "null")` runs one scenario.

`data_raw` is relative to the project folder, because the script runs
there. In a real project, run it with `Rscript v01_run.R` from the
project folder. Here [`source()`](https://rdrr.io/r/base/source.html)
with `chdir = TRUE` runs it, so the plan stays in this R session:

``` r
source(file.path(home, "v01_run.R"), chdir = TRUE)
#> Sourcing all code inside /tmp/RtmpnSszvI/pbn_get_started/project/v01_functions into R_GlobalEnv
#> Assertions of the scenario "known":
#>                        assertion expected observed pass
#>        outcome events in group b        6        6 TRUE
#>  crude odds ratio of b against a        6        6 TRUE
#> Assertions of the scenario "null":
#>                     assertion    expected observed pass
#>                   simulations           3    3.000 TRUE
#>     mean crude log odds ratio -0.5 to 0.5    0.133 TRUE
#>  mean permuted log odds ratio -0.5 to 0.5   -0.186 TRUE
```

plnr called `get_data()` once for each of the 5 scenarios and
simulations, and gave the data to every analysis of that simulation. The
plan has 24 analyses. pbn added the 5 analyses
`<scenario>__sim_<kkkk>__pbn_n_rows`, which count the rows of the data
for the manifest.

``` r
length(p$analyses)
#> [1] 24
head(names(p$analyses), 8)
#> [1] "real__sim_0001__pbn_n_rows"  "known__sim_0001__pbn_n_rows"
#> [3] "null__sim_0001__pbn_n_rows"  "null__sim_0002__pbn_n_rows" 
#> [5] "null__sim_0003__pbn_n_rows"  "real__sim_0001__table1__1"  
#> [7] "known__sim_0001__table1__1"  "null__sim_0001__table1__1"
```

## More than one worker

`workers = 2` runs the analyses on 2 new R sessions, in a PSOCK cluster.
pbn gives each worker the objects that `v01_functions/` defines and the
attached packages, so the functions find `CFG` and each other. Each data
function and each analysis sets its own seed, so the results are the
same:

``` r
r2 <- pbn::run_plan(p, workers = 2)
identical(r2, r)
#> [1] TRUE
```

This holds for functions that depend only on their arguments, the
functions and `CFG` of `v01_functions/`, the attached packages and the
seed. pbn does not copy the R options to the workers.

## The results

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
wrote one results object for each scenario and simulation. The scenario
`null` has one folder for each simulation, and its stacked
`results.qs2`. Each fake scenario has `check.csv`, the table of its
assertions.

``` r
list.files(file.path(home, "v01_results"), recursive = TRUE)
#> [1] "known/check.csv"           "known/results.qs2"        
#> [3] "null/check.csv"            "null/results.qs2"         
#> [5] "null/sim_0001/results.qs2" "null/sim_0002/results.qs2"
#> [7] "null/sim_0003/results.qs2" "real/results.qs2"         
#> [9] "real/table1.csv"
```

A results object is a named list. The first element is `manifest`, and
then comes one element for each analysis kind. The data.frames of one
kind are row-bound, with the extra dimension `adjust` as the first
column. A value that is not a data.frame stays as it is.
[`?save_results`](https://www.rwhite.no/pbn/reference/save_results.md)
gives the rules.

``` r
res <- pbn::read_results("v01", home = home)
names(res)
#> [1] "manifest" "table1"   "models"
res$models
#>   adjust       term estimate
#> 1   none or_group_b 1.747826
#> 2      x or_group_b 1.745184
```

[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads the real results by default. With `scenario`, it reads the results
of a fake scenario, and with `sim` one simulation. The stacked results
of `null` have a first column `sim`:

``` r
null <- pbn::read_results("v01", scenario = "null", home = home)
null$models
#>   sim adjust       term estimate
#> 1   1   none or_group_b 1.087506
#> 2   1      x or_group_b 1.096456
#> 3   2   none or_group_b 1.323457
#> 4   2      x or_group_b 1.324507
#> 5   3   none or_group_b 1.035040
#> 6   3      x or_group_b 1.036015
cat(readLines(file.path(home, "v01_results", "null", "check.csv")), sep = "\n")
#> "assertion","expected","observed","pass"
#> "simulations","3",3,TRUE
#> "mean crude log odds ratio","-0.5 to 0.5",0.133,TRUE
#> "mean permuted log odds ratio","-0.5 to 0.5",-0.186,TRUE
```

The manifest records where the results come from. It holds the versions
of R, pbn and the loaded packages, the date, the scenario and the
simulation. `source_md5` holds the md5 of `v01_run.R` and of each file
in `v01_functions/`. For the real data, `data_md5` holds the md5 of each
data file. For a fake scenario, `fake_md5` holds the md5 of the code of
the data function. `n_rows_data` holds the number of rows of the data.

``` r
str(res$manifest[c("scenario", "sim", "sims", "data_md5", "n_rows_data", "analyses")])
#> List of 6
#>  $ scenario   : chr "real"
#>  $ sim        : int 1
#>  $ sims       : int 1
#>  $ data_md5   : Named chr "396527785293e765115178711b7aab75"
#>   ..- attr(*, "names")= chr "d.csv"
#>  $ n_rows_data: Named int 400
#>   ..- attr(*, "names")= chr "d"
#>  $ analyses   :'data.frame': 2 obs. of  2 variables:
#>   ..$ analysis: chr [1:2] "table1" "models"
#>   ..$ control : logi [1:2] FALSE FALSE
```

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
checks each results object with
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
before it writes anything. It stops on a fitted model or a function in
the results, and on a table with one row for each row of the data. That
check compares sizes only, so check the small groups by hand.

## Stepping through one analysis by hand

The names of the analyses are `<scenario>__sim_<kkkk>__<name>__<i>`.
`<i>` counts the values of the extra dimensions. To step through one
analysis of `fit_models()` by hand, run the script up to
[`pbn::run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md),
then run the body of the function line by line. At the top level of the
console,
[`plnr::is_run_directly()`](https://www.rwhite.no/plnr/reference/is_run_directly.html)
is `TRUE`, so the first lines load that analysis:

``` r
argset <- p$get_argset("null__sim_0003__models__2")
data <- p$get_data("null__sim_0003__models__2")
argset
#> $scenario
#> [1] "null"
#> 
#> $sim
#> [1] 3
#> 
#> $analysis
#> [1] "models"
#> 
#> $seed
#> [1] 1253196396
#> 
#> $adjust
#> [1] "x"
nrow(data$d)
#> [1] 400
```

When the plan runs `fit_models()`,
[`plnr::is_run_directly()`](https://www.rwhite.no/plnr/reference/is_run_directly.html)
is `FALSE`, and the lines do nothing. The plan calls
`set.seed(argset$seed)` before each analysis. Run that line too before
you step through a function that draws random numbers, such as
`fit_permuted()`.
[`?add_analyses`](https://www.rwhite.no/pbn/reference/add_analyses.md)
says how pbn derives the seed from the kind, its counter and the
simulation.

## The output part

[`end_unless_real()`](https://www.rwhite.no/pbn/reference/end_unless_real.md)
comes directly after
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
When the run had the scenario `real`, it does nothing. Otherwise, under
`Rscript`, it quits R with status 0, so PART 3 runs for the real data
only. In an interactive R session, it stops with an error, and the R
session stays open.

Run the script for the scenario `null` only, in a new R session:

``` r
script <- sub("pbn::scenarios(CFG)", 'pbn::scenarios(CFG, only = "null")', readLines(file.path(home, "v01_run.R")), fixed = TRUE)
writeLines(script, file.path(root, "v01_run_null.R"))
unlink(file.path(home, "v01_results", "real", "table1.csv"))
out <- callr::rscript(file.path(root, "v01_run_null.R"), wd = home, show = FALSE, fail_on_status = FALSE)
out$status
#> [1] 0
cat(grep("^pbn:", strsplit(out$stdout, "\n")[[1]], value = TRUE), sep = "\n")
#> pbn: The run had no scenario "real", so it ends here. The output part of the script needs the real results.
file.exists(file.path(home, "v01_results", "real", "table1.csv"))
#> [1] FALSE
```

The run wrote under `v01_results/null/` only. The results of the other
scenarios stay as they are, also when they are out of date.

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
#> PASS  version_items_complete  every vNN_run.R has a vNN_functions/, and the reverse
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and vNN_results/<scenario>/sim_NNNN/results.qs2 passes check_results()
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  unknown_path  the house layout names every top-level path
#> 11 PASS, 0 WARN, 0 FAIL
```

In a non-interactive R session,
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
quits R with status 1 when a rule fails. Add a file with a capital
letter in its name, and run `check_project.R` in a new R session:

``` r
dir.create(file.path(home, "adhoc"))
writeLines("notes", file.path(home, "adhoc", "Notes.txt"))
out <- callr::rscript(file.path(home, "check_project.R"), wd = home, show = FALSE, fail_on_status = FALSE)
out$status
#> [1] 1
cat(grep("FAIL", strsplit(out$stdout, "\n")[[1]], value = TRUE), sep = "\n")
#> FAIL  lower_case_names  adhoc/Notes.txt: the name has a capital letter
#> 10 PASS, 0 WARN, 1 FAIL
unlink(file.path(home, "adhoc"), recursive = TRUE)
```

## A check that goes red

Make one small error in the analysis: swap the two branches of the `if`
in `fit_models()`. The crude model is then adjusted for `x`, and the
adjusted model is crude. Keep a copy of the file first.

``` r
file <- file.path(home, "v01_functions", "analyses.R")
file.copy(file, file.path(root, "analyses_copy.R"))
#> [1] TRUE
code <- readLines(file)
code <- sub('"group" else "group + x"', '"group + x" else "group"', code, fixed = TRUE)
writeLines(code, file)
grep("rhs <-", readLines(file), value = TRUE)
#> [1] "  rhs <- if (argset$adjust == \"none\") \"group + x\" else \"group\""
```

Run the script in a new R session:

``` r
out <- callr::rscript(file.path(home, "v01_run.R"), wd = home, show = FALSE, fail_on_status = FALSE)
out$status
#> [1] 1
cat(readLines(file.path(home, "v01_results", "known", "check.csv")), sep = "\n")
#> "assertion","expected","observed","pass"
#> "outcome events in group b",6,6,TRUE
#> "crude odds ratio of b against a",6,7.0369269141867,FALSE
```

The real data runs through the same functions as the fake data. A fake
scenario with a known answer thus tests the analysis that gives the real
results. On the real data, the swapped models give plausible odds ratios
and no error. Only the planted odds ratio shows the mistake.
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
now fails the rule `scenario_checks`. The results files stay, so that
you can find the fault, but
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
stops on them:

``` r
pbn::read_results("v01", scenario = "known", home = home)
#> Error:
#> ! /tmp/RtmpnSszvI/pbn_get_started/project/v01_results/known/check.csv holds an assertion that did not pass, so read_results() does not read /tmp/RtmpnSszvI/pbn_get_started/project/v01_results/known/results.qs2. Fix the analysis and run it again. To look at the results for debugging, read them with qs2::qs_read("/tmp/RtmpnSszvI/pbn_get_started/project/v01_results/known/results.qs2").
```

Put the copy back, and run the script again. Its assertions pass.

``` r
file.copy(file.path(root, "analyses_copy.R"), file, overwrite = TRUE)
#> [1] TRUE
out <- callr::rscript(file.path(home, "v01_run.R"), wd = home, show = FALSE, fail_on_status = FALSE)
out$status
#> [1] 0
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

- A run always runs every selected scenario and simulation. pbn keeps no
  results of an earlier run to resume from.
- No function starts a new version, for example `v02_run.R` and
  `v02_functions/` from version v01.
- No function creates the folders and the files of a new project.
- No tool shows the tracked changes between two Word manuscripts.
- No function formats a table for a journal.
  [`table1()`](https://www.rwhite.no/pbn/reference/table1.md) computes
  the numbers only, and it gives no p-value for a continuous variable.
- When [`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
  stops on a failed analysis, the results files of an earlier run stay
  as they are.
  [`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
  reads them.
- pbn finds an analysis that adds, removes, renames or replaces a column
  of the shared data. It does not find a change of values in place, for
  example `data$d[1, x := 0]`. Copy the data first with
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html).
- `workers` limits the number of R sessions. It does not limit the
  threads that R or the BLAS library starts in each of them.
- `fake_md5` is the md5 of the code of the data functions of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) only.
  A change in a function that they call, such as `get_data()`, does not
  change `fake_md5`. `source_md5` holds the md5 of its file.
- The check of the rows of the data compares sizes only. A copy of the
  data with one row less passes.
