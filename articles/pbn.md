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
holds the real data, and it is outside the repository. The file has 400
rows, and the column names of a typical export.

``` r
data_raw <- file.path(root, "data_raw")
dir.create(data_raw)
set.seed(4)
d <- data.frame(id = 1:400, group = rep(c("a", "b"), 200), x = round(rnorm(400, 50, 10)))
d$outcome <- rbinom(400, 1, ifelse(d$group == "b", 0.4, 0.25))
d$outcome[sample(400, 12)] <- NA
d$x[sample(400, 20)] <- NA
names(d) <- c("Person ID", "Group", "X", "Outcome")
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

`CFG$data` holds the config that the data code reads. The section
Imputation is data uses it.

``` r
CFG <- list(
  scenarios = data.frame(
    scenario = c("real", "known", "null"),
    sims = c(1, 1, 3)
  ),
  data = list(n_imp = 3),
  adjust = c("none", "x"),
  vars = data.table::data.table(
    var = c("outcome", "x"),
    type = c("bin", "cont")
  )
)
```

## The data function

The data function of the run script has three steps, and each step has
its own file in `v01_functions/`:

1.  `data_raw(scenario, sim)` reads the real file, or makes fake data
    with the same columns.
2.  `data_clean(d)` gives the columns clean names. It drops no row.
3.  `data_cohort(d)` applies the exclusions with `cohort`, and returns
    the cohort.

`data_raw()` reads the real file from the folder that
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
gives as `org::project$data_raw`. pbn sets the seed to `sim` before each
call, so each simulation has its own data, and a second run gives the
same data.

``` r
# known: group a has 11 rows, 2 with the outcome, 8 without, 1 missing.
# Group b has 11 rows, 6 with the outcome, 4 without, 1 missing.
# Odds ratio of b against a: (6 / 4) / (2 / 8) = 6.
# null: the outcome does not depend on the group.
data_raw <- function(scenario, sim) {
  switch(scenario,
    real = data.table::fread(file.path(org::project$data_raw, "d.csv")),
    known = data.table::data.table(
      `Person ID` = 1:22,
      Group = rep(c("a", "b"), each = 11),
      X = rep(c(31, 45, 38, 52, 40, 33, 47, 36, 50, 42, 44), 2),
      Outcome = c(rep(c(1, 0, NA), c(2, 8, 1)), rep(c(1, 0, NA), c(6, 4, 1)))
    ),
    data.table::data.table(
      `Person ID` = 1:400,
      Group = sample(c("a", "b"), 400, replace = TRUE),
      X = replace(round(stats::rnorm(400, 50, 10)), sample(400, 20), NA),
      Outcome = stats::rbinom(400, 1, 0.3)
    )
  )
}
```

The comment holds the arithmetic of `known`, so a reader can check each
expected value without R.

``` r
data_clean <- function(d) {
  data.table::setnames(d, janitor::make_clean_names(names(d)))
  d[, outcome := as.integer(outcome)]
  d
}
```

``` r
data_cohort <- function(d) {
  cp <- cohort::CohortPipeline$new(d)
  cp$exclude_and_track("root", "missing outcome", "is.na(outcome)")
  cp
}
```

The value of `data_cohort()` is a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html).
The analyses get its data.table after the exclusions, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stores its exclusion table.
[`?add_data`](https://www.rwhite.no/pbn/reference/add_data.md) gives the
rules.

## The analyses

An action function takes `data` and `argset`, as in any `plnr` plan.
`data$d` is the data of the scenario and simulation. `argset` holds
`scenario`, `sim`, the extra dimensions and the settings.

``` r
make_table1 <- function(data, argset) {
  pbn::table1(data$d, by = "group", vars = CFG$vars)
}

# A negative control: with a permuted group, the true odds ratio is 1.
fit_permuted <- function(data, argset) {
  d <- data.table::copy(data$d)
  d[, group := sample(group)]
  fit <- stats::glm(outcome ~ group, family = "binomial", data = d)
  data.frame(term = "or_group_b", estimate = exp(stats::coef(fit)[["groupb"]]))
}
```

``` r
fit_models <- function(data, argset) {
  if (plnr::is_run_directly()) {
    argset <- p$get_argset("null__sim_0003__models__2")
    data <- p$get_data("null__sim_0003__models__2")
  }
  rhs <- if (argset$adjust == "none") "group" else "group + x"
  fit <- stats::glm(stats::as.formula(paste("outcome ~", rhs)), family = "binomial", data = data$d)
  data.frame(term = "or_group_b", estimate = exp(stats::coef(fit)[["groupb"]]))
}
```

`fit_permuted()` changes the group, so it copies the data first. plnr
gives every analysis of one simulation the same data object.

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
pbn::add_data(p, "d", fn = function(scenario, sim) data_raw(scenario, sim) |> data_clean() |> data_cohort())
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
#> Sourcing all code inside /tmp/RtmpFNSQ1w/pbn_get_started/project/v01_functions into R_GlobalEnv
#> Assertions of the scenario "known":
#>                        assertion expected observed pass
#>        outcome events in group b        6        6 TRUE
#>  crude odds ratio of b against a        6        6 TRUE
#> Assertions of the scenario "null":
#>                     assertion    expected observed pass
#>                   simulations           3    3.000 TRUE
#>     mean crude log odds ratio -0.5 to 0.5   -0.032 TRUE
#>  mean permuted log odds ratio -0.5 to 0.5   -0.024 TRUE
```

plnr called the data function once for each of the 5 scenarios and
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
the second is `exclusions`, the exclusion table of the cohort. Then
comes one element for each analysis kind. The data.frames of one kind
are row-bound, with the extra dimension `adjust` as the first column. A
value that is not a data.frame stays as it is.
[`?save_results`](https://www.rwhite.no/pbn/reference/save_results.md)
gives the rules.

``` r
res <- pbn::read_results("v01", home = home)
names(res)
#> [1] "manifest"   "exclusions" "table1"     "models"
res$exclusions
#>   data            step n_before n_excluded n_after
#> 1    d missing outcome      400         12     388
res$models
#>   adjust       term estimate
#> 1   none or_group_b 1.747826
#> 2      x or_group_b 1.965332
```

[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads the real results by default. With `scenario`, it reads the results
of a fake scenario, and with `sim` one simulation. The stacked results
of `null` have a first column `sim`:

``` r
null <- pbn::read_results("v01", scenario = "null", home = home)
null$models
#>   sim adjust       term  estimate
#> 1   1   none or_group_b 0.8633094
#> 2   1      x or_group_b 0.8466263
#> 3   2   none or_group_b 1.0701754
#> 4   2      x or_group_b 1.0954111
#> 5   3   none or_group_b 0.9846777
#> 6   3      x or_group_b 0.9995009
cat(readLines(file.path(home, "v01_results", "null", "check.csv")), sep = "\n")
#> "assertion","expected","observed","pass"
#> "simulations","3",3,TRUE
#> "mean crude log odds ratio","-0.5 to 0.5",-0.032,TRUE
#> "mean permuted log odds ratio","-0.5 to 0.5",-0.024,TRUE
```

The manifest records where the results come from. It holds the versions
of R, pbn and the loaded packages, the date, the scenario and the
simulation. `source_md5` holds the md5 of `v01_run.R` and of each file
in `v01_functions/`. For the real data, `data_md5` holds the md5 of each
data file. For a fake scenario, `fake_md5` holds the md5 of the code of
the data function. `n_rows_data` holds the number of rows of the data
after the exclusions, and `n_rows_before_exclusions` the number of rows
given to the cohort.

``` r
str(res$manifest[c("scenario", "sim", "sims", "data_md5", "n_rows_data", "n_rows_before_exclusions", "analyses")])
#> List of 7
#>  $ scenario                : chr "real"
#>  $ sim                     : int 1
#>  $ sims                    : int 1
#>  $ data_md5                : Named chr "179004ea822c9ea08e7772775a477930"
#>   ..- attr(*, "names")= chr "d.csv"
#>  $ n_rows_data             : Named int 388
#>   ..- attr(*, "names")= chr "d"
#>  $ n_rows_before_exclusions: Named int 400
#>   ..- attr(*, "names")= chr "d"
#>  $ analyses                :'data.frame':    2 obs. of  3 variables:
#>   ..$ analysis: chr [1:2] "table1" "models"
#>   ..$ control : logi [1:2] FALSE FALSE
#>   ..$ file    : chr [1:2] "results.qs2" "results.qs2"
```

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
checks each results object with
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
before it writes anything. It stops on a fitted model or a function in
the results. It also stops on a table with one row for each row of the
data, before or after the exclusions. That check compares sizes only, so
check the small groups by hand.

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
#> PASS  gitexclude_not_tracked  the project is not a git repository, so nothing to check
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  unknown_path  the house layout names every top-level path
#> 12 PASS, 0 WARN, 0 FAIL
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
#> 11 PASS, 0 WARN, 1 FAIL
unlink(file.path(home, "adhoc"), recursive = TRUE)
```

## A check that goes red

Make one small error in the analysis: swap the two branches of the `if`
in `fit_models()`. The crude model is then adjusted for `x`, and the
adjusted model is crude. Keep a copy of the file first.

``` r
file <- file.path(home, "v01_functions", "fit_models.R")
file.copy(file, file.path(root, "fit_models_copy.R"))
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
#> ! /tmp/RtmpFNSQ1w/pbn_get_started/project/v01_results/known/check.csv holds an assertion that did not pass, so read_results() does not read /tmp/RtmpFNSQ1w/pbn_get_started/project/v01_results/known/results.qs2. Fix the analysis and run it again. To look at the results for debugging, read them with qs2::qs_read("/tmp/RtmpFNSQ1w/pbn_get_started/project/v01_results/known/results.qs2").
```

Put the copy back, and run the script again. Its assertions pass.

``` r
file.copy(file.path(root, "fit_models_copy.R"), file, overwrite = TRUE)
#> [1] TRUE
out <- callr::rscript(file.path(home, "v01_run.R"), wd = home, show = FALSE, fail_on_status = FALSE)
out$status
#> [1] 0
```

## Imputation is data

A multiple imputation can take hours, and every analysis reads its
result. So it belongs in the data function. With `cache = TRUE` in
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) saves
the value of the data function on disk for each scenario and simulation.
The next run reads it again while the cache key is the same. A change of
an analysis then runs the analyses only.

`data_impute()` comes after `data_cohort()`, so the imputation model
uses the people of the cohort only. It draws `CFG$data$n_imp` values for
each missing `x`, from a linear model of `x` on `group` with a normal
error. It writes the draws to the columns `x_1`, `x_2` and `x_3`, and
keeps one row for each person. This is stochastic regression imputation.
It does not draw the parameters of the model, so it is not a proper
multiple imputation.

``` r
data_impute <- function(cp) {
  d <- pbn::cohort_data(cp)
  fit <- stats::lm(x ~ group, data = d)
  miss <- which(is.na(d$x))
  mu <- stats::predict(fit, d[miss])
  for (k in seq_len(CFG$data$n_imp)) {
    col <- paste0("x_", k)
    d[, (col) := x]
    data.table::set(d, miss, col, round(mu + stats::rnorm(length(miss), 0, stats::sigma(fit))))
  }
  pbn::keep_exclusions(d, cp)
}
```

`pbn::cohort_data(cp)` gives the data of the cohort after its
exclusions. `pbn::keep_exclusions(d, cp)` returns the imputed data with
the exclusion table of the cohort, so
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
still writes `exclusions`. The value MAY be any R object, for example a
list of imputed data sets.
[`?cohort_data`](https://www.rwhite.no/pbn/reference/cohort_data.md)
gives the rules.

`fit_models()` now fits the model once for each imputation. Rubin’s
rules give the pooled estimate as the mean of the estimates. The project
computes no standard error here, so it needs no variance between the
imputations.

``` r
fit_models <- function(data, argset) {
  log_or <- numeric(0)
  for (k in seq_len(CFG$data$n_imp)) {
    rhs <- if (argset$adjust == "none") "group" else paste0("group + x_", k)
    fit <- stats::glm(stats::as.formula(paste("outcome ~", rhs)), family = "binomial", data = data$d)
    log_or[k] <- stats::coef(fit)[["groupb"]]
  }
  data.frame(term = "or_group_b", estimate = exp(mean(log_or)))
}
```

Add `data_impute()` and `cache = TRUE` to the data function of
`v01_run.R`:

``` r
run_file <- file.path(home, "v01_run.R")
script <- sub(
  "data_cohort())",
  "data_cohort() |> data_impute(), cache = TRUE)",
  readLines(run_file),
  fixed = TRUE
)
writeLines(script, run_file)
grep("add_data", script, value = TRUE)
#> [1] "pbn::add_data(p, \"d\", fn = function(scenario, sim) data_raw(scenario, sim) |> data_clean() |> data_cohort() |> data_impute(), cache = TRUE)"
```

The cache folder `v01_cache_gitexclude/` holds data for each person. In
a git repository,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) stops
until git ignores it, so add the line `*gitexclude*` to `.gitignore`:

``` text
*gitexclude*
```

[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) prints
one line for each scenario and simulation. This function runs the script
in a new R session and shows those lines:

``` r
run_lines <- function() {
  out <- callr::rscript(run_file, wd = home, show = FALSE, fail_on_status = FALSE)
  stopifnot(out$status == 0)
  lines <- grep("^pbn: data", strsplit(out$stdout, "\n")[[1]], value = TRUE)
  cat(lines, sep = "\n")
  invisible(lines)
}
first <- run_lines()
#> pbn: data d, real sim_0001: computed, because there is no cache file.
#> pbn: data d, known sim_0001: computed, because there is no cache file.
#> pbn: data d, null sim_0001: computed, because there is no cache file.
#> pbn: data d, null sim_0002: computed, because there is no cache file.
#> pbn: data d, null sim_0003: computed, because there is no cache file.
```

The first run computed the data, and wrote one file for each scenario
and simulation. The second run reads them:

``` r
list.files(file.path(home, "v01_cache_gitexclude"), recursive = TRUE)
#>  [1] "d/known/sim_0001_key.qs2" "d/known/sim_0001.qs2"    
#>  [3] "d/null/sim_0001_key.qs2"  "d/null/sim_0001.qs2"     
#>  [5] "d/null/sim_0002_key.qs2"  "d/null/sim_0002.qs2"     
#>  [7] "d/null/sim_0003_key.qs2"  "d/null/sim_0003.qs2"     
#>  [9] "d/real/sim_0001_key.qs2"  "d/real/sim_0001.qs2"
second <- run_lines()
#> pbn: data d, real sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, known sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0002: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0003: reused the cache of 2026-11-17 03:59.
```

Change an analysis: `fit_models()` also reports the number of
imputations. The data comes from the cache:

``` r
code <- readLines(file.path(home, "v01_functions", "fit_models.R"))
code <- sub("estimate = exp(mean(log_or)))", "estimate = exp(mean(log_or)), n_imp = length(log_or))", code, fixed = TRUE)
writeLines(code, file.path(home, "v01_functions", "fit_models.R"))
third <- run_lines()
#> pbn: data d, real sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, known sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0002: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0003: reused the cache of 2026-11-17 03:59.
pbn::read_results("v01", home = home)$models
#>   adjust       term estimate n_imp
#> 1   none or_group_b 1.747826     3
#> 2      x or_group_b 1.751888     3
```

Change the data code: `data_impute()` keeps one decimal of each draw.
pbn computes the data again and names the changed file:

``` r
code <- readLines(file.path(home, "v01_functions", "data_impute.R"))
code <- sub("round(mu + ", "round(digits = 1, mu + ", code, fixed = TRUE)
writeLines(code, file.path(home, "v01_functions", "data_impute.R"))
fourth <- run_lines()
#> pbn: data d, real sim_0001: computed, because v01_functions/data_impute.R changed.
#> pbn: data d, known sim_0001: computed, because v01_functions/data_impute.R changed.
#> pbn: data d, null sim_0001: computed, because v01_functions/data_impute.R changed.
#> pbn: data d, null sim_0002: computed, because v01_functions/data_impute.R changed.
#> pbn: data d, null sim_0003: computed, because v01_functions/data_impute.R changed.
```

A step by hand reads the cache too. Run the script up to
[`pbn::run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) in
this R session, and get the data of one analysis:

``` r
upto <- readLines(run_file)
upto <- upto[seq_len(grep("run_plan", upto) - 1)]
withr::with_dir(home, eval(parse(text = upto)))
#> Sourcing all code inside /tmp/RtmpFNSQ1w/pbn_get_started/project/v01_functions into R_GlobalEnv
data <- p$get_data("null__sim_0003__models__2")
#> pbn: data d, null sim_0003: reused the cache of 2026-11-17 03:59.
names(data$d)
#> [1] "person_id" "group"     "x"         "outcome"   "x_1"       "x_2"      
#> [7] "x_3"
```

The cache key holds the md5 of each `data_*.R` file of `v01_functions/`,
and `CFG$data`. It also holds the code of the data function, the raw
data, the settings, the versions of R and pbn, and
[`RNGkind()`](https://rdrr.io/r/base/Random.html). Beside the key, pbn
records the version of each package loaded when the data was computed. A
new version of one of them computes the data again.
[`?add_data`](https://www.rwhite.no/pbn/reference/add_data.md) gives the
full rule. Keep the data code in `data_*.R` files: a change in another
file does not change the key. To compute every value again, delete
`v01_cache_gitexclude/`.

## Results with data for each person

A results file goes to git, so it MUST NOT hold one row for each person.
Some results do: fitted models, the imputations, or a predicted risk for
each person. `save_results(gitexclude = )` writes the kinds that it
names to `results_gitexclude.qs2`, beside each `results.qs2`. The line
`*gitexclude*` of `.gitignore` keeps that file out of git.

`predict_risk()` gives the predicted risk of each person, from the model
of the first imputation:

``` r
predict_risk <- function(data, argset) {
  fit <- stats::glm(outcome ~ group + x_1, family = "binomial", data = data$d)
  data.frame(person_id = data$d$person_id, risk = stats::fitted(fit))
}
```

Add the kind `predicted` to `v01_run.R`, and name it in `gitexclude`:

``` r
script <- readLines(run_file)
script <- sub(
  "r <- pbn::run_plan(p)",
  "pbn::add_analyses(p, scenarios, name = \"predicted\", fn = predict_risk)\nr <- pbn::run_plan(p)",
  script,
  fixed = TRUE
)
script <- sub("save_results(r, p, scenarios)", "save_results(r, p, scenarios, gitexclude = \"predicted\")", script, fixed = TRUE)
writeLines(script, run_file)
fifth <- run_lines()
#> pbn: data d, real sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, known sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0001: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0002: reused the cache of 2026-11-17 03:59.
#> pbn: data d, null sim_0003: reused the cache of 2026-11-17 03:59.
list.files(file.path(home, "v01_results", "real"))
#> [1] "results_gitexclude.qs2" "results.qs2"            "table1.csv"
```

[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads both files and returns one list.
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
and
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
check `results.qs2` only, so `predicted` passes the check of the rows of
the data:

``` r
res <- pbn::read_results("v01", home = home)
names(res)
#> [1] "manifest"   "exclusions" "table1"     "models"     "predicted"
nrow(res$predicted)
#> [1] 388
```

A coauthor who clones the repository has no `results_gitexclude.qs2`.
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
then returns the other elements, with one warning:

``` r
private <- file.path(home, "v01_results", "real", "results_gitexclude.qs2")
file.rename(private, file.path(root, "kept.qs2"))
#> [1] TRUE
names(pbn::read_results("v01", home = home))
#> Warning: The manifest names the analysis kinds predicted in
#> /tmp/RtmpFNSQ1w/pbn_get_started/project/v01_results/real/results_gitexclude.qs2,
#> and that file is missing, for example in a clone of the repository.
#> read_results() returns the other elements.
#> [1] "manifest"   "exclusions" "table1"     "models"
file.rename(file.path(root, "kept.qs2"), private)
#> [1] TRUE
```

The
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
rule `gitexclude_not_tracked` fails when git tracks a path with
`gitexclude` in its name.

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
  A change in a function that they call, such as `data_raw()`, does not
  change `fake_md5`. `source_md5` holds the md5 of its file.
- The check of the rows of the data compares sizes only. A copy of the
  data with one row less passes.
- The cache stores the whole value of one data element for each scenario
  and simulation. A change of the code of one imputation computes every
  imputation again.
- The cache key holds the files `data_*.R` only. A change in data code
  in another file does not change the key, and the run uses the old
  data. This is the one change of the data code that the cache does not
  find.
- No argument computes the cached data again. Delete
  `v01_cache_gitexclude/` to do that.
- A use of the cache stops when a `data_*.R` file changed after
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md).
  Run
  [`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
  again.
- Do not run one scenario of one version in two R sessions at the same
  time. The two runs can mix the files of their results.
- [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
  checks only that `results_gitexclude.qs2` holds the kinds that the
  manifest names. It does not check what is in them.
