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
v01_results/<scenario>/   the results and the output of each scenario
v01_cache_gitexclude/     the cache of the data, which git ignores
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
between two groups, `a` and `b`, adjusted for a covariate `x`.

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

## The run script

`v01_run.R` runs every scenario as one `plnr` plan. This is the whole
script, and the sections below write each file that it sources:

``` r
pbn::initialize_project(version = "v01", home = ".", data_raw = "../data_raw")
scenarios <- pbn::scenarios(CFG)

p <- plnr::Plan$new()
pbn::add_data(p, "d",
  raw = data_raw,
  clean = data_clean,
  cohort = data_cohort,
  after_cohort = data_impute,
  cache = TRUE
)
pbn::add_analyses(p, scenarios, name = "table1", fn = make_table1)
pbn::add_analyses(p, scenarios, name = "models", fn = fit_models, adjust = "x")
pbn::add_analyses(p, scenarios, name = "crude", fn = fit_models, adjust = "none", control = TRUE)
pbn::add_analyses(p, scenarios, name = "predicted", fn = predict_risk)
r <- pbn::run_plan(p, workers = 2)
pbn::save_results(r, p, scenarios, gitexclude = "predicted")

pbn::make_outputs(make_output, scenarios)
```

1.  [`pbn::initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
    sources `v01_functions/` and sets the paths.
2.  [`pbn::scenarios()`](https://www.rwhite.no/pbn/reference/scenarios.md)
    checks `CFG$scenarios` and chooses the scenarios of the run.
3.  [`pbn::add_data()`](https://www.rwhite.no/pbn/reference/add_data.md)
    adds the four steps of the data, and
    [`pbn::add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
    adds each analysis kind.
4.  [`pbn::run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md)
    runs the plan on 2 workers.
5.  [`pbn::save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
    saves the results, and checks each fake scenario.
6.  [`pbn::make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
    writes the figures and the tables.

To look at the data at the console, run the script up to the calls of
[`pbn::add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md).
Then get the data of one scenario and simulation, here simulation 1 of
`real`:

``` r
d <- pbn::get_data(p, "real")$d
```

`data_raw` is relative to the project folder, because the script runs
there. In a real project, run it with `Rscript v01_run.R` from the
project folder.

Three items have `gitexclude` in their names: the cache, a results file
with data for each person, and the results of each simulation. In a git
repository,
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stop until git ignores them. So `.gitignore` MUST hold this line:

``` text
*gitexclude*
```

## The config and the scenarios

`v01_functions/_cfg.R` holds the config of version v01. `CFG$scenarios`
names each scenario and its number of simulations. The scenario `real`
MUST be there, and it uses the real data. Each other scenario is fake:

- `known` has 22 rows, with an odds ratio that a person computed by
  hand.
- `null` has 140 simulations where the outcome does not depend on the
  group.
- `confounded` has 140 simulations where `x` raises the chance of group
  `b` and of the outcome.

The column `output` chooses the fake scenarios that get the figures of
[`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md).
Each other column of `CFG$scenarios` is a setting of the scenario, and
[`?scenarios`](https://www.rwhite.no/pbn/reference/scenarios.md) gives
the rules. `CFG$data` holds the config that the data code reads, and
`CFG$level` is the level of the confidence intervals.

``` r
CFG <- list(
  scenarios = data.frame(
    scenario = c("real", "known", "null", "confounded"),
    sims = c(1, 1, 140, 140),
    output = c(FALSE, TRUE, FALSE, FALSE)
  ),
  data = list(n_imp = 3),
  level = 0.9,
  vars = data.table::data.table(
    var = c("outcome", "x"),
    type = c("bin", "cont")
  )
)
```

The project reports 90% intervals. A check of the coverage of a 95%
interval needs 288 simulations, and this article must build in a short
time. The section The checks gives the rule.

## The data steps

The data has four steps, and each step has its own file in
`v01_functions/`.
[`pbn::add_data()`](https://www.rwhite.no/pbn/reference/add_data.md)
gets each step as an argument, and runs the steps in this order:

1.  `raw`: `data_raw(scenario, sim)` reads the real file, or makes fake
    data with the same columns.
2.  `clean`: `data_clean(d)` gives the columns clean names.
3.  `cohort`: `data_cohort(d)` applies the exclusions with `cohort`, and
    returns the cohort.
4.  `after_cohort`: `data_impute(cp)` imputes the missing values of `x`.
    The section Imputation is data explains it.

pbn takes the steps apart to count the rows between them. Each exclusion
MUST go through `$exclude_and_track()` in `data_cohort()`, so that the
exclusion table counts it. So `data_clean()` MUST return as many rows as
`data_raw()` gave it, and the cohort MUST start with all of them.
Otherwise the data fails, and the error gives both numbers of rows. The
section A cleaning step that drops a row shows the error.

`data_raw()` reads the real file from the folder that
[`initialize_project()`](https://www.rwhite.no/pbn/reference/initialize_project.md)
gives as `org::project$data_raw`. pbn sets the seed to `sim` once,
before `data_raw()`, so each simulation has its own data, and a second
run gives the same data.

``` r
# known: group a has 11 rows, 2 with the outcome, 8 without, 1 missing.
# Group b has 11 rows, 6 with the outcome, 4 without, 1 missing.
# Odds ratio of b against a: (6 / 4) / (2 / 8) = 6.
# null: the outcome does not depend on the group or on x.
# confounded: x raises the chance of group b and of the outcome. The log odds
# ratio of b against a, adjusted for x, is 0.5.
data_raw <- function(scenario, sim) {
  if (scenario == "real") {
    return(data.table::fread(file.path(org::project$data_raw, "d.csv")))
  }
  if (scenario == "known") {
    return(data.table::data.table(
      `Person ID` = 1:22,
      Group = rep(c("a", "b"), each = 11),
      X = rep(c(31, 45, 38, 52, 40, 33, 47, 36, 50, 42, 44), 2),
      Outcome = c(rep(c(1, 0, NA), c(2, 8, 1)), rep(c(1, 0, NA), c(6, 4, 1)))
    ))
  }
  conf <- scenario == "confounded"
  x <- stats::rnorm(400, 50, 10)
  b <- stats::rbinom(400, 1, stats::plogis(conf * (x - 50) / 5))
  y <- stats::rbinom(400, 1, stats::plogis(-1 + conf * (0.5 * b + (x - 50) / 10)))
  data.table::data.table(
    `Person ID` = 1:400,
    Group = c("a", "b")[b + 1],
    X = replace(round(x), sample(400, 20), NA),
    Outcome = y
  )
}
```

The comment holds the arithmetic of `known` and the truth of
`confounded`, so a reader can check each expected value without R.

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

``` r
data_impute <- function(cp) {
  d <- pbn::cohort_data(cp)
  fit <- stats::lm(x ~ group + outcome, data = d)
  miss <- which(is.na(d$x))
  mu <- stats::predict(fit, d[miss])
  for (k in seq_len(CFG$data$n_imp)) {
    col <- paste0("x_", k)
    d[, (col) := x]
    data.table::set(d, miss, col, round(mu + stats::rnorm(length(miss), 0, stats::sigma(fit))))
  }
  d
}
```

The value of `data_cohort()` is a
[`cohort::CohortPipeline`](https://www.rwhite.no/cohort/reference/CohortPipeline.html).
`pbn::cohort_data(cp)` gives its data.table after the exclusions to the
next step. pbn puts the exclusion table of the cohort on the value of
`data_impute()`, so
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
still stores that table.
[`?add_data`](https://www.rwhite.no/pbn/reference/add_data.md) and
[`?cohort_data`](https://www.rwhite.no/pbn/reference/cohort_data.md)
give the rules.

Data that does not fit the four steps, for example data that joins two
cohorts, goes to
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) as one
function: `fn = function(scenario, sim)`. pbn then checks no rows inside
it, and a step after the cohort in `fn` returns its value through
`pbn::keep_exclusions(x, cp)`.

## The analyses and the controls

An action function takes `data` and `argset`, as in any `plnr` plan.
`data$d` is the data of the scenario and simulation. `argset` holds
`scenario`, `sim`, the extra dimensions and the settings.

``` r
make_table1 <- function(data, argset) {
  pbn::table1(data$d, by = "group", vars = CFG$vars)
}

# The predicted risk of each person, from the model of the first imputation.
predict_risk <- function(data, argset) {
  fit <- stats::glm(outcome ~ group + x_1, family = "binomial", data = data$d)
  data.frame(person_id = data$d$person_id, risk = stats::fitted(fit))
}
```

`fit_models()` fits the model once for each imputation, and pools the
log odds ratio of group `b` with Rubin’s rules. The extra dimension
`adjust` chooses the model: `"x"` adjusts for `x`, and `"none"` is the
crude model.

``` r
fit_models <- function(data, argset) {
  if (plnr::is_run_directly()) {
    argset <- pbn::get_argset(p, "confounded", 3, "models")
    data <- pbn::get_data(p, "confounded", 3)
  }
  est <- numeric(0)
  w <- numeric(0)
  for (k in seq_len(CFG$data$n_imp)) {
    rhs <- if (argset$adjust == "none") "group" else paste0("group + x_", k)
    fit <- stats::glm(stats::as.formula(paste("outcome ~", rhs)), family = "binomial", data = data$d)
    est[k] <- stats::coef(fit)[["groupb"]]
    w[k] <- stats::vcov(fit)["groupb", "groupb"]
  }
  # Rubin's rules: the variance within the imputations, plus (1 + 1/m) times
  # the variance between them.
  se <- sqrt(mean(w) + (1 + 1 / length(est)) * stats::var(est))
  z <- stats::qnorm(1 - (1 - CFG$level) / 2)
  data.frame(term = "group b", estimate = mean(est), se = se, lower = mean(est) - z * se, upper = mean(est) + z * se)
}
```

The kind `crude` has `control = TRUE`, so it runs for the fake scenarios
only and never appears in the real results. It is a control: in
`confounded` it MUST be wrong. A crude model without bias shows that the
confounder of the scenario is too weak for a check of the adjusted
model.

## The checks and the output function

Each fake scenario needs a function `expect_<scenario>(results)`. It
gets the results of the scenario, with one element for each analysis
kind, and returns the assertions on them. `expect_real()` is optional.
The section The checks explains these three:

``` r
expect_known <- function(results) {
  t1 <- results$table1
  observed <- c(
    t1$events[t1$var == "outcome" & t1$grp == "b"],
    exp(results$crude$estimate)
  )
  expected <- c(6, 6)
  data.frame(
    assertion = c("outcome events in group b", "crude odds ratio of b against a"),
    expected = expected,
    observed = observed,
    pass = abs(observed - expected) < 1e-6
  )
}

expect_null <- function(results) {
  pbn::expect_recovers(results$models, truth = 0, level = CFG$level, label = "adjusted model")
}

expect_confounded <- function(results) {
  rbind(
    pbn::expect_recovers(results$models, truth = 0.5, level = CFG$level, label = "adjusted model"),
    pbn::expect_biased(results$crude, truth = 0.5, label = "crude model")
  )
}
```

The output function `make_output(results, dir)` writes the figures and
the tables of one scenario into the folder `dir`. The section The output
explains it.

``` r
make_output <- function(results, dir) {
  pd <- results$table1[var == "outcome" & grp != "All"]
  q <- ggplot2::ggplot(pd, ggplot2::aes(x = grp, y = pct))
  q <- q + ggplot2::geom_col(width = 0.6, fill = "grey40")
  q <- q + ggplot2::expand_limits(y = 0)
  q <- q + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.01)))
  q <- q + ggplot2::labs(x = "Group", y = "With the outcome (%)")
  q <- q + ggplot2::theme_minimal(base_size = 13)
  q <- q + ggplot2::theme(
    axis.line = ggplot2::element_line(colour = "black", linewidth = 0.6),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.6),
    axis.text = ggplot2::element_text(colour = "black"),
    axis.title = ggplot2::element_text(colour = "black")
  )
  q <- pbn::stamp_fake(q)
  ggplot2::ggsave(file.path(dir, "outcome_by_group.png"), q, width = 5, height = 4, dpi = 96)
}
```

## Running the plan

Here [`source()`](https://rdrr.io/r/base/source.html) with
`chdir = TRUE` runs the script, so the plan stays in this R session.
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) prints
one line for each scenario and simulation, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
prints the assertions. This chunk counts the lines of the data:

``` r
out <- utils::capture.output(source(file.path(home, "v01_run.R"), chdir = TRUE))
#> Sourcing all code inside /tmp/Rtmpd3MLeU/pbn_get_started/project/v01_functions into R_GlobalEnv
data_lines <- grep("^pbn: data", out, value = TRUE)
table(sub("^pbn: data d, [a-z]+ sim_[0-9]{4}: ", "", data_lines))
#> 
#> computed, because there is no cache file. 
#>                                       282
```

plnr ran the data steps once for each of the 282 scenarios and
simulations, and gave the data to every analysis of that simulation.
[`add_analyses()`](https://www.rwhite.no/pbn/reference/add_analyses.md)
adds one analysis for each scenario, simulation and value of the extra
dimensions. pbn adds the analyses `<scenario>__sim_<kkkk>__pbn_n_rows`,
which count the rows of the data for the manifest.

``` r
length(p$analyses)
#> [1] 1409
head(names(p$analyses), 5)
#> [1] "real__sim_0001__pbn_n_rows"  "known__sim_0001__pbn_n_rows"
#> [3] "null__sim_0001__pbn_n_rows"  "null__sim_0002__pbn_n_rows" 
#> [5] "null__sim_0003__pbn_n_rows"
```

`workers = 2` runs the analyses on 2 new R sessions, in a PSOCK cluster.
pbn gives each worker the objects that `v01_functions/` defines and the
attached packages, so the functions find `CFG` and each other. The data
and each analysis set their own seed. So one analysis by hand in this R
session gives the value of the worker:

``` r
by_hand <- p$run_one("confounded__sim_0003__models__1")
#> pbn: data d, confounded sim_0003: reused the cache of 2026-11-17 11:28.
identical(by_hand, r[["confounded__sim_0003__models__1"]])
#> [1] TRUE
```

This holds for functions that depend only on their arguments, the
functions and `CFG` of `v01_functions/`, the attached packages and the
seed. pbn does not copy the R options to the workers.

## The results

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
wrote one results object for each scenario and simulation. git gets one
`results.qs2` for each scenario, and `check.csv`, the table of the
assertions of a fake scenario. For more than 1 simulation, `results.qs2`
holds the simulations stacked. The results of each simulation are in
`sims_gitexclude/`, which git ignores. Here `sim_<kkkk>` stands for the
140 simulations:

``` r
files <- list.files(file.path(home, "v01_results"), recursive = TRUE)
unique(sub("sim_[0-9]{4}", "sim_<kkkk>", files))
#>  [1] "confounded/check.csv"                                 
#>  [2] "confounded/results_gitexclude.qs2"                    
#>  [3] "confounded/results.qs2"                               
#>  [4] "confounded/sims_gitexclude/sim_<kkkk>_gitexclude.qs2" 
#>  [5] "confounded/sims_gitexclude/sim_<kkkk>.qs2"            
#>  [6] "known/check.csv"                                      
#>  [7] "known/output_fake_data/fake_data_outcome_by_group.png"
#>  [8] "known/results_gitexclude.qs2"                         
#>  [9] "known/results.qs2"                                    
#> [10] "null/check.csv"                                       
#> [11] "null/results_gitexclude.qs2"                          
#> [12] "null/results.qs2"                                     
#> [13] "null/sims_gitexclude/sim_<kkkk>_gitexclude.qs2"       
#> [14] "null/sims_gitexclude/sim_<kkkk>.qs2"                  
#> [15] "real/output/outcome_by_group.png"                     
#> [16] "real/results_gitexclude.qs2"                          
#> [17] "real/results.qs2"
```

A results object is a named list. The first element is `manifest`, and
the second is `exclusions`, the exclusion table of the cohort. Then
comes one element for each analysis kind. The data.frames of one kind
are row-bound, with the extra dimension `adjust` as the first column.
[`?save_results`](https://www.rwhite.no/pbn/reference/save_results.md)
gives the rules.

``` r
res <- pbn::read_results("v01", home = home)
names(res)
#> [1] "manifest"   "exclusions" "table1"     "models"     "predicted"
res$exclusions
#>   data            step n_before n_excluded n_after
#> 1    d missing outcome      400         12     388
res$models
#>   adjust    term estimate        se     lower     upper
#> 1      x group b 0.560694 0.2267864 0.1876635 0.9337245
```

[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads the real results by default. With `scenario`, it reads the results
of a fake scenario, and with `sim` one simulation from
`sims_gitexclude/`. A clone of the repository has no such file, so
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
with `sim` then stops and says how to make the file. The stacked results
of `confounded` have a first column `sim`:

``` r
confounded <- pbn::read_results("v01", scenario = "confounded", home = home)
head(confounded$models, 3)
#>   sim adjust    term  estimate        se      lower     upper
#> 1   1      x group b 0.6138863 0.2640282  0.1795986 1.0481741
#> 2   2      x group b 0.1540285 0.2794747 -0.3056665 0.6137234
#> 3   3      x group b 0.6156965 0.2775615  0.1591484 1.0722446
```

The manifest records where the results come from. It holds the versions
of R, pbn and the packages of this R session and of the workers, the
date, the scenario and the simulation. `source_md5` holds the md5 of
`v01_run.R` and of each file in `v01_functions/`. For the real data,
`data_md5` holds the md5 of each data file. For a fake scenario,
`fake_md5` holds the md5 of the code of the data steps. `n_rows_data`
holds the number of rows of the data after the exclusions.

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
#>  $ analyses                :'data.frame':    3 obs. of  3 variables:
#>   ..$ analysis: chr [1:3] "table1" "models" "predicted"
#>   ..$ control : logi [1:3] FALSE FALSE FALSE
#>   ..$ file    : chr [1:3] "results.qs2" "results.qs2" "results_gitexclude.qs2"
```

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
checks each results object with
[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
before it writes anything. It stops on a fitted model or a function in
the results. It also stops on a table with one row for each row of the
data, before or after the exclusions. That check compares sizes only, so
check the small groups by hand.

### Results with data for each person

A results file goes to git, so it MUST NOT hold one row for each person.
Some results do: fitted models, the imputations, or the predicted risk
of `predict_risk()`. `save_results(gitexclude = "predicted")` writes
that kind to `results_gitexclude.qs2`, beside `results.qs2`, and to
`sim_<kkkk>_gitexclude.qs2` for each simulation. The line `*gitexclude*`
of `.gitignore` keeps that file out of git.
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
reads both files and returns one list:

``` r
list.files(file.path(home, "v01_results", "real"))
#> [1] "output"                 "results_gitexclude.qs2" "results.qs2"
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
#> /tmp/Rtmpd3MLeU/pbn_get_started/project/v01_results/real/results_gitexclude.qs2,
#> and that file is missing, for example in a clone of the repository.
#> read_results() returns the other elements.
#> [1] "manifest"   "exclusions" "table1"     "models"
file.rename(file.path(root, "kept.qs2"), private)
#> [1] TRUE
```

[`check_results()`](https://www.rwhite.no/pbn/reference/check_results.md)
and
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
do not check what the kinds in `gitexclude` hold, so `predicted` passes
the check of the rows of the data. The
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
rule `gitexclude_not_tracked` fails when git tracks a path with
`gitexclude` in its name.

## The checks

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
calls `expect_<scenario>()` on the results of each fake scenario, and
writes the assertions to `check.csv`. It stops when one did not pass.

`expect_known()` tests two outputs against numbers that a person
computed by hand. It does not test how the analysis behaves on data as
large or as untidy as the real data.

`expect_null()` and `expect_confounded()` are recovery checks.
[`pbn::expect_recovers()`](https://www.rwhite.no/pbn/reference/expect_recovers.md)
checks the bias, the standard error and the coverage of the estimate
over the simulations. Each limit comes from the Monte Carlo standard
error of the measure.
[`pbn::expect_biased()`](https://www.rwhite.no/pbn/reference/expect_biased.md)
checks that the control is biased. These are the assertions of
`confounded`:

``` r
knitr::kable(utils::read.csv(file.path(home, "v01_results", "confounded", "check.csv")))
```

| assertion | expected | observed | pass |
|:---|:---|:---|:---|
| adjusted model: bias of estimate against 0.5 | \|bias\| \<= 0.0932 (3.89 MCSE); can detect \|bias\| of 0.113 | 0.031; 140 of 140 simulations | TRUE |
| adjusted model: model SE of estimate against its SD | \|relative error\| \<= 23.6% (3.89 MCSE); can detect ModSE/EmpSE of 0.772 or 1.37 | +1.1% (ModSE 0.286, EmpSE 0.283); 140 of 140 simulations | TRUE |
| adjusted model: coverage of 0.5 by the 90% interval | 0.801 to 0.999 (3.89 MCSE); can detect coverage of 0.771 | 0.914; 140 of 140 simulations | TRUE |
| crude model: bias of estimate against 0.5, a control | \|bias\| \> 0.0715 (3.89 MCSE); can detect \|bias\| of 0.087 | 1.05; 140 of 140 simulations | TRUE |

“can detect” is the smallest error that the check finds with a
probability of 80% at this number of simulations. A row passes when
`observed` is inside the limit in `expected`, and a large “can detect”
means that the row passes almost anything.

Give the checks the estimate on the scale where it is near normal: a log
odds ratio, with a truth on the log scale. Below 100 simulations, or 137
for the coverage at the level 0.9, each assertion fails with “too few
simulations to check”. `expect_recovers(coverage = FALSE)` makes no
coverage assertion, so 100 simulations are enough, and `x` needs no
`lower` and `upper`.
[`?expect_recovers`](https://www.rwhite.no/pbn/reference/expect_recovers.md)
gives the formulas, the rule for missing estimates and the probability
of a false alarm.

The crude model does not recover the truth of `confounded`. These are
the assertions that a wrong analysis gets:

``` r
pbn::expect_recovers(confounded$crude, truth = 0.5, level = CFG$level, label = "crude model")[, c("assertion", "observed", "pass")]
#>                                          assertion
#> 1        crude model: bias of estimate against 0.5
#> 2 crude model: model SE of estimate against its SD
#> 3 crude model: coverage of 0.5 by the 90% interval
#>                                                   observed  pass
#> 1                             1.05; 140 of 140 simulations FALSE
#> 2 +6.0% (ModSE 0.231, EmpSE 0.218); 140 of 140 simulations  TRUE
#> 3                            0.000; 140 of 140 simulations FALSE
```

### A check that goes red

The runs below need the scenarios `real` and `known` only. Change the
second line of `v01_run.R`, so that each run takes a few seconds:

``` r
run_file <- file.path(home, "v01_run.R")
script <- readLines(run_file)
script <- sub("pbn::scenarios(CFG)", 'pbn::scenarios(CFG, only = c("real", "known"))', script, fixed = TRUE)
writeLines(script, run_file)
script[2]
#> [1] "scenarios <- pbn::scenarios(CFG, only = c(\"real\", \"known\"))"
```

`run_lines()` runs the script in a new R session, and shows the lines of
the data:

``` r
run_lines <- function(status = 0) {
  out <- callr::rscript(run_file, wd = home, show = FALSE, fail_on_status = FALSE)
  stopifnot(out$status == status)
  lines <- grep("^pbn: data", strsplit(out$stdout, "\n")[[1]], value = TRUE)
  cat(lines, sep = "\n")
  invisible(lines)
}
```

Make one small error in the analysis: swap the two branches of the `if`
in `fit_models()`. The crude model is then adjusted for `x`, and the
adjusted model is crude. Keep a copy of the file first.

``` r
file <- file.path(home, "v01_functions", "fit_models.R")
file.copy(file, file.path(root, "fit_models_copy.R"))
#> [1] TRUE
code <- readLines(file)
code <- sub('"group" else paste0("group + x_", k)', 'paste0("group + x_", k) else "group"', code, fixed = TRUE)
writeLines(code, file)
grep("rhs <-", readLines(file), value = TRUE)
#> [1] "    rhs <- if (argset$adjust == \"none\") paste0(\"group + x_\", k) else \"group\""
red <- run_lines(status = 1)
#> pbn: data d, real sim_0001: reused the cache of 2026-11-17 11:27.
#> pbn: data d, known sim_0001: reused the cache of 2026-11-17 11:27.
cat(readLines(file.path(home, "v01_results", "known", "check.csv")), sep = "\n")
#> "assertion","expected","observed","pass"
#> "outcome events in group b",6,6,TRUE
#> "crude odds ratio of b against a",6,7.0369269141867,FALSE
```

The real data runs through the same functions as the fake data. A fake
scenario with a known answer thus tests the analysis that gives the real
results. On the real data, the swapped models give plausible odds ratios
and no error. Only the planted odds ratio shows the mistake. The results
files stay, so that you can find the fault, but
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
stops on them:

``` r
pbn::read_results("v01", scenario = "known", home = home)
#> Error:
#> ! /tmp/Rtmpd3MLeU/pbn_get_started/project/v01_results/known/check.csv holds an assertion that did not pass, so read_results() does not read /tmp/Rtmpd3MLeU/pbn_get_started/project/v01_results/known/results.qs2. Fix the analysis and run it again. To look at the results for debugging, read them with qs2::qs_read("/tmp/Rtmpd3MLeU/pbn_get_started/project/v01_results/known/results.qs2").
```

Put the copy back, and run the script again. Its assertions pass.

``` r
file.copy(file.path(root, "fit_models_copy.R"), file, overwrite = TRUE)
#> [1] TRUE
green <- run_lines()
#> pbn: data d, real sim_0001: reused the cache of 2026-11-17 11:27.
#> pbn: data d, known sim_0001: reused the cache of 2026-11-17 11:27.
```

### A cleaning step that drops a row

Make `data_clean()` drop the rows with a missing outcome. That is an
exclusion, and it belongs in `data_cohort()`, where it is already. Keep
a copy of the file first, and show the first failed analysis:

``` r
clean_file <- file.path(home, "v01_functions", "data_clean.R")
file.copy(clean_file, file.path(root, "data_clean_copy.R"))
#> [1] TRUE
code <- sub("^  d$", "  d[!is.na(outcome)]", readLines(clean_file))
writeLines(code, clean_file)
out <- callr::rscript(run_file, wd = home, show = FALSE, fail_on_status = FALSE)
lines <- strsplit(out$stdout, "\n")[[1]]
cat(lines[grep(" failed:$", lines)[1] + 0:1], sep = "\n")
#> pbn: the analysis real__sim_0001__pbn_n_rows failed:
#> The data function 'd' failed: The step clean of the data "d" returned 388 rows, but the step raw returned 400 rows. The step clean MUST return a data.frame with the same number of rows. Put each exclusion in the step cohort, with $exclude_and_track(), so that pbn counts it.
```

Each analysis of the run fails in the same way, and
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
does not run. Put the copy back:

``` r
file.copy(file.path(root, "data_clean_copy.R"), clean_file, overwrite = TRUE)
#> [1] TRUE
```

## Imputation is data

A multiple imputation can take hours, and every analysis reads its
result. So it belongs in the data, as the step `after_cohort`. With
`cache = TRUE` in
[`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md),
[`run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md) saves
the data on disk for each scenario and simulation, in
`v01_cache_gitexclude/`. The next run reads it again while the cache key
is the same. A change of an analysis then runs the analyses only.

`data_impute()` is the step after the cohort, so the imputation model
uses the people of the cohort only. It draws `CFG$data$n_imp` values for
each missing `x`, from a linear model of `x` on `group` and the outcome,
with a normal error. It writes the draws to the columns `x_1`, `x_2` and
`x_3`, and keeps one row for each person. This is stochastic regression
imputation. It does not draw the parameters of the model, so it is not a
proper multiple imputation.

The two runs of the section A check that goes red read the cache. Change
an analysis: `fit_models()` also reports the number of imputations. The
data comes from the cache again:

``` r
code <- readLines(file)
code <- sub("upper = mean(est) + z * se)", "upper = mean(est) + z * se, n_imp = length(est))", code, fixed = TRUE)
writeLines(code, file)
third <- run_lines()
#> pbn: data d, real sim_0001: reused the cache of 2026-11-17 11:27.
#> pbn: data d, known sim_0001: reused the cache of 2026-11-17 11:27.
pbn::read_results("v01", home = home)$models
#>   adjust    term estimate        se     lower     upper n_imp
#> 1      x group b 0.560694 0.2267864 0.1876635 0.9337245     3
```

Change the data code: `data_impute()` keeps one decimal of each draw.
pbn computes the data again, and names the changed file and the changed
step:

``` r
code <- readLines(file.path(home, "v01_functions", "data_impute.R"))
code <- sub("round(mu + ", "round(digits = 1, mu + ", code, fixed = TRUE)
writeLines(code, file.path(home, "v01_functions", "data_impute.R"))
fourth <- run_lines()
#> pbn: data d, real sim_0001: computed, because v01_functions/data_impute.R, the function given as `after_cohort` changed.
#> pbn: data d, known sim_0001: computed, because v01_functions/data_impute.R, the function given as `after_cohort` changed.
```

The cache key holds the md5 of each `data_*.R` file of `v01_functions/`,
and `CFG$data`. It also holds the code of each step, the raw data, the
settings, the versions of R and pbn, and
[`RNGkind()`](https://rdrr.io/r/base/Random.html). Beside the key, pbn
records the version of each package loaded when the data was computed. A
new version of one of them computes the data again.
[`?add_data`](https://www.rwhite.no/pbn/reference/add_data.md) gives the
full rule. Keep the data code in `data_*.R` files: a change in another
file does not change the key. To compute every value again, delete
`v01_cache_gitexclude/`.

## Stepping through one analysis by hand

The names of the analyses are `<scenario>__sim_<kkkk>__<name>__<i>`.
`<i>` counts the values of the extra dimensions.
`pbn::get_argset(p, "confounded", 3, "models", 1)` gives the argset of
`confounded__sim_0003__models__1`, and
`pbn::get_data(p, "confounded", 3)` gives its data. The last argument of
[`get_argset()`](https://www.rwhite.no/pbn/reference/get_data.md),
`<i>`, is 1 by default. Put back the second line of `v01_run.R`, so that
the plan has every scenario:

``` r
script <- sub('pbn::scenarios(CFG, only = c("real", "known"))', "pbn::scenarios(CFG)", readLines(run_file), fixed = TRUE)
writeLines(script, run_file)
```

To step through one analysis of `fit_models()` by hand, run the script
up to
[`pbn::run_plan()`](https://www.rwhite.no/pbn/reference/run_plan.md).
Then run the body of the function line by line. At the top level of the
console,
[`plnr::is_run_directly()`](https://www.rwhite.no/plnr/reference/is_run_directly.html)
is `TRUE`, so the first lines load that analysis:

``` r
upto <- script[seq_len(grep("run_plan", script) - 1)]
withr::with_dir(home, eval(parse(text = upto)))
#> Sourcing all code inside /tmp/Rtmpd3MLeU/pbn_get_started/project/v01_functions into R_GlobalEnv
argset <- pbn::get_argset(p, "confounded", 3, "models")
data <- pbn::get_data(p, "confounded", 3)
#> pbn: data d, confounded sim_0003: computed, because v01_functions/data_impute.R, the function given as `after_cohort` changed.
argset$adjust
#> [1] "x"
nrow(data$d)
#> [1] 400
```

[`pbn::get_data()`](https://www.rwhite.no/pbn/reference/get_data.md)
uses the cache, as `p$get_data()` does. The data code changed after the
first run, so it computed the data again, and it gave the reason. When
the plan runs `fit_models()`,
[`plnr::is_run_directly()`](https://www.rwhite.no/plnr/reference/is_run_directly.html)
is `FALSE`, and the first lines do nothing. The plan calls
`set.seed(argset$seed)` before each analysis. Run that line too before
you step through a function that draws random numbers.
[`?add_analyses`](https://www.rwhite.no/pbn/reference/add_analyses.md)
says how pbn derives the seed from the kind, its counter and the
simulation.

## The output

`pbn::make_outputs(make_output, scenarios)` calls the output function
for `real` and for each fake scenario with `output = TRUE`. The folder
of `real` is `v01_results/real/output/`. The folder of a fake scenario
is `v01_results/<scenario>/output_fake_data/`, and pbn renames each file
there so that its name starts with `fake_data_`. Before each call, it
empties the folder, so no file of an earlier run stays.

``` r
files <- list.files(file.path(home, "v01_results"), recursive = TRUE)
grep("output", files, value = TRUE)
#> [1] "known/output_fake_data/fake_data_outcome_by_group.png"
#> [2] "real/output/outcome_by_group.png"
```

`q <- pbn::stamp_fake(q)` in `make_output()` puts the label “FAKE DATA”
on the figure of a fake scenario. It returns the figure as it is for
`real`. This is the figure of `known`:

![](pbn_files/figure-html/fake_data_outcome_by_group.png)

A table or a text file of fake data has the name prefix and the folder
name only, and no label. The
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
rule `fake_data_names` fails a file in `output_fake_data/` without the
prefix, and a file with the prefix in another folder.

A script with output code after the run that is not in a function MAY
use `pbn::end_unless_real(scenarios)` after
[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md).
It ends the script when the run had no real data.

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
#> PASS  results_contract  every vNN_results/<scenario>/results.qs2 and each vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2 on the disk passes check_results()
#> PASS  gitexclude_not_tracked  the project is not a git repository, so nothing to check
#> PASS  scenario_checks  every vNN_results/<scenario>/check.csv passes
#> PASS  manual_manuscript_names  every file in manuscript/manual/ is named vNN_<INITIALS>.docx
#> PASS  manuscript_reads_real  manuscript/built/ reads only the real results
#> PASS  fake_data_names  every file in an output_fake_data/ folder, and no file elsewhere, has a name that starts with fake_data_
#> PASS  no_local_copy_of_pbn_function  no vNN_functions/ file defines a pbn export
#> PASS  loc_per_file  no .R file has more than 1000 code lines
#> PASS  no_environment_variables  no Sys.getenv or Sys.setenv in a run script or a functions folder
#> PASS  unknown_path  the house layout names every top-level path
#> 13 PASS, 0 WARN, 0 FAIL
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
#> 12 PASS, 0 WARN, 1 FAIL
unlink(file.path(home, "adhoc"), recursive = TRUE)
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
- `workers` limits the R sessions of pbn only. An analysis that starts
  its own processes in a worker multiplies them, for example with
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html).
  Two workers that each start 4 processes run 8 processes at the same
  time.
- `fake_md5` is the md5 of the code of the steps of
  [`add_data()`](https://www.rwhite.no/pbn/reference/add_data.md) only.
  A change in a function that a step calls does not change `fake_md5`.
  `source_md5` holds the md5 of its file.
- The checks between the steps compare the number of rows, not which
  rows. A `data_clean()`, or a `data_cohort()` before `$new()`, that
  drops one row and repeats another passes. pbn does not check the rows
  of the step `after_cohort`.
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
- The SE of the relative error of the model SE assumes a normal
  estimate, and an `se` independent of the estimate.
- [`make_outputs()`](https://www.rwhite.no/pbn/reference/make_outputs.md)
  cannot see a file that the output function writes outside `dir`.
