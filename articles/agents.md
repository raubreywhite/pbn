# pbn with AI agents

An AI agent can write most of the code of an analysis project. It is
harder to know whether that code is correct. Lauren Tan’s talk [All Code
Is Slop (Except Mine)](https://www.youtube.com/watch?v=ZZ19zhYnDKY) at
[AI Engineer Code 2026](https://ai.engineer/code/2026/talks/lauren-tan)
gives four rules for that trust:

1.  A choice that needs judgement to make well leads to bad code. Remove
    the choice, or forbid the option that goes wrong.
2.  Make each correction of an agent a deterministic check, not another
    line of instructions.
3.  Spend the effort on verification. Review what the code produced, not
    only the code.
4.  Write prose for the agent last, for the judgement that no check can
    make.

pbn applies these rules to a statistical analysis project, in four
layers. This article shows each layer, and three mistakes of an agent
that pbn stops.

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgNjQwIDMwMCIgcm9sZT0iaW1nIiBhcmlhLWxhYmVsbGVkYnk9InB5ci10aXRsZSIgc3R5bGU9IndpZHRoOiAxMDAlOyBtYXgtd2lkdGg6IDY0MHB4OyBoZWlnaHQ6IGF1dG87IGRpc3BsYXk6IGJsb2NrOyBtYXJnaW46IDAgYXV0bzsgZm9udC1mYW1pbHk6IGluaGVyaXQ7Ij48dGl0bGUgaWQ9InB5ci10aXRsZSI+CkZvdXIgbGF5ZXJzLiBUaGUgd2lkZXN0LCBhdCB0aGUgdG9wLCBpcyB0aGUgbGF5b3V0IGFuZCB0aGUgQVBJIG9mIHBibi4KQmVsb3cgaXQgYXJlIHRoZSBjaGVja3MsIHRoZW4gdmVyaWZpY2F0aW9uIG9uIGZha2UgZGF0YS4gVGhlIG5hcnJvd2VzdCwKYXQgdGhlIGJvdHRvbSwgaXMgcHJvc2UgZm9yIHRoZSBhZ2VudC4KPC90aXRsZT4KPGcgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMSIgZmlsbC1vcGFjaXR5PSIwLjE4Ij48cG9seWdvbiBwb2ludHM9IjEwLDEwIDYzMCwxMCA1OTAsNzUgNTAsNzUiIGZpbGw9IiMyMTY2YWMiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjUwLDgwIDU5MCw4MCA1NTAsMTQ1IDkwLDE0NSIgZmlsbD0iIzIxNjZhYyIgZmlsbC1vcGFjaXR5PSIwLjEzIj48L3BvbHlnb24+PHBvbHlnb24gcG9pbnRzPSI5MCwxNTAgNTUwLDE1MCA1MTAsMjE1IDEzMCwyMTUiIGZpbGw9IiM0ZDkyMjEiPjwvcG9seWdvbj48cG9seWdvbiBwb2ludHM9IjEzMCwyMjAgNTEwLDIyMCA0NzAsMjg1IDE3MCwyODUiIGZpbGw9IiM5OTk5OTkiPjwvcG9seWdvbj48L2c+PGcgZmlsbD0iY3VycmVudENvbG9yIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjE1Ij48dGV4dCB4PSIzMjAiIHk9IjM4IiBmb250LXdlaWdodD0iYm9sZCI+MS7CoExheW91dMKgYW5kwqBBUEk6wqBvbmXCoHdhecKgdG/CoGRvwqBlYWNowqB0aGluZzwvdGV4dD48dGV4dCB4PSIzMjAiIHk9IjYwIiBmb250LXNpemU9IjEzIj5vbmXCoHJ1bsKgc2NyaXB0LMKgZm91csKgZGF0YcKgc3RlcHMswqBwYm46OmZvbGRlcigpLMKgbm/CoGFzc2lnbigpPC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMTA4IiBmb250LXdlaWdodD0iYm9sZCI+Mi7CoENoZWNrczrCoGHCoGNvcnJlY3Rpb27CoHRoYXTCoGNvZGXCoGNhbsKgY2hlY2vCoGJlY29tZXPCoGHCoGNoZWNrPC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMTMwIiBmb250LXNpemU9IjEzIj5jaGVja19wcm9qZWN0KCkswqB0aGXCoGNoZWNrc8KgYmV0d2VlbsKgdGhlwqBkYXRhwqBzdGVwczwvdGV4dD48dGV4dCB4PSIzMjAiIHk9IjE3OCIgZm9udC13ZWlnaHQ9ImJvbGQiPjMuwqBWZXJpZmljYXRpb27CoG9uwqBmYWtlwqBkYXRhPC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMjAwIiBmb250LXNpemU9IjEzIj5zY2VuYXJpb3PCoHdpdGjCoGtub3duwqBhbnN3ZXJzLMKgY2hlY2suY3N2LMKgcnVuLm1kPC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMjQ4IiBmb250LXdlaWdodD0iYm9sZCI+NC7CoFByb3NlPC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMjcwIiBmb250LXNpemU9IjEzIj5BR0VOVFMubWQswqBwYm4ubWQ8L3RleHQ+PC9nPjwvc3ZnPg==)

The layers of pbn, after Tan. pbn puts most of its code into layers 1
and 2. Code enforces them. A project can turn a rule down to warn or off
only in pbn.yml, where a person sees it.

## The project

The project of this article is a smaller version of the one in [Get
started](https://www.rwhite.no/pbn/articles/pbn.md). It compares a 0/1
outcome between two groups, `a` and `b`, adjusted for a covariate `x`.
It has three scenarios:

- `real` reads the data file. Here it is a simulated csv file of 400
  rows, 12 of them with a missing outcome. In it, `x` is not related to
  the group.
- `known` has 20 rows, with a crude odds ratio of 6 that a person
  computed by hand.
- `confounded` has 140 simulations where `x` raises the chance of group
  `b` and of the outcome. The true log odds ratio, adjusted for `x`, is
  0.5.

The source of this article writes every file of the project. These are
two of the files that the agent changes below:

``` r
data_clean <- function(d) {
  data.table::setnames(d, tolower(names(d)))
  d
}
```

``` r
fit_models <- function(data, argset) {
  rhs <- if (argset$adjust == "x") "group + x" else "group"
  fit <- stats::glm(stats::as.formula(paste("outcome ~", rhs)), family = "binomial", data = data$d)
  z <- stats::qnorm(1 - (1 - CFG$level) / 2)
  est <- stats::coef(fit)[["groupb"]]
  se <- sqrt(stats::vcov(fit)["groupb", "groupb"])
  data.frame(term = "group b", estimate = est, se = se, lower = est - z * se, upper = est + z * se)
}
```

`run()` runs `v01_run.R` as `Rscript v01_run.R` does. It returns the
exit status and the lines of output:

``` r
run <- function() {
  out <- callr::rscript(file.path(home, "v01_run.R"), wd = home, show = FALSE, fail_on_status = FALSE)
  list(status = out$status, lines = strsplit(paste(out$stdout, out$stderr), "\r?\n")[[1]])
}
green <- run()
green$status
#> [1] 0
correct <- pbn::read_results("v01", home = home)$models
```

## Layer 1: one way to do each thing

An agent copies the patterns it finds. Where a project allows two ways
to do a thing, the agent picks either, and the next agent copies that
choice. pbn removes the choice:

| An agent would otherwise choose | What pbn sets |
|----|----|
| where a file goes | one layout, and [`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md) warns about a path that the layout does not name |
| where an exclusion happens | the step `cohort` only, through `$exclude_and_track()`, so that the exclusion table counts it |
| how code reaches the data | [`pbn::folder()`](https://www.rwhite.no/pbn/reference/folder.md) only. No absolute path and no environment variable. |
| where an analysis keeps state | nowhere. No [`assign()`](https://rdrr.io/r/base/assign.html) or `<<-`, and an analysis that changes `CFG` fails. |
| how fake output looks | its own folder `output_fake_data/`, the name prefix `fake_data_`, and the label “FAKE DATA” that [`pbn::stamp_fake()`](https://www.rwhite.no/pbn/reference/stamp_fake.md) puts on a figure |
| what a run reports | one run log, `run.md`, in one format |

pbn makes each of these decisions once, so a review does not need to
look for them.

A check between the data steps enforces the place of an exclusion. Its
error message names the fix, so the agent does not work around the
check. Here the agent drops the rows with a missing outcome in
`data_clean()`. That is the wrong step, where the exclusion table does
not count them:

``` r
clean_file <- file.path(home, "v01_functions", "data_clean.R")
clean_code <- readLines(clean_file)
writeLines(sub("^  d$", "  d[!is.na(outcome)]", clean_code), clean_file)
dropped <- run()
cat(dropped$lines[grep(" failed:$", dropped$lines)[1] + 0:1], sep = "\n")
#> pbn: the analysis real__sim_0001__pbn_n_rows failed:
#> The data function 'd' failed: The step clean of the data "d" returned 388 rows, but the step raw returned 400 rows. The step clean MUST return a data.frame with the same number of rows. Put each exclusion in the step cohort, with $exclude_and_track(), so that pbn counts it.
writeLines(clean_code, clean_file)
```

## Layer 2: each correction becomes a check

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgNjQwIDI5MCIgcm9sZT0iaW1nIiBhcmlhLWxhYmVsbGVkYnk9Imxvb3AtdGl0bGUiIHN0eWxlPSJ3aWR0aDogMTAwJTsgbWF4LXdpZHRoOiA2NDBweDsgaGVpZ2h0OiBhdXRvOyBkaXNwbGF5OiBibG9jazsgbWFyZ2luOiAwIGF1dG87IGZvbnQtZmFtaWx5OiBpbmhlcml0OyI+PHRpdGxlIGlkPSJsb29wLXRpdGxlIj4KQSBwZXJzb24gY29ycmVjdHMgYSBtaXN0YWtlIG9mIGFuIGFnZW50LiBJZiBjb2RlIGNhbiBjaGVjayBpdCwgdGhlCmNvcnJlY3Rpb24gYmVjb21lcyBhIGNoZWNrLCBhbmQgdGhlIG5leHQgYWdlbnQgZ2V0cyBhbiBlcnJvciB0aGF0IG5hbWVzCnRoZSBmaXguIElmIG5vdCwgaXQgYmVjb21lcyBvbmUgbGluZSBpbiBBR0VOVFMubWQuCjwvdGl0bGU+CjxkZWZzPjxtYXJrZXIgaWQ9ImFyciIgdmlld2JveD0iMCAwIDEwIDEwIiByZWZ4PSI5IiByZWZ5PSI1IiBtYXJrZXJ3aWR0aD0iNyIgbWFya2VyaGVpZ2h0PSI3IiBvcmllbnQ9ImF1dG8tc3RhcnQtcmV2ZXJzZSI+PHBhdGggZD0iTTAsMCBMMTAsNSBMMCwxMCB6IiBmaWxsPSJjdXJyZW50Q29sb3IiIC8+PC9tYXJrZXI+PC9kZWZzPjxnIHN0cm9rZT0iY3VycmVudENvbG9yIiBzdHJva2Utd2lkdGg9IjEiIGZpbGwtb3BhY2l0eT0iMC4xNSI+PHJlY3QgeD0iMTAiIHk9IjE1IiB3aWR0aD0iMTcwIiBoZWlnaHQ9IjU2IiByeD0iNiIgZmlsbD0iIzk5OTk5OSIgLz48cmVjdCB4PSIyMzUiIHk9IjE1IiB3aWR0aD0iMTcwIiBoZWlnaHQ9IjU2IiByeD0iNiIgZmlsbD0iIzk5OTk5OSIgLz48cmVjdCB4PSIyMzUiIHk9IjExNSIgd2lkdGg9IjE3MCIgaGVpZ2h0PSI2NCIgcng9IjYiIGZpbGw9IiMyMTY2YWMiIC8+PHJlY3QgeD0iNDYwIiB5PSIxMTUiIHdpZHRoPSIxNzAiIGhlaWdodD0iNjQiIHJ4PSI2IiBmaWxsPSIjNGQ5MjIxIiAvPjxyZWN0IHg9IjEwIiB5PSIxMTUiIHdpZHRoPSIxNzAiIGhlaWdodD0iNjQiIHJ4PSI2IiBmaWxsPSIjOTk5OTk5IiBmaWxsLW9wYWNpdHk9IjAuMDYiIHN0cm9rZS1kYXNoYXJyYXk9IjQgMyIgLz48cmVjdCB4PSI0NjAiIHk9IjIyMCIgd2lkdGg9IjE3MCIgaGVpZ2h0PSI2NCIgcng9IjYiIGZpbGw9IiM0ZDkyMjEiIGZpbGwtb3BhY2l0eT0iMC4wOCIgLz48L2c+PGcgc3Ryb2tlPSJjdXJyZW50Q29sb3IiIHN0cm9rZS13aWR0aD0iMS41IiBmaWxsPSJub25lIiBtYXJrZXItZW5kPSJ1cmwoI2FycikiPjxsaW5lIHgxPSIxODAiIHkxPSI0MyIgeDI9IjIzMyIgeTI9IjQzIj48L2xpbmU+PGxpbmUgeDE9IjMyMCIgeTE9IjcxIiB4Mj0iMzIwIiB5Mj0iMTEzIj48L2xpbmU+PGxpbmUgeDE9IjQwNSIgeTE9IjE0NyIgeDI9IjQ1OCIgeTI9IjE0NyI+PC9saW5lPjxsaW5lIHgxPSIyMzUiIHkxPSIxNDciIHgyPSIxODIiIHkyPSIxNDciIHN0cm9rZS1kYXNoYXJyYXk9IjQgMyI+PC9saW5lPjxsaW5lIHgxPSI1NDUiIHkxPSIxNzkiIHgyPSI1NDUiIHkyPSIyMTgiPjwvbGluZT48L2c+PGcgZmlsbD0iY3VycmVudENvbG9yIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEzIj48dGV4dCB4PSI5NSIgeT0iMzkiPlRoZcKgYWdlbnTCoG1ha2VzPC90ZXh0Pjx0ZXh0IHg9Ijk1IiB5PSI1NyI+YcKgbWlzdGFrZTwvdGV4dD48dGV4dCB4PSIzMjAiIHk9IjM5Ij5BwqBwZXJzb248L3RleHQ+PHRleHQgeD0iMzIwIiB5PSI1NyI+Y29ycmVjdHPCoGl0PC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMTQzIiBmb250LXdlaWdodD0iYm9sZCI+Q2FuwqBjb2RlPC90ZXh0Pjx0ZXh0IHg9IjMyMCIgeT0iMTYxIiBmb250LXdlaWdodD0iYm9sZCI+Y2hlY2vCoGl0PzwvdGV4dD48dGV4dCB4PSI1NDUiIHk9IjEzNiI+QcKgcnVsZcKgaW7CoHBibiw8L3RleHQ+PHRleHQgeD0iNTQ1IiB5PSIxNTMiPmHCoHN0ZXDCoGNoZWNrwqBvcjwvdGV4dD48dGV4dCB4PSI1NDUiIHk9IjE3MCI+YW7CoGV4cGVjdF/CoGZ1bmN0aW9uPC90ZXh0Pjx0ZXh0IHg9Ijk1IiB5PSIxNDMiPk9uZcKgbGluZcKgaW48L3RleHQ+PHRleHQgeD0iOTUiIHk9IjE2MSI+QUdFTlRTLm1kPC90ZXh0Pjx0ZXh0IHg9IjU0NSIgeT0iMjQxIj5UaGXCoG5leHTCoGFnZW50PC90ZXh0Pjx0ZXh0IHg9IjU0NSIgeT0iMjU4Ij5nZXRzwqBhbsKgZXJyb3LCoHRoYXQ8L3RleHQ+PHRleHQgeD0iNTQ1IiB5PSIyNzUiPm5hbWVzwqB0aGXCoGZpeDwvdGV4dD48dGV4dCB4PSI0MzEiIHk9IjEzOSIgZm9udC1zaXplPSIxMiI+eWVzPC90ZXh0Pjx0ZXh0IHg9IjIwOCIgeT0iMTM5IiBmb250LXNpemU9IjEyIj5ubzwvdGV4dD48L2c+PC9zdmc+)

Each correction goes into code where it can. A check holds for every
later agent. An agent can ignore a line of instructions.

pbn contains the checks for the corrections that apply to every project.
[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
checks the project folder against the rules that `pbn.yml` names, and
`check_project.R` runs it in one line. Here an agent makes two changes.
It writes the path of the data as an absolute path. It also adds its own
version of
[`pbn::folder()`](https://www.rwhite.no/pbn/reference/folder.md) that
reads an environment variable:

``` r
raw_file <- file.path(home, "v01_functions", "data_raw.R")
raw_code <- readLines(raw_file)
writeLines(sub('pbn::folder("main", "d.csv")', '"/home/ana/project/data/d.csv"', raw_code, fixed = TRUE), raw_file)
writeLines(c(
  "folder <- function(name, file) {",
  '  file.path(Sys.getenv("DATA_DIR"), file)',
  "}"
), file.path(home, "v01_functions", "data_folder.R"))
checked <- callr::rscript(file.path(home, "check_project.R"), wd = home, show = FALSE, fail_on_status = FALSE)
checked$status
#> [1] 1
cat(grep("^FAIL|PASS, ", strsplit(checked$stdout, "\r?\n")[[1]], value = TRUE), sep = "\n")
#> FAIL  no_local_copy_of_pbn_function  v01_functions/data_folder.R: defines folder, a pbn export. Fix: rename the function, or use the pbn export and correct any fault in pbn.
#> FAIL  no_environment_variables  v01_functions/data_folder.R: Sys.getenv on line 2. Fix: put the value in CFG, and the folder of data in the argument folders of pbn::initialize_project().
#> FAIL  no_absolute_paths  v01_functions/data_raw.R:3: "/home/ana/project/data/d.csv". Fix: start a path to data from pbn::folder(), and a path for output from the argument dir of the output function of pbn::make_outputs().
#> 18 PASS, 0 WARN, 3 FAIL, 0 OFF
writeLines(raw_code, raw_file)
unlink(file.path(home, "v01_functions", "data_folder.R"))
```

Each finding names the rule, the file, the line where one applies, and
the fix. Tan also notes that an agent copies a workaround as if it were
the pattern of the code. The rule `no_local_copy_of_pbn_function` stops
one such workaround: a local copy that patches a pbn function. The fix
belongs in pbn, where every project gets it.

A project MAY set a rule to warn or off in `pbn.yml`. The finding then
shows as WARN or OFF. A person who reads `pbn.yml` thus sees every
exception of the project.
[`?check_project`](https://www.rwhite.no/pbn/reference/check_project.md)
lists the locked rules, which `pbn.yml` cannot change.

Some requirements apply to one project only, for example a variable that
MUST NOT be in a model. Put such a requirement in a check in a data
step, or in an assertion of an `expect_` function.

## Layer 3: verification on fake data

A review of the code that an agent writes finds some mistakes. A run on
fake data with a known answer can find a mistake that looks correct in
the code. In pbn, the real data is one scenario among fake ones, and
every scenario runs through the same functions. So a fake scenario tests
the code that gives the real results.

Here the agent simplifies `fit_models()`. It removes the adjustment for
`x`, because it judges that `x` “adds little”. The model code has no
error:

``` r
fit_file <- file.path(home, "v01_functions", "fit_models.R")
fit_code <- readLines(fit_file)
writeLines(sub('if (argset$adjust == "x") "group + x" else "group"', '"group"', fit_code, fixed = TRUE), fit_file)
grep("rhs <-", readLines(fit_file), value = TRUE)
#> [1] "  rhs <- \"group\""
simplified <- run()
simplified$status
#> [1] 1
```

Yet the run fails, with status 1. On the real data, the estimate changes
in the third decimal only, because there `x` is not related to the
group. To look at the real results of a failed run for debugging, read
them with [`qs2::qs_read()`](https://rdrr.io/pkg/qs2/man/qs_read.html):

``` r
simplified_real <- qs2::qs_read(file.path(home, "v01_results", "real", "results.qs2"))$models
rbind(correct = correct, simplified = simplified_real)[, c("estimate", "lower", "upper")]
#>             estimate     lower     upper
#> correct    0.5568597 0.1839498 0.9297696
#> simplified 0.5583728 0.1857086 0.9310369
```

The scenario `confounded` has a known true log odds ratio, 0.5. Two of
its four assertions fail:

``` r
check <- utils::read.csv(file.path(home, "v01_results", "confounded", "check.csv"))
check[, c("assertion", "observed", "pass")]
#>                                              assertion
#> 1         adjusted model: bias of estimate against 0.5
#> 2  adjusted model: model SE of estimate against its SD
#> 3  adjusted model: coverage of 0.5 by the 90% interval
#> 4 crude model: bias of estimate against 0.5, a control
#>                                                   observed  pass
#> 1                             1.05; 140 of 140 simulations FALSE
#> 2 +6.0% (ModSE 0.231, EmpSE 0.218); 140 of 140 simulations  TRUE
#> 3                            0.000; 140 of 140 simulations FALSE
#> 4                             1.05; 140 of 140 simulations  TRUE
```

[`save_results()`](https://www.rwhite.no/pbn/reference/save_results.md)
stops the script when an assertion fails.
[`read_results()`](https://www.rwhite.no/pbn/reference/read_results.md)
does not read the results of a scenario with a failed assertion. It also
does not read the real results of a run with a fake scenario with a
failed assertion:

``` r
pbn::read_results("v01", scenario = "confounded", home = home)
#> Error:
#> ! /tmp/RtmpIppkPC/pbn_agents/project/v01_results/confounded/check.csv holds an assertion that did not pass, so read_results() does not read /tmp/RtmpIppkPC/pbn_agents/project/v01_results/confounded/results.qs2. Fix the analysis and run it again. To look at the results for debugging, read them with qs2::qs_read("/tmp/RtmpIppkPC/pbn_agents/project/v01_results/confounded/results.qs2").
pbn::read_results("v01", home = home)
#> Error:
#> ! manifest$failed_fake_checks of /tmp/RtmpIppkPC/pbn_agents/project/v01_results/real/results.qs2 names a fake scenario with a failed assertion: "confounded". Its assertions are in /tmp/RtmpIppkPC/pbn_agents/project/v01_results/confounded/check.csv. Correct the analysis, and run the scenario "real" again with the fake scenarios. To look at the results for debugging, read them with qs2::qs_read("/tmp/RtmpIppkPC/pbn_agents/project/v01_results/real/results.qs2").
```

[`check_project()`](https://www.rwhite.no/pbn/reference/check_project.md)
also fails the project:

``` r
checked <- callr::rscript(file.path(home, "check_project.R"), wd = home, show = FALSE, fail_on_status = FALSE)
cat(grep("^FAIL", strsplit(checked$stdout, "\r?\n")[[1]], value = TRUE), sep = "\n")
#> FAIL  scenario_checks  v01_results/confounded/check.csv: these assertions failed: adjusted model: bias of estimate against 0.5; adjusted model: coverage of 0.5 by the 90% interval. Fix: correct the analysis, and run the scenario again with pbn::run_plan() and pbn::save_results().
```

Put the code back, and the run succeeds:

``` r
writeLines(fit_code, fit_file)
run()$status
#> [1] 0
```

A person does not need to read every changed line to trust a run. These
outputs show what the run did:

- `v01_results/<scenario>/check.csv`: each assertion, what was expected
  and what was observed.
- `v01_results/run.md`: the rows before and after the exclusions, the
  exclusion tables, the assertions and the timings. It holds no text of
  an error, because an error can hold a value of the data. That text
  goes to `run_errors_gitexclude.md`, which git ignores.
- the manifest in each results file: the md5 of the run script and of
  each file of the functions folder that can change the results. For the
  scenario `real`, it also holds the size and the modification time of
  each data file.

An assertion on fake data is correct only when its expected value is
correct. A person chooses the fake scenarios and their expected values.
An agent MUST NOT decide them alone.

## Layer 4: prose last

The layout of pbn has the file `AGENTS.md`, for the rules of the project
that no check can state. Keep it short. It points the agent to two pages
that pbn writes for agents:

- [pbn.md](https://www.rwhite.no/pbn/articles/pbn.md): the manual, in
  plain Markdown, built to be read in one piece.
- [llms.txt](https://www.rwhite.no/pbn/llms.txt): the list of every
  other page.

The rest of `AGENTS.md` is what only a person knows: the question of the
study, and the decisions that are not checks yet. Each such decision is
a candidate for a check in layer 2.

pbn supports two more practices:

- **The analysis plan is the specification and the decision log.** A
  changed decision goes into the plan first, with a date and a reason,
  and then into the code. In pbn, the values of the plan go into `CFG`.
  The true values of its simulations go into the fake scenarios, where
  the `expect_` functions check them.
- **Keep the data on its server.** The agent works on the code and on
  the fake scenarios, and needs no real data. A person runs the code on
  the server.
  [`pbn::to_one_file()`](https://www.rwhite.no/pbn/reference/to_one_file.md)
  writes one R script that runs the scenario `real` on a server without
  a network.

## What the checks do not catch

The checks find only the mistakes that they name. [What pbn does not do
yet](https://www.rwhite.no/pbn/articles/pbn.html#what-pbn-does-not-do-yet)
lists the gaps. Two of them apply to code that an agent writes:

- pbn finds an analysis that adds, removes, renames or replaces a column
  of the shared data. It does not find a change of values in place, for
  example `data$d[1, x := 0]`. Copy the data first with
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html).
- Of the files of code, the cache key holds the files `data_*.R` and
  each data step. A helper that a data step calls, in another file, can
  change, and the run then uses the old data.
