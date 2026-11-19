# The check that an analysis does not change an object of the functions
# folder, such as CFG.

# A project whose functions folder holds peek_type(), which subsets the
# data.table CFG$vars with a logical, and vars_attributes(), which returns
# the attributes of CFG$vars. data.table adds an index to CFG$vars in place
# at the subset.
local_index_project <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(
    p$home,
    "v01_functions/index.R",
    c(
      "peek_type <- function(data, argset) {",
      "  return(CFG$vars[var == \"x\", type])",
      "}",
      "",
      "vars_attributes <- function(data, argset) {",
      "  return(sort(names(attributes(CFG$vars))))",
      "}"
    )
  )
  return(p)
}

# A plan with the kinds peek and attrs of the sourced environment `sourced`.
index_plan <- function(sourced, kinds = c("peek", "attrs")) {
  p <- plnr::Plan$new()
  fn <- function(scenario, sim) return(sourced$get_data(scenario, sim))
  environment(fn) <- list2env(list(sourced = sourced), parent = globalenv())
  add_data(p, "d", fn = fn)
  sc <- scenarios(sourced$CFG, only = "null")
  if ("peek" %in% kinds) {
    add_analyses(p, sc, "peek", fn = sourced$peek_type)
  }
  if ("attrs" %in% kinds) {
    add_analyses(p, sc, "attrs", fn = sourced$vars_attributes)
  }
  return(p)
}

test_that("initialize_project records the objects that the functions folder defines, and the check reads its objects that are not functions", {
  p <- local_index_project()
  sourced <- init(p)
  expect_setequal(
    state$sourced,
    c(
      "CFG",
      "get_data",
      "make_table1",
      "fit_models",
      "expect_known",
      "expect_null",
      "double_it",
      "peek_type",
      "vars_attributes"
    )
  )
  expect_identical(checked_names(sourced), "CFG")
  # A second source gives each object a new address, so the record is the
  # same.
  sourced2 <- init(p)
  expect_identical(checked_names(sourced2), "CFG")
})

test_that("an analysis that adds an index to a data.table of CFG fails, with 1 and 2 workers, and names CFG$vars and the index", {
  skip_if_pbn_not_installed()
  # data.table adds the index once. With 1 worker the first peek adds it in
  # this R session, and the run stops there. With 2 workers each of the 3
  # futures gets its own copy of CFG, one for each simulation, so a peek of
  # another simulation MAY fail before its worker sees the stop.
  for (workers in 1:2) {
    p <- local_index_project()
    sourced <- init(p)
    plan <- index_plan(sourced)
    out <- utils::capture.output(
      err <- tryCatch(run_plan(plan, workers = workers), error = function(e) e)
    )
    expect_s3_class(err, "error")
    expect_match(
      conditionMessage(err),
      if (workers == 1) {
        "^1 of 9 analyses failed, and the run stopped early: 5 analyses did not run\\. "
      } else {
        "^[1-3] of 9 analyses failed, and the run stopped early: [0-9] analyses did not run\\. "
      },
      info = paste("workers:", workers)
    )
    failed <- grep("^pbn: the analysis", out, value = TRUE)
    expect_true("pbn: the analysis null__sim_0001__peek__1 failed:" %in% failed)
    expect_match(
      failed,
      "^pbn: the analysis null__sim_000[1-3]__peek__1 failed:$"
    )
    text <- paste(out, collapse = "\n")
    expect_match(
      text,
      paste0(
        "The analysis changed the object 'CFG' of the functions folder in ",
        "place, at CFG$vars: a new attribute 'index'."
      ),
      fixed = TRUE
    )
    expect_match(text, "data.table::copy(CFG$table)", fixed = TRUE)
    expect_match(text, "options(datatable.auto.index = FALSE)", fixed = TRUE)
  }
})

test_that("without the change, the attributes of CFG are the same in each analysis, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  p <- local_index_project()
  sourced <- init(p)
  plan <- index_plan(sourced, kinds = "attrs")
  one <- run_plan(plan)
  two <- run_plan(plan, workers = 2)
  expect_identical(two, one)
  expect_identical(
    unique(unname(one[grepl("__attrs__", names(one))])),
    list(c(".internal.selfref", "class", "names", "row.names"))
  )
})

test_that("the check passes an analysis that reads CFG, calls a function of CFG and uses a formula of CFG", {
  p <- local_index_project()
  write_lines(
    p$home,
    "v01_functions/zz_cfg_more.R",
    c(
      "CFG$plus_one <- function(x) return(x + 1)",
      "CFG$model <- y ~ x",
      "reads_cfg <- function(data, argset) {",
      "  for (i in 1:5) CFG$plus_one(i)",
      "  return(c(length(CFG$outcomes), length(all.vars(CFG$model))))",
      "}"
    )
  )
  sourced <- init(p)
  plan <- index_plan(sourced, kinds = character(0))
  add_analyses(plan, scenarios(sourced$CFG), "reads", fn = sourced$reads_cfg)
  r <- run_plan(plan)
  expect_identical(r[["real__sim_0001__reads__1"]], c(2L, 2L))
  expect_identical(r[["null__sim_0003__reads__1"]], c(2L, 2L))
})

test_that("a data function that changes CFG fails each analysis that gets its data, with 1 and 2 workers, and blames no later data or analysis", {
  skip_if_pbn_not_installed()
  for (workers in 1:2) {
    p <- local_index_project()
    write_lines(
      p$home,
      "v01_functions/zz_data_peek.R",
      c(
        "data_peek <- function(scenario, sim) {",
        "  # In null, an index on var in simulation 1.",
        "  if (scenario == \"null\" && sim == 1) CFG$vars[var == \"y\", type]",
        "  return(data.frame(x = 1))",
        "}"
      )
    )
    sourced <- init(p)
    plan <- plnr::Plan$new()
    add_data(plan, "d", fn = sourced$data_peek)
    # The analyses of real and known run first, so their fingerprint is the
    # one that the data of null compares with. The data of null 2 and 3
    # MUST compare with the fingerprint after the data of null 1.
    add_analyses(
      plan,
      scenarios(sourced$CFG, only = c("real", "known")),
      "attrs",
      fn = sourced$vars_attributes
    )
    add_analyses(
      plan,
      scenarios(sourced$CFG, only = "null"),
      "attrs_null",
      fn = sourced$vars_attributes
    )
    out <- utils::capture.output(
      err <- tryCatch(run_plan(plan, workers = workers), error = function(e) e)
    )
    expect_match(
      conditionMessage(err),
      "^2 of 10 analyses failed, and the run stopped early: [0-9] analyses did not run\\. ",
      info = paste("workers:", workers)
    )
    expect_identical(
      grep("^pbn: the analysis", out, value = TRUE),
      c(
        "pbn: the analysis null__sim_0001__pbn_n_rows failed:",
        "pbn: the analysis null__sim_0001__attrs_null__1 failed:"
      ),
      info = paste("workers:", workers)
    )
    expect_match(
      paste(out, collapse = "\n"),
      paste0(
        "The data function 'd' failed: it changed the object 'CFG' of the ",
        "functions folder in place, at CFG$vars: a new attribute 'index'."
      ),
      fixed = TRUE
    )
  }
})

test_that("an analysis that leaves a new object in the global environment fails, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  withr::defer(suppressWarnings(rm("pbn_leftover", envir = globalenv())))
  leave <- function(data, argset) {
    if (argset$sim == 2) {
      assign("pbn_leftover", argset$sim, envir = globalenv())
    }
    return(1)
  }
  environment(leave) <- globalenv()
  for (workers in 1:2) {
    suppressWarnings(rm("pbn_leftover", envir = globalenv()))
    data_fn <- function(scenario, sim) return(data.frame(x = 1))
    environment(data_fn) <- globalenv()
    plan <- plnr::Plan$new()
    add_data(plan, "d", fn = data_fn)
    add_analyses(
      plan,
      scenarios(list(
        scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
      )),
      "a",
      fn = leave
    )
    out <- utils::capture.output(
      err <- tryCatch(run_plan(plan, workers = workers), error = function(e) e)
    )
    expect_match(
      conditionMessage(err),
      "^1 of 8 analyses failed",
      info = paste("workers:", workers)
    )
    expect_identical(
      grep("^pbn: the analysis", out, value = TRUE),
      "pbn: the analysis null__sim_0002__a__1 failed:"
    )
    expect_match(
      paste(out, collapse = "\n"),
      "The analysis left the new object 'pbn_leftover' in the global environment.",
      fixed = TRUE
    )
  }
})

test_that("on a worker, the first seed of a batch makes .Random.seed in the empty global environment, and no analysis fails for it", {
  skip_if_pbn_not_installed()
  # Without a data function with arguments, plnr runs each analysis as a
  # task of its own, so a future can start with an analysis that counts.
  # Its global environment then has no .Random.seed before the analysis.
  data_fn <- function() return(data.frame(x = 1))
  action <- function(data, argset) return(stats::runif(1))
  environment(data_fn) <- globalenv()
  environment(action) <- globalenv()
  plan <- plnr::Plan$new()
  add_data(plan, "d", fn = data_fn)
  add_analyses(
    plan,
    scenarios(list(
      scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
    )),
    "a",
    fn = action
  )
  one <- run_plan(plan)
  two <- run_plan(plan, workers = 2)
  expect_identical(two, one)
})

test_that("a data function that leaves a new object in the global environment fails no later analysis", {
  withr::defer(suppressWarnings(rm("pbn_data_leftover", envir = globalenv())))
  data_fn <- function(scenario, sim) {
    if (scenario == "null" && sim == 1) {
      assign("pbn_data_leftover", 1, envir = globalenv())
    }
    return(data.frame(x = 1))
  }
  action <- function(data, argset) return(1)
  environment(data_fn) <- globalenv()
  environment(action) <- globalenv()
  table <- data.frame(scenario = c("real", "null"), sims = c(1, 3))
  plan <- plnr::Plan$new()
  add_data(plan, "d", fn = data_fn)
  # The analysis of real runs first, so the data of null comes between two
  # analyses of this R session.
  add_analyses(
    plan,
    scenarios(list(scenarios = table), only = "real"),
    "a",
    fn = action
  )
  add_analyses(
    plan,
    scenarios(list(scenarios = table), only = "null"),
    "b",
    fn = action
  )
  r <- run_plan(plan)
  expect_identical(r[["null__sim_0001__b__1"]], 1)
  expect_true(exists("pbn_data_leftover", envir = globalenv()))
})

test_that("an analysis that changes the kind of random number generator changes no draw of a later data function or analysis, with 1 and 2 workers", {
  skip_if_pbn_not_installed()
  old <- RNGkind()
  withr::defer(RNGkind(old[1], old[2], old[3]))
  draws <- function(change) {
    # The last analysis of real changes the kind, so with 1 worker the data
    # of null comes next, and then each analysis of null.
    action <- function(data, argset) {
      if (argset$scenario == "real" && argset$k == 2) {
        if (change) {
          RNGkind("L'Ecuyer-CMRG")
        }
        return(NULL)
      }
      return(c(data$d$x, stats::runif(1)))
    }
    data_fn <- function(scenario, sim) return(data.frame(x = stats::runif(1)))
    environment(action) <- list2env(list(change = change), parent = globalenv())
    environment(data_fn) <- globalenv()
    plan <- plnr::Plan$new()
    add_data(plan, "d", fn = data_fn)
    table <- list(
      scenarios = data.frame(scenario = c("real", "null"), sims = c(1, 3))
    )
    add_analyses(
      plan,
      scenarios(table, only = "real"),
      "a",
      fn = action,
      k = 1:2
    )
    add_analyses(
      plan,
      scenarios(table, only = "null"),
      "b",
      fn = action,
      k = 1:2
    )
    return(plan)
  }
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  for (workers in 1:2) {
    plain <- run_plan(draws(FALSE), workers = workers)
    changed <- run_plan(draws(TRUE), workers = workers)
    attr(plain, "pbn_run") <- NULL
    attr(changed, "pbn_run") <- NULL
    expect_identical(changed, plain, info = paste("workers:", workers))
  }
})

test_that("objects_fingerprint sees an index and a new value, and not a compiled function or the contents of an environment", {
  env <- new.env()
  env$x <- list(
    t = data.table::data.table(a = 1:3),
    f = function(v) return(v + 1),
    m = stats::as.formula("y ~ x", env = new.env()),
    k = 1:10
  )
  before <- objects_fingerprint(env, "x")
  for (i in 1:5) {
    env$x$f(i)
  }
  assign("z", 1, envir = environment(env$x$m))
  expect_identical(objects_fingerprint(env, "x"), before)
  # In the global environment, as in a project, data.table subsets.
  peek <- function(t) return(t[a == 2])
  environment(peek) <- globalenv()
  invisible(peek(env$x$t))
  expect_false(is.null(attr(env$x$t, "index")))
  expect_false(identical(objects_fingerprint(env, "x"), before))
})

test_that("object_change names the first top-level element of a list that changed, and how: an attribute, the value, a new element or a gone one", {
  fp <- function(x) {
    env <- new.env()
    env$x <- x
    return(objects_fingerprint(env, "x")[["x"]])
  }
  cfg <- list(
    outcomes = c("y", "z"),
    vars = data.table::data.table(var = c("x", "y"))
  )
  before <- fp(cfg)
  indexed <- data.table::copy(cfg$vars)
  data.table::setindexv(indexed, "var")
  changed <- cfg
  changed$vars <- indexed
  expect_identical(
    object_change("CFG", before, fp(changed)),
    c("CFG$vars", "a new attribute 'index'")
  )
  changed <- cfg
  changed$outcomes <- "y"
  expect_identical(
    object_change("CFG", before, fp(changed)),
    c("CFG$outcomes", "the value changed")
  )
  expect_identical(
    object_change("CFG", before, fp(c(cfg, list(level = 0.95)))),
    c("CFG$level", "a new element")
  )
  expect_identical(
    object_change("CFG", before, fp(cfg["vars"])),
    c("CFG$outcomes", "the element is gone")
  )
  expect_identical(
    object_change("CFG", before, fp(structure(cfg, note = "x"))),
    c("CFG", "a new attribute 'note'")
  )
  expect_identical(
    object_change("k", fp(1:3), fp(1:4)),
    c("k", "the value changed")
  )
  expect_identical(
    object_change("u", fp(list(1, 2)), fp(list(1, 3))),
    c("u[[2]]", "the value changed")
  )
})

test_that("an output function that changes CFG in place fails its scenario through make_outputs, and names CFG$vars", {
  p <- local_index_project()
  run <- save_run(p)
  fn <- function(results, dir) {
    peek <- function() return(CFG$vars[var == "x", type])
    environment(peek) <- state$env
    peek()
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  err <- NULL
  utils::capture.output(
    err <- tryCatch(make_outputs(fn, run$scenarios), error = function(e) e)
  )
  expect_identical(
    conditionMessage(err),
    paste0(
      "The output function changed the object 'CFG' of the functions folder ",
      "in place, at CFG$vars: a new attribute 'index', for the scenario ",
      "\"real\". A later call of the output function or of an expect_ ",
      "function would see the change. Change a copy, for example ",
      "x <- data.table::copy(CFG$table)."
    )
  )
  lines <- readLines(file.path(p$home, "v01_results", "run.md"))
  expect_true(any(grepl(
    "make_outputs() failed for the scenario `real`.",
    lines,
    fixed = TRUE
  )))
  # Without the change, the same output passes.
  quiet <- function(results, dir) {
    writeLines("x", file.path(dir, "figure.png"))
    return(invisible(NULL))
  }
  sourced <- init(p)
  run <- save_run(p)
  utils::capture.output(expect_no_error(make_outputs(quiet, run$scenarios)))
})

test_that("an expect_ function that changes CFG in place fails one assertion, which names CFG$vars", {
  p <- local_index_project()
  write_lines(
    p$home,
    "v01_functions/expect_known.R",
    c(
      expect_known_lines(),
      "expect_known_plain <- expect_known",
      "expect_known <- function(results) {",
      "  CFG$vars[var == \"x\", type]",
      "  return(expect_known_plain(results))",
      "}"
    )
  )
  run <- save_run(p)
  expect_match(conditionMessage(run$err), "assertions failed")
  check <- utils::read.csv(file.path(
    p$home,
    "v01_results",
    "known",
    "check.csv"
  ))
  bad <- check[!check$pass, ]
  expect_identical(
    bad$assertion,
    "expect_known() changes no object of the functions folder"
  )
  expect_identical(
    bad$observed,
    "expect_known() changed CFG$vars in place: a new attribute 'index'"
  )
  # The assertions of the function itself stay.
  expect_true(all(check$pass[check$assertion != bad$assertion]))
  expect_gt(nrow(check), 1)
})
