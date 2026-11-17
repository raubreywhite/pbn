# Three simulation results objects of the fake scenario null, with the class
# `as_class` for each table.
three_runs <- function(as_class) {
  return(lapply(1:3, function(k) {
    x <- valid_results()
    x$manifest$scenario <- "null"
    x$manifest$data_files <- NULL
    x$manifest$fake_md5 <- "fedcba9876543210fedcba9876543210"
    x$manifest$sim <- k
    x$manifest$sims <- 3L
    x$exclusions <- as_class(x$exclusions)
    x$estimates <- as_class(data.frame(term = "a", estimate = k / 10))
    x$tables <- list(counts = as_class(x$tables$counts))
    return(x)
  }))
}

test_that("stack_results keeps the class of each table of simulation 1", {
  classes <- list(
    data.table = list(
      as_class = data.table::as.data.table,
      class = c("data.table", "data.frame")
    ),
    data.frame = list(as_class = as.data.frame, class = "data.frame"),
    subclass = list(
      as_class = function(x) {
        return(structure(as.data.frame(x), class = c("my_frame", "data.frame")))
      },
      class = c("my_frame", "data.frame")
    )
  )
  for (nm in names(classes)) {
    x <- stack_results(three_runs(classes[[nm]]$as_class), "null")
    for (part in list(x$estimates, x$exclusions, x$tables$counts)) {
      expect_identical(class(part), classes[[nm]]$class, info = nm)
      expect_identical(names(part)[1], "sim", info = nm)
    }
    expect_identical(x$estimates$sim, 1:3, info = nm)
    expect_identical(x$estimates$estimate, c(0.1, 0.2, 0.3), info = nm)
    expect_identical(x$tables$counts$sim, rep(1:3, each = 2), info = nm)
    expect_identical(check_results(x)$status, rep("PASS", 7), info = nm)
  }
})

test_that("stack_results stops when the simulations have different fake_md5", {
  runs <- three_runs(as.data.frame)
  expect_identical(
    stack_results(runs, "null")$manifest$fake_md5,
    "fedcba9876543210fedcba9876543210"
  )
  runs[[3]]$manifest$fake_md5 <- "0123456789abcdef0123456789abcdef"
  expect_error(
    stack_results(runs, "null"),
    "The simulations of the scenario \"null\" have different values of manifest$fake_md5",
    fixed = TRUE
  )
})

test_that("stack_results stops when a simulation of a fake scenario has no valid fake_md5", {
  for (bad in list(NULL, NA_character_, "x", 1)) {
    runs <- three_runs(as.data.frame)
    runs[[2]]$manifest["fake_md5"] <- list(bad)
    expect_error(
      stack_results(runs, "null"),
      paste0(
        "The simulations 2 of the scenario \"null\" have no valid ",
        "manifest$fake_md5, so pbn cannot stack them."
      ),
      fixed = TRUE,
      info = paste(format(bad), collapse = "")
    )
  }
  # A real scenario with simulations has data_files and no fake_md5.
  runs <- lapply(1:2, function(k) {
    x <- valid_results()
    x$manifest$sim <- k
    x$manifest$sims <- 2L
    return(x)
  })
  x <- stack_results(runs, "real")
  expect_identical(x$manifest$data_files, runs[[1]]$manifest$data_files)
  expect_false("fake_md5" %in% names(x$manifest))
})


test_that("stack_results keeps the settings of simulation 1 and stops when the simulations have different settings", {
  runs <- three_runs(as.data.frame)
  for (k in 1:3) {
    runs[[k]]$manifest$settings <- list(n_boot = 20, label = "fake")
  }
  expect_identical(
    stack_results(runs, "null")$manifest$settings,
    list(n_boot = 20, label = "fake")
  )
  runs[[2]]$manifest$settings$n_boot <- 21
  expect_error(
    stack_results(runs, "null"),
    "The simulations of the scenario \"null\" have different values of manifest$settings",
    fixed = TRUE
  )
})

# Three simulation results of the fake scenario null with each kind of
# element. Where a value can change, simulation k has a value from k.
three_rich_runs <- function() {
  runs <- three_runs(as.data.frame)
  return(lapply(1:3, function(k) {
    x <- runs[[k]]
    x$p_value <- k / 100
    x$n <- c(women = 20L + k, men = 20L - k)
    x$cor <- matrix(c(1, k / 10, k / 10, 1), 2)
    x$group <- factor(c("a", "b"))
    x$day <- as.Date("2026-01-01") + k
    x["nothing"] <- list(NULL)
    x$by_outcome <- list(
      pain = list(
        adjusted = list(estimates = data.frame(term = "x", estimate = k))
      )
    )
    x$empty <- data.frame(a = numeric(0))
    x$extra <- list(
      note = "synthetic",
      flow = data.frame(stage = "screened", n = 50L + k)
    )
    x$no_extra <- list()
    x$pair <- list(first = k, second = data.frame(v = k))
    return(x)
  }))
}

test_that("stack_results stacks every element: a data.frame by rows, a list element by element, and each other value as one entry for each simulation", {
  runs <- three_rich_runs()
  x <- stack_results(runs, "null")
  expect_identical(names(x), names(runs[[1]]))
  sims <- sprintf("sim_%04d", 1:3)
  expect_identical(x$p_value, stats::setNames(list(0.01, 0.02, 0.03), sims))
  expect_identical(
    x$n,
    stats::setNames(
      lapply(1:3, function(k) return(c(women = 20L + k, men = 20L - k))),
      sims
    )
  )
  expect_identical(x$cor$sim_0002, matrix(c(1, 0.2, 0.2, 1), 2))
  expect_identical(
    x$group,
    stats::setNames(rep(list(factor(c("a", "b"))), 3), sims)
  )
  expect_identical(x$day$sim_0003, as.Date("2026-01-04"))
  expect_identical(x$nothing, stats::setNames(list(NULL, NULL, NULL), sims))
  expect_identical(
    x$by_outcome$pain$adjusted$estimates,
    data.frame(sim = 1:3, term = "x", estimate = 1:3)
  )
  expect_identical(x$empty, data.frame(sim = integer(0), a = numeric(0)))
  expect_identical(
    x$extra$note,
    stats::setNames(rep(list("synthetic"), 3), sims)
  )
  expect_identical(
    x$extra$flow,
    data.frame(sim = 1:3, stage = "screened", n = 51:53)
  )
  expect_identical(x$no_extra, list())
  expect_identical(names(x$pair), c("first", "second"))
  expect_identical(x$pair$first, stats::setNames(list(1L, 2L, 3L), sims))
  expect_identical(x$pair$second, data.frame(sim = 1:3, v = 1:3))
  expect_identical(check_results(x)$status, rep("PASS", 7))
})

test_that("stack_results matches the elements of a named list by name", {
  runs <- three_rich_runs()
  runs[[2]]$extra <- runs[[2]]$extra[c("flow", "note")]
  x <- stack_results(runs, "null")
  expect_identical(names(x$extra), c("note", "flow"))
  expect_identical(x$extra$flow$n, 51:53)
})

test_that("stack_results stops when the simulations differ in structure, and names the path and the two simulations", {
  stack_error <- function(change) {
    runs <- three_rich_runs()
    runs <- change(runs)
    return(conditionMessage(tryCatch(
      stack_results(runs, "null"),
      error = function(e) return(e)
    )))
  }
  expect_identical(
    stack_error(function(runs) {
      runs[[3]]$extra <- list(note = "x", other = data.frame(n = 1))
      return(runs)
    }),
    paste0(
      "The simulations 1 and 3 of the scenario \"null\" differ at x$extra: ",
      "it is a list with the names \"note\", \"flow\" in simulation 1 and a ",
      "list with the names \"note\", \"other\" in simulation 3, so pbn cannot ",
      "stack them."
    )
  )
  expect_match(
    stack_error(function(runs) {
      runs[[2]]$pair <- list(first = 1)
      return(runs)
    }),
    paste0(
      "The simulations 1 and 2 of the scenario \"null\" differ at x$pair: ",
      "it is a list with the names \"first\", \"second\" in simulation 1 ",
      "and a list with the names \"first\" in simulation 2"
    ),
    fixed = TRUE
  )
  expect_match(
    stack_error(function(runs) {
      runs[[2]]$added <- 1
      return(runs)
    }),
    "The simulations 1 and 2 of the scenario \"null\" differ at x: it is a list with the names",
    fixed = TRUE
  )
  expect_match(
    stack_error(function(runs) {
      runs[[2]]$p_value <- data.frame(p = 0.02)
      return(runs)
    }),
    "differ at x$p_value: it is a value in simulation 1 and a data.frame in simulation 2",
    fixed = TRUE
  )
  expect_match(
    stack_error(function(runs) {
      runs[[3]]$by_outcome$pain$adjusted$estimates$se <- 0.1
      return(runs)
    }),
    paste0(
      "differ at x$by_outcome$pain$adjusted$estimates: it is a data.frame ",
      "with the columns term, estimate in simulation 1 and a data.frame with ",
      "the columns term, estimate, se in simulation 3"
    ),
    fixed = TRUE
  )
})


# The message of the error of stack_results() on three_rich_runs() after
# `change`, or "no error" when it stacks them.
rich_stack_error <- function(change) {
  runs <- change(three_rich_runs())
  return(tryCatch(
    {
      stack_results(runs, "null")
      "no error"
    },
    error = function(e) return(conditionMessage(e))
  ))
}

test_that("stack_results stops on a nested list that is unnamed, partly named or has a name twice, and names the path", {
  lists <- list(
    unnamed = function(k) return(list(k, k)),
    partly = function(k) return(list(a = k, k)),
    twice = function(k) return(list(a = k, a = k))
  )
  for (nm in names(lists)) {
    expect_identical(
      rich_stack_error(function(runs) {
        for (k in 1:3) {
          runs[[k]]$extra$parts <- lists[[nm]](k)
        }
        return(runs)
      }),
      paste0(
        "pbn cannot stack the scenario \"null\": in simulation 1, the list at ",
        "x$extra$parts has an element without a name or a name twice. Give ",
        "each element of the list its own name, for example list(a = 1, b = 2)."
      ),
      info = nm
    )
  }
  # An unnamed list in simulation 2 only.
  expect_match(
    rich_stack_error(function(runs) {
      runs[[2]]$pair <- unname(runs[[2]]$pair)
      return(runs)
    }),
    "in simulation 2, the list at x$pair has an element without a name",
    fixed = TRUE
  )
})

test_that("stack_results stops on a column of another class or a factor with other levels, and names the path, the column and the simulations", {
  expect_identical(
    rich_stack_error(function(runs) {
      runs[[3]]$extra$flow$n <- as.character(runs[[3]]$extra$flow$n)
      return(runs)
    }),
    paste0(
      "The simulations 1 and 3 of the scenario \"null\" differ at ",
      "x$extra$flow, column n: it is of the class integer in simulation 1 ",
      "and of the class character in simulation 3, so pbn cannot stack them."
    )
  )
  expect_identical(
    rich_stack_error(function(runs) {
      for (k in 1:3) {
        runs[[k]]$extra$flow$stage <- factor("screened")
      }
      runs[[2]]$extra$flow$stage <- factor("screened", c("screened", "seen"))
      return(runs)
    }),
    paste0(
      "The simulations 1 and 2 of the scenario \"null\" differ at ",
      "x$extra$flow, column stage: it is a factor with the levels screened ",
      "in simulation 1 and a factor with the levels screened, seen in ",
      "simulation 2, so pbn cannot stack them. Convert the column to ",
      "character, or give the factor the same levels in each simulation in ",
      "the analysis."
    )
  )
  # The same levels stack.
  runs <- three_rich_runs()
  for (k in 1:3) {
    runs[[k]]$extra$flow$stage <- factor("screened", c("screened", "seen"))
  }
  expect_identical(
    levels(stack_results(runs, "null")$extra$flow$stage),
    c("screened", "seen")
  )
})

test_that("stack_results stops on a data.frame with a column sim", {
  expect_identical(
    rich_stack_error(function(runs) {
      for (k in 1:3) {
        runs[[k]]$extra$flow$sim <- k
      }
      return(runs)
    }),
    paste0(
      "pbn cannot stack the scenario \"null\": in simulation 1, the ",
      "data.frame at x$extra$flow has a column sim. pbn adds that column when ",
      "it stacks the simulations, so give the column another name."
    )
  )
})

test_that("stack_results stops on NULL in one simulation and a value in another", {
  expect_identical(
    rich_stack_error(function(runs) {
      runs[[2]]["p_value"] <- list(NULL)
      return(runs)
    }),
    paste0(
      "The simulations 1 and 2 of the scenario \"null\" differ at ",
      "x$p_value: it is a value in simulation 1 and NULL in simulation 2, so ",
      "pbn cannot stack them."
    )
  )
  # NULL in every simulation stacks.
  x <- stack_results(three_rich_runs(), "null")
  expect_identical(
    x$nothing,
    stats::setNames(list(NULL, NULL, NULL), sprintf("sim_%04d", 1:3))
  )
})
