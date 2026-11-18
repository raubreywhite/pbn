# Stacking: the simulations of one scenario in one results object, and the
# assertions of the expect_<scenario>() functions.

# Stacks the simulation files of one scenario into
# <root>/<scenario>/results.qs2. `root` is <home>/<version>_results.
stack_scenario <- function(root, scenario, sims) {
  folder <- file.path(root, scenario)
  want <- sim_file(seq_len(sims))
  missing <- want[!file.exists(file.path(folder, want))]
  if (length(missing) > 0) {
    stop(
      "The scenario \"",
      scenario,
      "\" has ",
      sims,
      " simulations in CFG$scenarios, so pbn did not stack it. Missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  x <- stack_results(lapply(file.path(folder, want), qs2::qs_read), scenario)
  findings <- check_results(x)
  failed <- findings[findings$status == "FAIL", , drop = FALSE]
  if (nrow(failed) > 0) {
    stop(
      "The stacked results of the scenario \"",
      scenario,
      "\" fail check_results(), so pbn did not save them:\n",
      paste0("  ", failed$rule, ": ", failed$detail, collapse = "\n"),
      call. = FALSE
    )
  }
  file <- file.path(folder, "results.qs2")
  qs2::qs_save(x, file)
  # The simulations of results_gitexclude.qs2 are stacked apart, with the
  # same rules.
  private <- NULL
  if (length(gitexclude_kinds(x)) > 0) {
    private_files <- file.path(folder, sim_private_file(seq_len(sims)))
    private <- stack_element(lapply(private_files, qs2::qs_read), "x", scenario)
  }
  write_private(private, file.path(folder, gitexclude_file))
  return(invisible(file))
}

# Simulation k of `runs` is simulation k of the scenario.
stack_results <- function(runs, scenario) {
  # The stacked manifest is that of simulation 1, so its fake_md5 MUST be the
  # fake_md5 of each simulation.
  fake <- lapply(runs, function(r) return(r[["manifest"]][["fake_md5"]]))
  bad <- which(!vapply(fake, is_md5, logical(1)))
  if (scenario != "real" && length(bad) > 0) {
    stop(
      "The simulations ",
      paste(bad, collapse = ", "),
      " of the scenario \"",
      scenario,
      "\" have no valid manifest$fake_md5, so pbn cannot stack them. ",
      "Run the scenario again.",
      call. = FALSE
    )
  }
  if (!all(vapply(fake, identical, logical(1), fake[[1]]))) {
    stop(
      "The simulations of the scenario \"",
      scenario,
      "\" have different values of manifest$fake_md5, so a different ",
      "fake_",
      scenario,
      "() wrote their data. pbn cannot stack them. Run the scenario again.",
      call. = FALSE
    )
  }
  settings <- lapply(runs, function(r) return(r[["manifest"]][["settings"]]))
  if (!all(vapply(settings, identical, logical(1), settings[[1]]))) {
    stop(
      "The simulations of the scenario \"",
      scenario,
      "\" have different values of manifest$settings, so pbn cannot stack ",
      "them. Run the scenario again.",
      call. = FALSE
    )
  }
  manifest <- runs[[1]][["manifest"]]
  manifest$sim <- NA_integer_
  manifest$sims <- length(runs)
  manifest$n_rows_by_sim <- rows_by_sim(runs)
  manifest$n_rows_data <- NA_integer_
  if (!is.null(manifest$n_rows_lists)) {
    manifest$n_rows_lists <- stats::setNames(list(), character(0))
  }
  for (n in c("n_rows_before_exclusions", "n_rows_after_exclusions")) {
    if (!is.null(manifest[[n]])) {
      manifest[[n]] <- NA_integer_
    }
  }
  if (is.data.frame(manifest$data_cache)) {
    manifest$data_cache$from_cache <- NA
    manifest$data_cache$cache_key <- NA_character_
  }
  manifest$stacked <- TRUE
  results <- lapply(runs, function(r) return(r[names(r) != "manifest"]))
  return(c(list(manifest = manifest), stack_element(results, "x", scenario)))
}

# The row counts of the manifest of each simulation of `runs`, one row for
# each simulation and data element, with the columns sim, data and the
# columns of n_rows_columns. A count that a manifest does not hold is NA.
# NULL when no manifest holds n_rows_data.
rows_by_sim <- function(runs) {
  parts <- lapply(seq_along(runs), function(k) {
    m <- runs[[k]][["manifest"]]
    n <- m[["n_rows_data"]]
    if (!is.numeric(n) || length(n) == 0) {
      return(NULL)
    }
    out <- data.frame(
      sim = rep(k, length(n)),
      data = if (is.null(names(n))) NA_character_ else names(n)
    )
    for (col in n_rows_columns) {
      v <- m[[col]]
      out[[col]] <- if (is.numeric(v) && length(v) == length(n)) {
        as.integer(unname(v))
      } else {
        NA_integer_
      }
    }
    return(rbind(out, list_rows_by_sim(m[["n_rows_lists"]], k)))
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) {
    return(NULL)
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  return(out)
}

# The rows of the data.frames of manifest$n_rows_lists `x` of the
# simulation `k`, as rows of manifest$n_rows_by_sim: one row for each
# data.frame, with `data` as <data element>[[i]] and its rows in
# n_rows_data. NULL when `x` is empty.
list_rows_by_sim <- function(x, k) {
  if (!is.list(x) || length(x) == 0 || is.null(names(x))) {
    return(NULL)
  }
  n <- lapply(x, function(v) return(as.integer(v)))
  out <- data.frame(
    sim = rep(k, sum(lengths(n))),
    data = unlist(lapply(names(n), function(d) {
      return(paste0(d, "[[", seq_along(n[[d]]), "]]"))
    })),
    n_rows_data = unlist(n, use.names = FALSE)
  )
  out$n_rows_before_exclusions <- rep(NA_integer_, nrow(out))
  out$n_rows_after_exclusions <- rep(NA_integer_, nrow(out))
  return(out)
}

# The element at the path `where`, stacked from `values`, its value in each
# simulation. A data.frame gets a first column sim. A list is stacked element
# by element. Each other value becomes a list with one entry for each
# simulation, named sim_0001, sim_0002 and so on.
stack_element <- function(values, where, scenario) {
  kinds <- vapply(values, stack_kind, character(1))
  k <- match(TRUE, kinds != kinds[1])
  if (!is.na(k)) {
    stop_structure(scenario, where, k, kinds[1], kinds[k])
  }
  if (kinds[1] == "a data.frame") {
    return(stack_frames(values, where, scenario))
  }
  if (kinds[1] == "a list") {
    return(stack_list(values, where, scenario))
  }
  return(stats::setNames(values, sim_folder(seq_along(values))))
}

stack_kind <- function(o) {
  if (is.null(o)) {
    return("NULL")
  }
  if (is.data.frame(o)) {
    return("a data.frame")
  }
  if (is.list(o)) {
    return("a list")
  }
  return("a value")
}

# The lists `values` stacked element by element and matched by name. Each
# list MUST have a unique name for each element, so that no element of one
# simulation is stacked with another element of another simulation.
stack_list <- function(values, where, scenario) {
  first <- values[[1]]
  nm <- names(first)
  for (k in seq_along(values)) {
    nm_k <- names(values[[k]])
    named <- length(values[[k]]) == 0 ||
      (!is.null(nm_k) && all(nzchar(nm_k)) && !anyDuplicated(nm_k))
    if (!named) {
      stop(
        "pbn cannot stack the scenario \"",
        scenario,
        "\": in simulation ",
        k,
        ", the list at ",
        where,
        " has an element without a name or a name twice. Give each ",
        "element of the list its own name, for example list(a = 1, b = 2).",
        call. = FALSE
      )
    }
    if (!setequal(nm_k, nm) || length(values[[k]]) != length(first)) {
      stop_structure(
        scenario,
        where,
        k,
        list_shape(first),
        list_shape(values[[k]])
      )
    }
  }
  out <- lapply(seq_along(first), function(i) {
    parts <- lapply(values, function(v) return(v[[nm[[i]]]]))
    return(stack_element(parts, child_path(first, i, where), scenario))
  })
  names(out) <- nm
  return(out)
}

list_shape <- function(o) {
  if (is.null(names(o))) {
    return(paste("a list of length", length(o)))
  }
  return(paste(
    "a list with the names",
    paste(dQuote(names(o), FALSE), collapse = ", ")
  ))
}

# Stops, because simulation k is `found` at the path `where`, and simulation 1
# is `want`. `advice` is NULL or a sentence that says how to fix it.
stop_structure <- function(scenario, where, k, want, found, advice = NULL) {
  stop(
    "The simulations 1 and ",
    k,
    " of the scenario \"",
    scenario,
    "\" differ at ",
    where,
    ": it is ",
    want,
    " in simulation 1 and ",
    found,
    " in simulation ",
    k,
    ", so pbn cannot stack them.",
    if (!is.null(advice)) paste0(" ", advice),
    call. = FALSE
  )
}

# The frames with a first column sim, in one object of the class of
# frames[[1]]. A data.table stays a data.table.
stack_frames <- function(frames, where, scenario) {
  check_frames(frames, where, scenario)
  parts <- lapply(seq_along(frames), function(k) {
    f <- as.data.frame(frames[[k]])
    return(data.frame(sim = rep(k, nrow(f)), f, check.names = FALSE))
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  cls <- class(frames[[1]])
  if (data.table::is.data.table(frames[[1]])) {
    # setDT() gives the data.table its internal self-reference, which a
    # class set by hand does not.
    data.table::setDT(out)
  }
  if (!identical(class(out), cls)) {
    class(out) <- cls
  }
  return(out)
}

# Stops when the frames of the simulations at the path `where` cannot be
# row-bound without a change: a column sim, other columns, a column of
# another class, or a factor column with other levels.
check_frames <- function(frames, where, scenario) {
  columns <- names(frames[[1]])
  for (k in seq_along(frames)) {
    repeated <- unique(names(frames[[k]])[duplicated(names(frames[[k]]))])
    if (length(repeated) > 0) {
      stop(
        "pbn cannot stack the scenario \"",
        scenario,
        "\": in simulation ",
        k,
        ", the data.frame at ",
        where,
        " has more than one column named ",
        paste(repeated, collapse = ", "),
        ". Give each column its own name.",
        call. = FALSE
      )
    }
    if ("sim" %in% names(frames[[k]])) {
      stop(
        "pbn cannot stack the scenario \"",
        scenario,
        "\": in simulation ",
        k,
        ", the data.frame at ",
        where,
        " has a column sim. pbn adds that column when it stacks the ",
        "simulations, so give the column another name.",
        call. = FALSE
      )
    }
    if (
      ncol(frames[[k]]) != length(columns) ||
        !setequal(names(frames[[k]]), columns)
    ) {
      stop_structure(
        scenario,
        where,
        k,
        paste("a data.frame with the columns", paste(columns, collapse = ", ")),
        paste(
          "a data.frame with the columns",
          paste(names(frames[[k]]), collapse = ", ")
        )
      )
    }
    for (col in columns) {
      check_column(
        frames[[1]][[col]],
        frames[[k]][[col]],
        k,
        where,
        col,
        scenario
      )
    }
  }
  return(invisible(NULL))
}

# Stops when the column `col` of simulation k, `b`, has another class than in
# simulation 1, `a`, or other factor levels.
check_column <- function(a, b, k, where, col, scenario) {
  at <- paste0(where, ", column ", col)
  if (!identical(class(a), class(b))) {
    stop_structure(
      scenario,
      at,
      k,
      paste("of the class", paste(class(a), collapse = "/")),
      paste("of the class", paste(class(b), collapse = "/"))
    )
  }
  if (is.factor(a) && !identical(levels(a), levels(b))) {
    stop_structure(
      scenario,
      at,
      k,
      paste("a factor with the levels", paste(levels(a), collapse = ", ")),
      paste("a factor with the levels", paste(levels(b), collapse = ", ")),
      paste(
        "Convert the column to character, or give the factor the same",
        "levels in each simulation in the analysis."
      )
    )
  }
  return(invisible(NULL))
}
# Calls expect_<scenario>() on the results of the scenario under `root`,
# writes check.csv and prints the table. An error, zero rows or a wrong shape
# is one failed assertion. A change of an object of the functions folder in
# place, such as CFG, adds one failed assertion.
check_expectations <- function(fn, root, scenario) {
  folder <- file.path(root, scenario)
  fn_name <- paste0("expect_", scenario, "()")
  main <- file.path(folder, "results.qs2")
  results <- merge_gitexclude(qs2::qs_read(main), main)
  objects <- main_fingerprint()
  found <- tryCatch(fn(results), error = function(e) {
    return(failed_assertion(
      paste(fn_name, "runs without an error"),
      gsub("\\s+", " ", conditionMessage(e))
    ))
  })
  columns <- names(empty_checks())[-1]
  if (
    !is.data.frame(found) ||
      !identical(sort(names(found), na.last = TRUE), sort(columns))
  ) {
    found <- failed_assertion(
      paste(
        fn_name,
        "returns a data.frame with the columns",
        paste(columns, collapse = ", ")
      ),
      if (is.data.frame(found)) {
        paste("the columns", paste(names(found), collapse = ", "))
      } else {
        paste("an object of class", paste(class(found), collapse = ", "))
      }
    )
  } else if (nrow(found) == 0) {
    found <- failed_assertion(
      paste(fn_name, "returns at least one assertion"),
      "0 rows"
    )
  }
  found <- as.data.frame(found)[, columns, drop = FALSE]
  changed <- main_change(objects)
  if (!is.null(changed)) {
    found <- rbind(
      found,
      failed_assertion(
        paste(fn_name, "changes no object of the functions folder"),
        paste0(
          fn_name,
          " changed ",
          changed$where,
          " in place: ",
          changed$how
        )
      )
    )
  }
  write_csv_atomic(found, file.path(folder, "check.csv"))
  cat("Assertions of the scenario \"", scenario, "\":\n", sep = "")
  print(found, row.names = FALSE)
  return(data.frame(scenario = rep(scenario, nrow(found)), found))
}

# Writes `x` to a temporary file in the folder of `file`, then renames it to
# `file`. A rename in one folder replaces the file as a whole, so a reader
# sees the old file or the new one, never a part of the new one.
write_csv_atomic <- function(x, file) {
  tmp <- tempfile("check_", tmpdir = dirname(file), fileext = ".csv.tmp")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(x, tmp, row.names = FALSE)
  if (!rename_file(tmp, file)) {
    stop("pbn could not write ", file, call. = FALSE)
  }
  return(invisible(file))
}

# file.rename(), in one place, so that the tests can watch it.
rename_file <- function(from, to) {
  return(file.rename(from, to))
}

# Deletes the files or folders `paths`, and stops when one of them is still
# there. On Windows a file that another program holds open stays, and
# unlink() does not say so. A stale file would then stay beside the new
# results.
delete_paths <- function(paths, recursive = FALSE) {
  remove_paths(paths, recursive)
  left <- paths[file.exists(paths)]
  if (length(left) > 0) {
    stop(
      "pbn could not delete ",
      paste(left, collapse = ", "),
      ". Another program may hold it open, for example a viewer or an ",
      "editor. Close that program, and run the call again.",
      call. = FALSE
    )
  }
  return(invisible(paths))
}

# unlink(), in one place, so that the tests can make it fail.
remove_paths <- function(paths, recursive) {
  unlink(paths, recursive = recursive)
  return(invisible(NULL))
}

failed_assertion <- function(assertion, observed) {
  return(data.frame(
    assertion = assertion,
    expected = "see the assertion",
    observed = observed,
    pass = FALSE
  ))
}

empty_checks <- function() {
  return(data.frame(
    scenario = character(0),
    assertion = character(0),
    expected = character(0),
    observed = character(0),
    pass = logical(0)
  ))
}
