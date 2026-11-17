# The Markdown of the run log. It is plain GitHub-flavoured Markdown in
# ASCII, with short tables and no HTML, so it reads well as plain text. The
# section When comes first after the title, and the section Timings comes
# last. They hold everything that changes in every run, and
# run_log_stable() removes them.

# The most assertions of one scenario that the log shows in full.
log_max_assertions <- 30L

# The lines of the run log of `log`, in ASCII.
run_log_lines <- function(log) {
  if (identical(log$stage, "started")) {
    return(to_ascii(c(md_title(log), md_when(log), md_run(log))))
  }
  lines <- c(
    md_title(log),
    md_when(log),
    md_run(log),
    md_failures(log),
    md_scenarios(log),
    md_data(log),
    md_exclusions(log),
    md_assertions(log),
    md_outputs(log),
    md_files(log),
    md_timings(log)
  )
  return(to_ascii(lines))
}

md_title <- function(log) {
  return(c(
    paste0("# Run log of ", md_inline(log$folder), ", version ", log$version),
    ""
  ))
}

# `x` in ASCII. Each other character becomes <U+hhhh>.
to_ascii <- function(x) {
  out <- iconv(enc2utf8(x), "UTF-8", "ASCII", sub = "Unicode")
  out[is.na(out)] <- "?"
  return(out)
}

md_when <- function(log) {
  return(c(
    "## When",
    "",
    paste0(
      "The run started ",
      format(log$started, "%Y-%m-%d %H:%M:%S %z"),
      ". This section and the section Timings change in every run. The ",
      "other sections change when the code, the data, the cache, the ",
      "packages or the files on the disk change."
    ),
    ""
  ))
}

md_run <- function(log) {
  lines <- c(
    "## Run",
    "",
    paste0("- Result: ", log$result),
    paste0(
      "- Scenarios of this run: ",
      if (NROW(log$scenarios) == 0) "unknown" else and_list(log$scenarios$scenario),
      "."
    )
  )
  if (nrow(log$earlier) > 0) {
    lines <- c(
      lines,
      paste0(
        "- The results of the other scenarios on the disk are from earlier ",
        "runs: ",
        and_list(paste0(
          md_inline(log$earlier$scenario),
          " (run date ",
          md_inline(log$earlier$run_date),
          ")"
        )),
        "."
      )
    )
  }
  if (identical(log$stage, "started")) {
    return(c(lines, ""))
  }
  v <- log$versions
  return(c(
    lines,
    paste0("- ", log$r_version, "."),
    paste0("- Packages: ", and_list(paste(names(v), v)), "."),
    paste0("- Workers: ", log$workers, "."),
    ""
  ))
}

# The failed assertions of `log`, or NULL before the expect_ functions ran.
failed_checks <- function(log) {
  checks <- log$checks
  if (is.null(checks)) {
    return(NULL)
  }
  return(checks[!checks$pass %in% TRUE, , drop = FALSE])
}

md_failures <- function(log) {
  errors <- log$errors
  bad <- failed_checks(log)
  if (nrow(errors) == 0 && NROW(bad) == 0) {
    return(character(0))
  }
  lines <- c("## Failures", "")
  if (nrow(errors) > 0) {
    lines <- c(
      lines,
      paste0(
        "The run had ",
        nrow(errors),
        if (nrow(errors) == 1) " error, error 1. " else paste0(" errors, error 1 to error ", nrow(errors), ". "),
        errors_sentence(log)
      ),
      ""
    )
  }
  return(c(
    lines,
    md_failed_analyses(log$run$failed, log$run$n_analyses, errors),
    md_failed_assertions(checks_for_log(bad, errors)),
    md_contract(log$contract, errors),
    md_error(log$error, errors),
    md_output_failure(log$outputs$failed, errors)
  ))
}

# The failed analyses, grouped by their error. Each group names at most 10
# analyses.
md_failed_analyses <- function(failed, n, errors) {
  if (length(failed) == 0) {
    return(character(0))
  }
  lines <- c(
    "### Failed analyses",
    "",
    paste0(length(failed), " of ", n, " analyses failed."),
    ""
  )
  for (msg in unique(unname(failed))) {
    nm <- names(failed)[failed == msg]
    shown <- paste0("`", nm[seq_len(min(10, length(nm)))], "`")
    lines <- c(
      lines,
      paste0(
        "- ",
        if (length(nm) == 1) "1 analysis" else paste(length(nm), "analyses"),
        " failed with ",
        error_ref(errors, paste0("analysis\r", msg)),
        ": ",
        paste(shown, collapse = ", "),
        if (length(nm) > 10) paste0(" and ", length(nm) - 10, " more"),
        "."
      )
    )
  }
  return(c(lines, ""))
}

md_failed_assertions <- function(bad) {
  if (NROW(bad) == 0) {
    return(character(0))
  }
  return(c(
    "### Failed assertions",
    "",
    md_table(data.frame(
      Scenario = bad$scenario,
      Assertion = bad$assertion,
      Expected = bad$expected,
      Observed = bad$observed
    )),
    ""
  ))
}

# The failed rules of check_results(): the object and the rule, which pbn
# names, and the number of the error with the detail, which can quote a
# name of the project.
md_contract <- function(contract, errors) {
  if (length(contract) == 0) {
    return(character(0))
  }
  where <- regmatches(contract, regexec("^(.*?): ([A-Za-z0-9_.]+): ", contract))
  label <- vapply(
    where,
    function(m) return(if (length(m) == 3) paste0(m[2], ": ", m[3]) else "a rule"),
    character(1)
  )
  return(c(
    "### Failed rules of check_results()",
    "",
    paste0(
      "save_results() wrote no results file, because these rules of ",
      "check_results() failed:"
    ),
    "",
    paste0("- ", md_inline(label), ", ", error_ref(errors, paste0("contract\r", contract)), "."),
    ""
  ))
}

md_error <- function(error, errors) {
  if (is.null(error)) {
    return(character(0))
  }
  return(c(
    "### Error of save_results()",
    "",
    paste0("save_results() stopped with ", error_ref(errors, "save"), "."),
    ""
  ))
}

md_output_failure <- function(failed, errors) {
  if (is.null(failed)) {
    return(character(0))
  }
  return(c(
    "### Error of make_outputs()",
    "",
    paste0(
      "make_outputs() stopped at the scenario ",
      failed$scenario,
      " with ",
      error_ref(errors, "output"),
      "."
    ),
    ""
  ))
}

md_scenarios <- function(log) {
  s <- log$scenarios
  return(c(
    "## Scenarios",
    "",
    md_table(
      data.frame(
        Scenario = s$scenario,
        Simulations = s$sims,
        Analyses = s$analyses
      ),
      right = c(FALSE, TRUE, TRUE)
    ),
    ""
  ))
}

md_data <- function(log) {
  rows <- log$run$rows
  records <- log$run$data
  data <- unique(c(rows$data, records$data))
  scen <- log$scenarios
  out <- list()
  for (d in data) {
    for (i in seq_len(nrow(scen))) {
      s <- scen$scenario[i]
      r <- rows[rows$data == d & rows$scenario == s, , drop = FALSE]
      out[[length(out) + 1]] <- data.frame(
        Data = d,
        Scenario = s,
        Simulations = scen$sims[i],
        Cache = cache_cell(records[
          records$data == d & records$scenario %in% s,
        ]),
        `Rows before exclusions` = range_cell(r$n_before),
        `Rows after exclusions` = range_cell(r$n_after),
        `Rows of the data` = range_cell(r$n_rows),
        check.names = FALSE
      )
    }
  }
  lines <- c("## Data", "")
  if (length(out) > 0) {
    lines <- c(
      lines,
      md_table(
        do.call(rbind, out),
        right = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
      ),
      ""
    )
  }
  return(c(lines, md_cache_lines(records, scen$scenario, data)))
}

# How the data of one data element and scenario came, from its records:
# "no cache", or the number of simulations computed and reused, and failed.
cache_cell <- function(records) {
  if (NROW(records) == 0) {
    return("n/a")
  }
  parts <- if (all(is.na(records$line))) {
    "no cache"
  } else {
    c(
      if (any(!records$reused & !records$failed)) {
        paste("computed", sum(!records$reused & !records$failed))
      },
      if (any(records$reused)) paste("reused", sum(records$reused))
    )
  }
  if (any(records$failed)) {
    parts <- c(parts, paste("failed", sum(records$failed)))
  }
  return(paste(parts, collapse = ", "))
}

# The numbers `x` as one number, "<min> to <max>", or "n/a".
range_cell <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("n/a")
  }
  if (min(x) == max(x)) {
    return(as.character(min(x)))
  }
  return(paste(min(x), "to", max(x)))
}

# The cache line of simulation 1 of each scenario, as run_plan() printed it.
md_cache_lines <- function(records, scenarios, data) {
  first <- records[!is.na(records$line) & records$sim %in% 1L, , drop = FALSE]
  if (nrow(first) == 0) {
    return(character(0))
  }
  first <- first[
    order(match(first$scenario, scenarios), match(first$data, data)),
    ,
    drop = FALSE
  ]
  return(c(
    "The cache line of simulation 1 of each scenario, as run_plan() printed it:",
    "",
    md_code(first$line),
    ""
  ))
}

md_exclusions <- function(log) {
  tables <- log$run$exclusions
  lines <- c("## Exclusions", "")
  if (all(vapply(tables, is.null, logical(1)))) {
    return(c(
      lines,
      "No data of this run has a cohort, so the run has no exclusion table.",
      ""
    ))
  }
  scen <- log$scenarios
  for (i in seq_len(nrow(scen))) {
    s <- scen$scenario[i]
    x <- tables[[s]]
    lines <- c(lines, paste("###", s), "")
    if (scen$sims[i] > 1) {
      lines <- c(
        lines,
        paste0(
          "The table is per simulation. This is simulation 1 of ",
          scen$sims[i],
          "."
        ),
        ""
      )
    }
    if (is.null(x)) {
      lines <- c(lines, "No exclusion table.", "")
      next
    }
    lines <- c(
      lines,
      md_table(
        data.frame(
          Data = x$data,
          Step = x$step,
          `Rows before` = x$n_before,
          Excluded = x$n_excluded,
          `Rows after` = x$n_after,
          check.names = FALSE
        ),
        right = c(FALSE, FALSE, TRUE, TRUE, TRUE)
      ),
      ""
    )
  }
  return(lines)
}

md_assertions <- function(log) {
  lines <- c("## Assertions", "")
  if (!log$saved) {
    return(c(
      lines,
      if (isTRUE(log$preflight)) {
        "save_results() stopped before it saved anything, so the run has no assertion."
      } else {
        "save_results() did not run, so the run has no assertion."
      },
      ""
    ))
  }
  checks <- log$checks
  if (is.null(checks)) {
    return(c(
      lines,
      "save_results() stopped before the expect_ functions ran, so the run has no assertion.",
      ""
    ))
  }
  with_fn <- names(log$expected)[log$expected]
  if (length(with_fn) > 0) {
    lines <- c(
      lines,
      md_table(
        data.frame(
          Scenario = with_fn,
          Passed = vapply(
            with_fn,
            function(s) {
              return(sum(checks$pass[checks$scenario == s] %in% TRUE))
            },
            integer(1)
          ),
          Failed = vapply(
            with_fn,
            function(s) {
              return(sum(!checks$pass[checks$scenario == s] %in% TRUE))
            },
            integer(1)
          )
        ),
        right = c(FALSE, TRUE, TRUE)
      ),
      ""
    )
  }
  for (s in names(log$expected)[!log$expected]) {
    lines <- c(
      lines,
      paste0(s, " has no expect_", s, "(), so it has no assertion."),
      ""
    )
  }
  checks <- checks_for_log(checks, log$errors)
  for (s in with_fn) {
    lines <- c(
      lines,
      md_scenario_checks(
        checks[checks$scenario == s, , drop = FALSE],
        s,
        log$version
      )
    )
  }
  return(lines)
}

# The assertions of the scenario `s`: all of them up to log_max_assertions,
# and the failed ones only above it.
md_scenario_checks <- function(x, s, version) {
  lines <- c(paste("###", s), "")
  bad <- !x$pass %in% TRUE
  if (nrow(x) > log_max_assertions) {
    csv <- paste0(version, "_results/", s, "/check.csv")
    lines <- c(
      lines,
      paste0(
        s,
        " has ",
        nrow(x),
        " assertions, more than ",
        log_max_assertions,
        if (any(bad)) {
          ", so the table holds the failed ones only."
        } else {
          ", and all of them passed."
        },
        " The full list is in ",
        csv,
        "."
      ),
      ""
    )
    x <- x[bad, , drop = FALSE]
    if (nrow(x) == 0) {
      return(lines)
    }
  }
  return(c(
    lines,
    md_table(data.frame(
      Assertion = x$assertion,
      Expected = x$expected,
      Observed = x$observed,
      Pass = ifelse(x$pass %in% TRUE, "yes", "no")
    )),
    ""
  ))
}

md_outputs <- function(log) {
  lines <- c("## Output", "")
  o <- log$outputs
  if (is.null(o)) {
    return(c(
      lines,
      "make_outputs() did not run after save_results() in this R session.",
      ""
    ))
  }
  scen <- union(union(names(o$files), names(o$notes)), o$failed$scenario)
  if (length(scen) == 0) {
    return(c(lines, "make_outputs() had no scenario to write.", ""))
  }
  lines <- c(lines, "make_outputs() wrote these files:", "")
  for (s in scen) {
    lines <- c(lines, output_items(o, s))
  }
  return(c(lines, ""))
}

# The list items of make_outputs() for the scenario `s`: the stop, the
# printed note of a skipped scenario, or the files.
output_items <- function(o, s) {
  note <- o$notes[[s]]
  files <- o$files[[s]]$file
  stopped <- identical(o$failed$scenario, s)
  if (stopped) {
    return(paste0("- ", s, ": make_outputs() stopped here, see the section Failures."))
  }
  if (!is.null(note)) {
    return(paste0("- ", s, ": ", md_inline(sub("^pbn: ", "", note))))
  }
  if (length(files) == 0) {
    return(paste0("- ", s, ": no file."))
  }
  return(paste0("- ", s, ": ", md_inline(files)))
}

md_files <- function(log) {
  lines <- c("## Files", "")
  if (!log$saved) {
    return(c(
      lines,
      if (isTRUE(log$preflight)) {
        "save_results() stopped before it wrote a results file."
      } else {
        "save_results() did not run, so it wrote no results file."
      },
      ""
    ))
  }
  if (NROW(log$files) == 0) {
    return(c(lines, "save_results() wrote no results file.", ""))
  }
  return(c(
    lines,
    "These are the results files of the scenarios of this run:",
    "",
    md_table(
      data.frame(File = log$files$file, Size = log$files$size),
      right = c(FALSE, TRUE)
    ),
    ""
  ))
}

md_timings <- function(log) {
  run <- log$run
  analyses <- run$analyses[!is.na(run$analyses$seconds), , drop = FALSE]
  computing <- sum(run$data$seconds) + sum(analyses$seconds)
  where <- if (isTRUE(log$workers > 1)) {
    paste0("Summed over the ", log$workers, " workers")
  } else {
    "In this R session"
  }
  return(c(
    "## Timings",
    "",
    paste0(
      "The numbers of this section change in every run. pbn measures each ",
      "one with proc.time(), in the R session that does the work."
    ),
    "",
    "### Main session",
    "",
    md_table(main_timings(log), right = c(FALSE, TRUE)),
    "",
    paste0(
      where,
      ", the data and the analyses took ",
      secs(computing),
      " computing seconds. run_plan() took ",
      secs(log$timing$run),
      " wall seconds."
    ),
    "",
    "### Scenarios, in computing seconds",
    "",
    md_table(scenario_timings(log), right = c(FALSE, TRUE, TRUE, TRUE)),
    "",
    "### Data steps, in computing seconds",
    "",
    md_seconds_table(
      run$steps,
      c("data", "scenario", "step"),
      c("Data", "Scenario", "Step"),
      "Calls",
      log$scenarios$scenario
    ),
    "",
    "### Analysis kinds, in computing seconds",
    "",
    md_seconds_table(
      analyses,
      c("scenario", "kind"),
      c("Scenario", "Kind"),
      "Analyses",
      log$scenarios$scenario
    ),
    ""
  ))
}

# Seconds with two decimals.
secs <- function(x) {
  out <- sprintf("%.2f", x)
  out[is.na(x)] <- "n/a"
  return(out)
}

# The wall seconds of the steps of the main session.
main_timings <- function(log) {
  t <- log$timing
  steps <- c("run_plan()" = t$run)
  if (!is.null(t$save)) {
    steps <- c(
      steps,
      "save_results(): build, write and stack the results" = t$save[["write"]],
      "save_results(): the checks of check_results()" = t$save[["contract"]],
      "save_results(): the expect_ functions" = t$save[["expect"]],
      "save_results() in total" = t$save[["total"]]
    )
  }
  if (length(t$outputs) > 0) {
    steps <- c(
      steps,
      stats::setNames(t$outputs, paste0("make_outputs(): ", names(t$outputs)))
    )
  }
  return(data.frame(
    Step = names(steps),
    `Wall seconds` = secs(steps),
    check.names = FALSE
  ))
}

# The computing seconds of the data and the analyses of each scenario.
scenario_timings <- function(log) {
  s <- log$scenarios$scenario
  run <- log$run
  data <- vapply(
    s,
    function(x) return(sum(run$data$seconds[run$data$scenario %in% x])),
    numeric(1)
  )
  analyses <- vapply(
    s,
    function(x) {
      return(sum(
        run$analyses$seconds[run$analyses$scenario == x],
        na.rm = TRUE
      ))
    },
    numeric(1)
  )
  return(data.frame(
    Scenario = s,
    Data = secs(data),
    Analyses = secs(analyses),
    Total = secs(data + analyses)
  ))
}

# A table of the seconds of `x` for each group of the columns `by`, with
# the count, the total, the median and the maximum. The groups come in the
# order of the scenarios `scenarios`, then in their first order in `x`. A
# missing scenario is "all": data without the arguments scenario and sim.
md_seconds_table <- function(x, by, labels, count, scenarios) {
  if (nrow(x) == 0) {
    return("None.")
  }
  x$scenario[is.na(x$scenario)] <- "all"
  key <- do.call(paste, c(unname(as.list(x[by])), sep = "\r"))
  first <- !duplicated(key)
  groups <- x[first, by, drop = FALSE]
  keys <- key[first]
  o <- order(match(groups$scenario, c(scenarios, "all")), seq_along(keys))
  groups <- groups[o, , drop = FALSE]
  keys <- keys[o]
  stat <- function(f) {
    return(vapply(
      keys,
      function(k) return(f(x$seconds[key == k])),
      numeric(1),
      USE.NAMES = FALSE
    ))
  }
  out <- stats::setNames(groups, labels)
  out[[count]] <- vapply(
    keys,
    function(k) return(sum(key == k)),
    integer(1),
    USE.NAMES = FALSE
  )
  out$Total <- secs(stat(sum))
  out$Median <- secs(stat(stats::median))
  out$Maximum <- secs(stat(max))
  rownames(out) <- NULL
  return(md_table(
    out,
    right = c(rep(FALSE, length(by)), TRUE, TRUE, TRUE, TRUE)
  ))
}

# The lines `x` as one fenced code block. The fence is longer than each run
# of backticks in `x`.
md_code <- function(x) {
  x <- unlist(strsplit(x, "\r?\n"))
  runs <- unlist(regmatches(x, gregexpr("`+", x)))
  fence <- strrep("`", max(3, nchar(runs) + 1))
  return(c(fence, x, fence))
}

# The cells of `x` for a Markdown table: NA is "n/a", a line end is a space
# and a | is \|.
md_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "n/a"
  x <- md_flat(x)
  x <- trimws(gsub("|", "\\|", x, fixed = TRUE))
  x[!nzchar(x)] <- " "
  return(x)
}

# The data.frame `df` as a Markdown pipe table, with the columns of `right`
# aligned right. Pandoc reads the number of dashes under each header as the
# relative width of the column when a row is wider than 72 characters, and
# then it wraps the cells. So a long text gets a wide column, and the table
# stays on the page in a PDF.
md_table <- function(df, right = rep(FALSE, ncol(df))) {
  cells <- lapply(df, md_cell)
  header <- md_cell(names(df))
  width <- vapply(
    seq_along(cells),
    function(j) return(max(nchar(c(header[j], cells[[j]])))),
    numeric(1)
  )
  # Each cell is padded to the width of its column, so the table reads well
  # as plain text. The dashes give the relative widths for pandoc.
  pad <- function(x, j) {
    return(formatC(x, width = if (right[j]) width[j] else -width[j]))
  }
  row <- function(x) {
    x <- vapply(seq_along(x), function(j) return(pad(x[[j]], j)), "")
    return(paste0("| ", paste(x, collapse = " | "), " |"))
  }
  rule <- strrep("-", pmin(width, 60) + 2)
  rule[right] <- sub("-$", ":", rule[right])
  body <- vapply(
    seq_len(nrow(df)),
    function(i) return(row(lapply(cells, function(col) return(col[[i]])))),
    ""
  )
  return(c(row(header), paste0("|", paste(rule, collapse = "|"), "|"), body))
}
