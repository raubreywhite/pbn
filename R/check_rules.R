# The rule functions behind check_results() and check_project().
#
# A results rule returns one row: rule, status, detail.
# A project rule returns one row per finding: rule, status, path, detail.
# A project rule with no finding returns zero rows. project_findings() then
# adds its PASS row.

pbn_exports <- c(
  "check_project",
  "check_results",
  "initialize_project",
  "read_results",
  "run_version",
  "save_results",
  "table1"
)

results_names <- c("manifest", "exclusions", "estimates", "tables", "extra")
exclusions_columns <- c("step", "n_before", "n_excluded", "n_after")
model_classes <- c("lm", "glm", "gam", "merMod", "brmsfit", "coxph", "lavaan")
run_script_pattern <- "^v[0-9]{2}_run\\.R$"
functions_pattern <- "^v[0-9]{2}_functions$"
manual_pattern <- "^v[0-9]{2}_[A-Z]{2,4}[0-9]?\\.docx$"
max_code_lines <- 1000

# results rules ---------------------------------------------------------------

result_row <- function(rule, failures, pass_detail) {
  if (length(failures) == 0) {
    return(data.frame(rule = rule, status = "PASS", detail = pass_detail))
  }
  return(data.frame(
    rule = rule,
    status = "FAIL",
    detail = paste(failures, collapse = "; ")
  ))
}

rule_top_level_names <- function(x) {
  rule <- "top_level_names"
  if (!is.list(x) || is.data.frame(x)) {
    return(result_row(rule, "the object is not a list", ""))
  }
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep("", length(x))
  }
  failures <- character(0)
  missing <- setdiff(results_names, nm)
  if (length(missing) > 0) {
    failures <- c(failures, paste("missing:", paste(missing, collapse = ", ")))
  }
  others <- setdiff(nm, results_names)
  if (length(others) > 0) {
    failures <- c(
      failures,
      paste("not allowed:", paste(dQuote(others, FALSE), collapse = ", "))
    )
  }
  twice <- unique(nm[duplicated(nm)])
  if (length(twice) > 0) {
    failures <- c(
      failures,
      paste("more than once:", paste(twice, collapse = ", "))
    )
  }
  return(result_row(rule, failures, "the five top-level names, and no others"))
}

rule_exclusions_table <- function(x) {
  rule <- "exclusions_table"
  e <- if (is.list(x)) x[["exclusions"]] else NULL
  if (!is.data.frame(e)) {
    return(result_row(rule, "exclusions is not a data.frame", ""))
  }
  if (!setequal(names(e), exclusions_columns) || ncol(e) != 4) {
    return(result_row(
      rule,
      paste0(
        "the columns are ",
        paste(names(e), collapse = ", "),
        "; they must be ",
        paste(exclusions_columns, collapse = ", ")
      ),
      ""
    ))
  }
  counts <- e[, exclusions_columns[-1], drop = FALSE]
  if (!all(vapply(counts, is.numeric, logical(1)))) {
    return(result_row(
      rule,
      "n_before, n_excluded and n_after must be numeric",
      ""
    ))
  }
  ok <- e$n_before - e$n_excluded == e$n_after
  bad <- which(is.na(ok) | !ok)
  failures <- if (length(bad) > 0) {
    paste0(
      "n_before - n_excluded != n_after on these rows: ",
      paste(bad, collapse = ", ")
    )
  } else {
    character(0)
  }
  return(result_row(
    rule,
    failures,
    paste0("the counts add up on every row; the number of rows is ", nrow(e))
  ))
}

rule_no_fitted_models <- function(x) {
  where <- find_paths(x, "x", is_fitted_model)
  failures <- if (length(where) > 0) {
    paste("a fitted model at", paste(where, collapse = ", "))
  } else {
    character(0)
  }
  return(result_row(
    "no_fitted_models",
    failures,
    "no fitted model in the object"
  ))
}

rule_no_row_per_person <- function(x) {
  rule <- "no_row_per_person"
  n <- if (is.list(x) && is.list(x[["manifest"]])) {
    x[["manifest"]][["n_rows_data"]]
  } else {
    NULL
  }
  if (length(n) != 1 || is.na(n)) {
    return(result_row(
      rule,
      character(0),
      "n_rows_data is NA, so nothing to check"
    ))
  }
  where <- find_paths(x, "x", function(o) {
    return(is.data.frame(o) && nrow(o) == n)
  })
  failures <- if (length(where) > 0) {
    paste0(
      "a data.frame with ",
      n,
      " rows (n_rows_data) at ",
      paste(where, collapse = ", ")
    )
  } else {
    character(0)
  }
  return(result_row(rule, failures, paste0("no data.frame has ", n, " rows")))
}

is_fitted_model <- function(o) {
  return(inherits(o, model_classes))
}

# The paths of every element of `obj` for which `test` is TRUE. The walk does
# not go into a match, and it does not go into a fitted model:
# no_fitted_models reports a model, so its model frame is not reported again.
find_paths <- function(obj, where, test) {
  if (test(obj)) {
    return(where)
  }
  if (!is.list(obj) || is_fitted_model(obj)) {
    return(character(0))
  }
  nm <- names(obj)
  out <- character(0)
  for (i in seq_along(obj)) {
    step <- if (!is.null(nm) && nzchar(nm[i])) {
      paste0(where, "$", nm[i])
    } else {
      paste0(where, "[[", i, "]]")
    }
    out <- c(out, find_paths(obj[[i]], step, test))
  }
  return(out)
}

# project rules ---------------------------------------------------------------

project_row <- function(rule, status, path, detail) {
  return(data.frame(
    rule = rep(rule, length(path)),
    status = rep(status, length(path)),
    path = path,
    detail = detail
  ))
}

# Every file and folder in the project, as paths relative to `path`.
# .git/ is left out.
project_entries <- function(path) {
  top <- list.files(path, all.files = TRUE, no.. = TRUE)
  top <- setdiff(top, ".git")
  out <- top
  for (t in top[dir.exists(file.path(path, top))]) {
    inner <- list.files(
      file.path(path, t),
      all.files = TRUE,
      recursive = TRUE,
      include.dirs = TRUE,
      no.. = TRUE
    )
    out <- c(out, file.path(t, inner))
  }
  return(out)
}

run_scripts <- function(path) {
  return(list.files(path, pattern = run_script_pattern))
}

functions_folders <- function(path) {
  found <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  return(grep(functions_pattern, found, value = TRUE))
}

# The .R files in the vNN_functions/ folders, relative to `path`. This is the
# file pattern that org uses to choose the files it sources.
functions_files <- function(path) {
  out <- character(0)
  for (f in functions_folders(path)) {
    out <- c(
      out,
      file.path(f, list.files(file.path(path, f), pattern = "\\.[rR]$"))
    )
  }
  return(out)
}

parse_or_null <- function(file, keep_source = FALSE) {
  return(tryCatch(
    parse(file, keep.source = keep_source),
    error = function(e) return(NULL)
  ))
}

rule_lower_case_names <- function(path) {
  rule <- "lower_case_names"
  exempt <- c(
    "README.md",
    "CHANGELOG.md",
    "AGENTS.md",
    "CLAUDE.md",
    "LICENSE",
    "DESCRIPTION"
  )
  entries <- project_entries(path)
  entries <- entries[
    !(entries == "background" | startsWith(entries, "background/"))
  ]
  name <- basename(entries)
  is_dir <- dir.exists(file.path(path, entries))
  stem <- ifelse(is_dir, name, tools::file_path_sans_ext(name))
  manual <- dirname(entries) == "manuscript/manual" &
    grepl(manual_pattern, name)
  bad <- grepl("[A-Z]", stem) & !name %in% exempt & !manual
  return(project_row(
    rule,
    "FAIL",
    entries[bad],
    paste0(entries[bad], ": the name has a capital letter", recycle0 = TRUE)
  ))
}

rule_run_script_initialises <- function(path) {
  rule <- "run_script_initialises"
  out <- list()
  for (s in run_scripts(path)) {
    version <- substr(s, 1, 3)
    exprs <- parse_or_null(file.path(path, s))
    if (is.null(exprs)) {
      out[[s]] <- project_row(rule, "FAIL", s, paste0(s, ": R cannot parse it"))
      next
    }
    calls <- find_calls(exprs, function(cl) {
      return(identical(cl[[1]], quote(pbn::initialize_project)))
    })
    versions <- vapply(calls, init_version, character(1))
    if (!version %in% versions) {
      out[[s]] <- project_row(
        rule,
        "FAIL",
        s,
        paste0(
          s,
          ": no call pbn::initialize_project(version = \"",
          version,
          "\")"
        )
      )
    }
  }
  return(bind_rows(out))
}

# Every call in `x` for which `test` is TRUE. Only calls are walked, because
# a missing argument (as in x[, 1]) cannot be bound to a variable.
find_calls <- function(x, test) {
  out <- list()
  if (is.call(x) && test(x)) {
    out <- list(x)
  }
  if (is.call(x) || is.expression(x)) {
    parts <- as.list(x)
    for (i in seq_along(parts)) {
      if (is.call(parts[[i]])) {
        out <- c(out, find_calls(parts[[i]], test))
      }
    }
  }
  return(out)
}

# The version argument of one pbn::initialize_project() call, when it is one
# string, else NA.
init_version <- function(cl) {
  matched <- tryCatch(
    match.call(initialize_project, cl),
    error = function(e) return(NULL)
  )
  v <- if (is.null(matched)) NULL else matched$version
  if (is.character(v) && length(v) == 1) {
    return(v)
  }
  return(NA_character_)
}

rule_version_items_complete <- function(path) {
  rule <- "version_items_complete"
  runs <- substr(run_scripts(path), 1, 3)
  funs <- substr(functions_folders(path), 1, 3)
  no_funs <- setdiff(runs, funs)
  no_run <- setdiff(funs, runs)
  return(rbind(
    project_row(
      rule,
      "FAIL",
      paste0(no_funs, "_run.R", recycle0 = TRUE),
      paste0(
        no_funs,
        "_run.R: there is no ",
        no_funs,
        "_functions/",
        recycle0 = TRUE
      )
    ),
    project_row(
      rule,
      "FAIL",
      paste0(no_run, "_functions", recycle0 = TRUE),
      paste0(
        no_run,
        "_functions/: there is no ",
        no_run,
        "_run.R",
        recycle0 = TRUE
      )
    )
  ))
}

rule_results_contract <- function(path) {
  rule <- "results_contract"
  folders <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  folders <- grep("^v[0-9]{2}_results$", folders, value = TRUE)
  files <- file.path(folders, "results.qs2")
  files <- files[file.exists(file.path(path, files))]
  out <- list()
  for (f in files) {
    x <- tryCatch(qs2::qs_read(file.path(path, f)), error = function(e) {
      return(e)
    })
    if (inherits(x, "error")) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": qs2 cannot read it")
      )
      next
    }
    found <- check_results(x)
    failed <- found[found$status == "FAIL", , drop = FALSE]
    if (nrow(failed) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": ",
          paste(failed$rule, failed$detail, sep = ": ", collapse = "; ")
        )
      )
    }
  }
  return(bind_rows(out))
}

rule_manual_manuscript_names <- function(path) {
  rule <- "manual_manuscript_names"
  folder <- file.path(path, "manuscript", "manual")
  found <- list.files(folder)
  bad <- found[!grepl(manual_pattern, found)]
  where <- file.path("manuscript", "manual", bad)
  return(project_row(
    rule,
    "FAIL",
    where,
    paste0(where, ": the name must be vNN_<INITIALS>.docx", recycle0 = TRUE)
  ))
}

rule_no_local_copy_of_pbn_function <- function(path) {
  rule <- "no_local_copy_of_pbn_function"
  out <- list()
  for (f in functions_files(path)) {
    exprs <- parse_or_null(file.path(path, f))
    if (is.null(exprs)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
      next
    }
    defined <- character(0)
    for (e in as.list(exprs)) {
      if (is_function_assignment(e) && as.character(e[[2]]) %in% pbn_exports) {
        defined <- c(defined, as.character(e[[2]]))
      }
    }
    if (length(defined) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": defines ",
          paste(defined, collapse = ", "),
          ", a pbn export"
        )
      )
    }
  }
  return(bind_rows(out))
}

is_function_assignment <- function(e) {
  if (!is.call(e) || length(e) != 3 || !is.symbol(e[[1]])) {
    return(FALSE)
  }
  if (!as.character(e[[1]]) %in% c("<-", "=", "<<-") || !is.symbol(e[[2]])) {
    return(FALSE)
  }
  value <- e[[3]]
  return(is.call(value) && identical(value[[1]], as.name("function")))
}

rule_loc_per_file <- function(path) {
  rule <- "loc_per_file"
  entries <- project_entries(path)
  files <- entries[
    grepl("\\.[rR]$", entries) & !dir.exists(file.path(path, entries))
  ]
  out <- list()
  for (f in files) {
    n <- tryCatch(
      org::loc_per_file(file.path(path, f)),
      error = function(e) return(NA_integer_)
    )
    if (is.na(n)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
    } else if (n > max_code_lines) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": ", n, " code lines, more than ", max_code_lines)
      )
    }
  }
  return(bind_rows(out))
}

rule_no_environment_variables <- function(path) {
  rule <- "no_environment_variables"
  files <- c(run_scripts(path), functions_files(path))
  out <- list()
  for (f in files) {
    exprs <- parse_or_null(file.path(path, f), keep_source = TRUE)
    if (is.null(exprs)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
      next
    }
    tokens <- utils::getParseData(exprs)
    hits <- tokens[
      tokens$token %in%
        c("SYMBOL_FUNCTION_CALL", "SYMBOL") &
        tokens$text %in% c("Sys.getenv", "Sys.setenv"),
      ,
      drop = FALSE
    ]
    if (nrow(hits) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": ",
          paste0(hits$text, " on line ", hits$line1, collapse = ", ")
        )
      )
    }
  }
  return(bind_rows(out))
}

rule_unknown_path <- function(path) {
  known <- c(
    run_script_pattern,
    functions_pattern,
    "^v[0-9]{2}_results$",
    "^v[0-9]{2}_test$",
    "^check_project\\.R$",
    "^manuscript$",
    "^background$",
    "^adhoc$",
    "^data_describe$",
    "^README\\.md$",
    "^CHANGELOG\\.md$",
    "^AGENTS\\.md$",
    "^\\.gitignore$",
    "^\\.git$",
    "^pbn_allow\\.txt$"
  )
  top <- list.files(path, all.files = TRUE, no.. = TRUE)
  matched <- vapply(
    top,
    function(t) return(any(vapply(known, grepl, logical(1), x = t))),
    logical(1)
  )
  unknown <- top[!matched]
  return(project_row(
    "unknown_path",
    "WARN",
    unknown,
    paste0(unknown, ": the house layout does not name it", recycle0 = TRUE)
  ))
}

bind_rows <- function(rows) {
  if (length(rows) == 0) {
    return(project_row(character(0), character(0), character(0), character(0)))
  }
  return(do.call(rbind, unname(rows)))
}

# The rules in the order that check_project() prints them, with the detail of
# the PASS line.
project_rules <- function() {
  return(list(
    lower_case_names = list(
      rule_lower_case_names,
      "no capital letter in a name"
    ),
    run_script_initialises = list(
      rule_run_script_initialises,
      "every vNN_run.R calls pbn::initialize_project() with its own version"
    ),
    version_items_complete = list(
      rule_version_items_complete,
      "every vNN_run.R has a vNN_functions/, and the reverse"
    ),
    results_contract = list(
      rule_results_contract,
      "every vNN_results/results.qs2 passes check_results()"
    ),
    manual_manuscript_names = list(
      rule_manual_manuscript_names,
      "every file in manuscript/manual/ is named vNN_<INITIALS>.docx"
    ),
    no_local_copy_of_pbn_function = list(
      rule_no_local_copy_of_pbn_function,
      "no vNN_functions/ file defines a pbn export"
    ),
    loc_per_file = list(
      rule_loc_per_file,
      paste0("no .R file has more than ", max_code_lines, " code lines")
    ),
    no_environment_variables = list(
      rule_no_environment_variables,
      "no Sys.getenv or Sys.setenv in a run script or a functions folder"
    ),
    unknown_path = list(
      rule_unknown_path,
      "the house layout names every top-level path"
    )
  ))
}
