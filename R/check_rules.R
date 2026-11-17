# The rule functions behind check_project(). The rules behind
# check_results() are in check_rules_results.R.
#
# A project rule returns one row per finding: rule, status, path, detail.
# A project rule with no finding returns zero rows. project_findings() then
# adds its PASS row.

pbn_exports <- c(
  "check_project",
  "check_results",
  "end_if_fake_scenario",
  "initialize_project",
  "read_results",
  "run_scenarios",
  "save_results",
  "table1",
  "use_scenario"
)

run_script_pattern <- "^v[0-9]{2}_run\\.R$"
functions_pattern <- "^v[0-9]{2}_functions$"
manual_pattern <- "^v[0-9]{2}_[A-Z]{2,4}[0-9]?\\.docx$"
max_code_lines <- 1000

# project rules ---------------------------------------------------------------

project_row <- function(rule, status, path, detail) {
  return(data.frame(
    rule = rep(rule, length(path)),
    status = rep(status, length(path)),
    path = path,
    detail = detail
  ))
}

# The files and folders of an OS or an editor. No rule reads them.
dropping_files <- c(".DS_Store", ".Rhistory", ".RData")
dropping_folders <- c(".git", ".Rproj.user")

# Every file and folder that the rules read, as paths relative to `path`.
# When `path` is the top folder of a git repository, these are the files
# that git tracks or that git does not ignore, and their folders. Otherwise
# they are the files and folders on disk. The droppings are left out in both
# cases.
project_entries <- function(path) {
  files <- git_files(path)
  if (is.null(files)) {
    out <- disk_entries(path)
  } else {
    out <- c(files, parent_folders(files))
  }
  parts <- strsplit(out, "/", fixed = TRUE)
  dropping <- vapply(
    parts,
    function(p) {
      name <- p[length(p)]
      return(
        any(p %in% dropping_folders) ||
          name %in% dropping_files ||
          startsWith(name, "~$")
      )
    },
    logical(1)
  )
  return(out[!dropping])
}

disk_entries <- function(path) {
  top <- list.files(path, all.files = TRUE, no.. = TRUE)
  top <- setdiff(top, dropping_folders)
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

# `git ls-files --cached --others --exclude-standard` in `path`, or NULL when
# `path` is not the top folder of a git repository. A tracked file that is
# deleted on disk is left out.
git_files <- function(path) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    return(NULL)
  }
  top <- suppressWarnings(system2(
    git,
    c("-C", shQuote(path), "rev-parse", "--show-toplevel"),
    stdout = TRUE,
    stderr = FALSE
  ))
  if (!is.null(attr(top, "status")) || length(top) != 1) {
    return(NULL)
  }
  if (!identical(normalizePath(top), normalizePath(path))) {
    return(NULL)
  }
  files <- system2(
    git,
    c(
      "-c",
      "core.quotepath=off",
      "-C",
      shQuote(path),
      "ls-files",
      "--cached",
      "--others",
      "--exclude-standard"
    ),
    stdout = TRUE,
    stderr = FALSE
  )
  if (!is.null(attr(files, "status"))) {
    stop("git ls-files failed in ", path, call. = FALSE)
  }
  files <- unique(files)
  return(files[file.exists(file.path(path, files))])
}

# Every folder above the files, as paths relative to the project folder.
parent_folders <- function(files) {
  out <- character(0)
  folders <- dirname(files)
  while (length(folders) > 0) {
    folders <- unique(folders[folders != "."])
    out <- c(out, folders)
    folders <- dirname(folders)
  }
  return(unique(out))
}

# The top-level entries of `entries`.
top_entries <- function(entries) {
  return(unique(sub("/.*$", "", entries)))
}

# A dotfile or an RStudio project file at the top level. The house layout
# allows these, whatever their name.
is_root_tool_file <- function(path, entries) {
  return(
    !grepl("/", entries, fixed = TRUE) &
      (startsWith(entries, ".") | grepl("\\.Rproj$", entries)) &
      !dir.exists(file.path(path, entries))
  )
}

run_scripts <- function(path) {
  entries <- project_entries(path)
  return(entries[
    grepl(run_script_pattern, entries) & !dir.exists(file.path(path, entries))
  ])
}

functions_folders <- function(path) {
  entries <- project_entries(path)
  return(entries[
    grepl(functions_pattern, entries) & dir.exists(file.path(path, entries))
  ])
}

# The .R files in the vNN_functions/ folders, relative to `path`. This is the
# file pattern that org uses to choose the files it sources.
functions_files <- function(path) {
  entries <- project_entries(path)
  keep <- dirname(entries) %in%
    functions_folders(path) &
    grepl("\\.[rR]$", entries) &
    !dir.exists(file.path(path, entries))
  return(entries[keep])
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
    !(entries == "background" | startsWith(entries, "background/")) &
      !is_root_tool_file(path, entries)
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
    unknown <- unlist(lapply(calls, init_unknown_args))
    if (!version %in% versions) {
      detail <- paste0(
        s,
        ": no call pbn::initialize_project(version = \"",
        version,
        "\")"
      )
      if (length(unknown) > 0) {
        detail <- paste0(
          detail,
          ". pbn::initialize_project() has no argument ",
          paste(unique(unknown), collapse = ", ")
        )
      }
      out[[s]] <- project_row(rule, "FAIL", s, detail)
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

# The names of the arguments of one pbn::initialize_project() call that the
# function does not have.
init_unknown_args <- function(cl) {
  given <- names(as.list(cl))[-1]
  given <- given[!is.na(given) & nzchar(given)]
  return(setdiff(given, names(formals(initialize_project))))
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
  entries <- project_entries(path)
  is_file <- !dir.exists(file.path(path, entries))
  files <- entries[
    grepl(
      "^v[0-9]{2}_results/[^/]+/(sim_[0-9]{4}/)?results\\.qs2$",
      entries
    ) &
      is_file
  ]
  old_place <- entries[
    grepl("^v[0-9]{2}_results/results\\.qs2$", entries) & is_file
  ]
  out <- list(project_row(
    rule,
    "FAIL",
    old_place,
    paste0(
      old_place,
      ": a results file in the old place. pbn writes ",
      "vNN_results/<scenario>/results.qs2",
      recycle0 = TRUE
    )
  ))
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
  entries <- project_entries(path)
  found <- basename(entries[dirname(entries) == "manuscript/manual"])
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
    "^run_scenarios\\.R$",
    "^v[0-9]{2}_check_functions\\.R$",
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
  entries <- project_entries(path)
  top <- top_entries(entries[!is_root_tool_file(path, entries)])
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
    no_active_use_scenario = list(
      rule_no_active_use_scenario,
      "no vNN_run.R has an active call pbn::use_scenario()"
    ),
    use_scenario_lines = list(
      rule_use_scenario_lines,
      paste(
        "every vNN_run.R has one line # pbn::use_scenario(\"<name>\", sim = 1)",
        "for each scenario in CFG$scenarios, above pbn::initialize_project()"
      )
    ),
    version_items_complete = list(
      rule_version_items_complete,
      "every vNN_run.R has a vNN_functions/, and the reverse"
    ),
    results_contract = list(
      rule_results_contract,
      paste(
        "every vNN_results/<scenario>/results.qs2 and",
        "vNN_results/<scenario>/sim_NNNN/results.qs2 passes check_results()"
      )
    ),
    scenario_checks = list(
      rule_scenario_checks,
      "every vNN_results/<scenario>/check.csv passes"
    ),
    manual_manuscript_names = list(
      rule_manual_manuscript_names,
      "every file in manuscript/manual/ is named vNN_<INITIALS>.docx"
    ),
    manuscript_reads_real = list(
      rule_manuscript_reads_real,
      "manuscript/built/ reads only the real results"
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
