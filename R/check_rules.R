# The rule functions behind check_project(). The rules behind
# check_results() are in check_rules_results.R.
#
# A project rule returns one row per finding: rule, status, path, detail.
# A project rule with no finding returns zero rows. project_findings() then
# adds its PASS row.

pbn_exports <- c(
  "add_analyses",
  "add_data",
  "block",
  "check_one_file",
  "check_project",
  "check_results",
  "cohort_data",
  "create_project",
  "end_unless_real",
  "expect_biased",
  "expect_recovers",
  "export_blocks",
  "folder",
  "freeze_version",
  "get_argset",
  "get_data",
  "import_blocks",
  "initialize_project",
  "keep_exclusions",
  "make_checklists",
  "make_outputs",
  "new_version",
  "progress",
  "read_blocks",
  "read_results",
  "render_run",
  "run_plan",
  "save_results",
  "scenarios",
  "show_block",
  "show_blocks",
  "stamp_fake",
  "table1",
  "table1_styles",
  "to_one_file",
  "txt",
  "update_blocks",
  "verify"
)

# The exports that came after the set house_2026_11_17, each with the oldest
# set that knows it. The rule no_local_copy_of_pbn_function of an older set
# passes a function with such a name, as before.
new_exports <- c(
  block = "house_2026_11_26",
  create_project = "house_2026_11_19",
  export_blocks = "house_2026_11_26",
  freeze_version = "house_2026_11_19",
  import_blocks = "house_2026_11_26",
  make_checklists = "house_2026_11_26",
  new_version = "house_2026_11_19",
  read_blocks = "house_2026_11_26",
  table1_styles = "house_2026_11_20",
  txt = "house_2026_11_26",
  update_blocks = "house_2026_11_26",
  verify = "house_2026_11_19"
)

# The pbn exports that the rule no_local_copy_of_pbn_function of the set
# `set` knows.
set_exports <- function(set) {
  newer <- names(new_exports)[!set_is_from(set, new_exports)]
  return(setdiff(pbn_exports, newer))
}

# The pbn exports that a project MAY define in its functions folder. The
# migration table of NEWS.md names the data function of a project
# get_data(scenario, sim), and a run script calls the export as
# pbn::get_data(), so the two never meet.
local_copy_allowed <- "get_data"

run_script_pattern <- "^v[0-9]{2}_run\\.R$"
functions_pattern <- "^v[0-9]{2}_functions$"
# vNN_<INITIALS>.docx, or vNN_<INITIALS>_<suffix>.docx with one word of lower-case
# letters and digits as the suffix, for a second file of the same version.
manual_pattern <- "^v[0-9]{2}_[A-Z]{2,4}[0-9]?(_[a-z0-9]+)?\\.docx$"

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
# cases, and so is each folder of `ignore` with everything under it. A
# locked rule gives `ignore = character(0)`.
project_entries <- function(path, ignore = ignored_folders(path)) {
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
  out <- out[!dropping]
  top <- sub("/.*$", "", out)
  return(out[!top %in% ignore])
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
  # A draft of an agent in manuscript/draft/ has the names of a manual
  # manuscript, and stays outside the version counter.
  manual <- dirname(entries) %in%
    c("manuscript/manual", "manuscript/draft") &
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
    unknown <- lapply(calls, init_unknown_args)
    # A call with an argument that initialize_project() does not have stops
    # in its `...`, so it does not count.
    versions <- versions[lengths(unknown) == 0]
    unknown <- unlist(unknown)
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
  entries <- project_entries(path, ignore = character(0))
  is_file <- !dir.exists(file.path(path, entries))
  files <- entries[
    grepl(
      "^v[0-9]{2}_results/[^/]+/(sim_[0-9]{4}/)?results\\.qs2$",
      entries
    ) &
      is_file
  ]
  files <- union(files, sim_files_on_disk(path, entries))
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
    problems <- paste(failed$rule, failed$detail, sep = ": ", recycle0 = TRUE)
    # results_gitexclude.qs2 MAY hold data for each person. It MUST hold the
    # kinds that the manifest names, and nothing else. git ignores it, so it
    # is read from the disk. The same holds for sim_<kkkk>_gitexclude.qs2.
    private <- private_file_of(file.path(path, f))
    problems <- c(problems, read_gitexclude(x, private, basename(f))$problem)
    if (length(problems) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": ", paste(problems, collapse = "; "))
      )
    }
  }
  return(bind_rows(out))
}

# The results file of each simulation, sims_gitexclude/sim_<kkkk>.qs2, in
# each scenario folder of vNN_results/ in `entries`, relative to `path`.
# git ignores these files, so they are read from the disk. A clone of the
# repository has none.
sim_files_on_disk <- function(path, entries) {
  inside <- entries[grepl("^v[0-9]{2}_results/[^/]+/", entries)]
  folders <- unique(sub("^(v[0-9]{2}_results/[^/]+)/.*$", "\\1", inside))
  out <- character(0)
  for (folder in folders) {
    found <- list.files(
      file.path(path, folder, sims_folder),
      pattern = "^sim_[0-9]{4}\\.qs2$"
    )
    out <- c(out, file.path(folder, sims_folder, found))
  }
  return(out)
}

rule_gitexclude_not_tracked <- function(path) {
  rule <- "gitexclude_not_tracked"
  git <- git_binary()
  if (!nzchar(git)) {
    return(project_row(
      rule,
      "PASS",
      "",
      "git is not installed, so nothing to check"
    ))
  }
  if (!in_git_repository(git, path)) {
    return(project_row(
      rule,
      "PASS",
      "",
      "the project is not a git repository, so nothing to check"
    ))
  }
  files <- system2(
    git,
    c("-c", "core.quotepath=off", "-C", shQuote(path), "ls-files"),
    stdout = TRUE,
    stderr = FALSE
  )
  if (!is.null(attr(files, "status"))) {
    stop("git ls-files failed in ", path, call. = FALSE)
  }
  bad <- files[grepl("gitexclude", files, fixed = TRUE)]
  return(project_row(
    rule,
    "FAIL",
    bad,
    paste0(
      bad,
      ": git tracks it, and a path with gitexclude in its name holds data ",
      "for each person",
      recycle0 = TRUE
    )
  ))
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
    paste0(
      where,
      ": the name must be vNN_<INITIALS>.docx or vNN_<INITIALS>_<suffix>.docx",
      recycle0 = TRUE
    )
  ))
}

rule_no_local_copy_of_pbn_function <- function(
  path,
  set = "house_2026_11_17"
) {
  rule <- "no_local_copy_of_pbn_function"
  exports <- setdiff(set_exports(set), local_copy_allowed)
  out <- list()
  for (f in functions_files(path)) {
    exprs <- parse_or_null(file.path(path, f))
    if (is.null(exprs)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
      next
    }
    defined <- character(0)
    for (e in as.list(exprs)) {
      if (
        is_function_assignment(e) &&
          as.character(e[[2]]) %in% exports
      ) {
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

# The rule loc_per_file: no .R file has more than `limit` code lines. The
# limit comes from the rules of the project, see read_rules().
rule_loc_per_file <- function(path, limit) {
  return(loc_findings(path, code_files(path), limit))
}

# The findings of loc_per_file for the .R files `files`, relative to `path`:
# a FAIL for each file with more than `limit` code lines, as
# org::loc_per_file() counts them, and for each file that R cannot parse.
# check_project() gives it code_files(), and initialize_project() the files
# that org sources, see stop_on_long_files().
loc_findings <- function(path, files, limit) {
  rule <- "loc_per_file"
  out <- list()
  for (f in files) {
    n <- tryCatch(
      org::loc_per_file(file.path(path, f)),
      error = function(e) return(NA_integer_)
    )
    if (is.na(n)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
    } else if (n > limit) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": ", n, " code lines, more than ", limit)
      )
    }
  }
  return(bind_rows(out))
}

# The rule cyclomatic_complexity: each function that a top-level assignment
# of an .R file defines, also a function in a list of a top-level
# assignment, scores at most `limit`. The limit comes from the rules of the
# project, see read_rules(). The house limit 15 is the one of lintr's
# cyclocomp_linter(15) in the gate of the packages. It reads the files of
# loc_per_file. It parses each file and evaluates nothing. A function inside
# another function counts toward that function, as in cyclocomp. A file that
# R cannot parse fails loc_per_file, so this rule skips it.
rule_cyclomatic_complexity <- function(path, limit) {
  rule <- "cyclomatic_complexity"
  files <- code_files(path)
  out <- list()
  for (f in sort(files, method = "radix")) {
    scores <- function_scores(file.path(path, f))
    scores <- sort(scores[scores > limit], decreasing = TRUE)
    if (length(scores) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        rep(f, length(scores)),
        paste0(
          f,
          ": ",
          names(scores),
          " scores ",
          scores,
          " (limit ",
          limit,
          ")"
        )
      )
    }
  }
  return(bind_rows(out))
}

# The cyclomatic complexity of each function of the file `file`, named by
# the function: each top-level assignment of a function, and each function
# in a list() of a top-level assignment, named <list>$<element>. cyclocomp
# scores the parsed expression, as lintr's cyclocomp_linter() does, so no
# code of the file runs. Empty for a file that R cannot parse.
function_scores <- function(file) {
  exprs <- parse_or_null(file)
  out <- integer(0)
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (is_function_assignment(e)) {
      out[[as.character(e[[2]])]] <- cyclocomp::cyclocomp(exprs[i])
    } else if (is_list_assignment(e)) {
      out <- c(out, list_scores(e))
    }
  }
  return(out)
}

# The cyclomatic complexity of each function in the list() that `e`
# assigns, named <list>$<element>.
list_scores <- function(e) {
  parts <- as.list(e[[3]])[-1]
  out <- integer(0)
  for (k in which(nzchar(names(parts)))) {
    value <- parts[[k]]
    if (is.call(value) && identical(value[[1]], as.name("function"))) {
      name <- paste0(as.character(e[[2]]), "$", names(parts)[k])
      out[[name]] <- cyclocomp::cyclocomp(as.expression(list(value)))
    }
  }
  return(out)
}

# TRUE when `e` assigns a call of list() with named arguments to a name.
is_list_assignment <- function(e) {
  if (!is.call(e) || length(e) != 3 || !is.symbol(e[[1]])) {
    return(FALSE)
  }
  if (!as.character(e[[1]]) %in% c("<-", "=", "<<-") || !is.symbol(e[[2]])) {
    return(FALSE)
  }
  value <- e[[3]]
  return(
    is.call(value) &&
      identical(value[[1]], as.name("list")) &&
      !is.null(names(value))
  )
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

# The top-level names of the house layout of the set `set`, as regular
# expressions. The set house_2026_11_19 and the newer sets name
# vNN_frozen.md5 too. The set house_2026_11_26 and the newer sets name the
# folder guidelines/ too. The key ignore of the pbn.yml of a project of the
# set `set` MUST NOT name any of them.
layout_patterns <- function(set = utils::tail(house_sets(), 1)) {
  return(c(
    run_script_pattern,
    functions_pattern,
    one_file_pattern,
    "^v[0-9]{2}_results$",
    "^v[0-9]{2}_cache_gitexclude$",
    "^v[0-9]{2}_test$",
    "^check_project\\.R$",
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
    "^pbn\\.yml$",
    if (set_is_from(set, "house_2026_11_19")) frozen_pattern,
    if (set_is_from(set, "house_2026_11_26")) "^guidelines$"
  ))
}

# The rule unknown_path. The set house_2026_11_19 and the newer sets name
# vNN_frozen.md5 too, the file of freeze_version().
rule_unknown_path <- function(path, set = "house_2026_11_17") {
  known <- layout_patterns(set)
  entries <- project_entries(path)
  top <- top_entries(entries[!is_root_tool_file(path, entries)])
  matched <- vapply(
    top,
    function(t) return(any(vapply(known, grepl, logical(1), x = t))),
    logical(1)
  )
  unknown <- c(top[!matched], unknown_in_results(path, entries))
  return(project_row(
    "unknown_path",
    "WARN",
    unknown,
    paste0(unknown, ": the house layout does not name it", recycle0 = TRUE)
  ))
}

# The files directly in a folder vNN_results/ of `entries` that the house
# layout does not name. It names the scenario folders, and the run log
# run.md with its PDF run.pdf and its errors file. results_contract fails an old results.qs2
# there.
unknown_in_results <- function(path, entries) {
  inside <- entries[grepl("^v[0-9]{2}_results/[^/]+$", entries)]
  files <- inside[!dir.exists(file.path(path, inside))]
  return(files[
    !basename(files) %in%
      c("run.md", "run.pdf", errors_file_name, "results.qs2")
  ])
}

bind_rows <- function(rows) {
  if (length(rows) == 0) {
    return(project_row(character(0), character(0), character(0), character(0)))
  }
  return(do.call(rbind, unname(rows)))
}

# The rules in the order that check_project() prints them, with the detail of
# the PASS line. A rule with parameters has a function of its entry in the
# rules of the project, see read_rules(), that gives the detail.
project_rules <- function() {
  return(list(
    lower_case_names = list(
      rule_lower_case_names,
      "no capital letter in a name",
      "rename it in lower case"
    ),
    run_script_initialises = list(
      rule_run_script_initialises,
      "every vNN_run.R calls pbn::initialize_project() with its own version",
      "call pbn::initialize_project(version = \"vNN\") in it"
    ),
    version_items_complete = list(
      rule_version_items_complete,
      "every vNN_run.R has a vNN_functions/, and the reverse",
      paste(
        "create the missing vNN_run.R or vNN_functions/, or delete the one",
        "that is there"
      )
    ),
    frozen_versions_unchanged = list(
      rule_frozen_versions_unchanged,
      paste(
        "every version but the newest has a vNN_frozen.md5, and no file of a",
        "frozen version changed after pbn::freeze_version()"
      ),
      frozen_fix
    ),
    readme_and_changelog = list(
      rule_readme_and_changelog,
      paste(
        "README.md and CHANGELOG.md exist, and CHANGELOG.md has a section",
        "## vNN for every version"
      ),
      "create the missing file, or add the missing section ## vNN to CHANGELOG.md"
    ),
    changelog_with_code = list(
      rule_changelog_with_code,
      paste(
        "every uncommitted change to a vNN_run.R or a file under",
        "vNN_functions/ comes with a change to CHANGELOG.md"
      ),
      paste(
        "add what changed and why to CHANGELOG.md, under the section of the",
        "version"
      )
    ),
    results_contract = list(
      rule_results_contract,
      paste(
        "every vNN_results/<scenario>/results.qs2 and each",
        "vNN_results/<scenario>/sims_gitexclude/sim_NNNN.qs2 on the disk",
        "passes check_results()"
      ),
      paste(
        "delete the file, and write it again with pbn::run_plan() and",
        "pbn::save_results()"
      )
    ),
    gitexclude_not_tracked = list(
      rule_gitexclude_not_tracked,
      "git tracks no path with gitexclude in its name",
      paste(
        "remove the path from git with git rm --cached, and add the line",
        "*gitexclude* to .gitignore"
      )
    ),
    scenario_checks = list(
      rule_scenario_checks,
      "every vNN_results/<scenario>/check.csv passes",
      paste(
        "correct the analysis, and run the scenario again with pbn::run_plan()",
        "and pbn::save_results()"
      )
    ),
    manual_manuscript_names = list(
      rule_manual_manuscript_names,
      "every file in manuscript/manual/ is named vNN_<INITIALS>.docx or vNN_<INITIALS>_<suffix>.docx",
      "rename it to vNN_<INITIALS>.docx, for example v01_AB.docx, or vNN_<INITIALS>_<suffix>.docx, for example v01_AB_si.docx"
    ),
    manuscript_reads_real = list(
      rule_manuscript_reads_real,
      "manuscript/built/ reads only the real results",
      "remove the argument scenario from the call"
    ),
    built_reads_own_version = list(
      rule_built_reads_own_version,
      "every read_results() call in manuscript/built/vNN/ reads the version vNN",
      paste(
        "read the version of the folder, as in read_results(\"vNN\"), or move",
        "the file to the folder of the version that it reads"
      )
    ),
    built_reads_source = list(
      rule_built_reads_source,
      paste(
        "every manuscript in manuscript/built/vNN/ reads the numbers of its",
        "source: an imported vNN_results/real/blocks.qs2 of real pages with",
        "read_blocks() or txt() in a one-file version, and no read_blocks()",
        "in another version"
      ),
      paste(
        "in a one-file version, read the numbers with pbn::read_blocks() or",
        "pbn::txt() after pbn::import_blocks() of the pages of the server,",
        "and keep a manuscript on fake numbers in manuscript/draft/; in",
        "another version, read them with pbn::read_results() or pbn::txt()"
      )
    ),
    built_has_run = list(
      rule_built_has_run,
      "every folder manuscript/built/vNN/ has a vNN_run.R",
      paste(
        "move the folder to the version of the run that it reads, or create",
        "the run script of the version"
      )
    ),
    manual_not_ahead = list(
      rule_manual_not_ahead,
      paste(
        "every manuscript/manual/vNN_<INITIALS>.docx is older than the newest",
        "run version"
      ),
      paste(
        "start the next run version with the call of pbn::new_version() in",
        "the detail"
      )
    ),
    manual_has_supplement = list(
      rule_manual_has_supplement,
      paste(
        "every manuscript/manual/vNN_<INITIALS>.docx has a supplement",
        "manuscript/manual/vMM_<INITIALS>_si.docx of any initials with MM at",
        "or below NN"
      ),
      paste(
        "add the supplement manuscript/manual/vNN_<INITIALS>_si.docx, or",
        "turn the rule off with the line manual_has_supplement: off under",
        "rules: in pbn.yml"
      )
    ),
    manual_si_has_main = list(
      rule_manual_si_has_main,
      paste(
        "every manuscript/manual/vNN_<INITIALS>_si.docx has the main file",
        "manuscript/manual/vNN_<INITIALS>.docx"
      ),
      paste(
        "add the main file manuscript/manual/vNN_<INITIALS>.docx with the",
        "version and the initials of the _si file, or rename the _si file"
      )
    ),
    fake_data_names = list(
      rule_fake_data_names,
      paste(
        "every file in an output_fake_data/ folder, and no file elsewhere,",
        "has a name that starts with fake_data_"
      ),
      paste(
        "write the output of fake data with pbn::make_outputs(), and rename",
        "any other file whose name starts with fake_data_"
      )
    ),
    no_local_copy_of_pbn_function = list(
      rule_no_local_copy_of_pbn_function,
      "no vNN_functions/ file defines a pbn export",
      paste(
        "rename the function, or use the pbn export and correct any fault in",
        "pbn"
      )
    ),
    loc_per_file = list(
      rule_loc_per_file,
      function(entry) {
        return(paste0(
          "no .R file has more than ",
          entry$limit,
          " code lines"
        ))
      },
      "split the file into smaller files"
    ),
    cyclomatic_complexity = list(
      rule_cyclomatic_complexity,
      function(entry) {
        return(paste0(
          "no function of an .R file has a cyclomatic complexity above ",
          entry$limit
        ))
      },
      "split the function into smaller functions"
    ),
    comment_share = list(
      rule_comment_share,
      comment_share_pass_detail,
      "move the explanation to README.md (why) or CHANGELOG.md (what changed)"
    ),
    no_environment_variables = list(
      rule_no_environment_variables,
      "no Sys.getenv or Sys.setenv in a run script or a functions folder",
      paste(
        "put the value in CFG, and the folder of data in the argument",
        "folders of pbn::initialize_project()"
      )
    ),
    no_cross_version_reads = list(
      rule_no_cross_version_reads,
      paste(
        "no string in a run script or a functions folder names another",
        "version or adhoc/"
      ),
      "move the shared code into the functions folder of this version"
    ),
    no_local_assign = list(
      rule_no_local_assign,
      function(entry) return(local_assign_pass_detail(entry$forbid)),
      "return the value from a function, and assign it with <- where it is used"
    ),
    no_escape_hatches = list(
      rule_no_escape_hatches,
      paste(
        "no run script or functions folder calls",
        or_list(paste0(names(escape_hatch_calls), "()"))
      ),
      paste(
        "remove the call and let the warning or the error show, or allow the",
        "path in pbn.yml with a reason in a comment"
      )
    ),
    no_absolute_paths = list(
      rule_no_absolute_paths,
      paste(
        "no .R file has an absolute path or a file.path() from a string,",
        "outside the arguments of pbn::initialize_project()"
      ),
      paste(
        "start a path to data from pbn::folder(), and a path for output from",
        "the argument dir of the output function of pbn::make_outputs()"
      )
    ),
    no_plot_title = list(
      rule_no_plot_title,
      "no .R file gives a figure a title",
      paste(
        "remove the title, because the legend of a figure belongs in the",
        "manuscript, or allow the path in pbn.yml"
      )
    ),
    no_plot_subtitle = list(
      rule_no_plot_subtitle,
      "no .R file gives a figure a subtitle",
      paste(
        "remove the subtitle, because the legend of a figure belongs in the",
        "manuscript, or allow the path in pbn.yml"
      )
    ),
    no_plot_caption = list(
      rule_no_plot_caption,
      "no .R file gives a figure a caption",
      paste(
        "remove the caption, because the legend of a figure belongs in the",
        "manuscript, or allow the path in pbn.yml"
      )
    ),
    blocks_list = list(
      rule_blocks_list,
      paste(
        "blocks, when a functions folder has it, is a list of named",
        "functions in vNN_functions/blocks.R, and no other code uses a name",
        "of that file"
      ),
      paste(
        "keep the list blocks and the functions of the blocks in",
        "vNN_functions/blocks.R only, and use none of its names in another",
        "file"
      )
    ),
    real_results_are_fake = list(
      rule_real_results_are_fake,
      "no real results on the disk come from fake real data",
      paste(
        "run the scenario \"real\" again with",
        "pbn::initialize_project(fake_real_data = FALSE)"
      )
    ),
    guidelines_valid = list(
      rule_guidelines_valid,
      "every guidelines/*.yml follows the checklist schema",
      "correct the file to the checklist schema of ?check_project"
    ),
    guidelines_covered = list(
      rule_guidelines_covered,
      "every result item of guidelines/*.yml has a block, and every item of a block is in guidelines/*.yml",
      "name the item in items of a pbn::block() in vNN_functions/blocks.R, or correct the item of the block to one of guidelines/*.yml"
    ),
    display_numbering = list(
      rule_display_numbering,
      paste(
        "every output name and sheet name has a number or nonum_, and the",
        "manuscript cites each series in the order 1, 2, 3"
      ),
      paste(
        "rename the output file or the sheet by the scheme of ?check_project,",
        "or renumber the figures and the tables of the output function and",
        "of the manuscript in the order of their first citation"
      )
    ),
    unknown_path = list(
      rule_unknown_path,
      "the house layout names every top-level path",
      "move it into a folder of the layout, such as adhoc/, or delete it"
    )
  ))
}
