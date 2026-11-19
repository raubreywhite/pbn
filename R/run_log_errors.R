# The error messages of a run. run.md goes to git, so it never holds the
# text of an error that project code raised. It numbers the errors, and the
# file <version>_results/run_errors_gitexclude.md holds their full text.
# git MUST ignore that file, and pbn writes it only when git does.

errors_file_name <- "run_errors_gitexclude.md"

# The errors of the run `log`, numbered by their row, in a fixed order: the
# failed analyses grouped by their message, the expect_ functions that
# stopped, the failed rules of check_results(), the error of
# save_results(), and the error of make_outputs(). `key` names the source,
# `about` says in pbn's words where the error came from, and `text` is the
# message.
log_errors <- function(log) {
  key <- character(0)
  about <- character(0)
  text <- character(0)
  if (!is.null(log$run_error)) {
    key <- "run"
    about <- "run_plan() stopped outside an analysis with this error."
    text <- log$run_error
  }
  failed <- log$run$failed
  for (msg in unique(unname(failed))) {
    nm <- names(failed)[failed == msg]
    key <- c(key, paste0("analysis\r", msg))
    about <- c(
      about,
      paste0("The analyses with this error: ", paste(nm, collapse = ", "), ".")
    )
    text <- c(text, msg)
  }
  checks <- log$checks
  stopped <- expect_error_rows(checks)
  for (i in which(stopped)) {
    key <- c(key, paste0("expect\r", checks$scenario[i]))
    about <- c(
      about,
      paste0("expect_", checks$scenario[i], "() stopped with this error.")
    )
    text <- c(text, checks$observed[i])
  }
  for (x in log$contract) {
    key <- c(key, paste0("contract\r", x))
    about <- c(about, "A rule of check_results() failed with this detail.")
    text <- c(text, x)
  }
  if (!is.null(log$error)) {
    key <- c(key, "save")
    about <- c(about, "save_results() stopped with this error.")
    text <- c(text, log$error)
  }
  out <- log$outputs$failed
  if (!is.null(out)) {
    key <- c(key, "output")
    about <- c(
      about,
      paste0(
        "make_outputs() stopped at the scenario ",
        md_inline(out$scenario),
        "."
      )
    )
    text <- c(text, out$message)
  }
  return(data.frame(key = key, about = about, text = text))
}

# The number of the error with the key `key` in `errors`, as "error <n>".
error_ref <- function(errors, key) {
  return(paste("error", match(key, errors$key)))
}

# TRUE for each row of the assertions `checks` that holds the error of an
# expect_ function that stopped. Its column observed is that error.
expect_error_rows <- function(checks) {
  if (NROW(checks) == 0) {
    return(logical(0))
  }
  return(
    !checks$pass %in% TRUE &
      checks$assertion ==
        paste0("expect_", checks$scenario, "() runs without an error")
  )
}

# The assertions `checks` for run.md: the error of an expect_ function that
# stopped is replaced by its number.
checks_for_log <- function(checks, errors) {
  if (NROW(checks) == 0) {
    return(checks)
  }
  stopped <- expect_error_rows(checks)
  checks$observed[stopped] <- error_ref(
    errors,
    paste0("expect\r", checks$scenario[stopped])
  )
  return(checks)
}

# "written" when the errors file can be written, "none" without an error,
# and "not ignored" in a git repository whose git does not ignore it.
errors_file_status <- function(errors) {
  if (nrow(errors) == 0) {
    return("none")
  }
  rel <- paste0(state$version, "_results/", errors_file_name)
  return(if (git_ignores(state$home, rel)) "written" else "not ignored")
}

# TRUE when git ignores the path `rel` of `home`, or when `home` is not in a
# git repository. FALSE when git fails, so that pbn writes nothing then.
git_ignores <- function(home, rel) {
  git <- git_binary()
  if (!nzchar(git) || !in_git_repository(git, home)) {
    return(TRUE)
  }
  ignored <- tryCatch(
    git_ignored(git, home, rel),
    error = function(e) return(character(0))
  )
  return(rel %in% ignored)
}

# The sentence of run.md that says where the text of the errors is.
errors_sentence <- function(log) {
  rel <- paste0(log$version, "_results/", errors_file_name)
  if (identical(log$errors_file, "not ignored")) {
    return(paste0(
      "pbn did not write the text of the errors to ",
      rel,
      ", because git does not ignore that file. Add the line *gitexclude* ",
      "to .gitignore. The console showed each error."
    ))
  }
  return(paste0(
    "The text of the errors is in ",
    rel,
    ". git MUST ignore that file, because an error message can hold a ",
    "value of the data."
  ))
}

# The lines of the errors file of `log`.
errors_lines <- function(log, errors) {
  lines <- c(
    paste0(
      "# Errors of the run of ",
      md_inline(log$folder),
      ", version ",
      log$version
    ),
    "",
    paste0(
      "The run started ",
      format(log$started, "%Y-%m-%d %H:%M:%S %z"),
      ". run.md names each error by its number. git MUST ignore this file, ",
      "because an error message can hold a value of the data."
    ),
    ""
  )
  for (i in seq_len(nrow(errors))) {
    lines <- c(
      lines,
      paste("## Error", i),
      "",
      md_flat(errors$about[i]),
      "",
      md_code(errors$text[i]),
      ""
    )
  }
  return(to_ascii(lines))
}

# `x` on one line: each run of white space that holds a line end becomes one
# space.
md_flat <- function(x) {
  return(gsub("[ \t]*[\r\n]+[ \t]*", " ", as.character(x)))
}

# A project string `x` for run.md outside a table and a code block: on one
# line, as a code span, so that Markdown shows it as it is. A newline in it
# can thus never start a heading or a list item. The fence is longer than
# each run of backticks in `x`.
md_inline <- function(x) {
  x <- md_flat(x)
  return(vapply(
    x,
    function(s) {
      runs <- unlist(regmatches(s, gregexpr("`+", s)))
      fence <- strrep("`", max(0, nchar(runs)) + 1)
      pad <- if (grepl("^`|`$", s)) " " else ""
      return(paste0(fence, pad, s, pad, fence))
    },
    character(1),
    USE.NAMES = FALSE
  ))
}

# Writes the lines `lines` to `file` through a temporary file in the same
# folder that it renames, with LF line ends on each platform.
write_lines_atomic <- function(lines, file) {
  tmp <- tempfile("run_", tmpdir = dirname(file), fileext = ".md.tmp")
  on.exit(unlink(tmp), add = TRUE)
  write_lf_lines(lines, tmp)
  if (!file.rename(tmp, file)) {
    stop("pbn could not write ", file, call. = FALSE)
  }
  return(invisible(file))
}

# The section of run.md for an error of run_plan() outside an analysis.
md_run_error <- function(error, errors) {
  if (is.null(error)) {
    return(character(0))
  }
  return(c(
    "### Error of run_plan()",
    "",
    paste0(
      "run_plan() stopped outside an analysis with ",
      error_ref(errors, "run"),
      ". A worker stopped, or plnr or the backend of the workers failed."
    ),
    ""
  ))
}
