# The two results files: results.qs2, and results_gitexclude.qs2 for the
# analysis kinds that hold data for each person.

gitexclude_file <- "results_gitexclude.qs2"

# The argument gitexclude of save_results(), checked: a character vector of
# analysis kinds of the plan, or character(0) for NULL. In a git repository,
# git MUST ignore the files that it names. Stops before anything is written.
check_gitexclude <- function(gitexclude, info, table) {
  if (is.null(gitexclude)) {
    return(character(0))
  }
  if (
    !is.character(gitexclude) ||
      anyNA(gitexclude) ||
      !all(nzchar(gitexclude)) ||
      anyDuplicated(gitexclude) > 0
  ) {
    stop(
      "`gitexclude` must be NULL or a character vector of analysis kinds, ",
      "each once.",
      call. = FALSE
    )
  }
  unknown <- setdiff(gitexclude, info$kinds$analysis)
  if (length(unknown) > 0) {
    stop(
      "These names in `gitexclude` are not analysis kinds of the plan: ",
      paste(unknown, collapse = ", "),
      ". The kinds are: ",
      paste(info$kinds$analysis, collapse = ", "),
      ". pbn saved nothing.",
      call. = FALSE
    )
  }
  if (length(gitexclude) > 0) {
    folders <- paste0(state$version, "_results/", table$scenario)
    folders <- c(folders, paste0(folders, "/", sim_folder(1)))
    check_gitignored(
      state$home,
      paste0(folders, "/", gitexclude_file),
      "pbn saved nothing."
    )
  }
  return(gitexclude)
}

# The results object `x` split into the object of results.qs2, `public`, and
# the object of results_gitexclude.qs2, `private`. `private` is NULL when `x`
# has no kind of `gitexclude`. manifest$analyses names the file of each kind.
split_results <- function(x, gitexclude) {
  a <- x$manifest$analyses
  a$file[a$analysis %in% gitexclude] <- gitexclude_file
  x$manifest$analyses <- a
  kinds <- gitexclude_kinds(x)
  if (length(kinds) == 0) {
    return(list(public = x, private = NULL))
  }
  return(list(public = x[!names(x) %in% kinds], private = x[kinds]))
}

# The analysis kinds that the manifest of the results object `x` puts in
# results_gitexclude.qs2. A manifest without the column file puts none there.
gitexclude_kinds <- function(x) {
  m <- if (is.list(x)) x[["manifest"]] else NULL
  a <- if (is.list(m)) m[["analyses"]] else NULL
  if (!is.data.frame(a) || !all(c("analysis", "file") %in% names(a))) {
    return(character(0))
  }
  return(a$analysis[a$file %in% gitexclude_file])
}

# Writes `private` to results_gitexclude.qs2 in `folder`. For NULL, it
# deletes an old file there and prints one line, because read_results()
# would otherwise read its old values with the new results.
write_private <- function(private, folder) {
  file <- file.path(folder, gitexclude_file)
  if (!is.null(private)) {
    qs2::qs_save(private, file)
    return(invisible(file))
  }
  if (file.exists(file)) {
    unlink(file)
    cat(
      "pbn: deleted ",
      file,
      ", because this run puts no analysis kind in it.\n",
      sep = ""
    )
  }
  return(invisible(NULL))
}

# The object in results_gitexclude.qs2 beside the results object `x`, and
# the problem with it. `file` is the path of results_gitexclude.qs2. `value`
# is NULL when the file is missing. `problem` is character(0), or the reason
# why the file does not belong to `x`.
read_gitexclude <- function(x, file) {
  kinds <- gitexclude_kinds(x)
  out <- list(value = NULL, problem = character(0))
  if (!file.exists(file)) {
    return(out)
  }
  if (length(kinds) == 0) {
    out$problem <- paste(
      "the manifest of results.qs2 names no analysis kind in",
      "results_gitexclude.qs2, so the file is from another run.",
      "Delete it, or run the scenario again"
    )
    return(out)
  }
  y <- tryCatch(qs2::qs_read(file), error = function(e) return(e))
  if (inherits(y, "error")) {
    out$problem <- "qs2 cannot read results_gitexclude.qs2"
  } else if (!is.list(y) || is.data.frame(y) || !identical(names(y), kinds)) {
    out$problem <- paste0(
      "results_gitexclude.qs2 holds the elements ",
      paste(names(y), collapse = ", "),
      ", and the manifest of results.qs2 names ",
      paste(kinds, collapse = ", ")
    )
  } else {
    out$value <- y
  }
  return(out)
}

# The results object `x` of results.qs2 in `folder`, with the elements of
# results_gitexclude.qs2, in the order of a results object. Without that
# file, `x` and one warning when the manifest names kinds in it.
merge_gitexclude <- function(x, folder) {
  file <- file.path(folder, gitexclude_file)
  got <- read_gitexclude(x, file)
  if (length(got$problem) > 0) {
    stop(file, ": ", got$problem, ".", call. = FALSE)
  }
  kinds <- gitexclude_kinds(x)
  if (length(kinds) == 0) {
    return(x)
  }
  if (is.null(got$value)) {
    warning(
      "The manifest names the analysis kinds ",
      paste(kinds, collapse = ", "),
      " in ",
      file,
      ", and that file is missing, for example in a clone of the ",
      "repository. read_results() returns the other elements.",
      call. = FALSE
    )
    return(x)
  }
  all <- c(x, got$value)
  order <- x$manifest$analyses$analysis
  return(all[c(names(x)[!names(x) %in% order], order[order %in% names(all)])])
}
