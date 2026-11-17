# The two results files: results.qs2, and results_gitexclude.qs2 for the
# analysis kinds that hold data for each person.

gitexclude_file <- "results_gitexclude.qs2"

# The folder of the results of each simulation, in the folder of a scenario
# with more than 1 simulation. Its name holds gitexclude, so git ignores it,
# and only the stacked results go to git.
sims_folder <- "sims_gitexclude"

# The file of the results of simulation `sim`, relative to the folder of the
# scenario: sims_gitexclude/sim_<kkkk>.qs2.
sim_file <- function(sim) {
  return(file.path(sims_folder, paste0(sim_folder(sim), ".qs2")))
}

# The file of the kinds of gitexclude of simulation `sim`, relative to the
# folder of the scenario: sims_gitexclude/sim_<kkkk>_gitexclude.qs2.
sim_private_file <- function(sim) {
  return(file.path(sims_folder, paste0(sim_folder(sim), "_gitexclude.qs2")))
}

# The file of the kinds of gitexclude beside the results file `file`:
# results_gitexclude.qs2 beside results.qs2, and sim_<kkkk>_gitexclude.qs2
# beside sim_<kkkk>.qs2.
private_file_of <- function(file) {
  if (basename(file) == "results.qs2") {
    return(file.path(dirname(file), gitexclude_file))
  }
  return(sub("\\.qs2$", "_gitexclude.qs2", file))
}

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
    check_gitignored(
      state$home,
      paste0(folders, "/", gitexclude_file),
      "pbn saved nothing."
    )
  }
  return(gitexclude)
}

# In a git repository, stops when git does not ignore each file that this
# save writes into sims_gitexclude/: sim_<kkkk>.qs2 for each scenario of
# `table` with more than 1 simulation, and sim_<kkkk>_gitexclude.qs2 when
# `gitexclude` names a kind.
check_sims_ignored <- function(table, gitexclude) {
  multi <- table[table$sims > 1, , drop = FALSE]
  rel <- character(0)
  for (i in seq_len(nrow(multi))) {
    sims <- seq_len(multi$sims[i])
    files <- sim_file(sims)
    if (length(gitexclude) > 0) {
      files <- c(files, sim_private_file(sims))
    }
    rel <- c(rel, paste0(state$version, "_results/", multi$scenario[i], "/", files))
  }
  check_gitignored(state$home, rel, "pbn saved nothing.")
  return(invisible(NULL))
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

# Writes `private` to `file`, a results_gitexclude.qs2 or a
# sim_<kkkk>_gitexclude.qs2. For NULL, it deletes an old file there and
# prints one line, because read_results() would otherwise read its old
# values with the new results.
write_private <- function(private, file) {
  if (!is.null(private)) {
    qs2::qs_save(private, file)
    return(invisible(file))
  }
  if (file.exists(file)) {
    delete_paths(file)
    cat(
      "pbn: deleted ",
      file,
      ", because this run puts no analysis kind in it.\n",
      sep = ""
    )
  }
  return(invisible(NULL))
}

# The object in the file of the kinds of gitexclude beside the results
# object `x`, and the problem with it. `file` is the path of that file,
# results_gitexclude.qs2 or sim_<kkkk>_gitexclude.qs2, and `main` is the
# name of the file of `x`. `value` is NULL when the file is missing.
# `problem` is character(0), or the reason why the file does not belong to
# `x`.
read_gitexclude <- function(x, file, main = "results.qs2") {
  kinds <- gitexclude_kinds(x)
  out <- list(value = NULL, problem = character(0))
  if (!file.exists(file)) {
    return(out)
  }
  private <- basename(file)
  if (length(kinds) == 0) {
    out$problem <- paste0(
      "the manifest of ",
      main,
      " names no analysis kind in ",
      private,
      ", so the file is from another run. ",
      "Delete it, or run the scenario again"
    )
    return(out)
  }
  y <- tryCatch(qs2::qs_read(file), error = function(e) return(e))
  if (inherits(y, "error")) {
    out$problem <- paste("qs2 cannot read", private)
  } else if (!is.list(y) || is.data.frame(y) || !identical(names(y), kinds)) {
    out$problem <- paste0(
      private,
      " holds the elements ",
      paste(names(y), collapse = ", "),
      ", and the manifest of ",
      main,
      " names ",
      paste(kinds, collapse = ", ")
    )
  } else {
    out$value <- y
  }
  return(out)
}

# The results object `x` of the results file `main`, with the elements of
# the file of the kinds of gitexclude beside it, in the order of a results
# object. Without that file, `x` and one warning when the manifest names
# kinds in it.
merge_gitexclude <- function(x, main) {
  file <- private_file_of(main)
  got <- read_gitexclude(x, file, basename(main))
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
