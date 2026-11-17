#' Check the file of to_one_file() on this computer
#'
#' `check_one_file()` runs the file of [to_one_file()] in a new R session, on
#' copies of the folders of the project. It prints PASS when the real results
#' are those of the project, or else the first difference.
#'
#' @param version The version, for example `"v01"`. It MUST be the version of
#'   the last [initialize_project()] in this R session.
#' @details
#' Run the project as normal first, so that `<version>_results/real/` holds
#' its real results, and call [initialize_project()] again in this R session.
#' `check_one_file()` then does these steps:
#' 1. It makes a temporary folder with an empty project folder, and a copy of
#'    each folder of the project with all its files. The empty project folder
#'    stands for the server, where there is no functions folder and no
#'    `pbn.yml`.
#' 2. It writes the file of [to_one_file()] with these paths, and runs it with
#'    `Rscript` in a new R session.
#' 3. It compares `results.qs2` and `results_gitexclude.qs2` of the real
#'    results of the run with those of the project, element by element.
#'
#' Three fields of the manifest MAY differ, and the comparison leaves them
#' out:
#' - `run_date`, the date of the run.
#' - `packages`, the loaded packages, because the new R session loads other
#'   packages than the R session of the normal run.
#' - the column `from_cache` of `data_cache`, because the copy has no cache.
#'
#' Every other element MUST be identical, also `source_md5`, `data_files`
#' and `cache_key`. So the results do not depend on the place of the project
#' and of its folders. The copies keep the modification times of the files,
#' with `file.copy(copy.date = TRUE)`. That is why `data_files` and the cache
#' key agree. pbn reads no content of a data file, so a copy with new times
#' looks like changed data. The times are whole seconds, so a copy that
#' loses a part of a second, as on Windows, still agrees.
#'
#' When the run of the file stops with an error, `check_one_file()` prints
#' FAIL with the exit status and the last 20 lines of the output of the run,
#' and compares nothing. On PASS, it deletes the temporary folder. On FAIL,
#' it keeps it, and the text names the folder with the file and the output
#' of its run.
#'
#' The check copies each folder, so it needs the disk space of the real data
#' once more. These are its limits:
#' - It cannot check what differs on the server, for example the versions of
#'   R and of the packages there.
#' - It runs the file with `Rscript`, and on the server a person pastes it
#'   into an interactive R session. So it does not cover code that depends
#'   on `interactive()`.
#' - It compares the results files, and not what the blocks of
#'   [show_block()] print.
#' - pbn reads no content of a data file. A data file that changed in place
#'   and kept its size and its modification time is not seen.
#' @return Invisibly, `TRUE` on PASS and `FALSE` on FAIL.
#' @family one file
#' @seealso `vignette("pbn")`, section "Running on an air-gapped server".
#' @export
check_one_file <- function(version) {
  check_one_file_call(version, "check_one_file")
  local <- file.path(state$results, "real")
  if (!file.exists(file.path(local, "results.qs2"))) {
    stop(
      "There are no real results in ",
      paste0(version, "_results/real/"),
      ". Run the project first, then pbn::check_one_file().",
      call. = FALSE
    )
  }
  tmp <- tempfile("pbn_one_file_")
  dir.create(file.path(tmp, "home"), recursive = TRUE)
  # A temporary path on Windows holds \ and can hold a short name such as
  # RUNNER~1. Each path that goes into the file is the long path with /.
  tmp <- normalizePath(tmp, winslash = "/")
  home <- file.path(tmp, "home")
  copies <- copy_folders(state$folders, file.path(tmp, "folders"))
  file <- file.path(tmp, paste0(version, "_one_file.R"))
  write_lf_lines(one_file_lines(version, home, copies), file)
  log <- file.path(tmp, "run.log")
  status <- run_rscript(file, log)
  if (!identical(status, 0L)) {
    return(one_file_verdict(child_failure(status, log), tmp))
  }
  found <- one_file_difference(
    local,
    file.path(home, paste0(version, "_results"), "real")
  )
  return(one_file_verdict(found, tmp))
}

# The text of check_one_file() for a run of the file that stopped with the
# exit status `status`: the status, and the last 20 lines of the output in
# `log`, each indented, because the folder of the run can be gone later, for
# example after R CMD build.
child_failure <- function(status, log) {
  out <- if (file.exists(log)) readLines(log, warn = FALSE) else character(0)
  out <- sub("\r$", "", out)
  out <- utils::tail(out, 20)
  return(paste0(
    "the file of pbn::to_one_file() stopped in its new R session with the ",
    "exit status ",
    status,
    ", so there are no results to compare. The last ",
    length(out),
    " lines of its output:\n",
    paste0("    ", out, "\n", collapse = ""),
    "Its whole output is in ",
    log,
    "."
  ))
}

# Prints the line of check_one_file() for the difference `found`, or NULL
# for PASS, and deletes `tmp` on PASS. Returns TRUE on PASS, invisibly.
one_file_verdict <- function(found, tmp) {
  if (is.null(found)) {
    cat(
      "PASS: the file of pbn::to_one_file() gives the real results of the ",
      "project, apart from manifest$run_date, manifest$packages and ",
      "manifest$data_cache$from_cache.\n",
      sep = ""
    )
    unlink(tmp, recursive = TRUE)
    return(invisible(TRUE))
  }
  cat("FAIL: ", found, " The file and its run are in ", tmp, ".\n", sep = "")
  return(invisible(FALSE))
}

# Copies each folder of the named paths `folders` with all its files into
# `root`/<folder name>, and returns the named paths of the copies. A folder
# that does not exist gets a path, and no copy.
copy_folders <- function(folders, root) {
  out <- stats::setNames(file.path(root, names(folders)), names(folders))
  for (nm in names(folders)) {
    if (!dir.exists(folders[[nm]])) {
      next
    }
    dir.create(out[[nm]], recursive = TRUE)
    inner <- list.files(
      folders[[nm]],
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    file.copy(inner, out[[nm]], recursive = TRUE, copy.date = TRUE)
  }
  return(out)
}

# Runs the R script `file` with Rscript in a new R session, with the library
# paths of this R session, and writes its output to `log`. Returns the exit
# status.
run_rscript <- function(file, log) {
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  old <- Sys.getenv("R_LIBS", unset = NA)
  on.exit(
    if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old),
    add = TRUE
  )
  # R_LIBS is a transport between pbn's own processes, not a configuration.
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  # The new session also sets the library paths itself. Under R CMD check on
  # Windows the start-up of R sets other paths, so R_LIBS alone is not enough.
  launcher <- file.path(dirname(log), "launch_one_file.R")
  writeLines(
    c(
      paste0(".libPaths(", paste(deparse(.libPaths()), collapse = ""), ")"),
      paste0("source(", encodeString(file, quote = "\""), ")")
    ),
    launcher
  )
  status <- suppressWarnings(system2(
    rscript,
    shQuote(launcher),
    stdout = log,
    stderr = log
  ))
  return(as.integer(status))
}

# The first difference between the real results in the folder `local` and
# in the folder `remote`, as text, or NULL. It compares results.qs2 and
# results_gitexclude.qs2 without the fields of the manifest that MAY differ.
one_file_difference <- function(local, remote) {
  for (f in c("results.qs2", gitexclude_file)) {
    here <- file.exists(file.path(local, f))
    there <- file.exists(file.path(remote, f))
    if (!here && !there) {
      next
    }
    if (here != there) {
      return(paste0(
        "the ",
        if (here) "project" else "file of pbn::to_one_file()",
        " has a ",
        f,
        ", and the other has none."
      ))
    }
    a <- comparable_results(qs2::qs_read(file.path(local, f)))
    b <- comparable_results(qs2::qs_read(file.path(remote, f)))
    found <- first_difference(a, b, f)
    if (!is.null(found)) {
      return(found)
    }
  }
  return(NULL)
}

# The results `x` without the fields of the manifest that MAY differ between
# the normal run and the run of the file of to_one_file().
comparable_results <- function(x) {
  if (!is.list(x) || !is.list(x$manifest)) {
    return(x)
  }
  x$manifest$run_date <- NULL
  x$manifest$packages <- NULL
  if (is.data.frame(x$manifest$data_cache)) {
    x$manifest$data_cache$from_cache <- NULL
  }
  return(x)
}

# The first place where `a`, of the project, and `b`, of the file of
# to_one_file(), differ, as text that starts with the path `path`, or NULL
# when they are identical. Lists and data.frames are compared element by
# element, and an atomic vector value by value.
first_difference <- function(a, b, path) {
  if (identical(a, b)) {
    return(NULL)
  }
  if (is.list(a) && is.list(b)) {
    return(list_difference(a, b, path))
  }
  same_shape <- is.atomic(a) &&
    is.atomic(b) &&
    length(a) == length(b) &&
    identical(class(a), class(b))
  if (same_shape) {
    return(value_difference(a, b, path))
  }
  return(paste0(path, " differs in its type or its length."))
}

# The first difference of the lists `a` and `b`, which differ, element by
# element, as first_difference() gives it.
list_difference <- function(a, b, path) {
  if (!identical(names(a), names(b)) || length(a) != length(b)) {
    return(paste0(path, " has other names or another length."))
  }
  for (i in seq_along(a)) {
    nm <- names(a)[i]
    where <- if (is.null(nm) || !nzchar(nm)) {
      paste0(path, "[[", i, "]]")
    } else {
      paste0(path, "$", nm)
    }
    found <- first_difference(a[[i]], b[[i]], where)
    if (!is.null(found)) {
      return(found)
    }
  }
  return(paste0(path, " has other attributes."))
}

# The first value that differs in the atomic vectors `a` and `b` of the same
# class and length, which differ, as first_difference() gives it.
value_difference <- function(a, b, path) {
  same <- vapply(
    seq_along(a),
    function(i) return(identical(a[i], b[i])),
    logical(1)
  )
  if (all(same)) {
    return(paste0(path, " has other attributes."))
  }
  i <- which(!same)[1]
  return(paste0(
    path,
    "[",
    i,
    "] is ",
    format(a[i], digits = 15),
    " in the project and ",
    format(b[i], digits = 15),
    " in the file of pbn::to_one_file()."
  ))
}
