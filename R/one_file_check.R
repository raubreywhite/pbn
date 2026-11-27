#' Check the file of to_one_file() on this computer
#'
#' `check_one_file()` runs the file `<version>_one_file.R` of the project in a
#' new R session, on copies of the folders of the project. It prints PASS
#' when the real results are those of the project, or else the first
#' difference.
#'
#' @param ... Nothing. A call with an argument stops. `check_one_file()`
#'   takes the version and the paths of the server from the last
#'   [initialize_project()] of this R session.
#' @details
#' Run the project as normal first, so that `<version>_results/real/` holds
#' its real results. In a one-file project, that folder is
#' `<version>_results/fake/`, see the section The folder fake/ of
#' [initialize_project()]. The run of the file writes `real/` of its
#' temporary project folder, as on the server. Write the file with [to_one_file()], and call
#' [initialize_project()] again in this R session, with `one_file_home` and
#' `one_file_folders`.
#'
#' `check_one_file()` then does these steps:
#' 1. It makes the lines of [to_one_file()] in memory, with the code of the
#'    project and the paths of the server of the last
#'    [initialize_project()]. It compares them with `<version>_one_file.R` in
#'    the project, line by line. This R session is the one source of the
#'    paths, so a change of `one_file_home` in the run script also makes the
#'    file older. Only the line of the header with the version of pbn and the
#'    date does not count. When a line differs, the file is older than the
#'    code of the project. It prints FAIL with the number of the first line
#'    that differs and the line in both files, each cut to 80 characters. It
#'    says to run [to_one_file()] again. A file of an older pbn, with the
#'    argument `one_file`, also gets this FAIL, and the text says so. It
#'    prints FAIL also when the project has no such file.
#' 2. Without real results, it stops here. So the file of a project MAY be
#'    checked for ASCII, for a parse and for a left-out function before the
#'    first full run.
#' 3. It compares `manifest$source_md5` of the real results with the md5 of
#'    the files of the project now, without the files that the manifest
#'    leaves out, see `source_md5` of [save_results()].
#'    When they differ, the results of the project are older than its code.
#'    It prints FAIL with the changed files and says to run
#'    `<version>_run.R` again. It runs nothing.
#' 4. It reads `manifest$failed_fake_checks` of the real results. When it
#'    names a fake scenario with a failed assertion, it prints FAIL with the
#'    scenario and its `check.csv`, as [read_results()] does. It runs
#'    nothing.
#' 5. It makes a temporary folder with an empty project folder, and a copy of
#'    each folder of the project with all its files. The empty project folder
#'    stands for the server, where there is no functions folder and no
#'    `pbn.yml`.
#' 6. It writes a copy of the file of the project to the temporary folder. In
#'    the copy, each line with a path of the server gets the temporary path.
#'    These are the lines where [to_one_file()] with the temporary paths
#'    writes another line. It runs the copy with `Rscript` in a new R session.
#'    So the check runs the file that goes into git.
#' 7. It compares `results.qs2` and `results_gitexclude.qs2` of the real
#'    results of the run with those of the project, element by element.
#' 8. It calls each block of [show_block()] on the real results of the
#'    project and on those of the run, in one more new R session. That
#'    session attaches no package of the run script. It compares what
#'    [show_block()] prints for each page of each block, as text, without
#'    the line `FAKE REAL DATA`. A block that stops on either results, or
#'    that prints other text, is a FAIL that names the block.
#'
#' Five fields of the manifest MAY differ, and the comparison leaves them
#' out:
#' - `run_date`, the date of the run.
#' - `packages`, the loaded packages, because the new R session loads other
#'   packages than the R session of the normal run.
#' - the column `from_cache` of `data_cache`, because the copy has no cache.
#' - `fake_real_data`, because the file never runs on fake real data. See
#'   [initialize_project()].
#' - `failed_fake_checks`, because the file runs no fake scenario. Step 4
#'   checks it on the real results of the project.
#'
#' The comparison also leaves `<version>_functions/blocks.R` and
#' `<version>_functions/formatters.R` out of `source_md5`, also for results
#' of an older pbn that hold them. Every other
#' element MUST be the same, also `source_md5`, `data_files` and
#' `cache_key`. Two doubles are the same when they differ by at most
#' `sqrt(.Machine$double.eps)`, about 1.5e-8, of the larger absolute value.
#' That is the default tolerance of [all.equal()]. Below that size, the limit
#' is that absolute difference. The same model in two R sessions MAY differ
#' in the last digits, and no printed number shows it. Every other value
#' MUST be identical, also a double with a class, such as a `Date` or a
#' `POSIXct`. The attributes MUST be identical too, in any order. So the results do not depend on the place of the project
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
#' - It compares the blocks on the real data of this computer. A block that
#'   stops only on other data, for example on an empty table, passes here.
#' - pbn reads no content of a data file. A data file that changed in place
#'   and kept its size and its modification time is not seen.
#' @return Invisibly, `TRUE` on PASS and `FALSE` on FAIL.
#' @family one file
#' @seealso `vignette("pbn")`, section "Running on an air-gapped server".
#' @export
check_one_file <- function(...) {
  check_one_file_call("check_one_file", ...length())
  version <- state$version
  disk <- disk_one_file(version)
  if (!is.null(disk$found)) {
    cat("FAIL: ", disk$found, "\n", sep = "")
    return(invisible(FALSE))
  }
  local <- scenario_dir(state, "real")
  if (!file.exists(file.path(local, "results.qs2"))) {
    stop(
      "There are no real results in ",
      paste0(version, "_results/", basename(local), "/"),
      ". The file of pbn::to_one_file() passes the checks that need no ",
      "results: it is the file that pbn::to_one_file() writes now, its code ",
      "is ASCII, it parses, and it uses no function that it leaves out. Run ",
      "the project, then pbn::check_one_file().",
      call. = FALSE
    )
  }
  stale <- stale_results(local, version)
  if (!is.null(stale)) {
    cat("FAIL: ", stale, "\n", sep = "")
    return(invisible(FALSE))
  }
  failed <- tryCatch(
    check_run_of_real(
      local,
      file.path(local, "results.qs2"),
      results_manifest(local)
    ),
    error = function(e) return(conditionMessage(e))
  )
  if (is.character(failed)) {
    cat("FAIL: ", failed, "\n", sep = "")
    return(invisible(FALSE))
  }
  tmp <- tempfile("pbn_one_file_")
  create_folder(file.path(tmp, "home"))
  # A temporary path on Windows holds \ and can hold a short name such as
  # RUNNER~1. Each path that goes into the file is the long path with /.
  tmp <- normalizePath(tmp, winslash = "/")
  home <- file.path(tmp, "home")
  copies <- copy_folders(state$folders, file.path(tmp, "folders"))
  file <- file.path(tmp, paste0(version, "_one_file.R"))
  write_lf_lines(local_paths(disk, version, home, copies), file)
  log <- file.path(tmp, "run.log")
  status <- run_rscript(file, log)
  if (!identical(status, 0L)) {
    return(one_file_verdict(child_failure(status, log), tmp))
  }
  # The temporary project folder has no run script, as on the server, so
  # its real results are in real/.
  found <- one_file_difference(local, scenario_dir(version, "real", home))
  blocks <- NULL
  if (is.null(found)) {
    blocks <- check_blocks_of_file(file, home, tmp)
    found <- blocks$found
  }
  return(one_file_verdict(found, tmp, blocks$n))
}

# The file <version>_one_file.R of the project, against the lines that
# to_one_file() writes now, with the paths of the server of the last
# initialize_project(). The R session is the one source of the paths, so a
# file with other paths is older than the run script. The line of the header
# with the version of pbn and the date does not count. Returns a list.
# `found` is the text of the FAIL, or NULL. On NULL, the list also holds
# `lines`, the lines of the file, and `now`, the lines that to_one_file()
# writes now.
disk_one_file <- function(version) {
  name <- paste0(version, "_one_file.R")
  path <- file.path(state$home, name)
  if (!file.exists(path)) {
    return(list(
      found = paste0(
        "there is no file ",
        name,
        " in the project. Run pbn::to_one_file() first, then ",
        "pbn::check_one_file()."
      )
    ))
  }
  lines <- sub("\r$", "", readLines(path, warn = FALSE))
  stale <- paste0(
    "the file ",
    name,
    " in the project is older than the code of the project: run ",
    "pbn::to_one_file() again, then check."
  )
  now <- one_file_lines(version, state$one_file_home, state$one_file_folders)
  differs <- first_line_difference(lines, now)
  if (!is.null(differs)) {
    if (any(grepl("^[[:space:]]*one_file = list\\($", lines))) {
      stale <- paste0(
        stale,
        " An older pbn wrote it, with the argument one_file that this pbn ",
        "does not read."
      )
    }
    return(list(found = paste(stale, differs)))
  }
  return(list(found = NULL, lines = lines, now = now))
}

# The first line where the lines `disk`, of the file in the project, and
# `now`, of to_one_file() now, differ, as text, or NULL. The line of the
# header with the version of pbn and the date does not count.
first_line_difference <- function(disk, now) {
  generated <- paste0(
    "^# pbn [^ ]+ generated it on [0-9]{4}-[0-9]{2}-[0-9]{2} from the ",
    "project folder "
  )
  n <- min(length(disk), length(now))
  same <- disk[seq_len(n)] == now[seq_len(n)] |
    (grepl(generated, disk[seq_len(n)]) & grepl(generated, now[seq_len(n)]))
  i <- which(!same)[1]
  if (is.na(i) && length(disk) == length(now)) {
    return(NULL)
  }
  if (is.na(i)) {
    return(paste0(
      "The file has ",
      length(disk),
      " lines, and pbn::to_one_file() writes ",
      length(now),
      " lines now."
    ))
  }
  return(paste0(
    "The first line that differs is line ",
    i,
    ". The file has ",
    quoted_line(disk[i]),
    ", and pbn::to_one_file() writes ",
    quoted_line(now[i]),
    " now."
  ))
}

# The line `x` in double quotes. A line of more than 80 characters is cut
# to its first 80, and " [...]" follows the quotes.
quoted_line <- function(x) {
  if (nchar(x) <= 80) {
    return(encodeString(x, quote = "\""))
  }
  return(paste0(encodeString(substr(x, 1, 80), quote = "\""), " [...]"))
}

# The lines of the file of the project in `disk`, a value of
# disk_one_file(), with the temporary folder `home` and the copies `copies`
# in place of the paths of the server. A line of a path is a line where
# to_one_file() writes another line with the temporary paths. Such a line
# takes the line with the temporary paths. Every other line is the line of
# the file in the project.
local_paths <- function(disk, version, home, copies) {
  here <- one_file_lines(version, home, copies[names(state$one_file_folders)])
  if (length(here) != length(disk$now)) {
    stop(
      "pbn cannot put the temporary paths into the file: it gets ",
      length(here),
      " lines with them and ",
      length(disk$now),
      " lines with the paths of the server.",
      call. = FALSE
    )
  }
  out <- disk$lines
  swap <- here != disk$now
  out[swap] <- here[swap]
  return(out)
}

# The text of check_one_file() when the real results in the folder `local`
# come from other code than the files of the project on the disk now, or
# NULL. The file of the blocks does not count. The comparison would then
# show the age of the results, and no fault of the file.
stale_results <- function(local, version) {
  recorded <- tryCatch(
    qs2::qs_read(file.path(local, "results.qs2"))$manifest$source_md5,
    error = function(e) return(NULL)
  )
  if (!is_data_md5(recorded)) {
    return(NULL)
  }
  # blocks.R, and formatters.R from the rule set house_2026_11_26, do not
  # count when they cannot change the results, also for the results of an
  # older pbn that hold the md5 of blocks.R.
  now <- source_md5(state$home, version)
  inert <- inert_source_files(state$home, version, state$rule_set)
  recorded <- recorded[!names(recorded) %in% inert]
  now <- now[!names(now) %in% inert]
  both <- intersect(names(now), names(recorded))
  changed <- c(
    setdiff(union(names(now), names(recorded)), both),
    both[now[both] != recorded[both]]
  )
  if (length(changed) == 0) {
    return(NULL)
  }
  return(paste0(
    "the results of the project are older than its code. These files ",
    "changed after the run that made them: ",
    paste(sort(changed, method = "radix"), collapse = ", "),
    ". Run ",
    version,
    "_run.R again, then pbn::check_one_file()."
  ))
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
one_file_verdict <- function(found, tmp, n_blocks = NULL) {
  if (is.null(found)) {
    cat(
      "PASS: the file of pbn::to_one_file() gives the real results of the ",
      "project, apart from manifest$run_date, manifest$packages, ",
      "manifest$data_cache$from_cache, manifest$fake_real_data and ",
      "manifest$failed_fake_checks. ",
      if (is.null(n_blocks) || n_blocks == 0) {
        "The project has no blocks."
      } else {
        paste0(
          "Each page of the ",
          n_blocks,
          if (n_blocks == 1) " block" else " blocks",
          " prints the same for both."
        )
      },
      "\n",
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
    create_folder(out[[nm]])
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
  x$manifest$fake_real_data <- NULL
  # check_one_file() refuses real results with a failed fake scenario
  # before the run, and the file runs no fake scenario.
  x$manifest$failed_fake_checks <- NULL
  # A manifest of an older pbn holds the md5 of the blocks. They and the
  # formatters only print results, so they never count.
  if (!is.null(x$manifest$source_md5)) {
    x$manifest$source_md5 <- manifest_source_md5(
      x$manifest$source_md5,
      x$manifest$version
    )
  }
  if (is.data.frame(x$manifest$data_cache)) {
    x$manifest$data_cache$from_cache <- NULL
  }
  return(x)
}

# The relative tolerance of first_difference() for doubles, that of
# all.equal(). The same computation in two R sessions MAY differ in the last
# digits, for example through the order of a sum.
one_file_tolerance <- sqrt(.Machine$double.eps)

# The first place where `a`, of the project, and `b`, of the file of
# to_one_file(), differ, as text that starts with the path `path`, or NULL
# when they are the same. Lists and data.frames are compared element by
# element, and an atomic vector value by value. Two doubles are the same when
# they differ by at most one_file_tolerance of the larger absolute value, or
# by at most one_file_tolerance when that value is below one_file_tolerance.
# Every other value MUST be identical, also a double with a class, and so
# MUST the attributes.
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

# The first difference of the lists `a` and `b`, which are not identical,
# element by element, as first_difference() gives it.
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
  return(attributes_difference(a, b, path))
}

# The first value that differs in the atomic vectors `a` and `b` of the same
# class and length, which are not identical, as first_difference() gives it.
value_difference <- function(a, b, path) {
  same <- vapply(
    seq_along(a),
    function(i) return(identical(a[i], b[i])),
    logical(1)
  )
  # A double with a class, such as a Date or a POSIXct, MUST be identical.
  if (is.double(a) && is.double(b) && !is.object(a)) {
    x <- as.vector(a)
    y <- as.vector(b)
    ok <- is.finite(x) & is.finite(y)
    size <- pmax(abs(x), abs(y))
    limit <- one_file_tolerance * ifelse(size < one_file_tolerance, 1, size)
    same <- same | (ok & abs(x - y) <= limit)
  }
  if (all(same)) {
    return(attributes_difference(a, b, path))
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

# NULL when the attributes of `a` and `b` are identical, or else the text
# that they differ. As for identical(), the order of the attributes does not
# count.
attributes_difference <- function(a, b, path) {
  x <- attributes(a)
  y <- attributes(b)
  if (!is.null(x)) x <- x[order(names(x), method = "radix")]
  if (!is.null(y)) y <- y[order(names(y), method = "radix")]
  if (identical(x, y)) {
    return(NULL)
  }
  return(paste0(path, " has other attributes."))
}
