#' The path of a folder of the project
#'
#' `folder()` gives the path of one folder of `folders` of
#' [initialize_project()]. Project code reads each folder outside the project
#' through it.
#'
#' @param name The name of the folder in `folders` of [initialize_project()].
#'   The default `"main"` is also the name of one folder given without a name.
#' @param ... Parts of a path inside the folder, each one string, for example
#'   `pbn::folder("main", "sub", "d.csv")`. `folder()` joins them to the
#'   folder with `/`. A part MUST NOT start with `/`, `\\` or a drive such
#'   as `C:`. It MUST NOT hold `..`, so a path never leaves the folder.
#' @details
#' `folder()` stops before [initialize_project()], and on a name that the
#' project does not declare. The error names the declared folders. A folder
#' that does not exist still has its path, so the code of a fake scenario
#' runs on a computer without the real data.
#'
#' [run_plan()] gives the folders to each worker, so `folder()` works in a
#' data function and in an analysis on a worker too.
#'
#' The path of a folder is never in a results file or in a cache key. The
#' manifest names the files of a folder `<folder name>/<path in the folder>`,
#' so the same data in another place gives the same results.
#' @return The path of the folder, or of the path inside it, one string. A
#'   folder that existed at [initialize_project()] has its normalised path
#'   with `/`, and another folder has the path as `folders` gave it.
#' @examples
#' home <- file.path(tempdir(), "pbn_folder_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines("extends: house_2026_11_17", file.path(home, "pbn.yml"))
#' writeLines("CFG <- list()", file.path(home, "v01_functions", "_cfg.R"))
#' pbn::initialize_project(
#'   "v01",
#'   home,
#'   folders = c(main = file.path(home, "data"), lookup = file.path(home, "codes")),
#'   env = new.env()
#' )
#' pbn::folder("lookup")
#' pbn::folder("main", "sub", "d.csv")
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
folder <- function(name = "main", ...) {
  if (is.null(state$folders)) {
    stop("Call pbn::initialize_project() before pbn::folder().", call. = FALSE)
  }
  if (!is_one_string(name) || !name %in% names(state$folders)) {
    stop(
      "The project has no folder ",
      if (is_one_string(name)) {
        encodeString(name, quote = "\"")
      } else {
        "of that name"
      },
      ". The folders of pbn::initialize_project() are: ",
      paste(names(state$folders), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  parts <- list(...)
  for (part in parts) {
    check_folder_part(part, name)
  }
  return(paste(c(state$folders[[name]], unlist(parts)), collapse = "/"))
}

# Stops when `part` of folder() is not one string inside the folder `name`:
# empty, absolute, or with the part `..`.
check_folder_part <- function(part, name) {
  shown <- if (is_one_string(part)) {
    encodeString(part, quote = "\"")
  } else {
    "that is not one string"
  }
  bad <- !is_one_string(part) ||
    !nzchar(part) ||
    grepl("^([/\\\\]|[A-Za-z]:)", part) ||
    ".." %in% strsplit(part, "[/\\\\]")[[1]]
  if (bad) {
    stop(
      "The part ",
      shown,
      " of pbn::folder() leaves the folder ",
      name,
      " or is no path. Each part MUST be one string without a leading / or ",
      "\\ or a drive, and without .., so the path stays inside the folder.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The argument `folders` of initialize_project() after its checks: a named
# character vector of one or more paths. One folder without a name gets the
# name main.
check_folders <- function(folders) {
  if (
    !is.character(folders) ||
      length(folders) == 0 ||
      anyNA(folders) ||
      !all(nzchar(folders))
  ) {
    stop(
      "`folders` must be one or more paths of folders, as a named character ",
      "vector such as c(main = \"../data\").",
      call. = FALSE
    )
  }
  nm <- names(folders)
  if (is.null(nm)) {
    if (length(folders) > 1) {
      stop(
        "`folders` has ",
        length(folders),
        " folders without names. Give each a name, for example ",
        "c(main = \"../data\", lookup = \"../codes\").",
        call. = FALSE
      )
    }
    nm <- "main"
  }
  bad <- is.na(nm) | !nzchar(nm) | make.names(nm) != nm
  if (any(bad)) {
    stop(
      "Each name of `folders` must be a syntactic R name, such as main or ",
      "lookup. These are not: ",
      paste(encodeString(nm[bad], quote = "\""), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(nm) > 0) {
    stop(
      "Each name of `folders` must come once. These come more than once: ",
      paste(unique(nm[duplicated(nm)]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  names(folders) <- nm
  return(folders)
}

# The folders `folders` of the project in `home` at initialize_project():
# `folders`, the named paths, normalised for a folder that exists; `absent`,
# the names of the folders that do not exist; and `files`, the fingerprint
# of the files of the folders that exist, see folder_files(). Stops on a
# folder inside another, and on a folder that holds or is inside the results
# folder or the cache folder of `version`. pbn reads no content of a file.
folders_listing <- function(folders, home, version) {
  exists <- dir.exists(folders)
  folders[exists] <- normalizePath(folders[exists], winslash = "/")
  check_nested_folders(folders[exists], home, version)
  return(list(
    folders = folders,
    absent = names(folders)[!exists],
    files = folder_files(folders[exists])
  ))
}

# Stops when one folder of the named paths `folders` is inside another or
# is the same folder, or when a folder holds or is inside the results folder
# or the cache folder of `version` in `home`, which pbn writes in each run.
check_nested_folders <- function(folders, home, version) {
  key <- function(x) {
    x <- paste0(sub("/+$", "", x), "/")
    return(if (.Platform$OS.type == "windows") tolower(x) else x)
  }
  inside <- function(a, b) return(startsWith(key(a), key(b)))
  nm <- names(folders)
  for (i in seq_along(folders)) {
    for (j in seq_along(folders)[-i]) {
      if (inside(folders[[i]], folders[[j]])) {
        stop(
          "The folder ",
          nm[i],
          " of `folders` is inside the folder ",
          nm[j],
          ", or is the same folder. Declare each folder once, and no folder ",
          "inside another.",
          call. = FALSE
        )
      }
    }
  }
  written <- file.path(home, paste0(version, c("_results", "_cache_gitexclude")))
  for (i in seq_along(folders)) {
    hit <- vapply(
      written,
      function(w) return(inside(folders[[i]], w) || inside(w, folders[[i]])),
      logical(1)
    )
    if (any(hit)) {
      stop(
        "The folder ",
        nm[i],
        " of `folders` holds or is inside ",
        basename(written[hit][1]),
        "/, which pbn writes in each run. Declare a folder of data outside ",
        "it.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# The fingerprint of the files under each folder of the named paths
# `folders`, at any depth: a data.frame with `file`, named <folder
# name>/<path in the folder> with /, `size` in bytes and `mtime`, the
# modification time in seconds since 1970-01-01 UTC. It is sorted by `file`
# in the C locale. One list.files() and one file.info() for each folder, and
# no content of a file. A link to a file has the size and the time of its
# target. Stops on a file without a size, such as a broken link.
folder_files <- function(folders) {
  out <- list(data.frame(
    file = character(0),
    size = numeric(0),
    mtime = numeric(0)
  ))
  for (nm in names(folders)) {
    rel <- list.files(
      folders[[nm]],
      recursive = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    info <- file.info(file.path(folders[[nm]], rel), extra_cols = FALSE)
    out[[nm]] <- data.frame(
      file = paste0(nm, "/", rel, recycle0 = TRUE),
      size = as.numeric(info$size),
      mtime = as.numeric(info$mtime)
    )
  }
  x <- do.call(rbind, unname(out))
  x <- x[order(x$file, method = "radix"), , drop = FALSE]
  rownames(x) <- NULL
  bad <- x$file[is.na(x$size) | is.na(x$mtime)]
  if (length(bad) > 0) {
    stop(
      "pbn cannot read the size or the time of the file ",
      bad[1],
      ", for example a link to a file that does not exist.",
      call. = FALSE
    )
  }
  return(x)
}

# The folders of the project in this R session, for the workers of
# run_plan(). NULL without initialize_project().
worker_folders <- function() {
  return(state$folders)
}

# Sets the folders of the project in the R session that runs it, a worker of
# run_plan(), to `folders`, so that folder() works there.
set_worker_folders <- function(folders) {
  state$folders <- folders
  return(invisible(NULL))
}
