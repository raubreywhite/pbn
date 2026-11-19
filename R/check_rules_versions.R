# The rules of check_project() that keep each version of a project to its
# own files: frozen_versions_unchanged, no_cross_version_reads and
# no_escape_hatches, with freeze_version(). The set house_2026_11_19 brought
# them. Each rule parses the files and runs no code of them.

# The file of freeze_version() at the top level of a project.
frozen_pattern <- "^v[0-9]{2}_frozen\\.md5$"

# The start of the detail of a version older than the newest without its
# vNN_frozen.md5. add_fix() gives such a finding the fix frozen_missing_fix().
frozen_missing_pattern <- "^v[0-9]{2}_frozen\\.md5: missing, and v[0-9]{2} is older"

# The fix of a finding of frozen_missing_pattern with the detail `detail`.
frozen_missing_fix <- function(detail) {
  version <- substr(detail, 1, 3)
  return(paste0(
    "run pbn::freeze_version(\"",
    version,
    "\"), or restore ",
    version,
    "_frozen.md5 with git if it was deleted"
  ))
}

# The fix of the other findings of frozen_versions_unchanged.
frozen_fix <- paste(
  "put the files of the version back as they were and make the change in a",
  "new version, or run pbn::freeze_version() again to accept the change on",
  "purpose"
)

#' Freeze a version of a project
#'
#' `freeze_version()` writes the file `<version>_frozen.md5` in `home`, with
#' the md5 of `<version>_run.R` and of each file under `<version>_functions/`.
#' The rule `frozen_versions_unchanged` of [check_project()] then fails when
#' one of these files changes, or when a file comes or goes.
#'
#' @param version The version, `"v"` and two digits, for example `"v01"`.
#' @param home The project folder.
#' @details
#' The file has one line for each file, `<md5>  <path>`, in the format of
#' the command `md5sum`. Each path is relative to `home`, and the lines are
#' sorted by path in the C locale. So `md5sum -c v01_frozen.md5`, run in
#' `home`, also checks the files. The files under `<version>_functions/` are
#' all files at any depth, hidden files too.
#'
#' Freeze a version when its results go into a manuscript or to a reader.
#' A change after that goes into a new version. A second call of
#' `freeze_version()` writes the file again, so it accepts every change
#' since the first call. It then prints the files whose md5 it changes.
#' @return Invisibly, the path of the file, relative to `home`.
#' @examples
#' home <- file.path(tempdir(), "pbn_freeze_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines("x <- 1", file.path(home, "v01_run.R"))
#' writeLines("f <- function() return(1)", file.path(home, "v01_functions", "f.R"))
#' pbn::freeze_version("v01", home = home)
#' readLines(file.path(home, "v01_frozen.md5"))
#'
#' unlink(home, recursive = TRUE)
#' @family checks
#' @export
freeze_version <- function(version, home = ".") {
  check_version(version)
  run <- paste0(version, "_run.R")
  folder <- paste0(version, "_functions")
  if (
    !file.exists(file.path(home, run)) || !dir.exists(file.path(home, folder))
  ) {
    stop(
      "freeze_version() needs ",
      run,
      " and ",
      folder,
      "/ in ",
      home,
      ". Give the folder of the project as `home`.",
      call. = FALSE
    )
  }
  md5 <- frozen_md5(home, version)
  file <- paste0(version, "_frozen.md5")
  if (file.exists(file.path(home, file))) {
    old <- read_frozen(file.path(home, file))
    changed <- frozen_changes(old$md5, md5)
    if (nrow(changed) > 0) {
      message(
        "freeze_version(): ",
        file,
        " now accepts these changes:\n",
        paste0("  ", changed$path, ": ", changed$what, collapse = "\n")
      )
    }
  }
  con <- file(file.path(home, file), open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(paste0(md5, "  ", names(md5)), con, sep = "\n")
  return(invisible(file))
}

# The md5 of the run script of the version `version` in `home` and of each
# file under its functions folder, named by the path relative to `home` and
# sorted by it in the C locale. A file that is missing is left out.
frozen_md5 <- function(home, version) {
  run <- paste0(version, "_run.R")
  folder <- paste0(version, "_functions")
  inner <- list.files(
    file.path(home, folder),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  paths <- c(
    if (file.exists(file.path(home, run))) run,
    file.path(folder, inner)
  )
  paths <- sort(paths, method = "radix")
  return(stats::setNames(
    unname(tools::md5sum(file.path(home, paths))),
    paths
  ))
}

# The lines of the file `file` of freeze_version(): `md5`, the md5 named by
# the path, and `bad`, the numbers of the lines that are not
# "<md5>  <path>". A " *" before the path, the binary mode of md5sum, counts
# as "  ". An empty line does not count.
read_frozen <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  number <- seq_along(lines)
  keep <- nzchar(trimws(lines))
  ok <- grepl("^[0-9a-fA-F]{32} [ *].+$", lines)
  good <- keep & ok
  md5 <- stats::setNames(
    tolower(substr(lines[good], 1, 32)),
    substring(lines[good], 35)
  )
  return(list(md5 = md5, bad = number[keep & !ok]))
}

# The differences between the md5 `old` of freeze_version() and the md5
# `new` of the files now, as a data.frame with `path` and `what`, sorted by
# the path in the C locale.
frozen_changes <- function(old, new) {
  both <- intersect(names(old), names(new))
  changed <- both[old[both] != new[both]]
  removed <- setdiff(names(old), names(new))
  added <- setdiff(names(new), names(old))
  out <- data.frame(
    path = c(changed, removed, added),
    what = rep(
      c("changed", "removed", "added"),
      c(length(changed), length(removed), length(added))
    )
  )
  return(out[order(out$path, method = "radix"), , drop = FALSE])
}

# The rule frozen_versions_unchanged:
# - each version but the newest has its vNN_frozen.md5 at the top level. A
#   version is a vNN_run.R or a vNN_functions/ folder. So a deleted
#   vNN_frozen.md5 of an older version fails, and the newest version needs
#   none.
# - for each vNN_frozen.md5, the run script and the files of the functions
#   folder of vNN have the md5 of the file, and no file came or went. The
#   detail names the file, with changed, removed or added.
rule_frozen_versions_unchanged <- function(path) {
  rule <- "frozen_versions_unchanged"
  entries <- project_entries(path)
  frozen <- entries[
    grepl(frozen_pattern, entries) & !dir.exists(file.path(path, entries))
  ]
  out <- list(unfrozen_versions(path, frozen))
  for (f in sort(frozen, method = "radix")) {
    old <- read_frozen(file.path(path, f))
    if (length(old$bad) > 0) {
      out[[paste(f, "bad")]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": line ",
          old$bad[1],
          " is not \"<md5>  <path>\" of pbn::freeze_version()"
        )
      )
    }
    changes <- frozen_changes(old$md5, frozen_md5(path, substr(f, 1, 3)))
    out[[f]] <- project_row(
      rule,
      "FAIL",
      changes$path,
      paste0(
        changes$path,
        ": ",
        changes$what,
        " after pbn::freeze_version() wrote ",
        f,
        recycle0 = TRUE
      )
    )
  }
  return(bind_rows(out))
}

# The findings of frozen_versions_unchanged for each version of `path` but
# the newest without its file in `frozen`, the vNN_frozen.md5 files.
unfrozen_versions <- function(path, frozen) {
  versions <- unique(substr(
    c(run_scripts(path), functions_folders(path)),
    1,
    3
  ))
  versions <- sort(versions, method = "radix")
  newest <- versions[length(versions)]
  missing <- setdiff(versions[-length(versions)], substr(frozen, 1, 3))
  file <- paste0(missing, "_frozen.md5", recycle0 = TRUE)
  return(project_row(
    "frozen_versions_unchanged",
    "FAIL",
    file,
    paste0(
      file,
      ": missing, and ",
      missing,
      " is older than the newest version ",
      newest,
      recycle0 = TRUE
    )
  ))
}

# The run scripts and the files of the functions folders in `path`, the
# files of no_environment_variables, named by their version.
version_files <- function(path) {
  files <- c(run_scripts(path), functions_files(path))
  files <- sort(files, method = "radix")
  return(stats::setNames(files, substr(files, 1, 3)))
}

# The functions that the rule no_escape_hatches warns on a call of, each
# with the package that a call MAY name with `::` or `:::`. A call of the
# same name from another package passes.
escape_hatch_calls <- c(
  suppressWarnings = "base",
  suppressMessages = "base",
  try = "base",
  tryCatch = "base",
  withCallingHandlers = "base"
)

# The rule no_escape_hatches: no run script and no file of a functions folder
# calls a function of escape_hatch_calls, at any depth. A file that R cannot
# parse fails loc_per_file, so this rule skips it.
rule_no_escape_hatches <- function(path) {
  rule <- "no_escape_hatches"
  out <- list()
  for (f in version_files(path)) {
    tokens <- file_tokens(file.path(path, f))
    is_call <- tokens$token == "SYMBOL_FUNCTION_CALL" &
      tokens$text %in% names(escape_hatch_calls)
    package <- vapply(
      which(is_call),
      function(i) return(call_package(tokens, i)),
      character(1)
    )
    own <- is.na(package) | package == escape_hatch_calls[tokens$text[is_call]]
    hit <- which(is_call)[own]
    out[[f]] <- project_row(
      rule,
      "WARN",
      rep(f, length(hit)),
      paste0(
        f,
        ":",
        tokens$line1[hit],
        ": ",
        tokens$text[hit],
        "()",
        recycle0 = TRUE
      )
    )
  }
  return(bind_rows(out))
}

# The parse data of the file `file` in the order of the file, with the
# backticks of a quoted call name removed. No row for a file that R cannot
# parse.
file_tokens <- function(file) {
  tokens <- utils::getParseData(parse_or_null(file, keep_source = TRUE))
  if (is.null(tokens)) {
    return(data.frame(
      token = character(0),
      text = character(0),
      line1 = integer(0),
      col1 = integer(0),
      id = integer(0),
      parent = integer(0)
    ))
  }
  tokens <- tokens[order(tokens$line1, tokens$col1), , drop = FALSE]
  quoted <- tokens$token == "SYMBOL_FUNCTION_CALL"
  tokens$text[quoted] <- gsub("`", "", tokens$text[quoted], fixed = TRUE)
  return(tokens)
}

# A part of a path that names an item of the version vNN of the house
# layout, with the version in the first group.
version_item_pattern <- paste0(
  "^v([0-9]{2})(_(run\\.R|functions|results|cache_gitexclude|test|",
  "check_functions\\.R|one_file\\.R|frozen\\.md5))?$"
)

# What the string `x` names outside the version `version`: "vMM" for the
# first part of a path that names the version vMM or one of its items,
# "adhoc/" for a part adhoc, and NA for neither. The parts are split at "/"
# and "\".
cross_version_target <- function(x, version) {
  parts <- strsplit(x, "[/\\\\]+")[[1]]
  other <- parts[grepl(version_item_pattern, parts)]
  other <- other[sub(version_item_pattern, "v\\1", other) != version]
  if (length(other) > 0) {
    return(sub(version_item_pattern, "v\\1", other[1]))
  }
  if ("adhoc" %in% parts) {
    return("adhoc/")
  }
  return(NA_character_)
}

# The rule no_cross_version_reads: no string constant of a run script or of
# a file of a functions folder of vNN names another version or the folder
# adhoc/, see cross_version_target(). A file that R cannot parse fails
# loc_per_file, so this rule skips it.
rule_no_cross_version_reads <- function(path) {
  rule <- "no_cross_version_reads"
  files <- version_files(path)
  out <- list()
  for (k in seq_along(files)) {
    f <- files[[k]]
    tokens <- file_tokens(file.path(path, f))
    strings <- tokens[tokens$token == "STR_CONST", , drop = FALSE]
    values <- vapply(
      strings$text,
      string_value,
      character(1),
      USE.NAMES = FALSE
    )
    target <- vapply(
      values,
      function(v) {
        return(
          if (is.na(v)) {
            NA_character_
          } else {
            cross_version_target(v, names(files)[k])
          }
        )
      },
      character(1),
      USE.NAMES = FALSE
    )
    hit <- which(!is.na(target))
    out[[f]] <- project_row(
      rule,
      "FAIL",
      rep(f, length(hit)),
      paste0(
        f,
        ":",
        strings$line1[hit],
        ": ",
        vapply(values[hit], shown_string, character(1), USE.NAMES = FALSE),
        " names ",
        target[hit],
        recycle0 = TRUE
      )
    )
  }
  return(bind_rows(out))
}
