#' Write the run of the real data as one file for a server
#'
#' `to_one_file()` writes one R script that runs the scenario `"real"` of the
#' project on a server without a network. A person pastes the file into R
#' there and runs it from the top to the end.
#'
#' @param ... Nothing. A call with an argument stops, and the error names
#'   the arguments `one_file_home` and `one_file_folders` of
#'   [initialize_project()].
#' @details
#' Call [initialize_project()] first, in the project on this computer, with
#' the paths of the server as `one_file_home` and `one_file_folders`.
#' `to_one_file()` takes the version, the project and the paths from that
#' call. Without the paths, it stops and prints the two arguments to add. It
#' stops when the run script or a file of the functions folder changed after
#' that call. It needs no results. So you MAY generate and read the file
#' before the first full run. The results on the server go to
#' `<one_file_home>/<version>_results/`.
#'
#' The file holds these parts, in this order:
#' 1. A header with the version of pbn, the date, the project folder and the
#'    version. It also gives the md5 of each file in it and the workers.
#' 2. `setwd(home)`, so that the run script runs in its project folder, as
#'    `Rscript <version>_run.R` does.
#' 3. The run script as it is written, with three changes. Its call of
#'    `pbn::initialize_project()` gets the paths of the server as `home` and
#'    `folders`, and the argument `generated`. `generated` holds the code of
#'    each `.R` file of the functions folder, in the order that org sources
#'    them. The call has no `one_file_home`, `one_file_folders` or
#'    `fake_real_data`. The call `pbn::scenarios(CFG)` becomes
#'    `pbn::scenarios(CFG, only = "real")`. A line with only a comment such
#'    as `# pbn::to_one_file()` is not in the file. The section The md5 of
#'    the run script of [initialize_project()] gives the rule.
#' 4. A comment that says what to type next, for example
#'    `pbn::show_blocks()`.
#'
#' The section One file of [initialize_project()] gives the rules of
#' `generated`. On the server, the run needs no `pbn.yml` and no functions
#' folder. pbn writes the results and the run log under `one_file_home`, and, for a
#' data function with `cache = TRUE`, the cache there too. The manifest holds
#' the md5 of the files of this computer. On this computer,
#' [check_one_file()] checks that the file gives the real results of the
#' project.
#'
#' The functions `expect_<scenario>()` of the fake scenarios of
#' `CFG$scenarios` are not in the file, because the run has the real data
#' only. The comment lines directly above such a function go with it, up to
#' a blank line or a line with code. `expect_real()` stays. The fake data
#' code stays, because it is inside the data functions.
#'
#' The run script MAY give `fake_real_data = TRUE` for this computer. The
#' file never does, because the server holds the real data.
#'
#' The file is ASCII, because it goes through a clipboard, which can change
#' a byte that is not ASCII. pbn reads each file of the project as UTF-8 and
#' changes each character that is not ASCII on the parse data, token by
#' token. The files of the project stay as they are:
#' - In a string, the character becomes its escape, `\uXXXX`, or
#'   `\UXXXXXXXX` beyond the BMP, for example `"\u2265"` for the sign
#'   greater than or equal. Every other character of the string stays as it
#'   is written, also its quotes, its escapes and its line breaks. In a raw
#'   string `r"(...)"`, `\u` is text and not an escape. So pbn writes such a
#'   raw string again from its value. It becomes a normal string in double
#'   quotes on one line, with `\n` for each line break. R does not allow
#'   `\u` in a string with an escape `\x` or an octal escape. So pbn writes
#'   such a string from its value too. The code of the
#'   file is `identical()` to the code of the project. Such a string gets
#'   the encoding mark `"UTF-8"` where the string of the project MAY have
#'   `"unknown"`, and `identical()` treats the two as equal.
#' - In a comment, the character becomes the text `<U+XXXX>`.
#' - In a name, for example a name in backticks, `to_one_file()` stops,
#'   because R reads a name only as it is written.
#'
#' `to_one_file()` stops when, and writes no file:
#' - the run script has not exactly one top-level call
#'   `pbn::initialize_project()`, or not exactly one call
#'   `pbn::scenarios(CFG)` with no other argument. It names what it found.
#' - a file in the file holds a name that is not ASCII, a NUL, or a line
#'   that is not UTF-8. It also stops on a string that is not valid UTF-8
#'   after its escapes. The error names the file and the line.
#' - a path of the server or the name of the project folder is not ASCII.
#' - code that stays in the file uses a name of an `expect_<scenario>()` that
#'   the file leaves out. The file would stop on the server. The error names
#'   the file and the line.
#' - the assignment of such a function shares a line with other code. That
#'   is text before it on its first line, or text other than a comment after
#'   it on its last line.
#' - the file does not parse.
#'
#' The file has LF line ends. pbn does not cut long lines, and it prints the
#' length of the longest line. The file keeps the `workers` of
#' `pbn::run_plan()` of the run script, and its header says how to change
#' it. The paths go into the file as they are given, without a final `/` or
#' `\`, so give `/` in a path for Windows too, for example
#' `"N:/durable/project"`.
#'
#' The file is always `<version>_one_file.R` in the folder of the project of
#' the last [initialize_project()] of this R session. The printed line names
#' that folder.
#'
#' These are limits of the file:
#' - The comments of the code go into the file as they are written, with
#'   `<U+XXXX>` for a character that is not ASCII. So a comment with a path
#'   of this computer reaches the server.
#' - The rule `no_absolute_paths` of [check_project()] does not see a path
#'   with one name such as `"/data"`, or a path that `paste0()` or
#'   `sprintf()` builds. Such a path in the code reaches the server too.
#'
#' Put the file in git. It is what ran on the server. `check_project()`
#' knows its name, and its code rules do not read it, because it holds the
#' paths of the server by design.
#' @return Invisibly, the path of the file. It prints the path, the number of
#'   lines, the size in bytes and the length of the longest line.
#' @family one file
#' @seealso `vignette("pbn")`, section "Running on an air-gapped server".
#' @export
to_one_file <- function(...) {
  check_one_file_call("to_one_file", ...length())
  version <- state$version
  file <- file.path(state$home, paste0(version, "_one_file.R"))
  lines <- one_file_lines(version, state$one_file_home, state$one_file_folders)
  write_lf_lines(lines, file)
  cat(
    "pbn: wrote ",
    basename(file),
    " in the project folder ",
    state$home,
    ": ",
    length(lines),
    " lines, ",
    file.size(file),
    " bytes, the longest line ",
    max(nchar(lines)),
    " characters.\n",
    sep = ""
  )
  return(invisible(file))
}

# The lines of the file of to_one_file() for `version` with the paths of
# the server `home` and `folders`. Stops when the file would use a function
# that it leaves out, and when the file does not parse. The file has no line
# that holds only a comment of one_file_comment_pattern.
one_file_lines <- function(version, home, folders) {
  folders <- check_server_paths(home, folders)
  local <- state$home
  check_sources_unchanged()
  fake <- fake_scenarios()
  files <- sourcing_order(local, version)
  script_name <- paste0(version, "_run.R")
  sources <- lapply(c(files, script_name), function(f) {
    return(ascii_lines(local, f))
  })
  names(sources) <- c(files, script_name)
  dropped <- lapply(files, function(f) {
    return(expect_spans(sources[[f]], f, fake))
  })
  names(dropped) <- files
  check_dropped_names(sources, dropped)
  code <- unlist(lapply(files, function(f) {
    lines <- sources[[f]]
    for (ref in rev(dropped[[f]]$refs)) {
      ref <- with_comment_above(lines, ref, dropped[[f]]$tokens)
      lines <- replace_span(lines, ref, character(0))
    }
    return(c(paste0("# ---- ", f, " ----"), lines))
  }))
  run <- one_file_script(sources[[script_name]], version, home, folders, code)
  lines <- c(
    one_file_header(version, home, c(script_name, files), run$workers),
    paste0("setwd(", encodeString(home, quote = "\""), ")"),
    "",
    run$lines,
    "",
    one_file_footer(home, version)
  )
  parsed <- tryCatch(
    parse(text = lines, keep.source = TRUE),
    error = function(e) return(e)
  )
  if (inherits(parsed, "error")) {
    stop(
      "The file of pbn::to_one_file() does not parse, so pbn wrote no file. ",
      "R says: ",
      conditionMessage(parsed),
      call. = FALSE
    )
  }
  # The commented calls of to_one_file() and check_one_file() of the run
  # script are for this computer only.
  drop <- one_file_comment_lines(utils::getParseData(parsed))
  if (length(drop) > 0) {
    lines <- lines[-drop]
  }
  return(lines)
}

# Stops when `n_args`, the number of arguments of the call, is not 0, when
# initialize_project() did not start a project in this R session, when this
# R session runs a file of to_one_file(), or when that initialize_project()
# had no paths of the server. `caller` names the function for the error.
check_one_file_call <- function(caller, n_args) {
  if (n_args > 0) {
    stop(
      "pbn::",
      caller,
      "() takes no argument. It takes the version and the paths of the ",
      "server from the last pbn::initialize_project() of this R session. ",
      "Give the paths of the server there, as one_file_home and ",
      "one_file_folders.",
      call. = FALSE
    )
  }
  check_state(caller)
  if (isTRUE(state$generated)) {
    stop(
      "This R session runs a file of pbn::to_one_file(). Call pbn::",
      caller,
      "() in the project folder.",
      call. = FALSE
    )
  }
  if (is.null(state$one_file_home)) {
    nm <- names(state$folders)
    stop(
      "pbn::",
      caller,
      "() needs the paths of the server, and the last ",
      "pbn::initialize_project() of this R session has none. Add these two ",
      "arguments to the call pbn::initialize_project() of ",
      state$version,
      "_run.R, with the paths on the server, and run the call again:\n",
      "  one_file_home = \"<the project folder on the server>\",\n",
      "  one_file_folders = c(",
      paste0(nm, " = \"<the folder ", nm, " on the server>\"", collapse = ", "),
      ")",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The folders of the server `folders` in the order of the folders of the
# project, after the check that `home`, each path of `folders` and the name
# of the project are ASCII. initialize_project() checked the names.
check_server_paths <- function(home, folders) {
  folders <- folders[names(state$folders)]
  for (x in c(home, folders, state$project)) {
    if (!is_ascii(x)) {
      stop(
        "The path or the name ",
        encodeString(x, quote = "\""),
        " is not ASCII. The file goes through a clipboard, which can change ",
        "such a character.",
        call. = FALSE
      )
    }
  }
  return(folders)
}

# TRUE when each character of the string `x` is ASCII and not NUL.
is_ascii <- function(x) {
  bytes <- as.integer(charToRaw(enc2utf8(x)))
  return(all(bytes > 0L & bytes < 128L))
}

# Stops when the run script or a file of the functions folder is not the
# file that initialize_project() hashed: changed, added or removed. It
# compares the md5 of the bytes, so a change of the paths of the server in
# the run script counts.
check_sources_unchanged <- function() {
  now <- source_md5(state$home, state$version, raw = TRUE)
  then <- state$source_raw_md5
  both <- intersect(names(now), names(then))
  changed <- c(
    setdiff(union(names(now), names(then)), both),
    both[now[both] != then[both]]
  )
  if (length(changed) > 0) {
    stop(
      "These files changed after pbn::initialize_project() ran: ",
      paste(sort(changed, method = "radix"), collapse = ", "),
      ". Run pbn::initialize_project() again, then pbn::to_one_file().",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The fake scenarios of CFG$scenarios of the sourced environment.
fake_scenarios <- function() {
  cfg <- get0("CFG", envir = state$env, inherits = FALSE)
  s <- if (is.list(cfg) && is.data.frame(cfg$scenarios)) {
    as.character(cfg$scenarios$scenario)
  } else {
    character(0)
  }
  return(setdiff(s, "real"))
}

# The .R files of the functions folder of `version` in `home` that org
# sources, relative to `home`, in the order that org sources them. pbn calls
# the export of org for the rule and does not write a second copy of it.
sourcing_order <- function(home, version) {
  folder <- paste0(version, "_functions")
  files <- org::ls_files(file.path(home, folder), regexp = "\\.[rR]$")
  files <- files[!dir.exists(files)]
  sorted <- org::sort_for_sourcing(files)
  return(paste0(folder, "/", basename(sorted), recycle0 = TRUE))
}

# The lines of the file `rel` of `home`, without CR, as ASCII: see
# ascii_code(). Stops on a NUL and on a file that is not UTF-8, and names
# the file and its line.
ascii_lines <- function(home, rel) {
  path <- file.path(home, rel)
  bytes <- readBin(path, "raw", file.size(path))
  bytes <- bytes[bytes != as.raw(13)]
  nul <- which(bytes == as.raw(0))
  if (length(nul) > 0) {
    stop(
      rel,
      ":",
      sum(bytes[seq_len(nul[1])] == as.raw(10)) + 1,
      " holds a NUL byte. Remove it.",
      call. = FALSE
    )
  }
  lines <- split_lines(bytes)
  Encoding(lines) <- "UTF-8"
  bad <- which(!validUTF8(lines))
  if (length(bad) > 0) {
    stop(
      rel,
      ":",
      bad[1],
      " is not UTF-8. pbn::to_one_file() reads the code as UTF-8. Save the ",
      "file as UTF-8.",
      call. = FALSE
    )
  }
  return(ascii_code(lines, rel))
}

# The lines of the bytes `bytes`, split at each LF, without a last empty
# line after a final LF. pbn splits the bytes, so a line that is not valid
# in the locale stays as it is.
split_lines <- function(bytes) {
  if (length(bytes) == 0) {
    return(character(0))
  }
  nl <- which(bytes == as.raw(10))
  starts <- c(1L, nl + 1L)
  ends <- c(nl - 1L, length(bytes))
  if (bytes[length(bytes)] == as.raw(10)) {
    starts <- starts[-length(starts)]
    ends <- ends[-length(ends)]
  }
  return(vapply(
    seq_along(starts),
    function(i) {
      if (ends[i] < starts[i]) {
        return("")
      }
      return(rawToChar(bytes[starts[i]:ends[i]]))
    },
    character(1)
  ))
}

# The top-level assignments of expect_<scenario> for the fake scenarios
# `fake` in the lines `lines` of the file `rel`: `refs`, the source
# reference of each, `names`, the names, and `ids`, the ids of each in the
# parse data `tokens`. Stops when such an assignment shares a line with
# other code: text before it on its first line, or text other than a
# comment after it on its last line.
expect_spans <- function(lines, rel, fake) {
  exprs <- parse(text = lines, keep.source = TRUE)
  refs <- attr(exprs, "srcref")
  tokens <- utils::getParseData(exprs)
  targets <- paste0("expect_", fake)
  hit <- vapply(
    as.list(exprs),
    function(e) {
      return(any(vapply(targets, is_assignment_to, logical(1), e = e)))
    },
    logical(1)
  )
  out <- list(
    refs = list(),
    names = character(0),
    ids = integer(0),
    tokens = tokens
  )
  for (i in which(hit)) {
    full <- as.integer(refs[[i]])
    ref <- full[1:4]
    # The node of the expression in the parse data: at the top level, with
    # its first and last line and column.
    out$ids <- c(
      out$ids,
      tokens$id[
        tokens$parent == 0 &
          tokens$line1 == full[1] &
          tokens$col1 == full[5] &
          tokens$line2 == full[3] &
          tokens$col2 == full[6]
      ]
    )
    before <- trimws(substr(lines[ref[1]], 1, ref[2] - 1))
    after <- trimws(substr(lines[ref[3]], ref[4] + 1, nchar(lines[ref[3]])))
    name <- as.character(exprs[[i]][[2]])
    if (nzchar(before) || (nzchar(after) && !startsWith(after, "#"))) {
      stop(
        rel,
        ":",
        if (nzchar(before)) ref[1] else ref[3],
        ": the assignment of ",
        name,
        " shares its line with other code. pbn::to_one_file() leaves ",
        name,
        " out, so put it on lines of its own.",
        call. = FALSE
      )
    }
    out$refs <- c(out$refs, list(ref))
    out$names <- c(out$names, name)
  }
  return(out)
}

# Stops when code that stays in the file of to_one_file() uses a name of
# the functions that it leaves out. `sources` holds the lines of each file,
# and `dropped` the value of expect_spans() for each file of the functions
# folder. The error names the file and the line.
check_dropped_names <- function(sources, dropped) {
  gone <- unlist(lapply(dropped, function(d) return(d$names)))
  if (length(gone) == 0) {
    return(invisible(NULL))
  }
  for (rel in names(sources)) {
    tokens <- if (is.null(dropped[[rel]])) {
      utils::getParseData(parse(text = sources[[rel]], keep.source = TRUE))
    } else {
      dropped[[rel]]$tokens
    }
    if (is.null(tokens)) {
      next
    }
    ids <- if (is.null(dropped[[rel]])) integer(0) else dropped[[rel]]$ids
    parents <- stats::setNames(tokens$parent, tokens$id)
    uses <- tokens[
      tokens$token %in%
        c("SYMBOL", "SYMBOL_FUNCTION_CALL") &
        tokens$text %in% gone,
      ,
      drop = FALSE
    ]
    keep <- !vapply(
      uses$id,
      function(id) return(id %in% ids || has_ancestor(id, parents, ids)),
      logical(1)
    )
    uses <- uses[keep, , drop = FALSE]
    if (nrow(uses) > 0) {
      uses <- uses[order(uses$line1), , drop = FALSE]
      stop(
        rel,
        ":",
        uses$line1[1],
        " uses ",
        uses$text[1],
        ", and pbn::to_one_file() leaves that function of a fake scenario ",
        "out. The file would stop on the server. Move the code that the ",
        "real data needs into a function of its own.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# The source reference `ref` of a dropped function, moved up to the first
# line of the comment block directly above it: the lines that hold only a
# comment, up to a blank line or a line with code. The comment belongs to
# the function, so it goes with it.
with_comment_above <- function(lines, ref, tokens) {
  terminal <- tokens[tokens$terminal, , drop = FALSE]
  # A line holds only a comment when the one token that covers it is a
  # COMMENT. The parse data decides, so a line of a string is not a comment.
  only_comment <- function(n) {
    on <- terminal[terminal$line1 <= n & terminal$line2 >= n, , drop = FALSE]
    return(nrow(on) == 1 && on$token == "COMMENT")
  }
  first <- ref[[1]]
  while (first > 1 && only_comment(first - 1)) {
    first <- first - 1
  }
  if (first < ref[[1]]) {
    ref[[1]] <- first
    ref[[2]] <- 1L
  }
  return(ref)
}

# The lines `lines` with the text of the source reference `ref`, from its
# first to its last byte, replaced by the lines `new`. A line that holds
# only blanks after the change goes.
replace_span <- function(lines, ref, new) {
  first <- ref[[1]]
  last <- ref[[3]]
  before <- substr(lines[first], 1, ref[[2]] - 1)
  after <- substr(lines[last], ref[[4]] + 1, nchar(lines[last]))
  if (length(new) == 0) {
    middle <- paste0(before, after)
    middle <- middle[nzchar(trimws(middle))]
  } else {
    middle <- new
    middle[1] <- paste0(before, middle[1])
    middle[length(middle)] <- paste0(middle[length(middle)], after)
  }
  return(c(lines[seq_len(first - 1)], middle, lines[-seq_len(last)]))
}
