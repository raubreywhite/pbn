# create_project() and new_version(): the files of a new project, and a new
# version of a project. The files of a new project are in
# inst/templates/project/, and AGENTS.md is inst/templates/AGENTS.md.

# The rule set that the pbn.yml of create_project() names. The files of
# inst/templates/project/ pass each rule of this set.
create_project_set <- "house_2026_11_26"

#' Create a new project
#'
#' `create_project()` writes the files of a new project at version `v01` into
#' the folder `home`. The project passes [check_project()] at once, and
#' [verify()] gives a PASS on the fake scenarios without the real data.
#'
#' @param home The project folder. It MUST exist.
#' @param folders The folders outside the project that the project reads, as
#'   for [initialize_project()]. The run script gets them as they are given.
#' @details
#' `create_project()` writes these files:
#' - `pbn.yml`, with the line `extends: house_2026_11_26`.
#' - `v01_run.R`, the run script. Its data steps are `raw`, `clean` and
#'   `cohort`, and it has one analysis kind, `risk_difference`.
#' - `v01_functions/`, with `_cfg.R`, the data steps `data_raw.R`,
#'   `data_clean.R` and `data_cohort.R`, the analysis
#'   `fit_risk_difference.R`, the checks `expect_known.R` and the output
#'   function `output.R`.
#' - `README.md`, with a title from the name of `home`, a section How to run,
#'   and the arithmetic of the fake scenario.
#' - `CHANGELOG.md`, with the section `## v01`.
#' - `AGENTS.md`, a copy of
#'   `system.file("templates", "AGENTS.md", package = "pbn")`.
#' - `.gitignore`, with the line `*gitexclude*`.
#' - `check_project.R`, with the line `pbn::check_project()`.
#'
#' `CFG$scenarios` has the scenario `real` and the fake scenario `known`.
#' `known` has 10 rows. The step cohort excludes 1, and the difference of the
#' risks is 0.35. `expect_known()` checks both numbers.
#'
#' The run script calls `pbn::scenarios(CFG, only = setdiff(CFG$scenarios$scenario, "real"))`.
#' So a run includes each fake scenario of `CFG$scenarios`, and never
#' `real`. That is why `verify("v01")` gives a PASS on a computer without
#' the real data. Its verdict line then says that the PASS covers the fake
#' scenarios only. `data_raw()` stops for `real` with the message to
#' replace the `stop()` with code that reads the real data. When that code
#' is there, remove `only` from the call.
#'
#' The files hold no comments. `README.md` says why, and `CHANGELOG.md`
#' says what changed.
#'
#' `create_project()` writes no file that is already there. It stops before
#' it writes anything in each of these cases:
#' - one of its files exists. The error names each such file.
#' - the package cohort is not installed, because the step `cohort` needs
#'   it. The error gives the line that installs it:
#'   `pak::pak(c("raubreywhite/pbn", "raubreywhite/cohort"))`.
#'
#' When a write stops part way, `create_project()` removes each file and
#' folder that the call created. So a second call can start again.
#' @return Invisibly, the paths of the files that it wrote, relative to
#'   `home`.
#' @examples
#' home <- file.path(tempdir(), "pbn_create_example")
#' dir.create(home)
#' pbn::create_project(home = home)
#' pbn::check_project(home)
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @export
create_project <- function(home = ".", folders = c(main = "../data")) {
  check_folders(folders)
  if (!cohort_installed()) {
    stop(
      "create_project() writes a project whose data step cohort needs the ",
      "package cohort, and cohort is not installed. Install it with ",
      cohort_install_line,
      ", and run create_project() again.",
      call. = FALSE
    )
  }
  if (!dir.exists(home)) {
    stop(
      "The folder ",
      home,
      " does not exist. Create it, and run create_project() again.",
      call. = FALSE
    )
  }
  files <- project_template(home, folders)
  there <- names(files)[file.exists(file.path(home, names(files)))]
  if (length(there) > 0) {
    stop(
      "create_project() writes no file that is already there. These files ",
      "of the new project exist in ",
      home,
      ": ",
      paste(there, collapse = ", "),
      ". Move them, or give another folder as `home`.",
      call. = FALSE
    )
  }
  created <- new_paths(home, names(files))
  done <- FALSE
  on.exit(if (!done) remove_created(home, created), add = TRUE)
  for (f in names(files)) {
    write_text(file.path(home, f), files[[f]])
  }
  done <- TRUE
  message(
    "create_project(): wrote ",
    length(files),
    " files in ",
    normalizePath(home, winslash = "/"),
    ". The project is at version v01. Run pbn::verify(\"v01\") in that ",
    "folder."
  )
  return(invisible(names(files)))
}

# The files of a new project in `home` with the folders `folders`: a named
# list of the lines of each file, named by the path relative to `home`.
project_template <- function(home, folders) {
  folder <- system.file("templates", "project", package = "pbn")
  paths <- sort(
    list.files(folder, recursive = TRUE, all.files = TRUE),
    method = "radix"
  )
  files <- lapply(paths, function(p) {
    return(readLines(file.path(folder, p), warn = FALSE, encoding = "UTF-8"))
  })
  names(files) <- paths
  files[["v01_run.R"]] <- sub(
    "{{folders}}",
    deparse1(folders, collapse = " "),
    files[["v01_run.R"]],
    fixed = TRUE
  )
  files[["README.md"]] <- sub(
    "{{name}}",
    basename(normalizePath(home, winslash = "/")),
    files[["README.md"]],
    fixed = TRUE
  )
  agents <- system.file("templates", "AGENTS.md", package = "pbn")
  return(c(
    list(
      pbn.yml = paste0("extends: ", create_project_set),
      .gitignore = "*gitexclude*",
      check_project.R = "pbn::check_project()",
      AGENTS.md = readLines(agents, warn = FALSE, encoding = "UTF-8")
    ),
    files
  ))
}

# The line that installs pbn and cohort, for the error of create_project()
# and the prompt of README.md. DESCRIPTION names both under Remotes.
cohort_install_line <- "pak::pak(c(\"raubreywhite/pbn\", \"raubreywhite/cohort\"))"

# TRUE when the package cohort is installed. In one place, so that the tests
# can mock it.
cohort_installed <- function() {
  return(requireNamespace("cohort", quietly = TRUE))
}

# The paths of `paths`, relative to `home`, that do not exist, and each
# folder above them that does not exist, the deepest first. A call that
# writes `paths` creates exactly these.
new_paths <- function(home, paths) {
  folders <- parent_folders(paths)
  out <- c(paths, folders)
  out <- out[!file.exists(file.path(home, out))]
  depth <- lengths(strsplit(out, "/", fixed = TRUE))
  return(out[order(-depth, out, method = "radix")])
}

# Removes the paths `paths` of new_paths() under `home`, so that a call that
# stopped part way leaves nothing it created. A folder goes with all it
# holds, because the call created it.
remove_created <- function(home, paths) {
  unlink(file.path(home, paths), recursive = TRUE)
  return(invisible(paths))
}

# Writes the lines `lines` to the file `file`, each line with LF, also on
# Windows. Creates the folder of the file.
write_text <- function(file, lines) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(lines), con, sep = "\n", useBytes = TRUE)
  return(invisible(file))
}

#' Start a new version of a project
#'
#' `new_version()` freezes the version `from` and copies its run script and
#' its functions folder to the version `to`. The project then passes the
#' rule `frozen_versions_unchanged` of [check_project()].
#'
#' @param from The version to copy, `"v"` and two digits, for example
#'   `"v01"`.
#' @param to The new version, or `NULL`, the default, for the version after
#'   `from`. For `"v01"` that is `"v02"`. After a manual manuscript
#'   `manuscript/manual/vNN_<INITIALS>.docx`, `to` is `v(NN+1)`, as the rule
#'   `manual_not_ahead` of [check_project()] says.
#' @param home The project folder.
#' @details
#' `new_version()` does these steps in this order:
#' 1. It freezes `from` with [freeze_version()], when `<from>_frozen.md5`
#'    does not exist. When it exists and no file of `from` changed after it,
#'    `new_version()` leaves it as it is.
#' 2. It copies `<from>_run.R` to `<to>_run.R`, and each file under
#'    `<from>_functions/` to `<to>_functions/`.
#' 3. It changes the argument `version` of the call
#'    `pbn::initialize_project()` in `<to>_run.R` from `from` to `to`. It
#'    changes no other text of the file.
#' 4. It adds an entry `<to>_functions/<path>` to each list under `allow:`
#'    of `pbn.yml` that has the entry `<from>_functions/<path>`, so the
#'    copied file stays allowed. `<path>` MAY be empty, as in
#'    `<from>_functions/`. It edits two forms of a list, each with or
#'    without quotes around an entry and a trailing comment:
#'    - a flow list `rule: [a, b]` on one line. The new entries go at the
#'      end of the list.
#'    - a block list of lines `- a`. Each new entry goes on a new line
#'      after the old entry.
#'
#'    A new entry has the quotes of the old entry. A line that has no entry
#'    of `<from>_functions/` stays as it is, so each comment stays. The step
#'    adds no entry that the list has already. It prints each entry that it
#'    adds. After the edit it reads `pbn.yml` again with the rules of
#'    [check_project()]. When the entries under `allow:` are not the old
#'    entries and the new entries, it stops. The error gives the entries to
#'    add by hand. One cause is a flow list on more than one line.
#' 5. It adds the section `## <to>` above the first section `## ` of
#'    `CHANGELOG.md`, with one line that names `from`. It creates
#'    `CHANGELOG.md` when the file does not exist. When the section is there
#'    already, it leaves the file as it is, and its message says so.
#'
#' It stops before it writes anything in each of these cases:
#' - `<to>_run.R`, `<to>_functions/` or `<to>_frozen.md5` exists. The error
#'   names each one.
#' - `<from>_run.R` or `<from>_functions/` does not exist.
#' - `from` is not the newest version of the project. The error names the
#'   newest.
#' - `to` is not newer than each version of the project.
#' - `<from>_frozen.md5` exists, and a file of `from` changed, came or went
#'   after it. The error names each file. Put the files back, or record the
#'   change as intended with [freeze_version()].
#' - `<from>_run.R` has no call `pbn::initialize_project()` whose argument
#'   `version` is the string `from`.
#' - `pbn.yml` exists, and [check_project()] cannot read its rules.
#'
#' When a step stops after `new_version()` wrote something, it removes each
#' file and folder that the call created, also `<from>_frozen.md5`. It
#' writes `CHANGELOG.md` and `pbn.yml` back as they were. So a second call can start again.
#' @return Invisibly, `to`.
#' @examples
#' home <- file.path(tempdir(), "pbn_new_version_example")
#' dir.create(home)
#' pbn::create_project(home = home)
#' pbn::new_version("v01", home = home)
#' pbn::check_project(home)
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @export
new_version <- function(from = "v01", to = NULL, home = ".") {
  check_version(from)
  if (is.null(to)) {
    to <- next_version(from)
  }
  check_version(to)
  targets <- paste0(to, c("_run.R", "_functions", "_frozen.md5"))
  there <- targets[file.exists(file.path(home, targets))]
  if (length(there) > 0) {
    stop(
      "new_version() writes no file that is already there. These files and ",
      "folders of ",
      to,
      " exist in ",
      home,
      ": ",
      paste(there, collapse = ", "),
      ". Give another version as `to`.",
      call. = FALSE
    )
  }
  run <- paste0(from, "_run.R")
  folder <- paste0(from, "_functions")
  if (
    !file.exists(file.path(home, run)) || !dir.exists(file.path(home, folder))
  ) {
    stop(
      "new_version() needs ",
      run,
      " and ",
      folder,
      "/ in ",
      home,
      ". Give the folder of the project as `home`, or give a version that ",
      "exists as `from`.",
      call. = FALSE
    )
  }
  check_from_newest(home, from)
  check_newest(home, to)
  freeze <- check_frozen_from(home, from)
  run_lines <- run_script_with_version(file.path(home, run), from, to)
  allow <- allow_to_add(home, from, to)

  changelog <- file.path(home, "CHANGELOG.md")
  old_changelog <- read_bytes(changelog)
  yml <- file.path(home, rules_file)
  old_yml <- read_bytes(yml)
  created <- new_paths(
    home,
    c(targets[1:2], paste0(from, "_frozen.md5"), "CHANGELOG.md")
  )
  done <- FALSE
  on.exit(
    if (!done) {
      undo_new_version(home, created, changelog, old_changelog, yml, old_yml)
    },
    add = TRUE
  )
  if (freeze) {
    freeze_version(from, home)
  }
  write_text(file.path(home, targets[1]), run_lines)
  copy_folder(file.path(home, folder), file.path(home, targets[2]))
  add_allow_entries(home, allow, from)
  added <- add_changelog_section(changelog, from, to)
  done <- TRUE
  message(
    "new_version(): ",
    from,
    if (freeze) " is now frozen in " else " stays frozen in ",
    from,
    "_frozen.md5. Wrote ",
    to,
    "_run.R and ",
    to,
    "_functions/. ",
    if (added) {
      "Added the section ## "
    } else {
      "CHANGELOG.md already has the section ## "
    },
    to,
    if (added) " to CHANGELOG.md." else "."
  )
  for (i in seq_len(nrow(allow$add))) {
    message(
      "new_version(): added ",
      allow$add$new[i],
      " to allow: ",
      allow$add$rule[i],
      " in pbn.yml."
    )
  }
  return(invisible(to))
}

# The bytes of the file `file`, or NULL when it does not exist.
read_bytes <- function(file) {
  if (!file.exists(file)) {
    return(NULL)
  }
  return(readBin(file, "raw", file.size(file)))
}

# The version after `version`, "v01" to "v02". Stops after "v99".
next_version <- function(version) {
  n <- as.integer(substr(version, 2, 3)) + 1L
  if (n > 99) {
    stop(
      "There is no version after v99.",
      call. = FALSE
    )
  }
  return(sprintf("v%02d", n))
}

# Removes what a call of new_version() that stopped part way created: the
# paths `created` of new_paths() under `home`. Writes the bytes
# `old_changelog` back to the file `changelog` when it existed before, and
# the bytes `old_yml` back to the file `yml`, pbn.yml, when they differ.
undo_new_version <- function(
  home,
  created,
  changelog,
  old_changelog,
  yml = NULL,
  old_yml = NULL
) {
  remove_created(home, created)
  if (!is.null(old_changelog)) {
    writeBin(old_changelog, changelog)
  }
  if (!is.null(old_yml) && !identical(read_bytes(yml), old_yml)) {
    writeBin(old_yml, yml)
  }
  return(invisible(created))
}

# Stops when the version `from` is not the newest version of the project in
# `home`. A new version starts from the newest, so that each older version
# stays as it was frozen.
check_from_newest <- function(home, from) {
  versions <- project_versions(home)
  newest <- versions[length(versions)]
  if (!identical(from, newest)) {
    stop(
      "new_version() starts a new version from the newest version, ",
      newest,
      ", and ",
      from,
      " is older. Give from = \"",
      newest,
      "\".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Stops when the version `to` is not newer than each version of the project
# in `home`. Each version but the newest MUST be frozen, so a new version
# MUST be the newest.
check_newest <- function(home, to) {
  versions <- project_versions(home)
  newer <- versions[versions >= to]
  if (length(newer) > 0) {
    stop(
      "new_version() makes the newest version, and ",
      to,
      " is not newer than ",
      newer[length(newer)],
      ". Give a newer version as `to`.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# TRUE when the version `from` in `home` needs freeze_version(), FALSE when
# its vNN_frozen.md5 exists and holds the md5 of its files now. Stops when
# that file exists and a file changed, came or went after it.
check_frozen_from <- function(home, from) {
  file <- file.path(home, paste0(from, "_frozen.md5"))
  if (!file.exists(file)) {
    return(TRUE)
  }
  old <- read_frozen(file)
  changes <- frozen_changes(old$md5, frozen_md5(home, from))
  if (length(old$bad) == 0 && nrow(changes) == 0) {
    return(FALSE)
  }
  what <- c(
    if (length(old$bad) > 0) {
      paste0(basename(file), ": line ", old$bad[1], " is not \"<md5>  <path>\"")
    },
    paste0(changes$path, ": ", changes$what, recycle0 = TRUE)
  )
  stop(
    from,
    " is frozen, and ",
    basename(file),
    " does not match the files of ",
    from,
    ":\n",
    paste0("  ", what, collapse = "\n"),
    "\nPut the files back as they were, or run pbn::freeze_version(\"",
    from,
    "\") to record the change as intended.",
    call. = FALSE
  )
}

# The lines of the run script `file`, with the string `from` of the
# argument version of the call pbn::initialize_project() changed to `to`.
# Stops when there is no such call, or more than one.
run_script_with_version <- function(file, from, to) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  token <- init_version_token(file, from)
  if (nrow(token) != 1) {
    stop(
      "new_version() needs one call pbn::initialize_project(version = \"",
      from,
      "\") in ",
      basename(file),
      ", and it found ",
      nrow(token),
      ".",
      call. = FALSE
    )
  }
  line <- lines[token$line1]
  lines[token$line1] <- paste0(
    substr(line, 1, token$col1 - 1),
    "\"",
    to,
    "\"",
    substring(line, token$col2 + 1)
  )
  return(lines)
}

# The parse data of each string `version` that is the argument version of a
# call pbn::initialize_project() in the file `file`. The argument is named
# version, or it is the first argument without a name.
init_version_token <- function(file, version) {
  tokens <- file_tokens(file)
  terms <- tokens[tokens$terminal, , drop = FALSE]
  ns <- which(terms$token == "NS_GET")
  ns <- ns[ns > 1 & ns < nrow(terms)]
  calls <- ns[
    terms$text[ns - 1] == "pbn" &
      terms$token[ns + 1] == "SYMBOL_FUNCTION_CALL" &
      terms$text[ns + 1] == "initialize_project"
  ]
  out <- tokens[0, , drop = FALSE]
  for (i in calls) {
    call_id <- tokens$parent[tokens$id == terms$parent[i + 1]]
    arg <- version_argument(tokens, call_id)
    if (!is.null(arg) && identical(string_value(arg$text), version)) {
      out <- rbind(out, arg)
    }
  }
  return(out)
}

# The STR_CONST row of the parse data `tokens` that is the argument version
# of the call with the id `call_id`, or NULL when that argument is not one
# string.
version_argument <- function(tokens, call_id) {
  kids <- tokens[tokens$parent == call_id, , drop = FALSE]
  kids <- kids[order(kids$line1, kids$col1), , drop = FALSE]
  # The first child is the function, and the second is "(".
  kids <- kids[-(1:2), , drop = FALSE]
  named <- which(kids$token == "SYMBOL_SUB" & kids$text == "version")
  if (length(named) == 1) {
    value <- kids[named + 2, , drop = FALSE]
  } else {
    plain <- kids$token == "expr" &
      !c(FALSE, kids$token[-nrow(kids)] == "EQ_SUB")
    value <- kids[which(plain)[1], , drop = FALSE]
  }
  if (nrow(value) != 1 || is.na(value$id) || value$token != "expr") {
    return(NULL)
  }
  inner <- tokens[tokens$parent == value$id, , drop = FALSE]
  if (nrow(inner) != 1 || inner$token != "STR_CONST") {
    return(NULL)
  }
  return(inner)
}

# Copies each file under the folder `from` to the same path under the new
# folder `to`, hidden files too, with the folders between them.
copy_folder <- function(from, to) {
  dir.create(to)
  inner <- list.files(
    from,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  is_dir <- dir.exists(file.path(from, inner))
  for (d in inner[is_dir]) {
    dir.create(file.path(to, d), recursive = TRUE, showWarnings = FALSE)
  }
  for (f in inner[!is_dir]) {
    ok <- file.copy(file.path(from, f), file.path(to, f), copy.date = TRUE)
    if (!ok) {
      stop("new_version() could not copy ", f, " to ", to, call. = FALSE)
    }
  }
  return(invisible(to))
}

# Adds the section "## <to>" to the file `file` of the changes, above its
# first section "## ". A file without such a section gets it at its end.
# A file that does not exist gets a title too. A file that has the section
# already stays as it is. Returns TRUE when it added the section, and FALSE
# when the section was there.
add_changelog_section <- function(file, from, to) {
  lines <- if (file.exists(file)) {
    readLines(file, warn = FALSE, encoding = "UTF-8")
  } else {
    c("# Changelog", "")
  }
  if (any(grepl(paste0("^## ", to, "([[:space:]]|$)"), lines))) {
    return(FALSE)
  }
  section <- c(
    paste("##", to),
    "",
    paste0("- Started from ", from, " with `pbn::new_version()`."),
    ""
  )
  first <- which(startsWith(lines, "## "))
  if (length(first) == 0) {
    if (length(lines) > 0 && nzchar(lines[length(lines)])) {
      lines <- c(lines, "")
    }
    lines <- c(lines, section[-length(section)])
  } else {
    at <- first[1]
    lines <- c(lines[seq_len(at - 1)], section, lines[at:length(lines)])
  }
  write_text(file, lines)
  return(TRUE)
}

# The allow entries of pbn.yml in `home` that new_version() adds when it
# copies `<from>_functions/` to `<to>_functions/`. A list with `old`, the
# entries of read_rules() before the edit, and `add`, a data.frame with the
# columns `rule`, `written` (the old entry as pbn.yml writes it) and `new`.
# `add` has one row for each entry that starts with `<from>_functions/` and
# whose new entry the rule does not have yet. No pbn.yml gives no rows.
allow_to_add <- function(home, from, to) {
  none <- data.frame(
    rule = character(0),
    written = character(0),
    new = character(0)
  )
  if (!file.exists(file.path(home, rules_file))) {
    return(list(old = NULL, add = none))
  }
  old <- read_rules(home)$allow
  prefix <- paste0(from, "_functions/")
  hit <- old[startsWith(old$written, prefix), , drop = FALSE]
  add <- data.frame(
    rule = hit$rule,
    written = hit$written,
    new = paste0(
      to,
      "_functions/",
      substring(hit$written, nchar(prefix) + 1),
      recycle0 = TRUE
    )
  )
  key <- paste(add$rule, sub("/$", "", add$new), sep = "\n")
  keep <- !key %in% paste(old$rule, old$path, sep = "\n") & !duplicated(key)
  return(list(old = old, add = add[keep, , drop = FALSE]))
}

# Adds the entries `allow$add` of allow_to_add() to the pbn.yml in `home`.
# It edits the text, so each comment and each other line stays. Then it
# reads pbn.yml again, and stops when the entries are not the old ones and
# the new ones, or when a line changed that is not an entry of
# `<from>_functions/`. The error gives the entries to add by hand.
add_allow_entries <- function(home, allow, from) {
  if (nrow(allow$add) == 0) {
    return(invisible(FALSE))
  }
  file <- file.path(home, rules_file)
  text <- rawToChar(read_bytes(file))
  Encoding(text) <- "UTF-8"
  eol <- if (grepl("\r\n", text, fixed = TRUE)) "\r\n" else "\n"
  old_lines <- strsplit(text, eol, fixed = TRUE)[[1]]
  edit <- allow_lines_with(old_lines, allow$add)
  new_text <- paste0(
    paste(edit$lines, collapse = eol),
    if (endsWith(text, eol)) eol
  )
  writeBin(charToRaw(enc2utf8(new_text)), file)
  if (!allow_edit_ok(home, old_lines, edit$inserted, allow, from, eol)) {
    stop(
      "new_version() could not add these entries under allow: in pbn.yml. ",
      "pbn.yml MAY have a form that new_version() does not edit, such as a ",
      "flow list on more than one line. new_version() wrote pbn.yml back ",
      "and removed what it created. Add the entries by hand, and call ",
      "new_version() again:\n",
      paste0("  ", allow$add$rule, ": ", allow$add$new, collapse = "\n"),
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

# TRUE when the pbn.yml in `home`, after add_allow_entries() wrote it, has
# the entries of `allow$old` and `allow$add` under allow, and no others, and
# when each line of `old_lines` is there unchanged, except an entry line of
# allow_entry_lines() that names `<from>_functions/`. `inserted` marks the
# lines that the edit added.
allow_edit_ok <- function(home, old_lines, inserted, allow, from, eol) {
  new <- tryCatch(read_rules(home)$allow, error = function(e) return(NULL))
  if (is.null(new)) {
    return(FALSE)
  }
  want <- c(
    paste(allow$old$rule, allow$old$written, sep = "\n"),
    paste(allow$add$rule, allow$add$new, sep = "\n")
  )
  got <- paste(new$rule, new$written, sep = "\n")
  if (!identical(sort(want, method = "radix"), sort(got, method = "radix"))) {
    return(FALSE)
  }
  text <- rawToChar(read_bytes(file.path(home, rules_file)))
  Encoding(text) <- "UTF-8"
  disk <- strsplit(text, eol, fixed = TRUE)[[1]]
  if (length(disk) != length(inserted)) {
    return(FALSE)
  }
  kept <- disk[!inserted]
  if (length(kept) != length(old_lines)) {
    return(FALSE)
  }
  may_change <- allow_entry_lines(old_lines) &
    grepl(paste0(from, "_functions/"), old_lines, fixed = TRUE)
  return(all(may_change[kept != old_lines]))
}

# TRUE for each line of `lines` that is inside the key allow of pbn.yml:
# the lines after "allow:" up to the next key at the top level.
allow_section <- function(lines) {
  n <- length(lines)
  at <- seq_len(n)
  start <- which(grepl("^allow:[[:space:]]*(#.*)?$", lines))
  if (length(start) != 1) {
    return(rep(FALSE, n))
  }
  after <- which(at > start & grepl("^[^[:space:]#]", lines))
  end <- if (length(after) > 0) after[1] - 1 else n
  return(at > start & at <= end)
}

# TRUE for each line of `lines` that holds entries of a list under the key
# allow of pbn.yml: a line "rule: [...]" of a flow list, or a line "- entry"
# of a block list after a rule.
allow_entry_lines <- function(lines) {
  inside <- allow_section(lines)
  rule <- inside & grepl("^[[:space:]]+[A-Za-z0-9_]+:", lines)
  flow <- rule & grepl("^[[:space:]]+[A-Za-z0-9_]+:[[:space:]]*\\[", lines)
  item <- inside &
    grepl("^[[:space:]]*-[[:space:]]+", lines) &
    cumsum(rule) > 0
  return(flow | item)
}

# The lines `lines` of pbn.yml with the entries `add` of allow_to_add()
# under the key allow. A list with `lines` and `inserted`, TRUE for each
# line that is new. A flow list "rule: [a, b]" on one line gets the new
# entries at its end. A block list gets each new entry "- new" on the line
# after the old entry. A new entry has the quotes of the old one. A form
# that this does not know stays as it is.
allow_lines_with <- function(lines, add) {
  n <- length(lines)
  at <- seq_len(n)
  inside <- allow_section(lines)
  used <- rep(FALSE, nrow(add))
  out <- character(0)
  inserted <- logical(0)
  rule <- NA_character_
  for (i in at) {
    line <- lines[i]
    extra <- character(0)
    key <- regmatches(
      line,
      regexec("^[[:space:]]*([A-Za-z0-9_]+):[[:space:]]*(.*)$", line)
    )[[1]]
    item <- regmatches(line, regexec("^([[:space:]]*)-[[:space:]]+(.*)$", line))[[1]]
    if (inside[i] && length(key) == 3) {
      rule <- key[2]
      mine <- which(add$rule == rule & !used)
      flow <- flow_list_with(
        line,
        nchar(line) - nchar(key[3]) + 1,
        add[mine, , drop = FALSE]
      )
      line <- flow$line
      used[mine] <- flow$used
    } else if (inside[i] && length(item) == 3 && !is.na(rule)) {
      v <- yaml_scalar(item[3], flow = FALSE)
      j <- if (!is.null(v)) {
        which(add$rule == rule & add$written == v$value & !used)[1]
      } else {
        NA
      }
      if (!is.na(j)) {
        extra <- paste0(item[2], "- ", yaml_quoted(add$new[j], v$quote))
        used[j] <- TRUE
      }
    }
    out <- c(out, line, extra)
    inserted <- c(inserted, FALSE, rep(TRUE, length(extra)))
  }
  return(list(lines = out, inserted = inserted))
}

# The line `line` with the entries `add$new` at the end of the flow list
# that starts at the character `at`, one for each entry of the list that
# is in `add$written`. A list with `line` and `used`, TRUE for each row of
# `add` that the line got. A list that does not end on the line, or holds
# a list or a map, stays as it is.
flow_list_with <- function(line, at, add) {
  same <- list(line = line, used = rep(FALSE, nrow(add)))
  if (substr(line, at, at) != "[") {
    return(same)
  }
  skip_space <- function(pos) {
    rest <- substring(line, pos)
    return(pos + attr(regexpr("^[[:space:]]*", rest), "match.length"))
  }
  pos <- skip_space(at + 1)
  items <- list()
  last_end <- NA
  while (!startsWith(substring(line, pos), "]")) {
    v <- yaml_scalar(substring(line, pos), flow = TRUE)
    if (is.null(v)) {
      return(same)
    }
    items <- c(items, list(v))
    pos <- pos + v$len
    last_end <- pos - 1
    pos <- skip_space(pos)
    if (startsWith(substring(line, pos), ",")) {
      pos <- skip_space(pos + 1)
    } else if (!startsWith(substring(line, pos), "]")) {
      return(same)
    }
  }
  if (!grepl("^\\][[:space:]]*(#.*)?$", substring(line, pos))) {
    return(same)
  }
  used <- same$used
  new <- character(0)
  for (v in items) {
    j <- which(add$written == v$value & !used)[1]
    if (!is.na(j)) {
      new <- c(new, yaml_quoted(add$new[j], v$quote))
      used[j] <- TRUE
    }
  }
  if (length(new) == 0) {
    return(same)
  }
  line <- paste0(
    substr(line, 1, last_end),
    paste0(", ", new, collapse = ""),
    substring(line, last_end + 1)
  )
  return(list(line = line, used = used))
}

# The scalar at the start of the text `text` of a line of YAML: a list with
# `value`, `quote` ("'", "\"" or "") and `len`, the number of characters it
# takes. `flow` is TRUE inside a flow list, where "," and "]" end a plain
# scalar. NULL when there is no scalar. A double-quoted value keeps its
# escapes, so it matches no entry, and the check after the edit stops.
yaml_scalar <- function(text, flow) {
  q <- substr(text, 1, 1)
  if (q == "'" || q == "\"") {
    pattern <- if (q == "'") "^'([^']|'')*'" else "^\"([^\"\\\\]|\\\\.)*\""
    m <- regmatches(text, regexpr(pattern, text))
    if (length(m) == 0) {
      return(NULL)
    }
    value <- substr(m, 2, nchar(m) - 1)
    if (q == "'") {
      value <- gsub("''", "'", value, fixed = TRUE)
    }
    return(list(value = value, quote = q, len = nchar(m)))
  }
  m <- if (flow) {
    regmatches(text, regexpr("^[^],[{}#]*", text))
  } else {
    sub("[[:space:]]+#.*$", "", text)
  }
  m <- sub("[[:space:]]+$", "", m)
  if (length(m) == 0 || !nzchar(m)) {
    return(NULL)
  }
  return(list(value = m, quote = "", len = nchar(m)))
}

# The entry `x` of YAML with the quote `quote`: "'", "\"" or "" for none.
yaml_quoted <- function(x, quote) {
  if (quote == "'") {
    x <- gsub("'", "''", x, fixed = TRUE)
  }
  return(paste0(quote, x, quote))
}
