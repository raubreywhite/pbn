# The state of the project version. initialize_project() sets it, and
# run_plan() and save_results() read it.
state <- new.env(parent = emptyenv())

#' Start a project version
#'
#' `initialize_project()` sets the paths of a project version and sources its
#' functions folder. It MUST be the first call in every `vNN_run.R`.
#'
#' @param version The version, written as `"v"` and two digits, for example
#'   `"v01"`.
#' @param home The project folder. It holds `<version>_run.R`,
#'   `<version>_functions/` and the file of the rules `pbn.yml`.
#' @param folders The folders outside the project that the project reads, as
#'   a named character vector, for example
#'   `c(main = "../data", lookup = "../codes")`. One folder MAY have no name,
#'   and it then gets the name `main`. Each name MUST be a syntactic R name,
#'   once. A folder MAY be absent, for example on a computer without the real
#'   data. Only [save_results()] for the scenario `"real"` then stops.
#' @param env The environment that the functions folder is sourced into.
#' @param ... Nothing. A call with the removed argument `data_raw` stops, and
#'   the error says how to use `folders`. Each argument after `...` MUST be
#'   given with its full name.
#' @param fake_real_data `TRUE` when the folders of `folders` on this
#'   computer hold fake files in the formats of the real data, and `FALSE`,
#'   the default, otherwise. See the section Fake real data.
#' @param one_file_home The project folder on the server without a network,
#'   one string, for example `"N:/durable/project"`, or `NULL`, the default.
#'   [to_one_file()] writes it into its file. See the section Paths of the
#'   server.
#' @param one_file_folders The folders of `folders` on the server, as a named
#'   character vector, or `NULL`, the default. It MUST name exactly the
#'   folders of `folders`. One folder MAY have no name on both sides, and it
#'   then gets the name `main`.
#' @param generated `NULL`. The file of [to_one_file()] gives a value here.
#'   Do not write it by hand. See the section One file.
#' @details
#' `initialize_project()` does these steps in this order:
#' 1. It reads the rules of [check_project()] from `home/pbn.yml`. It stops
#'    on each error of `pbn.yml` that stops [check_project()], also when the
#'    file is missing.
#' 2. It computes the md5 of `<version>_run.R` and of each file in
#'    `<version>_functions/`, before it sources them. [save_results()] stores
#'    them in `manifest$source_md5`, without `<version>_functions/blocks.R`.
#'    The section The md5 of the run script gives the text of the run script
#'    that the md5 leaves out.
#' 3. It lists each file under each folder of `folders` that exists, at any
#'    depth, with its size and its modification time. This is the
#'    fingerprint of the data. pbn never reads the content of a data file, so
#'    a file of 60 GB costs what a small file costs. [save_results()] stores
#'    the fingerprint in `manifest$data_files` of the scenario `"real"`. A
#'    file that the run writes later is not in the list. The section The
#'    fingerprint of the data gives the rules.
#' 4. It calls [org::initialize_project()] with `folders_to_be_sourced` set to
#'    `<version>_functions`. `max_loc_per_file` is the `limit` of the rule
#'    `loc_per_file`, 1000 in the house set. When the rule is `warn` or
#'    `off`, it is `Inf`, because the check of org stops the run on a file
#'    with more lines. pbn gives org no folder of `folders`, so
#'    `org::project` holds `home`, `computer_id` and `env` only.
#'
#' Project code finds a folder with [folder()], for example
#' `file.path(pbn::folder("main"), "d.csv")`. Call [folder()] inside a
#' function: a top-level call in a file of the functions folder runs before
#' `initialize_project()` sets the folders, and it stops.
#'
#' The results folder is `<home>/<version>_results`. [save_results()] creates
#' it. `initialize_project()` creates no folder, also no folder with today's
#' date. It stops when `<version>_functions/` does not exist.
#'
#' `initialize_project()` reads no scenario, no simulation, no command-line
#' argument and no environment variable, and it sets no seed. [scenarios()]
#' chooses the scenarios, and [add_data()] and [add_analyses()] set the seeds.
#' @section The fingerprint of the data:
#' The fingerprint is a data.frame with one row for each file, sorted by
#' `file` in the C locale:
#' - `file`, the name `<folder name>/<path in the folder>` with `/`, for
#'   example `main/sub/d.csv`. It never holds the path of the folder.
#' - `size`, the size in bytes.
#' - `mtime`, the modification time in whole seconds since 1970-01-01 UTC,
#'   with the fraction of a second cut off. A copy of a file can lose part
#'   of that fraction, for example on Windows, and whole seconds stay the
#'   same.
#'
#' Hidden files count. A link to a file gives the size and the time of its
#' target, and [list.files()] follows a link to a folder. A broken link stops
#' `initialize_project()`. It also stops when a folder of `folders` is inside
#' another or is the same folder. It stops when a folder holds or is inside
#' `<version>_results/` or `<version>_cache_gitexclude/`, because pbn writes
#' there in each run. A folder elsewhere inside `home` is allowed.
#'
#' A file that changed in place, and kept its size and the second of its
#' modification time, is not seen. A copy that does not keep the modification times looks
#' like changed data, so the cache computes the data again. `file.copy(...,
#' copy.date = TRUE)` keeps them.
#' @section Fake real data:
#' The real data of a project MAY be on a server only. Its folders on this
#' computer MAY then hold fake files in the formats of the real data. Then the code
#' that reads the real data runs here, and [check_one_file()] has results to
#' compare. Give `fake_real_data = TRUE` in the run script of such a
#' computer. It is about the scenario `"real"` only:
#' - The manifest of the real results gets `fake_real_data = TRUE`. It is
#'   `FALSE` without the flag. A fake scenario has no such field.
#' - The run log says that the scenario real ran on fake real data, in the
#'   sections Run and History. The sections Data and Exclusions name the
#'   scenario `real (fake real data)`.
#' - [make_outputs()] writes the output of `"real"` as fake output, into
#'   `real/output_fake_data/` with the prefix `fake_data_`, and
#'   [stamp_fake()] marks its figures. It deletes an old `real/output/`.
#' - [show_blocks()] and [show_block()] print `FAKE REAL DATA`.
#'
#' The argument decides only what [save_results()] writes into the
#' manifest. [make_outputs()], [stamp_fake()], [show_blocks()] and
#' [show_block()] read `fake_real_data` from the manifest of the real
#' results on the disk. So an R session without the argument still marks
#' such results, and an R session with it never marks results from real
#' data.
#' - The rule `real_results_are_fake` of [check_project()] warns while such
#'   real results are on the disk.
#'
#' [read_results()] reads such results as usual. The file of [to_one_file()]
#' never gives `fake_real_data`, because the server holds the real data.
#' `fake_real_data = TRUE` together with `generated` stops.
#' @section Paths of the server:
#' Give `one_file_home` and `one_file_folders` together, or neither. A
#' project that never goes to a server gives neither. The run script holds
#' the paths, and a commented line `# pbn::to_one_file()` follows the call:
#'
#' ```r
#' pbn::initialize_project(
#'   version = "v01",
#'   home = ".",
#'   folders = c(main = "../data"),
#'   one_file_home = "N:/durable/project",
#'   one_file_folders = c(main = "N:/durable/data")
#' )
#' # pbn::to_one_file()
#' ```
#'
#' Write both values as literal strings: `one_file_home` as one string, and
#' `one_file_folders` as one string or as `c()` of strings. Only then does
#' the md5 of the run script leave them out, because a value that is code
#' could change the results. See the section The md5 of the run script.
#'
#' The paths are text only. `initialize_project()` stops on one of the two
#' without the other, and on a folder of `folders` that `one_file_folders`
#' does not name, or the reverse. It checks no folder on the disk,
#' normalises no path and uses no path in the run. The paths never go into
#' the manifest, a cache key or the run log.
#' @section The md5 of the run script:
#' `manifest$source_md5` holds the md5 of `<version>_run.R` without the
#' text that cannot change the results and that a project adds for the
#' server:
#' - each argument `one_file_home` and `one_file_folders` of a top-level call
#'   `pbn::initialize_project()` with a literal value, from the comma before
#'   its name to the end of its value. A literal value of `one_file_home` is
#'   one string constant. A literal value of `one_file_folders` is one string
#'   constant, or a call `c()` whose arguments are each one string constant,
#'   with or without a name. pbn reads this from the parse data and
#'   evaluates nothing.
#' - each line that holds only a comment with a call of `to_one_file()` or
#'   `check_one_file()` without arguments, for example
#'   `# pbn::to_one_file()`, with its line end. The call MAY have `pbn::`
#'   and any spaces.
#'
#' So the results of a run before such an edit stay current. A run script
#' without such text has the md5 of its bytes, as before. An argument
#' without a comma before it, for example the first argument of the call,
#' stays in the md5, and so does an argument with another value, for
#' example `paste0(base, "/a")`, a name or a block `{ }`. Such a value
#' counts as code: its edit makes the results of an earlier run older than
#' the code.
#' [to_one_file()] works with such a value too. The md5 of each other file
#' is the md5 of its bytes.
#' @section One file:
#' The file of [to_one_file()] calls `initialize_project()` with `generated`,
#' a list with these elements:
#' - `project`, the name of the project folder on the computer that made the
#'   file. The run log names the project with it.
#' - `source_md5`, the md5 of the run script and of each file of the
#'   functions folder on that computer. The manifest stores it.
#' - `code`, the code of the functions folder as one quoted block, in the
#'   order that org sources the files.
#'
#' With `generated`, `initialize_project()` reads no `pbn.yml`, needs no
#' functions folder, and checks no code lines. It evaluates `code` in `env`
#' in the place of the source of the functions folder. A use of the cache then
#' does not check the `data_*.R` files on the disk, because the code is in the
#' file.
#' @return Invisibly, a list with `version`, `home`, `results` and
#'   `folders`. `results` is the results folder, and `folders` the named
#'   paths of the folders.
#' @examples
#' home <- file.path(tempdir(), "pbn_initialize_example")
#' dir.create(file.path(home, "v01_functions"), recursive = TRUE)
#' writeLines("extends: house_2026_11_17", file.path(home, "pbn.yml"))
#' writeLines(
#'   "CFG <- list(scenarios = data.frame(scenario = c('real', 'null'), sims = c(1, 3)))",
#'   file.path(home, "v01_functions", "_cfg.R")
#' )
#'
#' env <- new.env()
#' p <- pbn::initialize_project(
#'   version = "v01",
#'   home = home,
#'   folders = c(main = file.path(home, "data")),
#'   env = env
#' )
#' p$results
#' pbn::folder("main")
#' env$CFG$scenarios
#'
#' unlink(home, recursive = TRUE)
#' @family project setup
#' @seealso `vignette("pbn")`, which builds and runs a small fake project.
#' @export
initialize_project <- function(
  version,
  home,
  folders,
  env = .GlobalEnv,
  ...,
  fake_real_data = FALSE,
  one_file_home = NULL,
  one_file_folders = NULL,
  generated = NULL
) {
  # The state stays empty until the last step sets it. A call that stops
  # leaves no project for run_plan() or save_results().
  rm(list = ls(state, all.names = TRUE), envir = state)
  check_no_dots(...names(), ...length())
  folders <- check_init_args(version, home, folders, env, generated)
  check_fake_real_data(fake_real_data, generated)
  server <- check_one_file_paths(
    one_file_home,
    one_file_folders,
    folders,
    generated
  )
  home <- normalizePath(home, winslash = "/", mustWork = TRUE)
  if (is.null(generated)) {
    code <- functions_folder_code(version, home)
  } else {
    code <- list(
      md5 = generated$source_md5,
      raw_md5 = generated$source_md5,
      manifest_md5 = generated$source_md5,
      stat = NULL
    )
  }
  found <- folders_listing(folders, home, version)
  # The objects that the folder defines are the new ones and those with a
  # new address. run_plan() fails an analysis that changes one of them.
  before <- object_addresses(env)
  if (is.null(generated)) {
    # results = NULL: org then creates no folder, also no folder with today's
    # date.
    org::initialize_project(
      env = env,
      home = home,
      folders_to_be_sourced = paste0(version, "_functions"),
      max_loc_per_file = code$max_loc
    )
  } else {
    org::initialize_project(
      env = env,
      home = home,
      folders_to_be_sourced = character(0)
    )
    eval(generated$code, envir = env)
  }
  out <- list(
    version = version,
    home = home,
    results = file.path(home, paste0(version, "_results")),
    folders = found$folders
  )
  # Every step that can stop is above this line.
  list2env(
    c(
      out,
      list(
        folders_absent = found$absent,
        data_files = found$files,
        source_md5 = code$md5,
        source_raw_md5 = code$raw_md5,
        manifest_md5 = code$manifest_md5,
        source_stat = code$stat,
        generated = !is.null(generated),
        fake_real_data = fake_real_data,
        one_file_home = server$home,
        one_file_folders = server$folders,
        project = if (is.null(generated)) basename(home) else generated$project,
        env = env,
        sourced = defined_names(env, before)
      )
    ),
    envir = state
  )
  return(invisible(out))
}

# The md5 and the size and mtime of the run script and of each file of the
# functions folder of `version` in `home`, and the limit of code lines of
# pbn.yml for org. Stops on a missing functions folder or a bad pbn.yml.
functions_folder_code <- function(version, home) {
  # org creates a missing folder with a warning. pbn stops instead, so that a
  # wrong `home` or `version` leaves no folder.
  folder <- file.path(home, paste0(version, "_functions"))
  if (!dir.exists(folder)) {
    stop("There is no functions folder ", folder, call. = FALSE)
  }
  # The limit of code lines that org checks comes from pbn.yml, so that
  # check_project() and the run use one limit.
  max_loc <- org_max_loc(read_rules(home))
  # Before org sources the functions folder, so the md5 is of the files as
  # the run found them.
  raw_md5 <- source_md5(home, version, raw = TRUE)
  md5 <- with_run_script_md5(raw_md5, home, version)
  return(list(
    md5 = md5,
    raw_md5 = raw_md5,
    manifest_md5 = manifest_md5_of(home, version, md5),
    stat = file_stat(file.path(home, names(md5)), names(md5)),
    max_loc = max_loc
  ))
}

# Stops on an argument in `...` of initialize_project(), with the names
# `names` and the count `n`. The removed argument data_raw gets the way to
# the new argument folders.
check_no_dots <- function(names, n) {
  if (n == 0) {
    return(invisible(NULL))
  }
  if ("data_raw" %in% names) {
    stop(
      "pbn::initialize_project() has no argument data_raw. Give the folder ",
      "as folders = c(main = \"<the folder>\"), and read it in the data code ",
      "with pbn::folder(\"main\") in the place of org::project$data_raw.",
      call. = FALSE
    )
  }
  if ("one_file" %in% names) {
    stop(
      "pbn::initialize_project() has no argument one_file. An older ",
      "pbn::to_one_file() wrote this file. Generate the file again with ",
      "pbn::to_one_file() in the project folder.",
      call. = FALSE
    )
  }
  named <- names[!is.na(names) & nzchar(names)]
  stop(
    "pbn::initialize_project() has no argument ",
    if (length(named) > 0) named[1] else "without a name after env",
    ".",
    call. = FALSE
  )
}

check_init_args <- function(version, home, folders, env, generated) {
  check_version(version)
  if (!is_one_string(home) || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  folders <- check_folders(folders)
  if (!is.environment(env)) {
    stop("`env` must be an environment.", call. = FALSE)
  }
  check_generated_arg(generated)
  return(folders)
}

# Stops when `fake_real_data` is not TRUE or FALSE, and on TRUE with
# `generated`: the file of to_one_file() runs on the server, where the data
# is real.
check_fake_real_data <- function(fake_real_data, generated) {
  if (!isTRUE(fake_real_data) && !isFALSE(fake_real_data)) {
    stop("`fake_real_data` must be TRUE or FALSE.", call. = FALSE)
  }
  if (fake_real_data && !is.null(generated)) {
    stop(
      "The file of pbn::to_one_file() runs on real data, so it takes no ",
      "fake_real_data = TRUE.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Stops when `generated` is not NULL and not the list that the file of
# to_one_file() writes: `project`, `source_md5` and `code`.
check_generated_arg <- function(generated) {
  if (is.null(generated)) {
    return(invisible(NULL))
  }
  shape <- is.list(generated) &&
    identical(names(generated), c("project", "source_md5", "code"))
  ok <- shape &&
    is_one_string(generated$project) &&
    is_data_md5(generated$source_md5) &&
    is_block_call(generated$code)
  if (!ok) {
    stop(
      "`generated` is for the file of pbn::to_one_file() only. Generate the ",
      "file again with pbn::to_one_file().",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The paths of the server, `one_file_home` as `home` and `one_file_folders`
# as `folders`, after their checks: `folders` gets the names and the order
# of the folders `project` of the project. Both are NULL without the paths.
# pbn never uses the paths on this computer, so it checks no folder on the
# disk and normalises no path. Stops on one of the two without the other,
# on the paths in the file of to_one_file() (`generated`), and on a folder
# of the project that `folders` does not name, or the reverse.
check_one_file_paths <- function(home, folders, project, generated) {
  given <- c(
    one_file_home = !is.null(home),
    one_file_folders = !is.null(folders)
  )
  if (!any(given)) {
    return(list(home = NULL, folders = NULL))
  }
  if (!is.null(generated)) {
    stop(
      "The file of pbn::to_one_file() runs on the server, so it takes no ",
      "one_file_home and no one_file_folders.",
      call. = FALSE
    )
  }
  if (!all(given)) {
    stop(
      "Give one_file_home and one_file_folders together, or neither. This ",
      "call gives ",
      names(given)[given],
      " only.",
      call. = FALSE
    )
  }
  if (!is_one_string(home) || !nzchar(home)) {
    stop(
      "`one_file_home` must be one path, the project folder on the server, ",
      "for example \"N:/durable/project\".",
      call. = FALSE
    )
  }
  folders <- check_folders(folders, "one_file_folders")
  want <- names(project)
  missing <- setdiff(want, names(folders))
  extra <- setdiff(names(folders), want)
  if (length(missing) > 0 || length(extra) > 0) {
    stop(
      "`one_file_folders` must name exactly the folders of `folders`: ",
      paste(want, collapse = ", "),
      ".",
      if (length(missing) > 0) {
        paste0(" Missing: ", paste(missing, collapse = ", "), ".")
      },
      if (length(extra) > 0) {
        paste0(" Not of the project: ", paste(extra, collapse = ", "), ".")
      },
      call. = FALSE
    )
  }
  return(list(home = home, folders = folders[want]))
}

# TRUE when `x` is a call of `{`, the block of quote({ ... }).
is_block_call <- function(x) {
  return(is.call(x) && identical(x[[1]], as.name("{")))
}

# One whole number of 1 or more, and not Inf.
is_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    return(FALSE)
  }
  return(is.finite(x) && x >= 1 && x == round(x))
}

# Stops when initialize_project() has not set the state of a project.
check_state <- function(caller) {
  if (is.null(state$results)) {
    stop(
      "Call pbn::initialize_project() before pbn::",
      caller,
      "().",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The size and the modification time of each of the files `files`, under the
# names `names`. pbn hashes a source file again only when one of them
# changed.
file_stat <- function(files, names) {
  return(data.frame(
    file = names,
    size = file.size(files),
    mtime = as.numeric(file.mtime(files))
  ))
}

# tools::md5sum() of the source files `files`, without names. In one place,
# so that the tests can count the files that pbn hashes. pbn hashes no data
# file.
hash_files <- function(files) {
  return(unname(tools::md5sum(files)))
}

# The md5 of the deparsed code of the functions `fns`, named by the names of
# `fns`. removeSource() and a fixed deparse control leave out the source
# references, so the value is the same with keep.source TRUE or FALSE, for
# example under RStudio and under Rscript.
function_md5 <- function(fns) {
  text <- unlist(lapply(names(fns), function(nm) {
    return(c(
      nm,
      deparse(
        utils::removeSource(fns[[nm]]),
        width.cutoff = 500L,
        control = c("keepNA", "keepInteger", "niceNames", "showAttributes")
      )
    ))
  }))
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  # writeBin(), not writeLines(), so the bytes are the same on each OS.
  writeBin(charToRaw(enc2utf8(paste(text, collapse = "\n"))), file)
  return(unname(tools::md5sum(file)))
}

# The md5 of the run script and of each file in the functions folder of
# `version`, named by the path relative to `home` and sorted in the C locale.
# A file that does not exist is not in the vector. With `raw = FALSE`, the
# md5 of the run script is that of run_script_md5(). With `raw = TRUE`, each
# md5 is that of the bytes of the file.
source_md5 <- function(home, version, raw = FALSE) {
  folder <- paste0(version, "_functions")
  inner <- list.files(
    file.path(home, folder),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  rel <- c(paste0(version, "_run.R"), file.path(folder, inner, fsep = "/"))
  rel <- sort(rel[file.exists(file.path(home, rel))], method = "radix")
  md5 <- unname(tools::md5sum(file.path(home, rel)))
  names(md5) <- rel
  if (raw) {
    return(md5)
  }
  return(with_run_script_md5(md5, home, version))
}

# The md5 values `md5` of source_md5(raw = TRUE), with the md5 of
# run_script_md5() for the run script of `version` in `home`.
with_run_script_md5 <- function(md5, home, version) {
  script <- paste0(version, "_run.R")
  if (script %in% names(md5)) {
    md5[[script]] <- run_script_md5(file.path(home, script), md5[[script]])
  }
  return(md5)
}

# The file of the blocks of show_block() in the functions folder of
# `version`, relative to the project folder.
blocks_file <- function(version) {
  return(paste0(version, "_functions/blocks.R"))
}

# The md5 values `md5` of the source files of `version` in `home` for
# manifest$source_md5: without vNN_functions/blocks.R when that file cannot
# change the results, see blocks_file_inert(), and else all of them.
manifest_md5_of <- function(home, version, md5) {
  if (blocks_file_inert(home, version)) {
    return(manifest_source_md5(md5, version))
  }
  return(md5)
}

# The md5 values `md5` of the source files without the file of the blocks of
# `version`. The blocks only print results, so the manifest leaves them out,
# and a change of the blocks does not make the results stale.
manifest_source_md5 <- function(md5, version) {
  if (!is_one_string(version)) {
    return(md5)
  }
  return(md5[names(md5) != blocks_file(version)])
}

check_version <- function(version) {
  if (
    !is.character(version) ||
      length(version) != 1 ||
      !grepl("^v[0-9]{2}$", version)
  ) {
    stop(
      "`version` must be \"v\" and two digits, for example \"v01\".",
      call. = FALSE
    )
  }
  return(invisible(version))
}

is_scenario_name <- function(x) {
  return(
    is.character(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      grepl("^[a-z0-9_]+$", x)
  )
}

# The name of one simulation, sim_<kkkk>, in the names of its files and
# its analyses.
sim_folder <- function(sim) {
  return(sprintf("sim_%04d", sim))
}
