# The output of a run: make_outputs() and stamp_fake().

# Whether the current call of the output function is for a fake scenario.
# make_outputs() sets it, and stamp_fake() reads it.
output_state <- new.env(parent = emptyenv())

#' Write the output of the real data and of chosen fake scenarios
#'
#' `make_outputs()` calls the output function of the project on the results
#' of `"real"`, and of each fake scenario with `output = TRUE` in
#' `CFG$scenarios`. The files of a fake scenario go to their own folder, and
#' their names start with `fake_data_`.
#'
#' @param fn The output function of the project, `function(results, dir)`. It
#'   gets the results of one scenario from [read_results()], and writes its
#'   files into the folder `dir`, for example with `ggplot2::ggsave()`.
#' @param scenarios The scenarios of the run, from [scenarios()].
#' @details
#' Call [initialize_project()] first. For each scenario of `scenarios`,
#' `make_outputs()` does this:
#' - `"real"`: it calls `fn(results, dir)` with
#'   `dir = <version>_results/real/output/`.
#' - A fake scenario with `output = TRUE`: it calls `fn` with
#'   `dir = <version>_results/<scenario>/output_fake_data/`. Then it renames
#'   each file in `dir` that `fn` wrote, so that the name starts with
#'   `fake_data_`. The results of a scenario with more than 1 simulation are
#'   the stacked results, with a column `sim` in each data.frame.
#' - A fake scenario without `output = TRUE`: nothing.
#'
#' Before it calls `fn`, `make_outputs()` deletes the folder `dir` and
#' creates it again. So no file of an earlier run stays. Keep no other file
#' in that folder. After each scenario, it prints one line with the folder and
#' the number of files.
#'
#' `make_outputs()` skips a scenario with one printed line when its
#' `results.qs2` does not exist, or when its `check.csv` holds an assertion
#' that did not pass. Then it deletes the output folder of the scenario, so
#' no output of an earlier run stays beside absent or failed results. The
#' line says so. It does not touch a scenario that is not in `scenarios`. A
#' `"real"` without `check.csv` is not skipped, because `expect_real()` is
#' optional. When `fn` stops for one scenario, `make_outputs()` stops with an
#' error that names the scenario. The output of the scenarios before it
#' stays.
#'
#' `fn` MUST write into `dir` only. Around each call for a fake scenario,
#' `make_outputs()` lists the path, the size and the modification time of
#' each file in `<version>_results/` outside `dir`. It also lists the md5 of
#' each file modified in the last 2 seconds. When `fn` added, changed
#' or removed one of them, `make_outputs()` deletes the added files and stops
#' with the paths. It does not restore a changed or a removed file. It does
#' not check the call for `"real"`, or files outside `<version>_results/`.
#'
#' A figure of fake data carries the label "FAKE DATA" when the output
#' function calls [stamp_fake()]. A table or a text file has the name prefix
#' `fake_data_` and the folder `output_fake_data/` only. The
#' [check_project()] rule `fake_data_names` fails a file in
#' `output_fake_data/` without the prefix, and a file with the prefix in
#' another folder.
#' @return Invisibly, a data.frame with the columns `scenario` and `file`, one
#'   row for each file written. `file` is relative to the project folder.
#' @examples
#' # See vignette("pbn"), which writes a figure for "real" and for a fake
#' # scenario.
#' @family output
#' @seealso [end_unless_real()] for output code after the run that is not in
#'   a function.
#' @export
make_outputs <- function(fn, scenarios) {
  check_state("make_outputs")
  if (!is.function(fn)) {
    stop("`fn` must be a function(results, dir).", call. = FALSE)
  }
  table <- check_scenario_table(scenarios, "`scenarios`")
  out <- list()
  for (i in seq_len(nrow(table))) {
    s <- table$scenario[i]
    if (s == "real" || isTRUE(table$output[i])) {
      out[[s]] <- output_scenario(fn, s)
    }
  }
  out <- do.call(
    rbind,
    c(list(data.frame(scenario = character(0), file = character(0))), out)
  )
  rownames(out) <- NULL
  return(invisible(out))
}

# The output of one scenario: calls fn, renames the files of a fake scenario,
# and prints one line. Returns the files written, relative to the project.
output_scenario <- function(fn, s) {
  fake <- s != "real"
  folder <- file.path(state$results, s)
  sub <- if (fake) "output_fake_data" else "output"
  dir <- file.path(folder, sub)
  rel <- file.path(paste0(state$version, "_results"), s, sub)
  if (skip_scenario(folder, dir, rel, s)) {
    return(NULL)
  }
  results <- read_results(state$version, s, home = state$home)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  skip <- file.path(s, sub)
  before <- if (fake) hash_recent(results_listing(state$results, skip))
  failed <- tryCatch(
    {
      call_output(fn, results, dir, fake, s)
      NULL
    },
    error = function(e) return(e)
  )
  if (fake) {
    check_outside(state$results, before, skip, rel, s)
  }
  if (!is.null(failed)) {
    stop(failed)
  }
  files <- list.files(dir, recursive = TRUE, all.files = TRUE)
  if (fake) {
    files <- prefix_fake(dir, files, s)
  }
  cat(
    "pbn: the output of the scenario \"",
    s,
    "\" is in ",
    rel,
    "/, which pbn emptied first: ",
    length(files),
    if (length(files) == 1) " file.\n" else " files.\n",
    sep = ""
  )
  return(data.frame(scenario = rep(s, length(files)), file = file.path(rel, files)))
}

# TRUE, with one printed line, when the scenario has no results.qs2, or when
# its check.csv holds a failure. Then it also deletes the output folder `dir`.
skip_scenario <- function(folder, dir, rel, s) {
  reason <- if (!file.exists(file.path(folder, "results.qs2"))) {
    "it has no results.qs2. Run the scenario first"
  } else if (!is.null(assertion_problem(folder))) {
    paste0("its check.csv ", assertion_problem(folder))
  }
  if (is.null(reason)) {
    return(FALSE)
  }
  existed <- dir.exists(dir)
  unlink(dir, recursive = TRUE)
  cat("pbn: no output for the scenario \"", s, "\", because ", reason,
    if (existed) paste0(". pbn deleted ", rel, "/, so no earlier output stays"),
    ".\n",
    sep = ""
  )
  return(TRUE)
}

# The path, the size and the modification time of each file under `root`,
# without the files under the folder `skip`, relative to `root`.
results_listing <- function(root, skip) {
  files <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!startsWith(files, paste0(skip, "/"))]
  info <- file.info(file.path(root, files), extra_cols = FALSE)
  return(data.frame(
    file = files,
    size = info$size,
    mtime = as.numeric(info$mtime)
  ))
}

# The listing with the md5 of each file modified in the last 2 seconds, NA
# for the others. The clock of the modification time ticks in steps of
# milliseconds or of seconds, so a rewrite of such a file with the same size
# can keep its size and its modification time. An older file that is
# rewritten gets a new modification time.
hash_recent <- function(listing, root = state$results) {
  recent <- listing$mtime >= as.numeric(Sys.time()) - 2
  listing$md5 <- rep(NA_character_, nrow(listing))
  listing$md5[recent] <- unname(tools::md5sum(file.path(root, listing$file[recent])))
  return(listing)
}

# Stops when the files under `root` outside `skip` differ from `before`. It
# deletes the added files first.
check_outside <- function(root, before, skip, rel, s) {
  after <- results_listing(root, skip)
  hashed <- after$file %in% before$file[!is.na(before$md5)]
  after$md5 <- rep(NA_character_, nrow(after))
  after$md5[hashed] <- unname(tools::md5sum(file.path(root, after$file[hashed])))
  added <- setdiff(after$file, before$file)
  removed <- setdiff(before$file, after$file)
  both <- merge(before, after, by = "file")
  changed <- both$file[
    both$size.x != both$size.y |
      both$mtime.x != both$mtime.y |
      (!is.na(both$md5.x) & both$md5.x != both$md5.y)
  ]
  if (length(c(added, removed, changed)) == 0) {
    return(invisible(NULL))
  }
  unlink(file.path(root, added))
  show <- function(x) {
    x <- file.path(basename(root), x)
    return(if (length(x) == 0) "none" else paste(x, collapse = ", "))
  }
  stop(
    "The output function wrote outside ",
    rel,
    "/ for the scenario \"",
    s,
    "\". Added, and deleted by pbn: ",
    show(added),
    ". Changed: ",
    show(changed),
    ". Removed: ",
    show(removed),
    ". pbn did not restore the changed or the removed files. Write into `dir` ",
    "only.",
    call. = FALSE
  )
}

# Calls fn(results, dir), with stamp_fake() set for a fake scenario. An error
# of fn names the scenario.
call_output <- function(fn, results, dir, fake, scenario) {
  output_state$fake <- fake
  on.exit(rm(list = ls(output_state), envir = output_state), add = TRUE)
  tryCatch(fn(results, dir), error = function(e) {
    stop(
      "The output function failed for the scenario \"",
      scenario,
      "\": ",
      conditionMessage(e),
      call. = FALSE
    )
  })
  return(invisible(NULL))
}

# Renames each file of `files` in `dir` so that its name starts with
# fake_data_, and returns the new names relative to `dir`.
prefix_fake <- function(dir, files, scenario) {
  name <- basename(files)
  todo <- !startsWith(name, "fake_data_")
  new <- files
  new[todo] <- file.path(dirname(files[todo]), paste0("fake_data_", name[todo]))
  new <- sub("^\\./", "", new)
  clash <- new[todo][new[todo] %in% files | duplicated(new)[todo]]
  if (length(clash) > 0) {
    stop(
      "The output function wrote files for the scenario \"",
      scenario,
      "\" with and without the prefix fake_data_: ",
      paste(clash, collapse = ", "),
      ". Give each file one name without the prefix.",
      call. = FALSE
    )
  }
  ok <- file.rename(file.path(dir, files[todo]), file.path(dir, new[todo]))
  if (!all(ok)) {
    stop(
      "pbn could not rename these files: ",
      paste(files[todo][!ok], collapse = ", "),
      call. = FALSE
    )
  }
  return(sort(new))
}

#' Mark a figure of fake data
#'
#' `stamp_fake()` adds a large diagonal label "FAKE DATA" to a ggplot when
#' [make_outputs()] calls the output function for a fake scenario. Otherwise
#' it returns the figure as it is.
#'
#' @param q A ggplot.
#' @details
#' Call it in the figure code of the output function, before the figure is
#' saved: `q <- pbn::stamp_fake(q)`. For `"real"`, and outside a call of
#' [make_outputs()], it returns `q` unchanged. The label is on each panel.
#'
#' For a fake scenario, `stamp_fake()` needs the package ggplot2. It stops
#' with an error when ggplot2 is not installed, and when `q` is not a ggplot.
#' @return `q`, with the label for a fake scenario.
#' @examples
#' # Outside make_outputs(), stamp_fake() returns the figure as it is.
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   q <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y))
#'   identical(pbn::stamp_fake(q), q)
#' }
#' @family output
#' @export
stamp_fake <- function(q) {
  if (!isTRUE(output_state$fake)) {
    return(q)
  }
  if (!has_ggplot2()) {
    stop(
      "pbn::stamp_fake() needs the package ggplot2 to mark a figure of fake ",
      "data. Install ggplot2.",
      call. = FALSE
    )
  }
  if (!inherits(q, "ggplot")) {
    stop("`q` must be a ggplot.", call. = FALSE)
  }
  label <- grid::textGrob(
    "FAKE DATA",
    rot = 30,
    gp = grid::gpar(col = "red", alpha = 0.45, fontsize = 40, fontface = "bold")
  )
  return(q + ggplot2::annotation_custom(label))
}

# requireNamespace("ggplot2"), in one place, so that the tests can mock it.
has_ggplot2 <- function() {
  return(requireNamespace("ggplot2", quietly = TRUE))
}
