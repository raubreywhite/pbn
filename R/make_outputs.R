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
#'   `dir = <version>_results/real/output/`. The output is fake output when
#'   the manifest of the real results on the disk has `fake_real_data`
#'   `TRUE`. It then goes to `real/output_fake_data/`, as for a fake
#'   scenario below. The
#'   manifest decides, and never the argument `fake_real_data` of
#'   [initialize_project()] in this R session. `make_outputs()` deletes the
#'   other of the two folders of `"real"` with one printed line, so no
#'   output of an earlier run on other data stays.
#' - A fake scenario with `output = TRUE`: it calls `fn` with
#'   `dir = <version>_results/<scenario>/output_fake_data/`. Then it renames
#'   each file in `dir` that `fn` wrote, so that the name starts with
#'   `fake_data_`. It stops when a new name equals the name of another file
#'   in `dir`. It ignores the case of the letters, as Windows and macOS do.
#'   The results of a scenario with more than 1 simulation are the stacked
#'   results, with a column `sim` in each data.frame.
#' - A fake scenario without `output = TRUE`: nothing.
#'
#' [save_results()] deletes `output/` and `output_fake_data/` of each
#' scenario that it writes, so no output of an earlier run stays beside new
#' results. Before it calls `fn`, `make_outputs()` deletes the folder `dir`
#' and creates it again. So no file of an earlier run stays. Keep no other file
#' in that folder. It stops when the folder is still there afterwards, for
#' example because another program holds a file open on Windows. After each
#' scenario, it prints one line with the folder and the number of files.
#'
#' After [save_results()] in the same R session, `make_outputs()` adds its
#' files, its seconds and the number of warnings of `fn` for each scenario to
#' the run log `<version>_results/run.md`. A warning still reaches the
#' console, and the log holds no text of it. When `make_outputs()` stops, the
#' result of the log says so, and the errors file of the log holds the error.
#' In another R session it does not change the run log.
#'
#' `fn` MUST NOT change an object of the functions folder in place, for
#' example with the index that a data.table subset `CFG$table[x == 1]` adds.
#' The next call of `fn` or of an `expect_` function in this R session would
#' see the change. `make_outputs()` compares these objects before and after
#' each call of `fn`, with the rules of the check of [add_analyses()]. A
#' change fails the scenario, with the first element that changed, for
#' example `CFG$table`.
#'
#' `make_outputs()` skips a scenario with one printed line when its
#' `results.qs2` does not exist, or when its `check.csv` holds an assertion
#' that did not pass. Then it deletes the output folder of the scenario, so
#' no output of an earlier run stays beside absent or failed results. The
#' line says so. It does not touch a scenario that is not in `scenarios`. A
#' `"real"` without `check.csv` is not skipped, because `expect_real()` is
#' optional. `make_outputs()` also skips `"real"` when its
#' `manifest$failed_fake_checks` names a fake scenario with a failed
#' assertion. [read_results()] has the same rule.
#'
#' When `fn` stops for one scenario, `make_outputs()` stops with an error
#' that names the scenario. The output of the scenarios before it stays.
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
  done <- list(
    files = list(),
    notes = list(),
    seconds = numeric(0),
    warnings = list()
  )
  failed <- NULL
  for (i in seq_len(nrow(table))) {
    s <- table$scenario[i]
    if (s == "real" || isTRUE(table$output[i])) {
      start <- proc.time()[["elapsed"]]
      got <- tryCatch(output_scenario(fn, s), error = function(e) return(e))
      done$seconds[[s]] <- elapsed_since(start)
      if (inherits(got, "error")) {
        # The run log gets the number of the error, and the errors file its
        # text.
        done$failed <- list(scenario = s, message = conditionMessage(got))
        failed <- got
        break
      }
      done$files[[s]] <- got$files
      done$notes[s] <- list(got$note)
      done$warnings[s] <- list(got$warnings)
    }
  }
  log_outputs(done)
  if (!is.null(failed)) {
    stop(failed)
  }
  out <- do.call(
    rbind,
    c(
      list(data.frame(scenario = character(0), file = character(0))),
      done$files
    )
  )
  rownames(out) <- NULL
  return(invisible(out))
}

# The output of one scenario: calls fn, renames the files of a fake scenario,
# and prints one line. Returns a list: `files`, the files written relative
# to the project, `note`, the printed line when pbn skipped the scenario,
# or NULL, and `warnings`, the number of warnings of fn.
output_scenario <- function(fn, s) {
  folder <- file.path(state$results, s)
  # The output of real results from fake real data is fake output. The
  # manifest of the results on the disk decides, not this R session.
  fake <- s != "real" || results_from_fake_real_data(folder)
  sub <- if (fake) "output_fake_data" else "output"
  dir <- file.path(folder, sub)
  rel <- file.path(paste0(state$version, "_results"), s, sub)
  if (s == "real") {
    drop_other_real_output(folder, fake)
  }
  note <- skip_scenario(folder, dir, rel, s)
  if (!is.null(note)) {
    return(list(files = NULL, note = note))
  }
  results <- read_results(state$version, s, home = state$home)
  delete_paths(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  skip <- file.path(s, sub)
  before <- if (fake) hash_recent(results_listing(state$results, skip))
  warned <- 0L
  failed <- tryCatch(
    {
      warned <- call_output(fn, results, dir, fake, s)
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
  return(list(
    files = data.frame(
      scenario = rep(s, length(files)),
      file = file.path(rel, files)
    ),
    note = NULL,
    warnings = warned
  ))
}

# TRUE when the results.qs2 of the scenario in `folder` has
# manifest$fake_real_data TRUE: the real results come from fake real data.
# FALSE without that file.
results_from_fake_real_data <- function(folder) {
  file <- file.path(folder, "results.qs2")
  if (!file.exists(file)) {
    return(FALSE)
  }
  m <- tryCatch(qs2::qs_read(file)$manifest, error = function(e) return(NULL))
  return(isTRUE(m$fake_real_data))
}

# Deletes the output folder of the scenario "real" in `folder` that does not
# fit its results: output/ beside results from fake real data, and
# output_fake_data/ beside results from real data. `fake` is TRUE for
# results from fake real data. It prints one line when the folder was there.
drop_other_real_output <- function(folder, fake) {
  other <- if (fake) "output" else "output_fake_data"
  dir <- file.path(folder, other)
  if (!dir.exists(dir)) {
    return(invisible(NULL))
  }
  delete_paths(dir, recursive = TRUE)
  cat(
    "pbn: deleted ",
    file.path(paste0(state$version, "_results"), "real", other),
    "/, because the real results come from ",
    if (fake) "fake real data" else "real data",
    ", so no output of ",
    if (fake) "real data" else "fake real data",
    " of an earlier run stays.\n",
    sep = ""
  )
  return(invisible(NULL))
}

# The printed line, when the scenario has no results.qs2, when its
# check.csv holds a failure, or for "real" when its manifest names a fake
# scenario with a failed assertion. Then it also deletes the output folder `dir`.
# NULL otherwise.
skip_scenario <- function(folder, dir, rel, s) {
  reason <- if (!file.exists(file.path(folder, "results.qs2"))) {
    "it has no results.qs2. Run the scenario first"
  } else if (!is.null(assertion_problem(folder))) {
    paste0("its check.csv ", assertion_problem(folder))
  } else if (s == "real") {
    # read_results() refuses real results whose manifest names a fake
    # scenario with a failed assertion.
    clause <- failed_fakes_clause(results_manifest(folder))
    if (!is.null(clause)) paste("its manifest$failed_fake_checks", clause)
  }
  if (is.null(reason)) {
    return(NULL)
  }
  existed <- dir.exists(dir)
  delete_paths(dir, recursive = TRUE)
  line <- paste0(
    "pbn: no output for the scenario \"",
    s,
    "\", because ",
    reason,
    if (existed) paste0(". pbn deleted ", rel, "/, so no earlier output stays"),
    "."
  )
  cat(line, "\n", sep = "")
  return(line)
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
  listing$md5[recent] <- unname(tools::md5sum(file.path(
    root,
    listing$file[recent]
  )))
  return(listing)
}

# Stops when the files under `root` outside `skip` differ from `before`. It
# deletes the added files first.
check_outside <- function(root, before, skip, rel, s) {
  after <- results_listing(root, skip)
  hashed <- after$file %in% before$file[!is.na(before$md5)]
  after$md5 <- rep(NA_character_, nrow(after))
  after$md5[hashed] <- unname(tools::md5sum(file.path(
    root,
    after$file[hashed]
  )))
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
  delete_paths(file.path(root, added))
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
# of fn names the scenario. It also stops when fn changed an object of the
# functions folder, such as CFG, in place: the next call of project code in
# this R session would see the change. Returns the number of warnings of fn.
# Each warning still reaches the console, as R gives it.
call_output <- function(fn, results, dir, fake, scenario) {
  output_state$fake <- fake
  on.exit(rm(list = ls(output_state), envir = output_state), add = TRUE)
  before <- main_fingerprint()
  warned <- 0L
  withCallingHandlers(
    tryCatch(fn(results, dir), error = function(e) {
      stop(
        "The output function failed for the scenario \"",
        scenario,
        "\": ",
        conditionMessage(e),
        call. = FALSE
      )
    }),
    warning = function(w) {
      warned <<- warned + 1L
      return(invisible(NULL))
    }
  )
  found <- main_change(before)
  if (!is.null(found)) {
    stop(
      "The output function changed ",
      objects_change_text(found),
      ", for the scenario \"",
      scenario,
      "\". A later call of the output function or of an expect_ function ",
      "would see the change. Change a copy, for example ",
      "x <- data.table::copy(CFG$table).",
      call. = FALSE
    )
  }
  return(warned)
}

# Renames each file of `files` in `dir` so that its name starts with
# fake_data_, and returns the new names relative to `dir`.
prefix_fake <- function(dir, files, scenario) {
  name <- basename(files)
  todo <- !startsWith(name, "fake_data_")
  new <- files
  new[todo] <- file.path(dirname(files[todo]), paste0("fake_data_", name[todo]))
  new <- sub("^\\./", "", new)
  # A file system on Windows or macOS ignores the case of a name, so the
  # names are compared in lower case.
  low <- tolower(new)
  clash <- new[todo][low[todo] %in% tolower(files) | duplicated(low)[todo]]
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
#' @param q A ggplot, or a patchwork of ggplots.
#' @details
#' Call it in the figure code of the output function, before the figure is
#' saved: `q <- pbn::stamp_fake(q)`. For `"real"` on real data, and outside
#' a call of [make_outputs()], it returns `q` unchanged. For `"real"` whose
#' manifest has `fake_real_data` `TRUE`, it adds the label.
#'
#' The label is one label over the whole figure, also for a ggplot with
#' facets. `stamp_fake()` wraps the figure in one element with
#' [patchwork::wrap_elements()], and puts the label on it with
#' [patchwork::inset_element()]. The label has no background, so each panel
#' stays visible. The value is a patchwork, which [ggplot2::ggsave()] saves
#' as before. Call `stamp_fake()` last, just before the figure is saved. A
#' `+` after it applies to the label, and a theme with a background then
#' hides the figure.
#'
#' It stops when `q` is not a ggplot or a patchwork, for example a grob or a
#' gtable of `gridExtra::arrangeGrob()`. Stamp each ggplot before you combine
#' it in such an object.
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
  if (!inherits(q, "ggplot")) {
    stop(
      "`q` must be a ggplot or a patchwork. Stamp each ggplot before you ",
      "combine it with another package.",
      call. = FALSE
    )
  }
  label <- grid::textGrob(
    "FAKE DATA",
    rot = 30,
    gp = grid::gpar(col = "red", alpha = 0.45, fontsize = 40, fontface = "bold")
  )
  # One label over the whole figure. A label added with + to a ggplot is on
  # each panel, and each panel clips it. An inset on a patchwork covers its
  # last plot only, and its opaque background hides that plot. So wrap the
  # whole figure as one element, and give the inset of the label no
  # background.
  inset <- patchwork::wrap_elements(full = label) +
    ggplot2::theme(plot.background = ggplot2::element_blank())
  return(
    patchwork::wrap_elements(full = q) +
      patchwork::inset_element(
        inset,
        left = 0,
        bottom = 0,
        right = 1,
        top = 1,
        align_to = "full",
        ignore_tag = TRUE
      )
  )
}
