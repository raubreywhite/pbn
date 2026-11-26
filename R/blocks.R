#' Show the numbers of the manuscript, one block at a time
#'
#' `show_blocks()` prints the index of the blocks of the project, and
#' `show_block()` prints one page of one block. They are for a screen whose
#' numbers leave a server only as a screenshot.
#'
#' @param k The block, by its number in `blocks` or by its name.
#' @param page The page of the block, a whole number from 1.
#' @param rows The rows of one page, a whole number from 1. The default 35
#'   fits one screenshot.
#' @param version The version, for example `"v01"`. The default `NULL` is the
#'   version of the last [initialize_project()] in this R session.
#' @param home The project folder. The default `NULL` is the folder of the
#'   last [initialize_project()] in this R session.
#' @details
#' The project declares the blocks in its functions folder, as one object
#' `blocks`. It is a named list of functions of `results`, in the order of
#' the manuscript. The name of a block is its place in the manuscript, for
#' example `"Table 1"` or `"Results para 1: exclusions"`. In
#' `show_block()`, each function gets the real results as [read_results()]
#' returns them. [save_results()] also calls each function on simulation 1
#' of every scenario of the run, and writes the values to
#' `<version>_results/<scenario>/blocks.qs2`. So a block MUST work on the
#' results of each fake scenario too. A function returns a data.frame, or a
#' character vector that prints as one line for each element. The function
#' formats the numbers for the manuscript, and
#' `show_block()` prints the table as the function returns it.
#'
#' ```r
#' blocks <- list(
#'   "Table 1" = function(results) results$table1,
#'   "Results para 1: exclusions" = function(results) results$exclusions
#' )
#' ```
#'
#' Put `blocks` in the file `<version>_functions/blocks.R`, and nowhere
#' else. pbn leaves that file out of `manifest$source_md5`, so a change of
#' a block does not make the results stale. The rule `blocks_list` of
#' [check_project()] fails a use of a name of that file in the other code,
#' because the run then depends on it.
#'
#' A block MAY run where the packages of the run script are missing.
#' Examples are the console after [initialize_project()] and the new R
#' session of [check_one_file()]. `show_block()` attaches no package. So a block function MUST name the package of each function that
#' it calls, for example `data.table::data.table()`. When R does not find a
#' function, the error says so.
#'
#' The results include the kinds of `gitexclude`, so a block MAY show rows
#' for each person, for example to find a fault. What a person copies or
#' screenshots from the screen is the responsibility of that person.
#'
#' Both functions need [initialize_project()] in this R session, because the
#' blocks come from the functions folder that it sourced. Without it they
#' stop. They read the real results from the disk at each call, with
#' [read_results()]. So they work in the project folder, and on a server
#' after the file of [to_one_file()]. They stop where [read_results()]
#' stops. So they also stop on real results whose
#' `manifest$failed_fake_checks` names a fake scenario with a failed
#' assertion.
#'
#' `show_blocks()` calls each block function once. For each block, it prints
#' the number, the name, the rows and the pages. For a function that stops,
#' it prints the error in the place of the rows, and goes on.
#'
#' `show_block()` prints these lines:
#' 1. the number and the name of the block,
#' 2. the page, for example `page 2 of 4, rows 36 to 70 of 140`,
#' 3. the rows of the page.
#'
#' Real results MAY come from fake real data, see `fake_real_data` of
#' [initialize_project()]. Then `show_block()` prints the line
#' `FAKE REAL DATA` between the first two lines. `show_blocks()` prints it
#' under its first line.
#'
#' It stops on a block function that stops, and the error names the block.
#' During the call, the option `width` is 270 and the row names are off. No
#' option of R or of data.table cuts a row or a column, and a wide table
#' wraps its columns. Both functions record the options before a block
#' function runs, and set them back at the end of the call. So an option
#' that a block function sets or adds does not stay.
#' @return `show_blocks()`: invisibly, a data.frame with the columns
#'   `block`, `name`, `rows`, `pages` and `error`. `show_block()`: invisibly,
#'   the whole value of the block function.
#' @family one file
#' @seealso `vignette("pbn")`, section "Running on an air-gapped server".
#' @export
show_blocks <- function(version = NULL, home = NULL) {
  restore <- keep_options()
  on.exit(restore(), add = TRUE)
  ctx <- blocks_context(version, home)
  rows <- integer(length(ctx$blocks))
  errors <- character(length(ctx$blocks))
  for (k in seq_along(ctx$blocks)) {
    got <- block_call(ctx, k)
    if (is.null(got$error)) {
      rows[k] <- block_rows(got$value)
    } else {
      rows[k] <- NA_integer_
      errors[k] <- got$error
    }
  }
  out <- data.frame(
    block = seq_along(ctx$blocks),
    name = names(ctx$blocks),
    rows = rows,
    pages = ifelse(
      is.na(rows),
      NA_integer_,
      pmax(1L, as.integer(ceiling(rows / 35)))
    ),
    error = ifelse(nzchar(errors), errors, NA_character_)
  )
  cat(
    "pbn: ",
    length(ctx$blocks),
    if (length(ctx$blocks) == 1) " block" else " blocks",
    " of ",
    ctx$version,
    ", from the real results of ",
    ctx$run_date,
    ":\n",
    if (ctx$fake_real_data) paste0(fake_real_label, "\n"),
    sep = ""
  )
  width <- max(nchar(out$name))
  shown <- ifelse(
    is.na(out$rows),
    paste0("error: ", gsub("\\s+", " ", out$error)),
    paste0(
      out$rows,
      ifelse(out$rows == 1, " row, ", " rows, "),
      out$pages,
      ifelse(out$pages == 1, " page", " pages")
    )
  )
  cat(
    sprintf(
      "%3d  %s  %s",
      out$block,
      formatC(out$name, width = -width),
      shown
    ),
    sep = "\n"
  )
  cat(
    "Type pbn::show_block(k) for block k, and pbn::show_block(k, page = 2)",
    "for its page 2.\n"
  )
  return(invisible(out))
}

#' @rdname show_blocks
#' @export
show_block <- function(k, page = 1, rows = 35, version = NULL, home = NULL) {
  if (!is_count(page) || !is_count(rows)) {
    stop("`page` and `rows` must be whole numbers of 1 or more.", call. = FALSE)
  }
  restore <- keep_options()
  on.exit(restore(), add = TRUE)
  ctx <- blocks_context(version, home)
  k <- block_number(ctx$blocks, k)
  value <- block_value(ctx, k)
  n <- block_rows(value)
  pages <- max(1L, as.integer(ceiling(n / rows)))
  if (page > pages) {
    stop(
      "The block ",
      k,
      " has ",
      pages,
      if (pages == 1) " page" else " pages",
      ", so it has no page ",
      page,
      ".",
      call. = FALSE
    )
  }
  show_page(ctx, k, value, page, rows)
  return(invisible(value))
}

# Prints the page `page` of `rows` rows of the value `value` of the block
# `k` of `ctx`, as show_block() does. It sets the options of show_block(),
# and the caller sets them back.
show_page <- function(ctx, k, value, page, rows) {
  n <- block_rows(value)
  pages <- max(1L, as.integer(ceiling(n / rows)))
  i <- seq_len(n)
  i <- i[ceiling(i / rows) == page]
  options(
    width = 270,
    max.print = .Machine$integer.max,
    datatable.print.nrows = Inf,
    datatable.print.colnames = "top",
    datatable.print.class = FALSE,
    datatable.print.trunc.cols = FALSE
  )
  cat(
    "[",
    k,
    "] ",
    names(ctx$blocks)[k],
    "\n",
    if (ctx$fake_real_data) paste0(fake_real_label, "\n"),
    "page ",
    page,
    " of ",
    pages,
    if (n == 0) {
      ", no rows"
    } else {
      paste0(", rows ", min(i), " to ", max(i), " of ", n)
    },
    "\n\n",
    sep = ""
  )
  print_page(value, i)
  return(invisible(NULL))
}

# The label of the blocks of real results from fake real data, see
# initialize_project(fake_real_data = TRUE).
fake_real_label <- "FAKE REAL DATA"

# The blocks of the project, the real results and the version, for
# show_blocks() and show_block(). `version` and `home` default to the last
# initialize_project() of this R session.
blocks_context <- function(version, home) {
  if (is.null(state$results)) {
    stop(
      "Call pbn::initialize_project() before pbn::show_blocks() or ",
      "pbn::show_block(). The blocks come from the functions folder that it ",
      "sources.",
      call. = FALSE
    )
  }
  if (is.null(version)) {
    version <- state$version
  }
  if (is.null(home)) {
    home <- state$home
  }
  if (!identical(version, state$version)) {
    stop(
      "The blocks in this R session are those of ",
      state$version,
      ", not of ",
      version,
      ". Call pbn::initialize_project() for ",
      version,
      " first.",
      call. = FALSE
    )
  }
  blocks <- get0("blocks", envir = state$env, inherits = FALSE)
  check_blocks(blocks, state$version)
  results <- read_results(version, "real", home = home)
  run_date <- results$manifest$run_date
  return(list(
    blocks = blocks,
    results = results,
    version = version,
    run_date = if (is_one_string(run_date)) run_date else "an unknown date",
    fake_real_data = isTRUE(results$manifest$fake_real_data)
  ))
}

# The file of the values of the blocks in the folder of each scenario.
blocks_qs2_file <- "blocks.qs2"

# Step 7 of save_results(): evaluates each block of the functions folder on
# simulation 1 of each scenario of `scenarios` under `root`, and writes the
# values of a scenario to <root>/<scenario>/blocks.qs2, a named list in the
# order of `blocks`. It writes no file when the functions folder has no
# blocks. It stops at the first block that fails, before it writes a file.
save_blocks <- function(scenarios, root) {
  blocks <- get0("blocks", envir = state$env, inherits = FALSE)
  if (is.null(blocks)) {
    return(invisible(NULL))
  }
  check_blocks(blocks, state$version)
  restore <- keep_options()
  on.exit(restore(), add = TRUE)
  values <- list()
  for (s in scenarios) {
    ctx <- list(
      blocks = blocks,
      results = read_results(state$version, s, sim = 1, home = state$home)
    )
    values[[s]] <- stats::setNames(
      vector("list", length(blocks)),
      names(blocks)
    )
    for (k in seq_along(blocks)) {
      got <- block_call(ctx, k)
      if (!is.null(got$error)) {
        stop(
          "block ",
          k,
          " \"",
          names(blocks)[k],
          "\" failed on scenario ",
          s,
          ": ",
          got$error,
          call. = FALSE
        )
      }
      values[[s]][k] <- list(got$value)
    }
  }
  for (s in scenarios) {
    qs2::qs_save(values[[s]], file.path(root, s, blocks_qs2_file))
  }
  return(invisible(NULL))
}

# Stops when `blocks` is not a named list of functions with names that come
# once.
check_blocks <- function(blocks, version) {
  if (is.null(blocks)) {
    stop(
      version,
      "_functions/ defines no object blocks. ?pbn::show_block says how to ",
      "declare it.",
      call. = FALSE
    )
  }
  if (!is_blocks_list(blocks)) {
    stop(
      "blocks MUST be a list of functions of results, each with a name of ",
      "its own. pbn::check_project() names the fault in its rule blocks_list.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# TRUE when `blocks` is a list of one or more functions, each with a name of
# its own.
is_blocks_list <- function(blocks) {
  if (!is.list(blocks) || length(blocks) == 0) {
    return(FALSE)
  }
  nm <- names(blocks)
  return(
    !is.null(nm) &&
      all(nzchar(nm)) &&
      anyDuplicated(nm) == 0 &&
      all(vapply(blocks, is.function, logical(1)))
  )
}

# The number of the block `k` of `blocks`, given by number or by name.
block_number <- function(blocks, k) {
  if (is_one_string(k) && k %in% names(blocks)) {
    return(match(k, names(blocks)))
  }
  if (is_count(k) && k <= length(blocks)) {
    return(as.integer(k))
  }
  stop(
    "There is no block ",
    if (is_one_string(k)) encodeString(k, quote = "\"") else format(k),
    ". The project has ",
    length(blocks),
    " blocks: give a number from 1 to ",
    length(blocks),
    ", or a name. pbn::show_blocks() lists them.",
    call. = FALSE
  )
}

# The value of the block `k` for the results of `ctx`. Stops when the block
# function stops, or when its value is not a data.frame or a character
# vector, and the error names the block.
block_value <- function(ctx, k) {
  got <- block_call(ctx, k)
  if (!is.null(got$error)) {
    stop(
      "The block ",
      k,
      ", \"",
      names(ctx$blocks)[k],
      "\", failed: ",
      got$error,
      call. = FALSE
    )
  }
  return(got$value)
}

# The value of the block `k` for the results of `ctx`, and `error`: NULL, the
# error of the block function, or the fault of its value.
block_call <- function(ctx, k) {
  value <- tryCatch(ctx$blocks[[k]](ctx$results), error = function(e) {
    return(e)
  })
  if (inherits(value, "error")) {
    return(list(value = NULL, error = block_error(value)))
  }
  if (!is.data.frame(value) && !is.character(value)) {
    return(list(
      value = NULL,
      error = paste0(
        "it returned an object of the class ",
        class(value)[1],
        ", and a block MUST return a data.frame or a character vector"
      )
    ))
  }
  return(list(value = value, error = NULL))
}

# The message of the error `e` of a block function. When R found no
# function, it adds that a block MUST name the package of each call,
# because show_block() does not attach the packages of the run script.
block_error <- function(e) {
  msg <- conditionMessage(e)
  english <- "could not find function \"%s\""
  starts <- sub("%s.*$", "", c(english, gettext(english, domain = "R")))
  if (any(startsWith(msg, starts[nzchar(starts)]))) {
    msg <- paste0(
      msg,
      ". A block function MUST name the package of each function that it ",
      "calls, for example data.table::data.table(), because ",
      "pbn::show_block() does not attach the packages of the run script"
    )
  }
  return(msg)
}

# Records the options of R now, and returns a function that sets them back
# and removes each option that was added since.
keep_options <- function() {
  before <- options()
  return(function() {
    added <- setdiff(names(options()), names(before))
    options(stats::setNames(rep(list(NULL), length(added)), added))
    options(before)
    return(invisible(NULL))
  })
}

# The rows of a value of a block: of a data.frame, or one for each element of
# a character vector.
block_rows <- function(value) {
  if (is.data.frame(value)) {
    return(nrow(value))
  }
  return(length(value))
}

# Prints the rows `i` of the value `value` of a block: a character vector as
# lines, a data.table and a data.frame without row names, with the options
# that show_block() sets. A tibble prints as
# a data.frame, because its print method cuts columns.
print_page <- function(value, i) {
  if (is.character(value)) {
    cat(value[i], sep = "\n")
    return(invisible(NULL))
  }
  if (inherits(value, "tbl_df")) {
    value <- as.data.frame(value)
  }
  page <- value[i, , drop = FALSE]
  if (data.table::is.data.table(page)) {
    page <- data.table::setalloccol(page)
  }
  # The options of show_block() keep data.table from cutting the rows.
  print(page, row.names = FALSE)
  return(invisible(NULL))
}
