# The blocks part of check_one_file(): each block runs on the real results
# of the project and on those of the run of the file, in a new R session
# that attaches no package of the run script, and each page that
# show_block() would print MUST be the same text.

# The blocks check of check_one_file() for the file `file`, whose run wrote
# its results under `home`. `tmp` is the folder of the check. Returns a
# list: `found`, the text of the first difference or NULL, and `n`, the
# number of blocks.
check_blocks_of_file <- function(file, home, tmp) {
  if (!exists("blocks", envir = state$env, inherits = FALSE)) {
    return(list(found = NULL, n = 0L))
  }
  out <- file.path(tmp, "blocks.qs2")
  script <- file.path(tmp, "show_blocks.R")
  writeLines(
    c(
      paste0(
        "exprs <- parse(",
        quoted(file),
        ", keep.source = FALSE, encoding = \"UTF-8\")"
      ),
      "init <- vapply(as.list(exprs), function(e) {",
      "  return(is.call(e) && identical(e[[1]], quote(pbn::initialize_project)))",
      "}, logical(1))",
      "invisible(eval(exprs[[which(init)]], envir = globalenv()))",
      "found <- list(",
      paste0("  project = pbn:::block_prints(", quoted(state$home), "),"),
      paste0("  file = pbn:::block_prints(", quoted(home), ")"),
      ")",
      paste0("qs2::qs_save(found, ", quoted(out), ")")
    ),
    script
  )
  log <- file.path(tmp, "blocks.log")
  status <- run_rscript(script, log)
  if (!identical(status, 0L) || !file.exists(out)) {
    return(list(
      found = sub(
        "the file of pbn::to_one_file() stopped",
        "the check of the blocks stopped",
        child_failure(status, log),
        fixed = TRUE
      ),
      n = NA_integer_
    ))
  }
  found <- qs2::qs_read(out)
  return(list(
    found = blocks_difference(found$project, found$file),
    n = length(found$project$blocks)
  ))
}

# What show_block() prints for each page of each block of the real results
# in the project folder `home`: a list with `blocks`, one element for each
# block with `text` or `error`, or `error` when the blocks cannot run. Each
# block gets the options of R from before it.
block_prints <- function(home) {
  ctx <- tryCatch(blocks_context(NULL, home), error = function(e) return(e))
  if (inherits(ctx, "error")) {
    return(list(error = conditionMessage(ctx)))
  }
  out <- lapply(seq_along(ctx$blocks), function(k) {
    restore <- keep_options()
    on.exit(restore(), add = TRUE)
    got <- block_call(ctx, k)
    if (!is.null(got$error)) {
      return(list(error = got$error))
    }
    pages <- max(1L, as.integer(ceiling(block_rows(got$value) / 35)))
    text <- lapply(seq_len(pages), function(page) {
      return(utils::capture.output(show_page(ctx, k, got$value, page, 35)))
    })
    return(list(text = unlist(text)))
  })
  names(out) <- names(ctx$blocks)
  return(list(blocks = out))
}

# The first difference of the prints `a`, of the project, and `b`, of the
# run of the file, from block_prints(), as text, or NULL. The line FAKE REAL
# DATA does not count, because the run of the file is never on fake real
# data.
blocks_difference <- function(a, b) {
  sides <- c("the project", "the file of pbn::to_one_file()")
  for (j in 1:2) {
    x <- list(a, b)[[j]]
    if (!is.null(x$error)) {
      return(paste0(
        "the blocks do not run on the real results of ",
        sides[j],
        ": ",
        x$error
      ))
    }
  }
  for (k in seq_along(a$blocks)) {
    found <- block_difference(
      a$blocks[[k]],
      b$blocks[[k]],
      k,
      names(a$blocks)[k],
      sides
    )
    if (!is.null(found)) {
      return(found)
    }
  }
  return(NULL)
}

# The difference of the prints `x`, of the project, and `y`, of the run of
# the file, of the block `k` with the name `name`, as text, or NULL.
block_difference <- function(x, y, k, name, sides) {
  head <- paste0("the block ", k, ", \"", name, "\",")
  for (j in 1:2) {
    err <- list(x, y)[[j]]$error
    if (!is.null(err)) {
      return(paste(
        head,
        "failed on the real results of",
        sides[j],
        "with:",
        err
      ))
    }
  }
  ta <- x$text[x$text != fake_real_label]
  tb <- y$text[y$text != fake_real_label]
  if (identical(ta, tb)) {
    return(NULL)
  }
  line_of <- function(z, i) return(if (i <= length(z)) z[i] else NA)
  i <- which(vapply(
    seq_len(max(length(ta), length(tb))),
    function(i) return(!identical(line_of(ta, i), line_of(tb, i))),
    logical(1)
  ))[1]
  return(paste0(
    head,
    " prints differently. Line ",
    i,
    " of its pages is ",
    shown_line(line_of(ta, i)),
    " for the project and ",
    shown_line(line_of(tb, i)),
    " for the file of pbn::to_one_file()."
  ))
}

# A line of a page in double quotes, or "no line" for NA.
shown_line <- function(x) {
  return(if (is.na(x)) "no line" else paste0("\"", x, "\""))
}
