#' Take the blocks off a server as text, and read them back
#'
#' `export_blocks()` prints the raw value of each block as R code, in pages
#' with a checksum. `import_blocks()` reads the captured pages into
#' `<version>_results/real/blocks.qs2`, and `read_blocks()` returns those
#' values for a manuscript.
#'
#' @param version The version, for example `"v01"`. For `export_blocks()`,
#'   the default `NULL` is the version of the last [initialize_project()]
#'   in this R session.
#' @param home The project folder. The default `"."` is the working folder.
#' @details
#' ## export_blocks()
#'
#' `export_blocks()` runs on the server, after the file of [to_one_file()].
#' It needs [initialize_project()] in this R session, and it reads the
#' blocks and the real results as [show_blocks()] does. For each block, it
#' keeps the raw value, the value that [save_results()] writes to
#' `blocks.qs2`. It does not call the formatter of a [block()]. It turns a
#' data.table into a data.frame, because R cannot read the pointer of a
#' data.table back. It stops on a block that stops, and the error names the
#' block.
#'
#' The text of block `k` is the output of base R `dput()` for a list. The
#' list holds one element: the raw value, with the name of the block. The
#' `control` of
#' `dput()` is `c("keepNA", "keepInteger", "niceNames", "showAttributes",
#' "digits17")`. With `"digits17"`, each number has 17 significant digits,
#' so the number after the import is the same double.
#'
#' Each page has at most 35 lines: a header line and at most 34 lines of
#' text. The header line is
#' `pbn-blocks <version> run <run date> block <k>/<n> page <p>/<P> md5 <m>`.
#' The run date is that of [show_block()]. `<m>` is the first 8 hex
#' characters of [tools::md5sum()] of the text lines of the page. pbn takes
#' them without trailing white space, joined by `"\n"`, with no final
#' newline. An empty line follows each page.
#'
#' When the real results have `fake_real_data` `TRUE` in their manifest,
#' each header line ends with the token ` fake`. Examples are a run of a
#' one-file project on this computer, see [initialize_project()]. The pages
#' of real data have no token.
#'
#' ## import_blocks()
#'
#' Copy or type the pages from the screen of the server into
#' `<version>_results/real/export_blocks.txt`. `import_blocks()` reads that
#' file. It removes the trailing white space of each line and each empty
#' line, and it ignores the lines before the first header line. The lines
#' after a header line, up to the next header line, are the text of that
#' page.
#'
#' `import_blocks()` stops and writes nothing when:
#' - the file has no header line,
#' - a header line names another version, or the pages name more than one
#'   run date or more than one count of blocks,
#' - some header lines end with the token ` fake` and some do not,
#' - the md5 of the text of a page differs from its header, and the error
#'   names the block and the page,
#' - a page is missing, and the error names the block and the page,
#' - a page comes twice with other text. A page that comes twice with the
#'   same text counts once.
#' - R cannot parse the text of a block, or the text is not a list of one
#'   named value.
#'
#' It rebuilds each value with `eval(parse(text = text), envir = baseenv())`.
#' So import only the pages that `export_blocks()` printed, because
#' `import_blocks()` runs the file as R code. It writes the named list of
#' the values to `<version>_results/real/blocks.qs2` and overwrites that
#' file. It writes there in a one-file project too, where the scenario
#' `real` of this computer is in `fake/`, see [initialize_project()]. When
#' the pages end with the token ` fake`, the list has the attribute
#' `pbn_fake = TRUE`.
#'
#' ## read_blocks()
#'
#' `read_blocks()` returns `<version>_results/real/blocks.qs2` when it
#' exists. Else it returns the `blocks.qs2` of the scenario `real` of this
#' computer, and it gives a warning that the numbers are fake. In a one-file
#' project that file is `<version>_results/fake/blocks.qs2`. In a project
#' that is not one-file it is `real/blocks.qs2` itself, so `read_blocks()`
#' stops. [txt()] and [make_checklists()] read the blocks in the same way.
#' The rule `built_reads_source` of [check_project()] says which functions
#' a manuscript in `manuscript/built/` MUST use.
#'
#' The values are fake when they come from another file than
#' `real/blocks.qs2`, or when `real/blocks.qs2` has `pbn_fake = TRUE`. For
#' fake values in a document that knitr renders, such as a `.qmd` or an
#' `.Rmd`, `read_blocks()`, [txt()] and [make_checklists()] add a knitr
#' document hook. The hook puts the line `**FAKE DATA**` once at the top of
#' the output, after the YAML header. A document that reads only real
#' values gets no such line.
#' @return `export_blocks()`: invisibly, the printed lines. `import_blocks()`:
#'   invisibly, the path of the `blocks.qs2` that it wrote. `read_blocks()`:
#'   a named list with the raw value of each block.
#' @family one file
#' @seealso `vignette("pbn")`, section "Running on an air-gapped server".
#' @export
export_blocks <- function(version = NULL) {
  restore <- keep_options()
  on.exit(restore(), add = TRUE)
  ctx <- blocks_context(version, NULL)
  n <- length(ctx$blocks)
  out <- character(0)
  for (k in seq_len(n)) {
    value <- stats::setNames(list(export_value(ctx, k)), names(ctx$blocks)[k])
    out <- c(out, export_pages(export_text(value), ctx, k, n))
  }
  cat(paste0(out, "\n"), sep = "")
  return(invisible(out))
}

#' @rdname export_blocks
#' @export
import_blocks <- function(version, home = ".") {
  if (!is_one_string(version)) {
    stop("`version` must be one string, for example \"v01\".", call. = FALSE)
  }
  real <- file.path(home, paste0(version, "_results"), "real")
  file <- file.path(real, export_blocks_file)
  if (!file.exists(file)) {
    stop(
      "There is no ",
      file,
      ". Save the pages of pbn::export_blocks() on the server there.",
      call. = FALSE
    )
  }
  pages <- check_pages(read_pages(file), version)
  values <- list()
  for (k in seq_len(pages$n[1])) {
    values <- c(values, rebuild_block(pages[pages$k == k, ], k))
  }
  if (pages$fake[1]) {
    attr(values, "pbn_fake") <- TRUE
  }
  out <- file.path(real, blocks_qs2_file)
  qs2::qs_save(values, out)
  cat(
    "pbn: wrote ",
    out,
    " from ",
    nrow(pages),
    " pages of ",
    length(values),
    " blocks",
    if (pages$fake[1]) ", marked fake",
    ".\n",
    sep = ""
  )
  return(invisible(out))
}

#' @rdname export_blocks
#' @export
read_blocks <- function(version, home = ".") {
  return(blocks_source(version, home)$values)
}

# The file of the pages of export_blocks() in <version>_results/real/.
export_blocks_file <- "export_blocks.txt"

# The lines of one page of export_blocks(): a header line and at most 34
# lines of text.
export_page_lines <- 35L

# The control of dput() for the text of a block.
export_control <- c(
  "keepNA",
  "keepInteger",
  "niceNames",
  "showAttributes",
  "digits17"
)

# The header line of a page of export_blocks(), with the groups version,
# run date, block, blocks, page, pages, md5 and the token " fake" of the
# pages of fake data.
page_header_pattern <- paste0(
  "^pbn-blocks (v[0-9]{2}) run (.+?) block ([0-9]+)/([0-9]+) ",
  "page ([0-9]+)/([0-9]+) md5 ([0-9a-f]{8})( fake)?$"
)

# The token at the end of the header line of a page of fake data.
fake_token <- "fake"

# The raw value of the block `k` of `ctx`, a data.table as a data.frame.
# Stops when the block stops, and the error names the block.
export_value <- function(ctx, k) {
  got <- block_call(ctx, k, format = FALSE)
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
  if (data.table::is.data.table(got$raw)) {
    return(as.data.frame(got$raw))
  }
  return(got$raw)
}

# The lines of dput() of `x`, without trailing white space.
export_text <- function(x) {
  lines <- utils::capture.output(dput(x, control = export_control))
  return(sub("[[:space:]]+$", "", lines))
}

# The pages of the text `text` of the block `k` of `n` of `ctx`: each page
# is its header line, its lines of text and an empty line. The header line
# ends with fake_token when `ctx$fake_real_data` is TRUE.
export_pages <- function(text, ctx, k, n) {
  pages <- split(text, ceiling(seq_along(text) / (export_page_lines - 1L)))
  out <- character(0)
  for (p in seq_along(pages)) {
    header <- sprintf(
      "pbn-blocks %s run %s block %d/%d page %d/%d md5 %s",
      ctx$version,
      ctx$run_date,
      k,
      n,
      p,
      length(pages),
      page_md5(pages[[p]])
    )
    if (isTRUE(ctx$fake_real_data)) {
      header <- paste(header, fake_token)
    }
    out <- c(out, header, pages[[p]], "")
  }
  return(out)
}

# The first 8 hex characters of the md5 of the lines `lines`, joined by
# "\n", with no final newline.
page_md5 <- function(lines) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste(lines, collapse = "\n"))), tmp)
  return(substr(unname(tools::md5sum(tmp)), 1, 8))
}

# The pages of the file `file` of export_blocks(): a data.frame with a row
# for each header line, its fields, and `text`, a list of its lines.
read_pages <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  lines <- sub("[[:space:]]+$", "", lines)
  lines <- lines[nzchar(lines)]
  head <- grepl(page_header_pattern, lines)
  if (!any(head)) {
    stop(
      file,
      " has no page of pbn::export_blocks(): no line matches ",
      "pbn-blocks <version> run <date> block <k>/<n> page <p>/<P> md5 <m>.",
      call. = FALSE
    )
  }
  keep <- seq_along(lines) >= which(head)[1]
  lines <- lines[keep]
  head <- head[keep]
  groups <- split(lines, cumsum(head))
  fields <- do.call(
    rbind,
    regmatches(
      lines[head],
      regexec(page_header_pattern, lines[head])
    )
  )
  pages <- data.frame(
    version = fields[, 2],
    run = fields[, 3],
    k = as.integer(fields[, 4]),
    n = as.integer(fields[, 5]),
    p = as.integer(fields[, 6]),
    pages = as.integer(fields[, 7]),
    md5 = fields[, 8],
    fake = fields[, 9] == paste0(" ", fake_token)
  )
  pages$text <- unname(lapply(groups, function(g) return(g[-1])))
  return(pages)
}

# The pages `pages` of read_pages() after the checks of import_blocks() for
# the version `version`, each page once, in the order of the blocks and
# the pages. Stops at the first fault.
check_pages <- function(pages, version) {
  other <- pages$version != version
  if (any(other)) {
    stop(
      "A page of the file is of the version ",
      pages$version[other][1],
      ", not of ",
      version,
      ".",
      call. = FALSE
    )
  }
  for (field in c("run", "n")) {
    if (length(unique(pages[[field]])) != 1) {
      stop(
        "The pages come from more than one export: they name the ",
        if (field == "run") "run dates " else "counts of blocks ",
        paste(unique(pages[[field]]), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }
  if (length(unique(pages$fake)) != 1) {
    stop(
      "The header lines disagree on the token ",
      fake_token,
      ": some end with it and some do not. Keep the pages of one export only.",
      call. = FALSE
    )
  }
  check_page_md5(pages)
  pages <- drop_same_pages(pages)
  check_missing_pages(pages)
  return(pages[order(pages$k, pages$p), ])
}

# Stops at the first page of `pages` whose text has another md5 than its
# header line, and the error names the block and the page.
check_page_md5 <- function(pages) {
  for (i in seq_len(nrow(pages))) {
    got <- page_md5(pages$text[[i]])
    if (!identical(got, pages$md5[i])) {
      stop(
        "The block ",
        pages$k[i],
        ", page ",
        pages$p[i],
        " of ",
        pages$pages[i],
        ": the md5 of its lines is ",
        got,
        ", and its header line says ",
        pages$md5[i],
        ". Correct the lines of that page from the screen.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# The pages `pages` with each page once. Stops at a page that comes twice
# with other text, and the error names the block and the page.
drop_same_pages <- function(pages) {
  key <- paste(pages$k, pages$p)
  for (i in which(duplicated(key))) {
    first <- match(key[i], key)
    if (!identical(pages$text[[i]], pages$text[[first]])) {
      stop(
        "The block ",
        pages$k[i],
        ", page ",
        pages$p[i],
        " comes twice in the file, with other lines. Keep the page of one ",
        "export only.",
        call. = FALSE
      )
    }
  }
  return(pages[!duplicated(key), ])
}

# Stops at the first page of the blocks 1 to n of `pages` that is not
# there, and the error names the block and the page.
check_missing_pages <- function(pages) {
  for (k in seq_len(pages$n[1])) {
    of_k <- pages[pages$k == k, ]
    total <- if (nrow(of_k) == 0) 1L else max(of_k$pages)
    if (nrow(of_k) > 0 && length(unique(of_k$pages)) != 1) {
      stop(
        "The pages of the block ",
        k,
        " give more than one count of pages.",
        call. = FALSE
      )
    }
    lost <- setdiff(seq_len(total), of_k$p)
    if (length(lost) > 0) {
      stop(
        "The block ",
        k,
        ", page ",
        lost[1],
        " of ",
        if (nrow(of_k) == 0) "its pages" else total,
        " is missing from the file.",
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# The value of the block `k` from its pages `pages`, in the order of the
# pages: a list of one value, with the name of the block.
rebuild_block <- function(pages, k) {
  text <- unlist(pages$text)
  value <- tryCatch(
    eval(parse(text = text), envir = baseenv()),
    error = function(e) return(e)
  )
  if (inherits(value, "error")) {
    stop(
      "The block ",
      k,
      ": R cannot read its lines: ",
      conditionMessage(value),
      call. = FALSE
    )
  }
  named <- !is.null(names(value)) && all(nzchar(names(value)))
  if (!is.list(value) || length(value) != 1 || !named) {
    stop(
      "The block ",
      k,
      ": its lines are not a list of one named value, as ",
      "pbn::export_blocks() prints it.",
      call. = FALSE
    )
  }
  return(value)
}

# The blocks.qs2 that blocks_source() reads for `version` in `home`:
# <version>_results/real/blocks.qs2 when it exists, else the blocks.qs2 of
# the scenario "real" of this computer, see scenario_dir().
blocks_source_file <- function(version, home) {
  real <- file.path(home, paste0(version, "_results"), "real", blocks_qs2_file)
  if (file.exists(real)) {
    return(real)
  }
  return(file.path(scenario_dir(version, "real", home), blocks_qs2_file))
}

# The one reader of blocks.qs2 for read_blocks(), txt() and
# make_checklists(): a list with `values`, the raw values, `file`, the file
# of blocks_source_file(), and `fake`. The values are fake when they come
# from another file than real/blocks.qs2, and then it gives a warning. They
# are fake too when real/blocks.qs2 has the attribute pbn_fake of
# import_blocks(). For fake values in a knitr document, it calls
# mark_fake_document(). Stops when the file is missing.
blocks_source <- function(version, home) {
  real <- file.path(home, paste0(version, "_results"), "real", blocks_qs2_file)
  file <- blocks_source_file(version, home)
  if (!file.exists(file)) {
    stop(
      "There is no ",
      file,
      ". Write it with pbn::update_blocks() or with a run.",
      call. = FALSE
    )
  }
  values <- qs2::qs_read(file)
  fallback <- !identical(file, real)
  if (fallback) {
    warning(
      "The numbers of ",
      version,
      " are fake: there is no ",
      real,
      ", because pbn::import_blocks() has not run. pbn returns ",
      file,
      ".",
      call. = FALSE
    )
  }
  fake <- fallback || isTRUE(attr(values, "pbn_fake"))
  if (fake) {
    mark_fake_document()
  }
  return(list(values = values, file = file, fake = fake))
}

# The text at the top of a knitr document that reads fake values.
fake_document_label <- "**FAKE DATA**"

# During knitr::knit(), adds a document hook that puts fake_document_label
# at the top of the output, after a YAML header. It adds the hook once for
# each document, and it keeps the document hook that was there. knitr sets
# its options and hooks back at the end of each document. Outside knitr it
# does nothing.
mark_fake_document <- function() {
  if (!isTRUE(getOption("knitr.in.progress"))) {
    return(invisible(NULL))
  }
  if (isTRUE(knitr::opts_knit$get("pbn_fake_marked"))) {
    return(invisible(NULL))
  }
  knitr::opts_knit$set(pbn_fake_marked = TRUE)
  old <- knitr::knit_hooks$get("document")
  knitr::knit_hooks$set(document = function(x) {
    if (is.function(old)) {
      x <- old(x)
    }
    return(add_fake_label(x))
  })
  return(invisible(NULL))
}

# The output `x` of a knitr document with fake_document_label at its top,
# after the YAML header when the output starts with one.
add_fake_label <- function(x) {
  text <- paste(x, collapse = "\n")
  yaml <- regmatches(text, regexpr("^---\\n(?s).*?\\n---\\n", text, perl = TRUE))
  rest <- substring(text, nchar(paste(yaml, collapse = "")) + 1)
  return(paste0(
    paste(yaml, collapse = ""),
    "\n",
    fake_document_label,
    "\n\n",
    rest
  ))
}
