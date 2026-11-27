#' Write the filled-in checklist of each reporting guideline
#'
#' `make_checklists()` writes one Markdown table for each checklist file
#' `guidelines/<x>.yml`, with the status of each item in the manuscript. An
#' item gets `numbers found` only when each formatted string of each block
#' that names it occurs in the manuscript text.
#'
#' @param version The version, for example `"v02"`. The default is the
#'   version of the last [initialize_project()] in this R session.
#' @param manuscript The path of one `.md` or `.docx` file, relative to the
#'   working folder. The default `NULL` is each `.md` file in
#'   `manuscript/built/<version>/` of `home`, without the files
#'   `checklist_*.md`.
#' @param home The project folder. The default is the folder of the last
#'   [initialize_project()] of `version` in this R session, and else the
#'   working folder.
#' @details
#' The items come from `guidelines/*.yml`, after the key `design` of each
#' file, as [check_project()] reads them. A block names its items in the
#' argument `items` of [block()] in `<version>_functions/blocks.R`, which
#' `make_checklists()` reads without running code.
#'
#' The formatted strings of a block are its formatted value, as [txt()]
#' gives it. That is the raw value of the real `blocks.qs2` of the version,
#' through the formatter of the block. So a change of a formatter needs no
#' [update_blocks()]. A formatter that returns a character vector gives each
#' element. A formatter that returns a data.frame gives each cell of each
#' row, row by row, with each cell as `as.character()` gives it. An `NA`
#' string is never found.
#'
#' The text of a `.md` file is its lines. A heading is a line that starts
#' with 1 to 6 `#` and a space, outside a fenced code block. The text of a
#' `.docx` file is in `word/document.xml`, which `utils::unzip()` reads. Each
#' paragraph `<w:p>` is one line, the contents of its `<w:t>` runs joined
#' with no space. A heading is a paragraph whose style `w:pStyle` starts
#' with `Heading`. The text before the first heading of a file is under the
#' name of the file.
#'
#' The search normalises the text and each string in 7 steps, in this
#' order. It changes `<br>`, `<br/>` and `<br />` to a space. It removes
#' each other HTML tag, which is `<`, a letter, then letters and digits, and
#' optional attributes after a space, then `>` or `/>`. It decodes `&lt;`,
#' `&gt;`, `&nbsp;` and `&amp;`. It removes the Markdown backslash before
#' `*`, `<`, `>`, `_`, `[` and `]`. It changes U+00A0 and U+2009 to a space.
#' It changes each run of white space to one space, and it trims the ends.
#' It keeps the minus sign U+2212 and the case of each letter. So the
#' Markdown `p \< 0.001` matches the string `p < 0.001`.
#'
#' A string is found when it occurs in the text of all files, joined line
#' by line. The column Reported in names each heading whose section holds
#' one of the strings of the item, joined by `; `.
#'
#' For each checklist, `make_checklists()` writes
#' `manuscript/built/<version>/checklist_<id>.md` in `home`, and it creates
#' that folder when it is absent. It writes no other file. The table has the
#' columns Section, Item, Recommendation, Reported in and Status. The
#' Recommendation is the full key `text` of the item. The Status is one of:
#'
#' - `numbers found`: a result item, and each string of each of its blocks
#'   occurs in the text. A reader still confirms that the text answers the
#'   item.
#' - `numbers missing: <string>`: a result item, and `<string>` is the first
#'   string that the text does not hold. The blocks are in the order of
#'   `blocks`.
#' - `no block`: a result item that no block names.
#' - `to fill`: a prose or an author item.
#'
#' The files `checklist_*.md` are never part of the text. A `manuscript`
#' path whose name matches `checklist_*.md` is an error.
#'
#' `make_checklists()` stops in each of these cases:
#'
#' - a block of [block()] has no string `format` or no literal `items`.
#' - a block that names a result item is not in the real `blocks.qs2`, or
#'   that file is missing.
#' - a checklist file breaks the schema of [check_project()].
#' @return The table of all checklists, invisibly. It is a data.frame with
#'   one row per item and the columns Guideline (the key `id`), Section,
#'   Item, Recommendation, Reported in and Status. Its cells are the text
#'   without the Markdown escapes of the files.
#' @seealso [block()], [txt()], [check_project()].
#' @export
make_checklists <- function(version = NULL, manuscript = NULL, home = NULL) {
  where <- checklists_where(version, home)
  version <- where$version
  home <- where$home
  built <- file.path("manuscript", "built", version)
  files <- checklist_manuscript_files(home, built, manuscript)
  text <- manuscript_text(files)
  items <- read_guidelines(home)
  strings <- tagged_strings(home, version, items)
  out <- checklist_rows(items, strings, text)
  if (nrow(items) > 0) {
    dir.create(file.path(home, built), recursive = TRUE, showWarnings = FALSE)
  }
  for (id in unique(items$id)) {
    rows <- out[out$Guideline == id, , drop = FALSE]
    first <- items[items$id == id, , drop = FALSE][1, ]
    writeLines(
      enc2utf8(checklist_lines(rows, first, files, home, version)),
      file.path(home, built, paste0("checklist_", id, ".md")),
      useBytes = TRUE
    )
  }
  return(invisible(out))
}

# The version and the project folder of make_checklists(), as txt_where()
# gives them for txt().
checklists_where <- function(version, home) {
  if (is.null(version)) {
    if (is.null(state$results)) {
      stop(
        "Give pbn::make_checklists() the version, as in version = \"v01\", ",
        "or call pbn::initialize_project() first.",
        call. = FALSE
      )
    }
    version <- state$version
  }
  check_version(version)
  if (is.null(home)) {
    home <- if (identical(version, state$version)) state$home else "."
  }
  if (!is_nonempty_string(home) || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  return(list(version = version, home = home))
}

# The pattern of the name of an output file of make_checklists().
checklist_file_pattern <- "^checklist_.*\\.md$"

# The manuscript files of make_checklists(): `manuscript`, checked, or each
# .md file of the folder `built` of `home` without the checklist files.
checklist_manuscript_files <- function(home, built, manuscript) {
  if (!is.null(manuscript)) {
    if (!is_nonempty_string(manuscript)) {
      stop(
        "`manuscript` must be NULL or one path of a .md or a .docx file.",
        call. = FALSE
      )
    }
    if (
      grepl(checklist_file_pattern, basename(manuscript), ignore.case = TRUE)
    ) {
      stop(
        "`manuscript` is ",
        manuscript,
        ", a file checklist_*.md that pbn::make_checklists() writes. Give ",
        "the manuscript itself.",
        call. = FALSE
      )
    }
    if (!grepl("\\.(md|docx)$", manuscript, ignore.case = TRUE)) {
      stop("`manuscript` must be a .md or a .docx file.", call. = FALSE)
    }
    if (!file.exists(manuscript) || dir.exists(manuscript)) {
      stop("There is no file ", manuscript, ".", call. = FALSE)
    }
    return(manuscript)
  }
  folder <- file.path(home, built)
  names <- if (dir.exists(folder)) {
    list.files(folder, pattern = "\\.md$")
  } else {
    character(0)
  }
  names <- names[!grepl(checklist_file_pattern, names, ignore.case = TRUE)]
  names <- names[!dir.exists(file.path(folder, names))]
  if (length(names) == 0) {
    stop(
      "There is no .md file in ",
      folder,
      " other than checklist_*.md. Build the manuscript there, or give it ",
      "as `manuscript`.",
      call. = FALSE
    )
  }
  return(file.path(folder, sort(names, method = "radix")))
}

# The normalised text of the files `files`, for the search of
# make_checklists(): `full`, all files joined line by line, and `sections`,
# a data.frame with `heading` and `text`, one row per section of each file.
manuscript_text <- function(files) {
  sections <- do.call(
    rbind,
    lapply(files, function(f) {
      lines <- if (grepl("\\.docx$", f, ignore.case = TRUE)) {
        docx_lines(f)
      } else {
        md_lines(f)
      }
      return(text_sections(lines$text, lines$heading, basename(f)))
    })
  )
  return(list(
    full = norm_text(paste(sections$raw, collapse = "\n")),
    sections = data.frame(
      heading = norm_text(sections$heading),
      text = norm_text(sections$raw)
    )
  ))
}

# The lines of the .md file `file`: `text`, and `heading`, the text of the
# heading of each line that is a heading and NA for the others.
md_lines <- function(file) {
  text <- readLines(file, warn = FALSE, encoding = "UTF-8")
  fence <- grepl("^ {0,3}(```|~~~)", text)
  inside <- (cumsum(fence) %% 2 == 1) | fence
  is_heading <- !inside & grepl("^ {0,3}#{1,6}( |\t|$)", text)
  heading <- rep(NA_character_, length(text))
  h <- sub("^ {0,3}#{1,6}", "", text[is_heading])
  h <- sub("[ \t]+#+[ \t]*$", "", h)
  h <- sub("[ \t]*\\{[^{}]*\\}[ \t]*$", "", h)
  heading[is_heading] <- trimws(h)
  return(list(text = text, heading = heading))
}

# The paragraphs of the .docx file `file` as lines: `text`, the contents of
# the runs <w:t> of each paragraph <w:p> joined with no space, and
# `heading`, that text for a paragraph whose style starts with Heading and
# NA for the others. Each pattern has (?s), so that `.` matches a newline:
# the XML of a paragraph MAY span lines, as in a pretty-printed
# document.xml. The text of a tracked deletion <w:del> and of the old place
# of moved text <w:moveFrom> is not read, also when it is in <w:t>.
docx_lines <- function(file) {
  tmp <- tempfile("pbn_docx")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  got <- suppressWarnings(tryCatch(
    utils::unzip(file, files = "word/document.xml", exdir = tmp),
    error = function(e) return(character(0))
  ))
  xml_file <- file.path(tmp, "word", "document.xml")
  if (length(got) == 0 || !file.exists(xml_file)) {
    stop(file, " is not a .docx file with word/document.xml.", call. = FALSE)
  }
  xml <- paste(
    readLines(xml_file, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  # A self-closing <w:del/> marks a deleted paragraph mark, and holds no text.
  xml <- gsub(
    "(?s)<w:(del|moveFrom)(\\s[^>]*[^/>])?>.*?</w:\\1>",
    "",
    xml,
    perl = TRUE
  )
  paragraphs <- regmatches(
    xml,
    gregexpr("(?s)<w:p(\\s[^>]*)?(/>|>.*?</w:p>)", xml, perl = TRUE)
  )[[1]]
  text <- vapply(
    paragraphs,
    function(p) {
      runs <- regmatches(
        p,
        gregexpr("(?s)<w:t(\\s[^>]*)?>.*?</w:t>", p, perl = TRUE)
      )[[1]]
      runs <- sub("(?s)^<w:t(\\s[^>]*)?>", "", runs, perl = TRUE)
      runs <- sub("</w:t>$", "", runs)
      return(xml_decode(paste(runs, collapse = "")))
    },
    character(1),
    USE.NAMES = FALSE
  )
  is_heading <- grepl(
    "(?s)<w:pStyle\\s+w:val=[\"']Heading",
    paragraphs,
    perl = TRUE
  )
  heading <- ifelse(is_heading, text, NA_character_)
  return(list(text = text, heading = heading))
}

# The text `x` of an XML element with its five entities and its character
# references decoded. &amp; goes last, so "&amp;lt;" gives "&lt;".
xml_decode <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  pattern <- "&#([0-9]+|x[0-9A-Fa-f]+);"
  refs <- regmatches(x, gregexpr(pattern, x))
  regmatches(x, gregexpr(pattern, x)) <- lapply(refs, function(r) {
    hex <- startsWith(r, "&#x")
    num <- ifelse(hex, substr(r, 4, nchar(r) - 1), substr(r, 3, nchar(r) - 1))
    code <- ifelse(hex, strtoi(num, 16L), strtoi(num, 10L))
    return(vapply(code, intToUtf8, character(1)))
  })
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  return(x)
}

# The sections of the lines `text` of one file: a data.frame with `heading`
# and `raw`, the lines of the section joined by "\n". A heading line starts
# a section and is part of it. The lines before the first heading are a
# section with the heading `first`.
text_sections <- function(text, heading, first) {
  if (length(text) == 0) {
    return(data.frame(heading = character(0), raw = character(0)))
  }
  starts <- !is.na(heading)
  group <- cumsum(starts)
  labels <- c(first, heading[starts])
  raw <- vapply(
    split(text, group),
    paste,
    character(1),
    collapse = "\n",
    USE.NAMES = FALSE
  )
  return(data.frame(heading = labels[sort(unique(group)) + 1], raw = raw))
}

# The text `x` normalised for the search of make_checklists(), in the 7
# steps of ?make_checklists. This is the norm() of the checklist work, and
# the same function as norm() of
# reporting-guidelines/check_checklists.R of the methods repository. Step 2
# removes a real tag only, so "p \< 0.001 and x > y" keeps its text.
norm_text <- function(x) {
  x <- gsub("<br\\s*/?>", " ", x, perl = TRUE)
  x <- gsub("</?[A-Za-z][A-Za-z0-9]*(\\s[^<>]*)?/?>", "", x, perl = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("\\\\([*<>_\\[\\]])", "\\1", x, perl = TRUE)
  x <- gsub("[\u00a0\u2009]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  return(trimws(x))
}

# The formatted strings of the blocks that name the result items of
# `items`, the value of read_guidelines(), in the version `version` of
# `home`. A named list, one element per tag "<id>:<item>" that a block
# names, in the order of `blocks`: the strings of each block of the tag
# joined. A result item without an element has no block.
tagged_strings <- function(home, version, items) {
  tags <- paste0(items$id, ":", items$item, recycle0 = TRUE)
  wanted <- unique(tags[items$kind == "result"])
  file <- file.path(home, blocks_file(version))
  if (length(wanted) == 0 || !file.exists(file)) {
    return(list())
  }
  elements <- checklist_block_elements(file)
  out <- list()
  from <- NULL
  for (name in names(elements)) {
    e <- elements[[name]]
    tags <- checklist_block_tags(name, e, wanted, file)
    if (length(tags) == 0) {
      next
    }
    # The values and the formatters are read once, at the first block that
    # names a wanted item.
    if (is.null(from)) {
      from <- list(
        saved = read_real_blocks(home, version),
        env = formatters_of(home, version)
      )
    }
    value <- checklist_block_value(name, e, from, home, version)
    for (tag in tags) {
      out[[tag]] <- c(out[[tag]], formatted_strings(value))
    }
  }
  return(out)
}

# The elements of the list blocks of the file `file`, read without running
# it, see blocks_elements_of_file(). Stops when the file has no such list.
checklist_block_elements <- function(file) {
  elements <- blocks_elements_of_file(file)
  if (is.null(elements)) {
    stop(
      "pbn::make_checklists() cannot read the list blocks of ",
      file,
      ". It needs one assignment blocks <- list(...) in a file that parses.",
      call. = FALSE
    )
  }
  return(elements)
}

# The tags of `wanted` that the element `e` of the file `file`, the block
# `name`, names. Nothing for an element that is not a pbn::block(). Stops on
# a pbn::block() without a string format or a literal items.
checklist_block_tags <- function(name, e, wanted, file) {
  if (!e$is_block) {
    return(character(0))
  }
  if (is.null(e$items) || !is_formatter_name(e$format)) {
    stop(
      "The block \"",
      name,
      "\" of ",
      file,
      " needs a string format and a literal items = c(\"<id>:<item>\", ",
      "...), so that pbn::make_checklists() reads its items.",
      call. = FALSE
    )
  }
  return(intersect(e$items, wanted))
}

# The formatted value of the block `name`, the element `e`. `from` is a
# list with `saved`, the raw values, and `env`, the project formatters.
# Stops when the raw values of `version` in `home` hold no such block.
checklist_block_value <- function(name, e, from, home, version) {
  if (!name %in% names(from$saved)) {
    stop(
      blocks_source_file(version, home),
      " holds no block \"",
      name,
      "\". Write it again with pbn::update_blocks().",
      call. = FALSE
    )
  }
  return(format_block(
    name,
    list(format = e$format),
    from$saved[[name]],
    from$env
  ))
}

# The raw values of the blocks of `version` in `home`, from
# blocks_source(), which read_blocks() and txt() use too. Stops when the
# file is missing.
read_real_blocks <- function(home, version) {
  return(blocks_source(version, home)$values)
}

# Each string of the formatted value `value`: each element of a character
# vector, or each cell of a data.frame, row by row.
formatted_strings <- function(value) {
  if (!is.data.frame(value)) {
    return(as.character(value))
  }
  if (nrow(value) == 0 || ncol(value) == 0) {
    return(character(0))
  }
  cells <- vapply(
    seq_len(ncol(value)),
    function(j) return(as.character(value[[j]])),
    character(nrow(value))
  )
  cells <- matrix(cells, nrow = nrow(value))
  return(as.vector(t(cells)))
}

# The rows of the table of make_checklists() for the items `items`, with the
# formatted strings `strings` of tagged_strings() and the text `text` of
# manuscript_text().
checklist_rows <- function(items, strings, text) {
  n <- nrow(items)
  reported <- rep("", n)
  status <- rep("to fill", n)
  for (i in seq_len(n)) {
    if (items$kind[i] != "result") {
      next
    }
    s <- strings[[paste0(items$id[i], ":", items$item[i])]]
    if (is.null(s)) {
      status[i] <- "no block"
      next
    }
    found <- vapply(s, string_found, logical(1), text = text$full)
    status[i] <- if (all(found)) {
      "numbers found"
    } else {
      paste0("numbers missing: ", s[!found][1])
    }
    hit <- rep(FALSE, nrow(text$sections))
    for (x in s[found]) {
      hit <- hit | vapply(text$sections$text, string_found, logical(1), x = x)
    }
    reported[i] <- paste(unique(text$sections$heading[hit]), collapse = "; ")
  }
  return(data.frame(
    Guideline = items$id,
    Section = items$section,
    Item = items$item,
    Recommendation = items$text,
    `Reported in` = reported,
    Status = status,
    check.names = FALSE
  ))
}

# TRUE when the string `x`, normalised, occurs in the normalised text
# `text`. An NA string is never found.
string_found <- function(x, text) {
  if (is.na(x)) {
    return(FALSE)
  }
  return(grepl(norm_text(x), text, fixed = TRUE))
}

# The lines of the file checklist_<id>.md: the meaning of the status, where
# the items and the text came from, and the table of the rows `rows`.
# `first` is the first item of the checklist.
checklist_lines <- function(rows, first, files, home, version) {
  cell <- function(x) {
    x <- gsub("\r?\n", "<br>", x)
    return(gsub("|", "\\|", x, fixed = TRUE))
  }
  table <- paste0(
    "| ",
    cell(rows$Section),
    " | ",
    cell(rows$Item),
    " | ",
    cell(rows$Recommendation),
    " | ",
    cell(rows$`Reported in`),
    " | ",
    cell(rows$Status),
    " |"
  )
  return(c(
    paste(
      "`numbers found` means that each formatted string of the blocks of",
      "the item occurs in the manuscript text. A reader confirms each item."
    ),
    "",
    paste0(
      "Checklist: ",
      first$file,
      ". Manuscript: ",
      paste(manuscript_names(files, home), collapse = ", "),
      ". Blocks: ",
      blocks_file(version),
      " and ",
      version,
      "_results/",
      basename(dirname(blocks_source_file(version, home))),
      "/blocks.qs2."
    ),
    "",
    "| Section | Item | Recommendation | Reported in | Status |",
    "|---|---|---|---|---|",
    table
  ))
}

# The paths `files` relative to `home` where they are under it.
manuscript_names <- function(files, home) {
  top <- paste0(normalizePath(home, winslash = "/"), "/")
  full <- normalizePath(files, winslash = "/", mustWork = FALSE)
  under <- startsWith(full, top)
  files[under] <- substring(full[under], nchar(top) + 1)
  return(files)
}
