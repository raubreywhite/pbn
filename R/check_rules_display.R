# The rule display_numbering of check_project(), from the set
# house_2026_11_27. The numbered figures and tables of the outputs, of the
# manuscript and of its citations agree.
#
# A key names one numbered item: fig_1, fig_s1, tab_1 or tab_s1. Its series
# is the key without the number: fig_, fig_s, tab_ or tab_s.

# The names of an output file, after the prefix fake_data_.
display_external_pattern <- "^(fig|tab)_s?[1-9][0-9]*_.*_external\\.txt$"
display_figure_pattern <- "^fig_s?[1-9][0-9]*_"
display_table_file_pattern <- "^tab_s?[1-9][0-9]*_"
display_nonum_pattern <- "^nonum_"
display_tables_file <- "tables.xlsx"
display_figure_data_file <- "data_behind_figures.xlsx"

# The names of a sheet of tables.xlsx and of data_behind_figures.xlsx.
display_table_sheet_pattern <- "^tab_s?[1-9][0-9]*$"
display_figure_sheet_pattern <- "^fig_s?[1-9][0-9]*_"

# A citation: an optional "Supplementary", the word, and a number or a
# range, such as "Figure 2", "Tables 1-3" or "Supplementary Figure 1". After
# the plural word, a list follows the Oxford comma: two items, "Figures 1
# and 2", or three or more, "Figures 1, 2, and 3". A list without the Oxford
# comma, "Figures 1, 2 and 3", counts too, and display_oxford_rows() fails
# it. A comma after one item ends the citation, so "Figures 1 and 2, 25 and
# 30 persons" cites Figures 1 and 2 only. A number is not followed by % or a
# decimal part.
display_number <- "S?[0-9]+[A-Za-z]?(?![0-9]|\\s*%|[.,][0-9])"
display_range_part <- paste0("\\s*[-\u2013\u2014]\\s*", display_number)
display_item <- paste0(display_number, "(?:", display_range_part, ")?")
display_comma_items <- paste0("(?:\\s*,\\s*", display_item, ")+")
display_list_part <- paste0(
  "(?:",
  display_comma_items,
  "\\s*,\\s*(?:and\\s+|&\\s*)",
  display_item,
  "|",
  display_comma_items,
  "(?:\\s+and\\s+|\\s*&\\s*)",
  display_item,
  "|",
  "(?:\\s+and\\s+|\\s*&\\s*)",
  display_item,
  ")?"
)
display_supplementary <- "(?:([Ss]upplementary)\\s+)?"
display_cite_pattern <- paste0(
  "\\b",
  display_supplementary,
  "(?:",
  "(?:[Ff]igures|[Ff]igs\\.|[Tt]ables)\\s+",
  display_item,
  display_list_part,
  "|",
  "(?:[Ff]igure|[Ff]ig\\.?|[Tt]able)\\s+",
  display_item,
  ")\\b"
)
# A caption: a paragraph that starts with "Figure 2." or "Table S1:", also
# in Markdown emphasis, such as "**Figure 2.**", or as the text of an image,
# such as "![Figure 2. The gap](fig.png)".
display_caption_pattern <- paste0(
  "^\\s*(?:!\\[\\s*)?(?:\\*{1,2}|_{1,2})?",
  "(?:([Ss]upplementary)\\s+)?(Figure|Table)\\s+(S?[0-9]+)",
  "(?:\\*{1,2}|_{1,2})?[.:](?:\\*{1,2}|_{1,2})?"
)

# The rule display_numbering. It checks the names of the outputs of the
# newest run version, and the manuscript of the newest manuscript version
# when there is one.
rule_display_numbering <- function(path) {
  entries <- project_entries(path)
  files <- entries[!dir.exists(file.path(path, entries))]
  version <- display_newest_run(files)
  outputs <- display_output_files(files, version)
  found <- list(
    display_name_rows(outputs),
    display_workbook_rows(path, outputs),
    display_figure_data_rows(path, outputs)
  )
  manuscript <- display_manuscript_files(files)
  folder <- display_match_folder(path, outputs, version)
  if (length(manuscript) > 0) {
    text <- display_manuscript_text(path, manuscript)
    found <- c(
      found,
      list(
        text$rows,
        display_order_rows(text$cites),
        display_caption_rows(text$cites, text$captions),
        display_match_rows(path, outputs, text, manuscript[1], folder)
      )
    )
  }
  found <- bind_rows(found)
  if (nrow(found) > 0) {
    return(found)
  }
  return(display_row_pass(length(manuscript) > 0, folder))
}

# The PASS row. It names the output folder that the manuscript matched, or
# says that there is no manuscript.
display_row_pass <- function(has_manuscript, folder) {
  names <- "every output name and sheet name has a number or nonum_"
  detail <- if (has_manuscript) {
    paste0(
      names,
      ", the manuscript cites each series in the order 1, 2, 3, and its ",
      "items are the numbered items of ",
      folder
    )
  } else {
    paste0(names, ", and there is no manuscript to match")
  }
  return(project_row("display_numbering", "PASS", "", detail))
}

display_row <- function(path, detail) {
  return(project_row("display_numbering", "FAIL", path, detail))
}

# The newest run version vNN of the files `files`, or NA without a run
# script.
display_newest_run <- function(files) {
  runs <- files[grepl(run_script_pattern, files)]
  if (length(runs) == 0) {
    return(NA_character_)
  }
  return(sprintf("v%02d", max(version_number(runs))))
}

# The files of `files` in the output folders of the run version `version`,
# as a data.frame: `file`, its path; `folder`, its output folder; and
# `name`, its name without the prefix fake_data_.
display_output_files <- function(files, version) {
  pattern <- paste0(
    "^",
    version,
    "_results/[^/]+/(output|output_fake_data)/"
  )
  out <- if (is.na(version)) character(0) else files[grepl(pattern, files)]
  folder <- regmatches(out, regexpr(pattern, out))
  return(data.frame(
    file = out,
    folder = folder,
    name = sub("^fake_data_", "", basename(out))
  ))
}

display_real_folder <- function(version) {
  if (is.na(version)) {
    version <- "vNN"
  }
  return(paste0(version, "_results/real/output/"))
}

# The output folder that the manuscript matches: vNN_results/real/output/
# when it exists. Otherwise the first output_fake_data/ folder of the run
# version in name order, such as fake/output_fake_data/ of a one-file
# project, because its names are the same after the prefix fake_data_.
display_match_folder <- function(path, outputs, version) {
  real <- display_real_folder(version)
  if (dir.exists(file.path(path, real)) || nrow(outputs) == 0) {
    return(real)
  }
  fake <- unique(outputs$folder[endsWith(outputs$folder, "/output_fake_data/")])
  if (length(fake) == 0) {
    return(real)
  }
  return(sort(fake, method = "radix")[1])
}

# The kind of each output name `name`: external, figure, tables,
# figure_data, nonum, table_file or unknown.
display_kind <- function(name) {
  kind <- rep("unknown", length(name))
  kind[grepl(display_nonum_pattern, name)] <- "nonum"
  kind[grepl(display_table_file_pattern, name)] <- "table_file"
  kind[name == display_figure_data_file] <- "figure_data"
  kind[name == display_tables_file] <- "tables"
  kind[grepl(display_figure_pattern, name)] <- "figure"
  kind[grepl(display_external_pattern, name)] <- "external"
  return(kind)
}

# The key of each name `x` that starts with a key, such as fig_1 of
# fig_1_gap.png or of the sheet fig_1_cutoffs.
display_key <- function(x) {
  return(sub("^((fig|tab)_s?[0-9]+)(_.*)?$", "\\1", x))
}

display_name_rows <- function(outputs) {
  kind <- display_kind(outputs$name)
  table_file <- outputs$file[kind == "table_file"]
  unknown <- outputs$file[kind == "unknown"]
  return(rbind(
    display_row(
      table_file,
      paste0(
        table_file,
        ": a numbered table MUST be a sheet tab_N or tab_sN of ",
        "tables.xlsx, or an item tab_N_<name>_external.txt",
        recycle0 = TRUE
      )
    ),
    display_row(
      unknown,
      paste0(
        unknown,
        ": an output name MUST start with fig_N_, fig_sN_ or nonum_, end ",
        "with _external.txt after tab_N_ or tab_sN_, or be tables.xlsx or ",
        "data_behind_figures.xlsx",
        recycle0 = TRUE
      )
    )
  ))
}

# The sheet names of the .xlsx file `file`, from xl/workbook.xml, or NULL
# when the file is not a zip file with xl/workbook.xml.
xlsx_sheets <- function(file) {
  tmp <- tempfile("pbn_xlsx")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  got <- suppressWarnings(tryCatch(
    utils::unzip(file, files = "xl/workbook.xml", exdir = tmp),
    error = function(e) return(character(0))
  ))
  xml_file <- file.path(tmp, "xl", "workbook.xml")
  if (length(got) == 0 || !file.exists(xml_file)) {
    return(NULL)
  }
  xml <- paste(
    readLines(xml_file, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  tags <- regmatches(
    xml,
    gregexpr("<([A-Za-z0-9]+:)?sheet\\s[^>]*>", xml, perl = TRUE)
  )[[1]]
  names <- sub("^.*\\sname=([\"'])(.*?)\\1.*$", "\\2", tags, perl = TRUE)
  return(xml_decode(names))
}

# The sheets of each tables.xlsx and data_behind_figures.xlsx of
# `outputs`, as a data.frame: `file`, `kind` and `sheet`. A file whose sheets
# R cannot read gives one row with the sheet NA.
display_sheets <- function(path, outputs) {
  kind <- display_kind(outputs$name)
  books <- outputs[kind %in% c("tables", "figure_data"), , drop = FALSE]
  rows <- lapply(seq_len(nrow(books)), function(i) {
    sheets <- xlsx_sheets(file.path(path, books$file[i]))
    if (is.null(sheets)) {
      sheets <- NA_character_
    }
    return(data.frame(
      file = rep(books$file[i], length(sheets)),
      folder = rep(books$folder[i], length(sheets)),
      kind = rep(display_kind(books$name[i]), length(sheets)),
      sheet = sheets
    ))
  })
  return(do.call(
    rbind,
    c(
      list(data.frame(
        file = character(0),
        folder = character(0),
        kind = character(0),
        sheet = character(0)
      )),
      rows
    )
  ))
}

display_workbook_rows <- function(path, outputs) {
  s <- display_sheets(path, outputs)
  unread <- s[is.na(s$sheet), , drop = FALSE]
  s <- s[!is.na(s$sheet), , drop = FALSE]
  wanted <- ifelse(
    s$kind == "tables",
    display_table_sheet_pattern,
    display_figure_sheet_pattern
  )
  good <- vapply(
    seq_len(nrow(s)),
    function(i) {
      return(
        grepl(wanted[i], s$sheet[i]) ||
          grepl(display_nonum_pattern, s$sheet[i])
      )
    },
    logical(1)
  )
  bad <- s[!good, , drop = FALSE]
  return(rbind(
    display_row(
      unread$file,
      paste0(
        unread$file,
        ": R cannot read its sheet names, because it is not a zip file ",
        "with xl/workbook.xml",
        recycle0 = TRUE
      )
    ),
    display_row(
      bad$file,
      paste0(
        bad$file,
        ": the sheet \"",
        bad$sheet,
        "\" MUST be named ",
        ifelse(bad$kind == "tables", "tab_N or tab_sN", "fig_N_<name>"),
        " or nonum_<name>",
        recycle0 = TRUE
      )
    )
  ))
}

# Each numbered figure file other than an external item has a sheet of its
# number in data_behind_figures.xlsx of its folder. Each sheet fig_N_ has a
# figure file of its number in that folder.
display_figure_data_rows <- function(path, outputs) {
  kind <- display_kind(outputs$name)
  s <- display_sheets(path, outputs)
  s <- s[s$kind == "figure_data" & !is.na(s$sheet), , drop = FALSE]
  s <- s[grepl(display_figure_sheet_pattern, s$sheet), , drop = FALSE]
  sheet_key <- paste(s$folder, display_key(s$sheet))
  figures <- outputs[kind == "figure", , drop = FALSE]
  key <- paste(figures$folder, display_key(figures$name))
  first <- !duplicated(key) & !key %in% sheet_key
  missing <- figures[first, , drop = FALSE]
  with_book <- outputs$folder[kind == "figure_data"]
  where <- ifelse(
    missing$folder %in% with_book,
    paste0(": data_behind_figures.xlsx of ", missing$folder, " has no sheet "),
    paste0(
      ": ",
      missing$folder,
      " has no data_behind_figures.xlsx with a sheet "
    )
  )
  is_fig <- kind %in% c("figure", "external") & startsWith(outputs$name, "fig_")
  file_key <- paste(outputs$folder[is_fig], display_key(outputs$name[is_fig]))
  orphan <- s[!duplicated(sheet_key) & !sheet_key %in% file_key, , drop = FALSE]
  return(rbind(
    display_row(
      missing$file,
      paste0(
        missing$file,
        where,
        display_key(missing$name),
        "_<name> with the data of the figure",
        recycle0 = TRUE
      )
    ),
    display_row(
      orphan$file,
      paste0(
        orphan$file,
        ": the sheet ",
        orphan$sheet,
        " has no figure file ",
        display_key(orphan$sheet),
        "_<name> in ",
        orphan$folder,
        recycle0 = TRUE
      )
    )
  ))
}

# The manuscript of the newest manuscript version NN among the files
# `files`: each manuscript/manual/vNN_<INITIALS>.docx, then each
# vNN_<INITIALS>_si.docx, each in name order, then each
# manuscript/built/vNN/*.md but the checklist files. character(0) when there
# is none.
display_manuscript_files <- function(files) {
  manual <- files[
    dirname(files) == "manuscript/manual" &
      grepl(manual_pattern, basename(files))
  ]
  main <- !grepl("_[a-z0-9]+\\.docx$", basename(manual))
  manual <- manual[main | grepl("_si\\.docx$", basename(manual))]
  built <- files[
    grepl("^manuscript/built/v[0-9]{2}/[^/]+\\.md$", files) &
      !grepl(checklist_file_pattern, basename(files), ignore.case = TRUE)
  ]
  n_manual <- version_number(basename(manual))
  n_built <- version_number(sub("^manuscript/built/", "", built))
  if (length(c(n_manual, n_built)) == 0) {
    return(character(0))
  }
  newest <- max(c(n_manual, n_built))
  manual <- manual[n_manual == newest]
  manual <- manual[order(
    grepl("_si\\.docx$", manual),
    manual,
    method = "radix"
  )]
  return(c(manual, sort(built[n_built == newest], method = "radix")))
}

# The paragraphs of the manuscript file `file` in `path`, or NULL when R
# cannot read it. A .docx paragraph is a paragraph of docx_lines(). An .md
# paragraph is a run of lines that are not blank.
display_paragraphs <- function(path, file) {
  full <- file.path(path, file)
  if (grepl("\\.docx$", file)) {
    lines <- tryCatch(docx_lines(full), error = function(e) return(NULL))
    return(lines$text)
  }
  text <- md_lines(full)$text
  blank <- !nzchar(trimws(text))
  id <- cumsum(blank)[!blank]
  return(unname(vapply(
    split(text[!blank], id),
    paste,
    character(1),
    collapse = " "
  )))
}

# The keys of the citations of the text `x`, in their order. A list gives
# each number, and a range gives each number from its first to its last.
display_cited_keys <- function(x) {
  hits <- regmatches(x, gregexpr(display_cite_pattern, x, perl = TRUE))[[1]]
  return(unlist(lapply(hits, display_hit_keys)))
}

display_hit_keys <- function(hit) {
  supplementary <- grepl("^[Ss]upplementary\\s", hit)
  hit <- sub("^[Ss]upplementary\\s+", "", hit)
  series <- if (grepl("^[Tt]", hit)) "tab_" else "fig_"
  list_part <- sub("^[A-Za-z]+\\.?\\s+", "", hit, perl = TRUE)
  tokens <- regmatches(
    list_part,
    gregexpr("S?[0-9]+|[-\u2013\u2014]", list_part, perl = TRUE)
  )[[1]]
  dash <- grepl("^[-\u2013\u2014]$", tokens)
  out <- character(0)
  i <- 1
  while (i <= length(tokens)) {
    keys <- if (dash[i]) character(0) else tokens[i]
    if (!dash[i] && i + 2 <= length(tokens) && dash[i + 1] && !dash[i + 2]) {
      keys <- display_range(tokens[i], tokens[i + 2])
      i <- i + 2
    }
    out <- c(out, keys)
    i <- i + 1
  }
  supplement <- supplementary | startsWith(out, "S")
  return(paste0(
    series,
    ifelse(supplement, "s", ""),
    as.integer(sub("^S", "", out))
  ))
}

# The numbers of the range from `a` to `b`, such as S1 to S3. The S of `a`
# holds for the range. A range that goes down is its two ends.
display_range <- function(a, b) {
  s <- if (startsWith(a, "S")) "S" else ""
  from <- as.integer(sub("^S", "", a))
  to <- as.integer(sub("^S", "", b))
  if (to <= from) {
    return(c(a, paste0(s, to)))
  }
  return(paste0(s, seq(from, to)))
}

# The citations and the captions of the manuscript files `manuscript`:
# `cites`, a data.frame of `file` and `key`, without the key of the caption
# of its own paragraph; `captions`, a data.frame of `file` and `key`; and
# `rows`, a finding for each file that R cannot read.
display_manuscript_text <- function(path, manuscript) {
  cites <- list()
  captions <- list()
  oxford <- list()
  unread <- character(0)
  for (f in manuscript) {
    paragraphs <- display_paragraphs(path, f)
    if (is.null(paragraphs)) {
      unread <- c(unread, f)
      next
    }
    for (p in paragraphs) {
      cap <- display_caption_key(p)
      body <- if (is.na(cap)) {
        p
      } else {
        sub(display_caption_pattern, "", p, perl = TRUE)
      }
      keys <- display_cited_keys(body)
      keys <- keys[is.na(cap) | keys != cap]
      oxford[[length(oxford) + 1]] <- display_oxford_rows(f, body)
      cites[[length(cites) + 1]] <- data.frame(
        file = rep(f, length(keys)),
        key = keys
      )
      if (!is.na(cap)) {
        captions[[length(captions) + 1]] <- data.frame(file = f, key = cap)
      }
    }
  }
  empty <- data.frame(file = character(0), key = character(0))
  return(list(
    cites = do.call(rbind, c(list(empty), cites)),
    captions = do.call(rbind, c(list(empty), captions)),
    rows = rbind(
      display_row(
        unread,
        paste0(
          unread,
          ": R cannot read its citations, because it is not a .docx file ",
          "with word/document.xml",
          recycle0 = TRUE
        )
      ),
      bind_rows(oxford)
    )
  ))
}

# A finding of the file `file` for each list of the text `x` without the
# Oxford comma, such as "Figures 1, 2 and 3". Such a list has a comma, and
# no comma before its last "and" or "&".
display_oxford_rows <- function(file, x) {
  hits <- regmatches(x, gregexpr(display_cite_pattern, x, perl = TRUE))[[1]]
  bad <- hits[grepl(",", hits) & !grepl(",\\s*(and\\s|&)", hits, perl = TRUE)]
  fixed <- sub(
    "\\s*(and|&)\\s*(?=[^,]*$)",
    ", \\1 ",
    bad,
    perl = TRUE
  )
  return(display_row(
    rep(file, length(bad)),
    paste0(
      file,
      ": the list \"",
      bad,
      "\" MUST use the Oxford comma: \"",
      fixed,
      "\"",
      recycle0 = TRUE
    )
  ))
}

# The key of the caption that starts the paragraph `p`, or NA.
display_caption_key <- function(p) {
  m <- regmatches(p, regexec(display_caption_pattern, p, perl = TRUE))[[1]]
  if (length(m) == 0) {
    return(NA_character_)
  }
  return(display_hit_keys(trimws(paste(m[2], m[3], m[4]))))
}

# "Figure 1", "Figure S1", "Table 1" or "Table S1" for each key `key`.
display_label <- function(key) {
  word <- ifelse(startsWith(key, "fig_"), "Figure ", "Table ")
  number <- sub("^(fig|tab)_", "", key)
  return(paste0(word, toupper(number)))
}

# In each series, the first citations come in the order 1, 2, 3, ... with
# no gap.
display_order_rows <- function(cites) {
  first <- cites[!duplicated(cites$key), , drop = FALSE]
  series <- sub("[0-9]+$", "", first$key)
  rows <- lapply(unique(series), function(s) {
    these <- first[series == s, , drop = FALSE]
    n <- as.integer(sub("^.*_s?", "", these$key))
    wrong <- which(n != seq_along(n))
    if (length(wrong) == 0) {
      return(display_row(character(0), character(0)))
    }
    return(display_row(
      these$file[wrong[1]],
      paste0(
        these$file[wrong[1]],
        ": the first citations of ",
        display_series_name(s),
        " come in the order ",
        paste(display_label(these$key), collapse = ", "),
        ". They MUST come in the order 1, 2, 3, ... with no gap, so ",
        display_label(these$key[wrong[1]]),
        " MUST be ",
        display_label(paste0(s, wrong[1]))
      )
    ))
  })
  return(bind_rows(rows))
}

display_series_name <- function(s) {
  names <- c(
    fig_ = "the figures",
    fig_s = "the supplementary figures",
    tab_ = "the tables",
    tab_s = "the supplementary tables"
  )
  return(unname(names[s]))
}

# Each item with a caption is cited outside its own caption.
display_caption_rows <- function(cites, captions) {
  bad <- captions[!captions$key %in% cites$key, , drop = FALSE]
  bad <- bad[!duplicated(bad$key), , drop = FALSE]
  return(display_row(
    bad$file,
    paste0(
      bad$file,
      ": ",
      display_label(bad$key),
      " has a caption, but the text cites it nowhere outside its caption",
      recycle0 = TRUE
    )
  ))
}

# The numbered items of the output folder `folder`: the figure files, the
# sheets tab_ of tables.xlsx and the external items. A data.frame of `file`
# and `key`.
display_output_items <- function(path, outputs, folder) {
  real <- outputs[outputs$folder == folder, , drop = FALSE]
  kind <- display_kind(real$name)
  numbered <- real[kind %in% c("figure", "external"), , drop = FALSE]
  s <- display_sheets(path, real)
  s <- s[
    s$kind == "tables" & grepl(display_table_sheet_pattern, s$sheet),
    ,
    drop = FALSE
  ]
  return(rbind(
    data.frame(file = numbered$file, key = display_key(numbered$name)),
    data.frame(file = s$file, key = s$sheet)
  ))
}

# Each numbered item of the output folder `folder` is an item of the
# manuscript, and each item of the manuscript is a numbered item of that
# folder. `main` is the first file of the manuscript.
display_match_rows <- function(path, outputs, text, main, folder) {
  items <- display_output_items(path, outputs, folder)
  items <- items[!duplicated(items$key), , drop = FALSE]
  wanted <- rbind(text$cites, text$captions)
  wanted <- wanted[!duplicated(wanted$key), , drop = FALSE]
  extra <- items[!items$key %in% wanted$key, , drop = FALSE]
  lost <- wanted[!wanted$key %in% items$key, , drop = FALSE]
  return(rbind(
    display_row(
      extra$file,
      paste0(
        extra$file,
        ": ",
        display_label(extra$key),
        " of the output is not in the manuscript ",
        main,
        recycle0 = TRUE
      )
    ),
    display_row(
      lost$file,
      paste0(
        lost$file,
        ": ",
        display_label(lost$key),
        " of the manuscript has no ",
        display_item_names(lost$key),
        " in ",
        folder,
        recycle0 = TRUE
      )
    )
  ))
}

# The output names that hold the item of each key `key`.
display_item_names <- function(key) {
  return(ifelse(
    startsWith(key, "fig_"),
    paste0("figure file ", key, "_<name>"),
    paste0(
      "sheet ",
      key,
      " of tables.xlsx or file ",
      key,
      "_<name>_external.txt"
    )
  ))
}
