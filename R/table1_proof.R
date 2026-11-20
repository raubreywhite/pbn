# The proof of a Table 1 style. A style is proven when table1() reproduces
# the format of each real published table of its reference folder, cell by
# cell, from fake data. A label cell matches its string, a value cell matches
# its shape, and every cell that differs is a listed exception. ?table1,
# section Styles, describes the folder and its files.

# The files of one reference table k, and the extension of each.
proof_kinds <- c(
  reference = "csv",
  source = "yml",
  fake = "csv",
  vars = "csv",
  layout = "yml"
)

proof_file_pattern <- "^(reference|source|fake|vars|layout)_([1-9][0-9]*)\\.(csv|yml)$"

exception_rules <- c(
  "guideline",
  "other_reference",
  "not_computable",
  "published_error",
  "author_decision"
)

exception_fields <- c(
  "reference",
  "row",
  "column",
  "printed",
  "style",
  "rule",
  "source",
  "justification"
)

source_fields <- c(
  "authors",
  "title",
  "journal",
  "year",
  "volume",
  "pages",
  "doi",
  "table",
  "transcribed"
)

# A citation of one cell: reference_<k>, row <i>, column "<printed header>".
cell_citation <- "reference_([0-9]+),? row ([0-9]+),? column \"([^\"]*)\""

# A decision of a named person: decision: <Name>, <YYYY-MM-DD>: <reason>.
decision_citation <- "^decision: ([^,:]*[[:alpha:]][^,:]*), ([0-9-]+):(.*)$"
decision_form <- "decision: <Name>, <YYYY-MM-DD>: <reason>"

# The failures of the proofs of the installed styles `styles`. Each failure
# starts with the name of its style. character(0) means every style is
# proven.
style_proofs <- function(styles = table1_styles()) {
  out <- lapply(styles, function(s) {
    return(style_proof(file.path(table1_style_dir(), paste0(s, ".yml"))))
  })
  return(as.character(unlist(out)))
}

# The failures of the proof of the style file `file`, against the folder of
# the same name beside it. Each failure starts with the name of the style.
style_proof <- function(file) {
  failures <- proof_failures(file, sub("\\.yml$", "", file))
  if (length(failures) == 0) {
    return(character(0))
  }
  return(paste0(sub("\\.yml$", "", basename(file)), ": ", failures))
}

# An error whose message is one failure of a proof.
proof_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

# The message of the error of `expr`, or `value` of `expr` when it has none.
proof_try <- function(expr, prefix) {
  return(tryCatch(expr, error = function(e) {
    return(structure(
      paste0(prefix, conditionMessage(e)),
      class = "proof_failure"
    ))
  }))
}

is_proof_failure <- function(x) {
  return(inherits(x, "proof_failure"))
}

proof_failures <- function(file, dir) {
  if (!dir.exists(dir)) {
    return(paste0("there is no reference folder ", basename(dir), "/."))
  }
  found <- proof_files(dir)
  inputs <- lapply(found$complete, function(k) {
    return(proof_try(read_proof_inputs(dir, k), paste0("reference_", k, ": ")))
  })
  bad <- vapply(inputs, is_proof_failure, logical(1))
  failures <- c(found$failures, as.character(unlist(inputs[bad])))
  inputs <- inputs[!bad]
  names(inputs) <- paste0(
    "reference_",
    vapply(inputs, function(x) return(x$k), integer(1)),
    recycle0 = TRUE
  )
  style <- proof_try(read_table1_style(file), "")
  if (is_proof_failure(style)) {
    return(c(failures, as.character(style)))
  }
  cited <- citable_references(dir, found$complete)
  failures <- c(failures, lint_style_sources(style, cited))
  exceptions <- read_exceptions(dir)
  failures <- c(failures, exceptions$failures)
  failures <- c(failures, lint_exceptions(exceptions$entries, cited))
  cells <- lapply(inputs, function(x) {
    return(proof_try(
      proof_cells(x, file),
      paste0("reference_", x$k, ": ")
    ))
  })
  bad <- vapply(cells, is_proof_failure, logical(1))
  failures <- c(failures, as.character(unlist(cells[bad])))
  return(c(
    failures,
    match_exceptions(
      do.call(rbind, c(list(empty_cells()), cells[!bad])),
      exceptions$entries,
      compared = names(cells)[!bad],
      known = paste0("reference_", found$all, recycle0 = TRUE)
    )
  ))
}

# The reference numbers in the folder `dir`. `all` holds each number that
# any file names, and `complete` each number that has all five files.
proof_files <- function(dir) {
  files <- list.files(dir)
  ok <- grepl(proof_file_pattern, files)
  kind <- sub(proof_file_pattern, "\\1", files[ok])
  ok[ok] <- proof_kinds[kind] == sub(proof_file_pattern, "\\3", files[ok])
  unknown <- setdiff(files[!ok], "exceptions.yml")
  failures <- paste0(
    "the reference folder holds the file ",
    unknown,
    ", which is not a file of a proof.",
    recycle0 = TRUE
  )
  all <- sort(unique(as.integer(sub(proof_file_pattern, "\\2", files[ok]))))
  if (length(all) == 0) {
    failures <- c(failures, "the reference folder holds no reference_1.csv.")
  }
  needed <- unlist(lapply(all, function(k) {
    return(paste0(names(proof_kinds), "_", k, ".", proof_kinds))
  }))
  absent <- setdiff(c(needed, "exceptions.yml"), files)
  failures <- c(
    failures,
    paste0("the reference folder lacks the file ", absent, ".", recycle0 = TRUE)
  )
  complete <- all[vapply(
    all,
    function(k) return(!any(grepl(paste0("_", k, "\\."), absent))),
    logical(1)
  )]
  return(list(failures = failures, all = all, complete = complete))
}

# A CSV file of printed cells: every cell is a string, as written.
read_cells <- function(file) {
  return(utils::read.csv(
    file,
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(0),
    strip.white = FALSE,
    encoding = "UTF-8"
  ))
}

# A file that `reader` reads, or an error that names the file.
read_named <- function(file, reader) {
  return(tryCatch(reader(file), error = function(e) {
    return(proof_stop(
      "pbn cannot read ",
      basename(file),
      ": ",
      conditionMessage(e)
    ))
  }))
}

# The five files of the reference k, each checked.
read_proof_inputs <- function(dir, k) {
  path <- function(kind) {
    return(file.path(dir, paste0(kind, "_", k, ".", proof_kinds[[kind]])))
  }
  out <- list(
    k = k,
    reference = read_named(path("reference"), read_cells),
    source = read_named(path("source"), read_yaml_file),
    layout = read_named(path("layout"), read_yaml_file),
    vars = read_named(path("vars"), read_cells),
    fake = read_named(path("fake"), function(f) {
      return(utils::read.csv(
        f,
        check.names = FALSE,
        na.strings = c("", "NA"),
        stringsAsFactors = FALSE,
        encoding = "UTF-8"
      ))
    })
  )
  check_source_entry(out$source, k)
  check_layout(out$layout, out$reference, k)
  check_vars(out$vars, out$layout, k)
  return(out)
}

# TRUE when `x` holds text that is not empty.
has_text <- function(x) {
  return(
    !is.null(x) &&
      length(x) > 0 &&
      nzchar(trimws(paste(unlist(x), collapse = "")))
  )
}

is_text <- function(x) {
  return(is.character(x) && length(x) == 1 && !is.na(x))
}

check_source_entry <- function(x, k) {
  file <- paste0("source_", k, ".yml")
  if (!is.list(x) || is.null(names(x))) {
    proof_stop(file, " must be a YAML map.")
  }
  for (name in source_fields) {
    if (!has_text(x[[name]])) {
      proof_stop(file, " lacks the field ", name, ", or it is empty.")
    }
  }
  check_source_people(x, file)
  return(invisible(NULL))
}

# The authors of a source are maps with the key family, and transcribed
# names the file, the person and the date.
check_source_people <- function(x, file) {
  first <- x$authors[[1]]
  if (!is.list(x$authors) || !is.list(first) || !has_text(first$family)) {
    proof_stop(file, " must list the authors as maps with the key family.")
  }
  for (name in c("file", "by", "date")) {
    if (!has_text(x$transcribed[[name]])) {
      proof_stop(
        file,
        " lacks the field transcribed.",
        name,
        ", or it is empty."
      )
    }
  }
  return(invisible(NULL))
}

# The layout names one printed column of the reference for each column but
# the first, in order, and one printed row for each row, in order.
check_layout <- function(layout, reference, k) {
  file <- paste0("layout_", k, ".yml")
  if (!is.list(layout) || !all(c("columns", "rows") %in% names(layout))) {
    proof_stop(file, " must be a YAML map with the keys columns and rows.")
  }
  cols <- vapply(layout$columns, check_layout_column, character(1), file)
  printed <- names(reference)[-1]
  if (!identical(enc2utf8(unname(cols)), enc2utf8(printed))) {
    proof_stop(
      file,
      " lists the columns ",
      quoted_list(cols),
      ", but reference_",
      k,
      ".csv has the columns ",
      quoted_list(printed),
      "."
    )
  }
  if (length(layout$rows) != nrow(reference)) {
    proof_stop(
      file,
      " has ",
      length(layout$rows),
      " rows, but reference_",
      k,
      ".csv has ",
      nrow(reference),
      "."
    )
  }
  for (i in seq_along(layout$rows)) {
    check_layout_row(layout$rows[[i]], i, file)
  }
  return(invisible(NULL))
}

# The strings of `x`, each in double quotes, separated by commas.
quoted_list <- function(x) {
  return(paste(encodeString(as.character(x), quote = "\""), collapse = ", "))
}

check_layout_column <- function(x, file) {
  ok <- is.list(x) &&
    is_text(x$column) &&
    is_text(x$cell) &&
    (identical(x$cell, "p") || (identical(x$cell, "value") && is_text(x$grp)))
  if (!ok) {
    proof_stop(
      file,
      " has a column that is not a map of column and cell p, or of column, ",
      "cell value and grp."
    )
  }
  return(x$column)
}

check_layout_row <- function(x, i, file) {
  kinds <- c("value", "heading", "missing", "section", "none")
  if (!is.list(x) || !is_text(x$row) || !x$row %in% kinds) {
    proof_stop(
      file,
      " row ",
      i,
      " must be a map with a row of value, heading, missing, section or none."
    )
  }
  if (x$row %in% c("value", "heading", "missing") && !is_text(x$var)) {
    proof_stop(file, " row ", i, " needs a var.")
  }
  check_layout_keys(x, i, file)
  return(invisible(NULL))
}

# A layout row has only the keys var, row and level, so no label comes from
# the layout.
check_layout_keys <- function(x, i, file) {
  extra <- setdiff(names(x), c("var", "row", "level"))
  if (length(extra) > 0) {
    proof_stop(
      file,
      " row ",
      i,
      " has the key ",
      extra[1],
      ", and a layout row has only the keys var, row and level."
    )
  }
  return(invisible(NULL))
}

# vars: one row of type "by", the variables of table1(), and the printed
# label of the first row of each variable.
check_vars <- function(vars, layout, k) {
  file <- paste0("vars_", k, ".csv")
  if (!all(c("var", "type", "label", "levels", "section") %in% names(vars))) {
    proof_stop(
      file,
      " must have the columns var, type, label, levels and section."
    )
  }
  if (sum(vars$type == "by") != 1) {
    proof_stop(file, " must have one row of type by.")
  }
  bad <- vars$var[!vars$type %in% c("by", "bin", "cont", "cat")]
  if (length(bad) > 0) {
    proof_stop(
      file,
      " gives ",
      bad[1],
      " a type other than by, bin, cont or cat."
    )
  }
  row_vars <- vapply(
    layout$rows,
    function(r) return(if (is_text(r$var)) r$var else NA_character_),
    character(1)
  )
  shown <- vars$var[vars$type != "by"]
  absent <- setdiff(shown, row_vars)
  if (length(absent) > 0) {
    proof_stop("layout_", k, ".yml has no row of the variable ", absent[1], ".")
  }
  unknown <- setdiff(row_vars[!is.na(row_vars)], shown)
  if (length(unknown) > 0) {
    proof_stop(
      "layout_",
      k,
      ".yml names the variable ",
      unknown[1],
      ", which ",
      file,
      " does not list."
    )
  }
  return(invisible(NULL))
}

# The levels of the column `var` of the fake data, from `levels` of vars. A
# value that `levels` does not list is an error.
fake_levels <- function(x, var, levels, k) {
  if (!nzchar(levels)) {
    return(x)
  }
  levels <- strsplit(levels, "|", fixed = TRUE)[[1]]
  x <- as.character(x)
  extra <- setdiff(x[!is.na(x)], levels)
  if (length(extra) > 0) {
    proof_stop(
      "fake_",
      k,
      ".csv has values of ",
      var,
      " that vars_",
      k,
      ".csv does not list: ",
      paste(extra, collapse = ", "),
      "."
    )
  }
  return(factor(x, levels = levels))
}

# The cells of one reference: the printed string and the string of the style
# for each row and each column but the first.
proof_cells <- function(x, file) {
  vars <- x$vars
  d <- x$fake
  for (i in which(nzchar(vars$levels))) {
    if (vars$var[i] %in% names(d)) {
      d[[vars$var[i]]] <- fake_levels(
        d[[vars$var[i]]],
        vars$var[i],
        vars$levels[i],
        x$k
      )
    }
  }
  keep <- vars$type != "by"
  t1 <- tryCatch(
    table1(
      d,
      by = vars$var[!keep],
      vars = data.table::data.table(
        var = vars$var[keep],
        type = vars$type[keep],
        label = vars$label[keep],
        section = vars$section[keep]
      ),
      style = file
    ),
    error = function(e) proof_stop("table1() stops: ", conditionMessage(e))
  )
  ref <- x$reference
  cells <- expand.grid(
    row = seq_len(nrow(ref)),
    j = seq_len(ncol(ref)) - 1L,
    KEEP.OUT.ATTRS = FALSE
  )
  cells$style <- vapply(
    seq_len(nrow(cells)),
    function(m) {
      row <- x$layout$rows[[cells$row[m]]]
      if (cells$j[m] == 0 && row$row == "section") {
        return(section_cell(t1, x$layout, cells$row[m]))
      }
      if (cells$j[m] == 0) {
        column <- list(column = names(ref)[1], cell = "label")
        return(style_cell(t1, row, column, cells$row[m], x$k))
      }
      return(style_cell(
        t1,
        row,
        x$layout$columns[[cells$j[m]]],
        cells$row[m],
        x$k
      ))
    },
    character(1)
  )
  return(data.frame(
    reference = paste0("reference_", x$k),
    row = cells$row,
    label = ref[[1]][cells$row],
    column = names(ref)[cells$j + 1],
    is_label = cells$j == 0,
    kind = vapply(
      cells$row,
      function(i) return(x$layout$rows[[i]]$row),
      character(1)
    ),
    printed = vapply(
      seq_len(nrow(cells)),
      function(m) return(ref[[cells$j[m] + 1]][cells$row[m]]),
      character(1)
    ),
    style = cells$style,
    stringsAsFactors = FALSE
  ))
}

# The cells of no reference, with the columns of proof_cells().
empty_cells <- function() {
  return(data.frame(
    reference = character(0),
    row = integer(0),
    label = character(0),
    column = character(0),
    is_label = logical(0),
    kind = character(0),
    printed = character(0),
    style = character(0),
    stringsAsFactors = FALSE
  ))
}

# The string of the style for one printed row and column. A row of "none"
# and a cell that is NA give the empty string.
style_cell <- function(t1, row, column, i, k) {
  if (row$row %in% c("none", "section")) {
    return("")
  }
  level <- if (is.null(row$level)) {
    NA_character_
  } else {
    as.character(unclass(row$level))
  }
  same_level <- if (is.na(level)) is.na(t1$level) else t1$level %in% level
  grp <- if (column$cell %in% c("p", "label")) "All" else column$grp
  hit <- which(
    t1$var == row$var & t1$row == row$row & same_level & t1$grp == grp
  )
  if (length(hit) != 1) {
    proof_stop(
      "layout_",
      k,
      ".yml row ",
      i,
      " and column ",
      quoted_list(column$column),
      " name the variable ",
      row$var,
      ", the row ",
      row$row,
      ", the level ",
      level,
      " and the group ",
      grp,
      ", and table1() gives ",
      length(hit),
      " such rows."
    )
  }
  value <- switch(
    column$cell,
    p = t1$p_pretty[hit],
    label = t1$label[hit],
    t1$value_pretty[hit]
  )
  return(if (is.na(value)) "" else value)
}

# The label of the printed section row i of the layout: the label of the
# section row that table1() puts right before the first row of the next
# variable of the layout. It is "" when table1() puts no section row there.
section_cell <- function(t1, layout, i) {
  after <- unlist(lapply(layout$rows[-seq_len(i)], function(r) return(r$var)))
  if (length(after) == 0) {
    return("")
  }
  m <- match(after[1], t1$var)
  if (is.na(m) || m == 1 || !identical(t1$row[m - 1], "section")) {
    return("")
  }
  label <- t1$label[m - 1]
  return(if (is.na(label)) "" else label)
}

# The sources of a style ------------------------------------------------------

# TRUE when `src` cites a cell of a reference in `inputs`, other than those
# of `exclude`.
cites_reference_cell <- function(src, inputs, exclude = character(0)) {
  found <- regmatches(src, gregexpr(cell_citation, src))[[1]]
  return(any(vapply(
    found,
    function(m) return(is_cited_cell(m, inputs, exclude)),
    logical(1)
  )))
}

# TRUE when the citation `m` names a row and a column of a reference in
# `inputs` that `exclude` does not name.
is_cited_cell <- function(m, inputs, exclude) {
  ref <- paste0("reference_", sub(cell_citation, "\\1", m))
  row <- as.integer(sub(cell_citation, "\\2", m))
  table <- inputs[[ref]]$reference
  if (ref %in% exclude || is.null(table)) {
    return(FALSE)
  }
  return(
    row >= 1 &&
      row <= nrow(table) &&
      sub(cell_citation, "\\3", m) %in% names(table)
  )
}

# TRUE when `src` names the family name of the first author and the table of
# the source of a reference in `inputs`.
cites_author_table <- function(src, inputs) {
  for (x in inputs) {
    family <- first_family(x$source)
    if (is.null(family) || !has_text(x$source$table)) {
      next
    }
    table <- paste0("\\bTable ", x$source$table, "\\b")
    if (grepl(family, src, fixed = TRUE) && grepl(table, src)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# The family name of the first author of the source `x`, or NULL.
first_family <- function(x) {
  family <- tryCatch(x$authors[[1]]$family, error = function(e) return(NULL))
  if (!is_text(family) || !nzchar(family)) {
    return(NULL)
  }
  return(family)
}

# The reference table and the source of each reference k of `ks` that pbn
# can read, by name. A citation can name them even when a later check of the
# reference fails.
citable_references <- function(dir, ks) {
  out <- lapply(ks, function(k) {
    return(tryCatch(
      list(
        reference = read_cells(file.path(dir, paste0("reference_", k, ".csv"))),
        source = read_yaml_file(file.path(dir, paste0("source_", k, ".yml")))
      ),
      error = function(e) return(NULL)
    ))
  })
  names(out) <- paste0("reference_", ks, recycle0 = TRUE)
  return(Filter(Negate(is.null), out))
}

# TRUE when `src` names a guide or a guideline and a page.
cites_guideline_page <- function(src) {
  return(
    grepl("guide", src, ignore.case = TRUE) &&
      grepl("\\b(pp?\\.|page) ?[0-9]+", src, ignore.case = TRUE)
  )
}

# TRUE when `src` is a decision in the form decision_form: a name with a
# letter, a real date, and a reason of three words or more. The word count
# alone rejects an empty reason.
is_decision <- function(src) {
  src <- trimws(src)
  if (!grepl(decision_citation, src)) {
    return(FALSE)
  }
  date <- sub(decision_citation, "\\2", src)
  reason <- trimws(sub(decision_citation, "\\3", src))
  real <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date) &&
    identical(format(as.Date(date, optional = TRUE)), date)
  return(real && length(strsplit(reason, "\\s+")[[1]]) >= 3)
}

# The failure of the source `src` of the field `field`, or NULL.
source_problem <- function(src, field, inputs) {
  head <- paste0("the source of the field ", field)
  if (grepl("editorial", src, ignore.case = TRUE)) {
    return(paste0(head, " says editorial."))
  }
  if (grepl("^decision:", trimws(src))) {
    if (is_decision(src)) {
      return(NULL)
    }
    return(paste0(
      head,
      " is a decision that is not in the form ",
      decision_form,
      "."
    ))
  }
  if (
    cites_reference_cell(src, inputs) ||
      cites_author_table(src, inputs) ||
      cites_guideline_page(src)
  ) {
    return(NULL)
  }
  return(paste0(
    head,
    " cites no reference cell, no author and table, no guideline page, and ",
    "no decision."
  ))
}

# The failures of the sources of every field of `style`, the footnotes too.
lint_style_sources <- function(style, inputs) {
  out <- character(0)
  for (section in intersect(names(style_schema), names(style))) {
    for (name in intersect(
      names(style_schema[[section]]),
      names(style[[section]])
    )) {
      out <- c(
        out,
        source_problem(
          style[[section]][[name]]$source,
          paste0(section, ".", name),
          inputs
        )
      )
    }
  }
  for (i in seq_along(style$footnotes)) {
    out <- c(
      out,
      source_problem(
        style$footnotes[[i]]$source,
        paste0("footnotes[", i, "]"),
        inputs
      )
    )
  }
  return(out)
}

# The exceptions ----------------------------------------------------------------

# The entries of exceptions.yml, and the failures of reading it.
read_exceptions <- function(dir) {
  file <- file.path(dir, "exceptions.yml")
  if (!file.exists(file)) {
    return(list(entries = list(), failures = character(0)))
  }
  x <- tryCatch(read_yaml_file(file), error = function(e) {
    return(structure(conditionMessage(e), class = "proof_failure"))
  })
  if (is_proof_failure(x)) {
    return(list(
      entries = list(),
      failures = paste0("pbn cannot read exceptions.yml: ", x)
    ))
  }
  if (
    !is.list(x) || !identical(names(x), "exceptions") || !is.list(x$exceptions)
  ) {
    return(list(
      entries = list(),
      failures = "exceptions.yml must be a YAML map with the one key exceptions, a list."
    ))
  }
  return(list(entries = x$exceptions, failures = character(0)))
}

# The failure of the key fields of the exception `x`, entry i, or NULL. The
# key fields name the cell and its two strings.
exception_key_problem <- function(x, i) {
  head <- paste0("exceptions.yml entry ", i, " ")
  if (!is.list(x) || is.null(names(x))) {
    return(paste0(head, "must be a YAML map."))
  }
  absent <- setdiff(exception_fields, names(x))
  if (length(absent) > 0) {
    return(paste0(head, "lacks the field ", absent[1], "."))
  }
  unknown <- setdiff(names(x), exception_fields)
  if (length(unknown) > 0) {
    return(paste0(head, "has the unknown field ", unknown[1], "."))
  }
  ok <- c(
    is_text(x$reference) && grepl("^reference_[1-9][0-9]*$", x$reference),
    is_row_number(x$row),
    vapply(x[c("column", "printed", "style")], is_text, logical(1))
  )
  if (!all(ok)) {
    return(paste0(
      head,
      "needs reference as reference_<k>, row as a whole number, and column, ",
      "printed and style as quoted strings."
    ))
  }
  return(NULL)
}

# TRUE when `x` is one whole number of 1 or more.
is_row_number <- function(x) {
  return(
    is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 1 && x == round(x)
  )
}

# The failure of the rule, the source and the justification of the
# exception `x`, entry i, or NULL.
exception_rule_problem <- function(x, i, inputs) {
  head <- paste0("exceptions.yml entry ", i, " ")
  if (!is_text(x$rule) || !x$rule %in% exception_rules) {
    return(paste0(
      head,
      "has a rule that is not one of ",
      paste(exception_rules, collapse = ", "),
      "."
    ))
  }
  for (name in c("source", "justification")) {
    if (!is_text(x[[name]]) || !nzchar(trimws(x[[name]]))) {
      return(paste0(head, "has an empty ", name, "."))
    }
  }
  return(exception_citation_problem(x, head, inputs))
}

# The source of an exception of any rule MUST NOT say editorial. A guideline
# exception cites a page, an other_reference exception cites a cell of
# another reference, and the source of an author_decision exception is a
# decision.
exception_citation_problem <- function(x, head, inputs) {
  if (grepl("editorial", x$source, ignore.case = TRUE)) {
    return(paste0(head, "has a source that says editorial."))
  }
  if (x$rule == "guideline" && !cites_guideline_page(x$source)) {
    return(paste0(
      head,
      "has the rule guideline, and its source cites no guideline page."
    ))
  }
  if (
    x$rule == "other_reference" &&
      !cites_reference_cell(x$source, inputs, exclude = x$reference)
  ) {
    return(paste0(
      head,
      "has the rule other_reference, and its source cites no cell of another ",
      "reference."
    ))
  }
  if (x$rule == "author_decision" && !is_decision(x$source)) {
    return(paste0(
      head,
      "has the rule author_decision, and its source is not in the form ",
      decision_form,
      "."
    ))
  }
  return(NULL)
}

lint_exceptions <- function(entries, inputs) {
  out <- character(0)
  for (i in seq_along(entries)) {
    problem <- exception_key_problem(entries[[i]], i)
    if (is.null(problem)) {
      problem <- exception_rule_problem(entries[[i]], i, inputs)
    }
    out <- c(out, problem)
  }
  return(out)
}

# The name of a cell in a failure.
cell_name <- function(reference, row, label, column) {
  return(paste0(
    reference,
    ", row ",
    row,
    " (",
    label,
    "), column ",
    quoted_list(column)
  ))
}

# The failures of the cells of `cells` against the exceptions `entries`.
# `compared` names the references whose cells are in `cells`, and `known`
# each reference of the folder. An exception of a cell that a compared
# reference does not have fails, and so does one of an unknown reference.
match_exceptions <- function(cells, entries, compared, known) {
  valid <- Filter(
    function(x) return(is.null(exception_key_problem(x, 0))),
    entries
  )
  key <- function(reference, row, column) {
    return(paste(reference, row, column, sep = "\r"))
  }
  ex_key <- vapply(
    valid,
    function(x) return(key(x$reference, x$row, x$column)),
    character(1)
  )
  out <- character(0)
  twice <- unique(ex_key[duplicated(ex_key)])
  for (k in twice) {
    x <- valid[[match(k, ex_key)]]
    out <- c(
      out,
      paste0(
        "exceptions.yml lists the cell ",
        x$reference,
        ", row ",
        x$row,
        ", column ",
        quoted_list(x$column),
        " more than once."
      )
    )
  }
  cell_key <- key(cells$reference, cells$row, cells$column)
  for (m in seq_len(nrow(cells))) {
    x <- if (cell_key[m] %in% ex_key) {
      valid[[match(cell_key[m], ex_key)]]
    } else {
      NULL
    }
    out <- c(out, cell_problem(cells[m, ], x))
  }
  ref <- vapply(valid, function(x) return(x$reference), character(1))
  orphan <- !ex_key %in% cell_key & (ref %in% compared | !ref %in% known)
  for (x in valid[orphan]) {
    out <- c(
      out,
      paste0(
        "exceptions.yml names ",
        x$reference,
        ", row ",
        x$row,
        ", column ",
        quoted_list(x$column),
        ", which is not a cell of ",
        x$reference,
        ".csv."
      )
    )
  }
  return(out)
}

#' The shape of a cell string
#'
#' The shape is the string with each number masked. The digits before a
#' decimal point `.` collapse to one `N`, and a thousands separator between
#' them stays as printed. Each digit after a decimal point becomes its own
#' `N`, so the shape keeps the number of decimals. Every other character stays
#' as printed, and the empty string stays empty.
#'
#' So `"397 (27.0%)"` and `"5 (2.8%)"` share the shape `"N (N.N%)"`.
#' `"57 (76%)"` gives `"N (N%)"`, `"<0.0001"` gives `"<N.NNNN"`, and
#' `"12,347 (61.7%)"` gives `"N,N (N.N%)"`.
#'
#' @param x A character vector of cell strings.
#' @return A character vector of the shapes, of the length of `x`.
#' @noRd
cell_shape <- function(x) {
  x <- gsub("(?:\\.|\\G(?!^))\\K[0-9]", "N", enc2utf8(x), perl = TRUE)
  return(gsub("[0-9]+", "N", x))
}

# The failure of one cell, or NULL. `x` is its exception, or NULL. A label
# cell compares the two strings, and a value cell compares their shapes. A
# cell that matches MUST NOT have an exception. An exception holds when its
# two strings match those of its cell in the same way.
cell_problem <- function(cell, x) {
  printed <- enc2utf8(cell$printed)
  styled <- enc2utf8(cell$style)
  key <- if (cell$is_label) identity else cell_shape
  shown <- function(s) {
    if (cell$is_label) {
      return(quoted_list(s))
    }
    return(paste0(quoted_list(s), " of the shape ", quoted_list(cell_shape(s))))
  }
  name <- cell_name(cell$reference, cell$row, cell$label, cell$column)
  if (key(printed) == key(styled)) {
    if (is.null(x)) {
      return(NULL)
    }
    return(paste0(
      name,
      ": exceptions.yml lists the cell, but it is printed ",
      shown(printed),
      " and the style gives ",
      shown(styled),
      ", so the cell MUST NOT have an exception."
    ))
  }
  if (is.null(x)) {
    return(paste0(
      name,
      ": printed ",
      shown(printed),
      ", the style gives ",
      shown(styled),
      ", and exceptions.yml does not list the cell."
    ))
  }
  if (
    key(enc2utf8(x$printed)) == key(printed) &&
      key(enc2utf8(x$style)) == key(styled)
  ) {
    return(none_rule_problem(cell, x, name))
  }
  return(paste0(
    name,
    ": exceptions.yml lists printed ",
    shown(x$printed),
    " and style ",
    shown(x$style),
    ", but the cell is printed ",
    shown(printed),
    " and the style gives ",
    shown(styled),
    "."
  ))
}

# A row of "none" has nothing that table1() computes, so an exception of its
# cell MUST have the rule not_computable.
none_rule_problem <- function(cell, x, name) {
  if (cell$kind != "none" || identical(x$rule, "not_computable")) {
    return(NULL)
  }
  return(paste0(
    name,
    ": the row is of none, so its exception MUST have the rule ",
    "not_computable."
  ))
}
