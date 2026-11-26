# The checklists of a project: the files guidelines/*.yml. Each file is one
# reporting guideline in the checklist schema of the methods repository,
# reporting-guidelines/SCHEMA.md. The rule guidelines_valid of the set
# house_2026_11_26 checks each file, and read_guidelines() gives the items of
# all of them.
#
# All list access uses [[ ]], never $. `$` matches a part of a name, so
# y$design gives y$designs when the file has no key design.

# The top-level keys of a checklist file, and the keys of one entry of items.
guideline_keys <- c(
  "id",
  "title",
  "version",
  "source",
  "citation",
  "designs",
  "design",
  "items"
)
guideline_item_keys <- c("item", "section", "topic", "kind", "designs", "text")

# The top-level keys that MAY be missing. A key of these that is there MUST
# have a value.
guideline_optional_keys <- c(
  "version",
  "source",
  "citation",
  "designs",
  "design"
)

# The values of the key kind of an entry.
guideline_kinds <- c("result", "prose", "author")

# The columns of read_guidelines(), in their order.
guideline_columns <- c(
  "id",
  "item",
  "section",
  "topic",
  "kind",
  "text",
  "designs",
  "file",
  "file_md5"
)

# The checklist files of the project in `home`: each file guidelines/<x>.yml
# in the list of files of the rules, as paths relative to `home`, sorted.
# A file in a folder under guidelines/ and a file with another extension,
# such as .yaml, are not checklist files. The key ignore of pbn.yml does not
# remove them.
guideline_files <- function(home) {
  entries <- project_entries(home, ignore = character(0))
  files <- entries[grepl("^guidelines/[^/]+\\.yml$", entries)]
  files <- files[!dir.exists(file.path(home, files))]
  return(sort(files, method = "radix"))
}

# TRUE when `x` is one string that is not NA and not empty.
is_one_string <- function(x) {
  return(is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x))
}

# TRUE when `x` is a YAML sequence of strings with at least one element.
# read_guideline_file() keeps each sequence as an unnamed list, so a scalar
# string, such as designs: cohort, is a character vector and fails, and a
# map is a named list and fails.
is_string_list <- function(x) {
  return(
    is.list(x) &&
      is.null(names(x)) &&
      length(x) > 0 &&
      all(vapply(x, is_one_string, logical(1)))
  )
}

# Reads and checks the checklist file `file` of the project in `home`.
# Returns a list:
# - problems: a character vector, one element per way that the file breaks
#   the schema. Empty for a valid file.
# - items: NULL when there is a problem, else the data.frame of the entries
#   that the key design keeps, with the columns of read_guidelines().
# - id: the key id when it is one string, else NA.
read_guideline_file <- function(home, file) {
  full <- file.path(home, file)
  y <- tryCatch(
    yaml::yaml.load(
      paste(readLines(full, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      eval.expr = FALSE,
      handlers = list(seq = function(x) return(x))
    ),
    error = function(e) return(e)
  )
  if (inherits(y, "error")) {
    msg <- gsub("\\s+", " ", conditionMessage(y))
    return(list(
      problems = paste0("it is not valid YAML: ", trimws(msg)),
      items = NULL,
      id = NA_character_
    ))
  }
  if (!is.list(y) || is.null(names(y))) {
    return(list(
      problems = "it is not a YAML map with the keys id, title and items",
      items = NULL,
      id = NA_character_
    ))
  }
  problems <- guideline_top_problems(y)
  entries <- guideline_entries(y)
  problems <- c(problems, entries$problems)
  id <- if (is_one_string(y[["id"]])) y[["id"]] else NA_character_
  if (length(problems) > 0) {
    return(list(problems = problems, items = NULL, id = id))
  }
  items <- entries$items
  items$id <- rep(id, nrow(items))
  items$file <- rep(file, nrow(items))
  items$file_md5 <- rep(unname(tools::md5sum(full)), nrow(items))
  return(list(
    problems = character(0),
    items = items[, guideline_columns],
    id = id
  ))
}

# The problems of the top-level keys of the parsed checklist file `y`.
guideline_top_problems <- function(y) {
  out <- character(0)
  unknown <- setdiff(names(y), guideline_keys)
  if (length(unknown) > 0) {
    out <- c(out, paste0("the key ", unknown, " is not a key of the schema"))
  }
  for (key in c("id", "title")) {
    if (!is_one_string(y[[key]])) {
      out <- c(out, paste0("the key ", key, " is missing or not one string"))
    }
  }
  if (is_one_string(y[["id"]]) && !grepl("^[a-z0-9-]+$", y[["id"]])) {
    out <- c(
      out,
      paste0("the id ", y[["id"]], " has a character other than a-z, 0-9 and -")
    )
  }
  # A key with no value, such as "version:" or "citation: ~", is NULL in
  # `y` but is in names(y). It fails, and is not treated as a missing key.
  empty <- names(y)[vapply(y, is.null, logical(1))]
  for (key in intersect(guideline_optional_keys, empty)) {
    out <- c(out, paste0("the key ", key, " is present but empty"))
  }
  for (key in c("version", "source", "citation")) {
    if (!is.null(y[[key]]) && !is_one_string(y[[key]])) {
      out <- c(out, paste0("the key ", key, " is not one string"))
    }
  }
  designs <- y[["designs"]]
  if (!is.null(designs) && !is_string_list(designs)) {
    out <- c(out, "the key designs is not a list of strings")
  }
  design <- y[["design"]]
  if (
    !is.null(design) &&
      !(is_one_string(design) &&
        is_string_list(designs) &&
        design %in% unlist(designs))
  ) {
    out <- c(out, "the key design is not one of the key designs")
  }
  items <- y[["items"]]
  if (!is.list(items) || length(items) == 0) {
    out <- c(out, "the key items is missing or empty")
  } else if (!is.null(names(items))) {
    out <- c(out, "the key items is not a YAML sequence of entries")
  }
  return(out)
}

# The entries of the key items of the parsed checklist file `y` that the key
# design keeps, and the problems of all entries. An entry without the key
# designs belongs to each design of the file. The key design keeps the
# entries of that design. Returns list(problems, items).
guideline_entries <- function(y) {
  entries <- y[["items"]]
  if (!is.list(entries) || !is.null(names(entries))) {
    return(list(problems = character(0), items = NULL))
  }
  all_designs <- if (is_string_list(y[["designs"]])) {
    unlist(y[["designs"]])
  } else {
    character(0)
  }
  design <- if (is_one_string(y[["design"]])) y[["design"]] else NULL
  problems <- character(0)
  rows <- list()
  for (i in seq_along(entries)) {
    e <- entries[[i]]
    label <- if (is.list(e) && is_one_string(e[["item"]])) {
      paste0("item ", e[["item"]])
    } else {
      paste0("entry ", i)
    }
    found <- guideline_entry_problems(e, all_designs)
    problems <- c(problems, paste0(label, ": ", found, recycle0 = TRUE))
    if (length(found) > 0) {
      next
    }
    d <- if (is.null(e[["designs"]])) all_designs else unlist(e[["designs"]])
    if (!is.null(design) && !design %in% d) {
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      item = e[["item"]],
      section = e[["section"]],
      topic = e[["topic"]],
      kind = e[["kind"]],
      text = e[["text"]],
      designs = if (is.null(design)) paste(d, collapse = ", ") else design
    )
  }
  items <- if (length(rows) == 0) {
    empty_guidelines()[, c(
      "item",
      "section",
      "topic",
      "kind",
      "text",
      "designs"
    )]
  } else {
    do.call(rbind, rows)
  }
  return(list(
    problems = c(problems, guideline_duplicates(items)),
    items = items
  ))
}

# The problems of the entry `e` of items. `all_designs` holds the key designs
# of the file.
guideline_entry_problems <- function(e, all_designs) {
  if (!is.list(e) || is.null(names(e))) {
    return("it is not a YAML map")
  }
  out <- character(0)
  unknown <- setdiff(names(e), guideline_item_keys)
  if (length(unknown) > 0) {
    out <- c(out, paste0("the key ", unknown, " is not a key of an item"))
  }
  for (key in c("item", "section", "topic", "kind", "text")) {
    if (!is_one_string(e[[key]])) {
      out <- c(out, paste0("the key ", key, " is missing or not one string"))
    }
  }
  if (is_one_string(e[["kind"]]) && !e[["kind"]] %in% guideline_kinds) {
    out <- c(
      out,
      paste0("the kind ", e[["kind"]], " is not result, prose or author")
    )
  }
  if ("designs" %in% names(e) && is.null(e[["designs"]])) {
    out <- c(out, "the key designs is present but empty")
  }
  d <- e[["designs"]]
  if (!is.null(d)) {
    if (length(all_designs) == 0) {
      out <- c(out, "it has the key designs, and the file has none")
    } else if (!is_string_list(d)) {
      out <- c(out, "the key designs is not a list of strings")
    } else if (!all(unlist(d) %in% all_designs)) {
      out <- c(out, "the key designs holds a value that the file does not")
    }
  }
  return(out)
}

# The problems of the item ids in `items`: an item id that comes twice for
# one design. A file without designs has one design.
guideline_duplicates <- function(items) {
  if (nrow(items) == 0) {
    return(character(0))
  }
  designs <- strsplit(items$designs, ", ", fixed = TRUE)
  designs[lengths(designs) == 0] <- ""
  item <- rep(items$item, lengths(designs))
  design <- unlist(designs)
  twice <- duplicated(paste(item, design, sep = "\r"))
  item <- item[twice]
  design <- design[twice]
  keep <- !duplicated(paste(item, design, sep = "\r"))
  return(paste0(
    "item ",
    item[keep],
    ": the item id comes more than once",
    ifelse(nzchar(design[keep]), paste0(" for the design ", design[keep]), ""),
    recycle0 = TRUE
  ))
}

# A data.frame with the columns of read_guidelines() and no row.
empty_guidelines <- function() {
  out <- as.data.frame(
    stats::setNames(
      rep(list(character(0)), length(guideline_columns)),
      guideline_columns
    )
  )
  return(out)
}

# Reads each checklist file guidelines/<x>.yml of the project in `home`, and
# checks it. Returns a list with one element per file, named by the file, as
# read_guideline_file() gives it. A second file with the id of a file before
# it, in sorted order, gets a problem.
check_guidelines <- function(home) {
  files <- guideline_files(home)
  out <- stats::setNames(lapply(files, read_guideline_file, home = home), files)
  ids <- vapply(out, function(x) return(x[["id"]]), character(1))
  for (i in seq_along(out)) {
    first <- match(ids[i], ids)
    if (!is.na(ids[i]) && first < i) {
      out[[i]][["problems"]] <- c(
        out[[i]][["problems"]],
        paste0("the id ", ids[i], " is also the id of ", files[first])
      )
      out[[i]][["items"]] <- NULL
    }
  }
  return(out)
}

# The items of the checklists of the project in `home`, for the rules and the
# manuscript code of pbn. `home` is the top folder of the project.
#
# Reads each file guidelines/<x>.yml of guideline_files(), and keeps the
# entries of its key design. Stops with an error that names each file that
# breaks the schema, with its first problem, because a result from a broken
# file is not a checklist. The rule guidelines_valid gives each problem.
#
# Returns one data.frame, a row per entry that the key design keeps, in the
# order of the files and of their entries. Every column is character:
# - id: the key id of the file.
# - item, section, topic, kind and text: the keys of the entry.
# - designs: the designs of the entry, joined by ", ". These are its key
#   designs, or the key designs of the file for an entry without it. A file
#   with the key design gives that design. A file without designs gives "".
# - file: the path of the file, such as guidelines/strobe.yml.
# - file_md5: the md5 of the bytes of the file.
# With no checklist file, the data.frame has these columns and no row.
read_guidelines <- function(home) {
  checked <- check_guidelines(home)
  bad <- Filter(function(x) return(length(x[["problems"]]) > 0), checked)
  if (length(bad) > 0) {
    first <- vapply(bad, function(x) return(x[["problems"]][1]), character(1))
    stop(
      "These checklist files break the schema of ?check_project:\n",
      paste0("- ", names(bad), ": ", first, collapse = "\n"),
      call. = FALSE
    )
  }
  items <- lapply(checked, function(x) return(x[["items"]]))
  out <- do.call(rbind, c(list(empty_guidelines()), unname(items)))
  rownames(out) <- NULL
  return(out)
}

# The number of problems that one finding of guidelines_valid names. The
# detail counts the rest.
guideline_problems_shown <- 3

# The rule guidelines_valid: each file guidelines/<x>.yml of
# guideline_files() follows the checklist schema. One FAIL per file that
# breaks it.
rule_guidelines_valid <- function(path) {
  checked <- check_guidelines(path)
  bad <- Filter(function(x) return(length(x[["problems"]]) > 0), checked)
  detail <- vapply(
    names(bad),
    function(f) {
      p <- bad[[f]][["problems"]]
      shown <- utils::head(p, guideline_problems_shown)
      rest <- length(p) - length(shown)
      return(paste0(
        f,
        ": ",
        paste(shown, collapse = "; "),
        if (rest > 0) paste0("; and ", rest, " more")
      ))
    },
    character(1),
    USE.NAMES = FALSE
  )
  return(project_row("guidelines_valid", "FAIL", names(bad), detail))
}

# The rule guidelines_covered. It finds nothing yet, so it always passes. A
# later version of pbn checks that the manuscript answers each item of
# read_guidelines().
rule_guidelines_covered <- function(path) {
  return(project_row(character(0), character(0), character(0), character(0)))
}
