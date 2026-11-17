# The project rules about scenarios: the assertions that save_results()
# writes, the manuscript that MUST read the real results only, and the names
# of the output of fake data.

rule_scenario_checks <- function(path) {
  rule <- "scenario_checks"
  entries <- project_entries(path)
  files <- entries[
    grepl("^v[0-9]{2}_results/[^/]+/check\\.csv$", entries) &
      !dir.exists(file.path(path, entries))
  ]
  out <- list()
  for (f in files) {
    found <- tryCatch(
      utils::read.csv(file.path(path, f), stringsAsFactors = FALSE),
      error = function(e) return(NULL)
    )
    if (!is.data.frame(found) || !"pass" %in% names(found)) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": R cannot read a column pass in it")
      )
      next
    }
    if (nrow(found) == 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": it holds no assertion")
      )
      next
    }
    bad <- !found$pass %in% TRUE
    if (any(bad)) {
      what <- if ("assertion" %in% names(found)) {
        found$assertion[bad]
      } else {
        paste("row", which(bad))
      }
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(f, ": these assertions failed: ", paste(what, collapse = "; "))
      )
    }
  }
  return(bind_rows(out))
}

# The .R, .qmd and .Rmd files in manuscript/built/, relative to `path`.
manuscript_files <- function(path) {
  entries <- project_entries(path)
  return(entries[
    startsWith(entries, "manuscript/built/") &
      grepl("\\.(R|r|qmd|Rmd)$", entries) &
      !dir.exists(file.path(path, entries))
  ])
}

rule_manuscript_reads_real <- function(path) {
  rule <- "manuscript_reads_real"
  out <- list()
  for (f in manuscript_files(path)) {
    exprs <- manuscript_code(file.path(path, f))
    if (is.null(exprs)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
      next
    }
    calls <- find_calls(exprs, is_read_results_call)
    scenarios <- vapply(calls, read_scenario, character(1))
    bad <- scenarios[scenarios != "\"real\""]
    if (length(bad) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": read_results() with scenario = ",
          paste(unique(bad), collapse = ", "),
          ". A manuscript reads only scenario \"real\""
        )
      )
    }
  }
  return(bind_rows(out))
}

# The R code of one manuscript file as an expression, or NULL when R cannot
# parse it. For .qmd and .Rmd, it is the code of the R chunks that knitr::purl()
# extracts, and the inline R code.
manuscript_code <- function(file) {
  if (grepl("\\.[rR]$", file)) {
    return(parse_or_null(file))
  }
  out <- tempfile(fileext = ".R")
  on.exit(unlink(out), add = TRUE)
  ok <- tryCatch(
    {
      knitr::purl(
        file,
        output = out,
        quiet = TRUE,
        documentation = 0,
        envir = new.env()
      )
      TRUE
    },
    error = function(e) return(FALSE)
  )
  if (!ok) {
    return(NULL)
  }
  chunks <- parse_or_null(out)
  inline <- tryCatch(
    parse(text = inline_code(file), keep.source = FALSE),
    error = function(e) return(NULL)
  )
  if (is.null(chunks) || is.null(inline)) {
    return(NULL)
  }
  return(c(chunks, inline))
}

# The inline R code of a markdown file: `r code` and `{r} code`. The first
# pattern is the one that knitr uses.
inline_code <- function(file) {
  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  # Each pattern with the number of the group that holds the code. The
  # second pattern MUST NOT match the fence of a chunk, ```{r}.
  patterns <- list(
    list(knitr::all_patterns$md$inline.code, "\\3"),
    list("(?<!`)`\\{r\\}[ \\t]+([^`]+)`", "\\1")
  )
  code <- character(0)
  for (p in patterns) {
    hits <- regmatches(text, gregexpr(p[[1]], text, perl = TRUE))[[1]]
    code <- c(code, sub(p[[1]], p[[2]], hits, perl = TRUE))
  }
  return(code)
}

# read_results(), pbn::read_results() or the same with three colons.
is_read_results_call <- function(cl) {
  f <- cl[[1]]
  if (identical(f, quote(read_results))) {
    return(TRUE)
  }
  return(
    is.call(f) &&
      length(f) == 3 &&
      as.character(f[[1]]) %in% c("::", ":::") &&
      identical(f[[2]], quote(pbn)) &&
      identical(f[[3]], quote(read_results))
  )
}

# The scenario argument of one read_results() call, deparsed. A call without
# it reads the real results, so it gives "\"real\"".
read_scenario <- function(cl) {
  matched <- tryCatch(
    match.call(read_results, cl),
    error = function(e) return(NULL)
  )
  if (is.null(matched)) {
    return(paste(deparse(cl), collapse = " "))
  }
  s <- matched[["scenario"]]
  if (is.null(s)) {
    return("\"real\"")
  }
  return(paste(deparse(s), collapse = " "))
}

# A file in a folder output_fake_data/ MUST have a name that starts with
# fake_data_, and a file with such a name MUST be in such a folder.
rule_fake_data_names <- function(path) {
  rule <- "fake_data_names"
  entries <- project_entries(path)
  files <- entries[!dir.exists(file.path(path, entries))]
  inside <- vapply(
    strsplit(files, "/", fixed = TRUE),
    function(p) return("output_fake_data" %in% p[-length(p)]),
    logical(1)
  )
  prefixed <- startsWith(basename(files), "fake_data_")
  no_prefix <- files[inside & !prefixed]
  outside <- files[!inside & prefixed]
  return(rbind(
    project_row(
      rule,
      "FAIL",
      no_prefix,
      paste0(
        no_prefix,
        ": a file in output_fake_data/ holds output of fake data, so its ",
        "name MUST start with fake_data_",
        recycle0 = TRUE
      )
    ),
    project_row(
      rule,
      "FAIL",
      outside,
      paste0(
        outside,
        ": a name that starts with fake_data_ is for a file in a folder ",
        "output_fake_data/ only",
        recycle0 = TRUE
      )
    )
  ))
}
