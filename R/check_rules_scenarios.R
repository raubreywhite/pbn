# The project rules about scenarios: the run script that MUST NOT choose a
# scenario and MUST list each scenario for stepping by hand, the assertions
# that run_scenarios() writes, and the manuscript that MUST read the real
# results only.

rule_no_active_use_scenario <- function(path) {
  rule <- "no_active_use_scenario"
  out <- list()
  for (s in run_scripts(path)) {
    exprs <- parse_or_null(file.path(path, s), keep_source = TRUE)
    if (is.null(exprs)) {
      out[[s]] <- project_row(rule, "FAIL", s, paste0(s, ": R cannot parse it"))
      next
    }
    refs <- attr(exprs, "srcref")
    lines <- integer(0)
    for (k in seq_along(exprs)) {
      if (length(find_calls(exprs[[k]], is_use_scenario_call)) > 0) {
        lines <- c(lines, refs[[k]][[1]])
      }
    }
    if (length(lines) > 0) {
      out[[s]] <- project_row(
        rule,
        "FAIL",
        s,
        paste0(
          s,
          ": an active pbn::use_scenario() in the expression on line ",
          paste(lines, collapse = ", "),
          ". It is for stepping through the script by hand: comment it out"
        )
      )
    }
  }
  return(bind_rows(out))
}

# use_scenario(), pbn::use_scenario() or the same with three colons. The
# reference pbn::use_scenario without a call matches too.
is_use_scenario_call <- function(cl) {
  if (identical(cl[[1]], quote(use_scenario))) {
    return(TRUE)
  }
  # pbn::use_scenario and pbn:::use_scenario. call() builds them, because R
  # CMD check notes a written ::: to the package itself.
  refs <- lapply(c("::", ":::"), function(op) {
    return(call(op, quote(pbn), quote(use_scenario)))
  })
  hit <- vapply(
    refs,
    function(r) return(identical(cl, r) || identical(cl[[1]], r)),
    logical(1)
  )
  return(any(hit))
}

rule_use_scenario_lines <- function(path) {
  rule <- "use_scenario_lines"
  out <- list()
  folders <- functions_folders(path)
  for (s in run_scripts(path)) {
    version <- substr(s, 1, 3)
    exprs <- parse_or_null(file.path(path, s), keep_source = TRUE)
    if (is.null(exprs)) {
      out[[s]] <- project_row(rule, "FAIL", s, paste0(s, ": R cannot parse it"))
      next
    }
    tokens <- utils::getParseData(exprs)
    at <- init_call_line(tokens)
    table <- if (is.na(at)) {
      "there is no call pbn::initialize_project() to find the lines above"
    } else {
      read_scenario_table(path, version, folders)
    }
    if (!is.data.frame(table)) {
      out[[s]] <- project_row(rule, "WARN", s, paste0(s, ": ", table))
      next
    }
    problems <- use_scenario_problems(
      use_scenario_names(tokens, at),
      table$scenario
    )
    if (length(problems) > 0) {
      out[[s]] <- project_row(
        rule,
        "FAIL",
        s,
        paste0(s, ": ", paste(problems, collapse = "; "))
      )
    }
  }
  return(bind_rows(out))
}

# The first line of the first call pbn::initialize_project() in the parse
# data, or NA when there is none.
init_call_line <- function(tokens) {
  hits <- which(
    tokens$token == "SYMBOL_FUNCTION_CALL" & tokens$text == "initialize_project"
  )
  lines <- integer(0)
  for (k in hits) {
    head <- tokens[tokens$parent == tokens$parent[k], , drop = FALSE]
    if (
      any(head$token == "SYMBOL_PACKAGE" & head$text == "pbn") &&
        any(head$token == "NS_GET")
    ) {
      lines <- c(lines, min(head$line1))
    }
  }
  if (length(lines) == 0) {
    return(NA_integer_)
  }
  return(min(lines))
}

# What is wrong with the scenarios `found` of the commented lines, given the
# scenarios of CFG$scenarios: a missing line, a line for another scenario,
# and a scenario with more than one line.
use_scenario_problems <- function(found, scenarios) {
  counts <- table(found)
  twice <- names(counts)[counts > 1]
  return(c(
    paste0(
      "no line # pbn::use_scenario(\"",
      setdiff(scenarios, found),
      "\", sim = 1) above pbn::initialize_project()",
      recycle0 = TRUE
    ),
    paste0(
      "the line # pbn::use_scenario(\"",
      setdiff(unique(found), scenarios),
      "\", sim = 1) names a scenario that is not in CFG$scenarios",
      recycle0 = TRUE
    ),
    paste0(
      "the scenario ",
      twice,
      " has ",
      as.integer(counts[twice]),
      " lines # pbn::use_scenario(\"",
      twice,
      "\", sim = 1); it MUST have 1",
      recycle0 = TRUE
    )
  ))
}

# The scenario of each comment `# pbn::use_scenario("<name>", sim = 1)` that
# starts on a line before `at`, once per comment. Any whitespace between the
# parts is allowed. A comment counts only when it is at the top level of the
# script and fills its line: getParseData() gives a top-level comment a
# parent of 0 or less (minus the id of the next top-level expression), and a
# comment in a function body or a braced block the id of that expression.
use_scenario_names <- function(tokens, at) {
  pattern <- paste0(
    "^#\\s*pbn\\s*::\\s*use_scenario\\s*\\(\\s*\"([^\"]*)\"",
    "\\s*,\\s*sim\\s*=\\s*1\\s*\\)\\s*$"
  )
  code <- tokens[tokens$terminal & tokens$token != "COMMENT", , drop = FALSE]
  comments <- tokens[
    tokens$token == "COMMENT" & tokens$line1 < at & tokens$parent <= 0,
    ,
    drop = FALSE
  ]
  # A comment after code on the same line is not a line of its own.
  alone <- vapply(
    comments$line1,
    function(l) return(!any(code$line1 <= l & code$line2 >= l)),
    logical(1)
  )
  text <- comments$text[alone]
  text <- text[grepl(pattern, text, perl = TRUE)]
  return(sub(pattern, "\\1", text, perl = TRUE))
}

# CFG$scenarios of one version, read as run_scenarios() reads it: a new R
# session sources <version>_functions/. When pbn cannot read it, the reason as
# one string.
read_scenario_table <- function(path, version, folders) {
  folder <- paste0(version, "_functions")
  if (!folder %in% folders) {
    return(paste0(
      "there is no ",
      folder,
      "/, so pbn cannot read CFG$scenarios"
    ))
  }
  home <- normalizePath(path, winslash = "/")
  return(tryCatch(
    read_project_functions(home, version)[["scenarios"]],
    error = function(e) {
      cause <- if (inherits(e$parent, "condition")) e$parent else e
      return(paste0(
        "pbn cannot read CFG$scenarios: ",
        gsub("\\s+", " ", conditionMessage(cause))
      ))
    }
  ))
}

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
