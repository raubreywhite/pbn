# The project rules about scenarios: the assertions that save_results()
# writes, the manuscript that MUST read the real results only, and the names
# of the output of fake data. The rules of the one version counter of the
# manuscripts are here too: built_reads_own_version, built_has_run and
# manual_not_ahead. The set house_2026_11_20 brought them.

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

# The rule manuscript_reads_real: each read_results() call of a file in
# manuscript/built/ reads the scenario "real". From the rule set
# house_2026_11_26 of `set`, a pbn::txt() call counts too. txt() reads the
# real blocks only, so a txt() call fails when its arguments do not match
# txt(), for example with an argument scenario.
rule_manuscript_reads_real <- function(path, set = "house_2026_11_17") {
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
    found <- character(0)
    if (length(bad) > 0) {
      found <- paste0(
        "read_results() with scenario = ",
        paste(unique(bad), collapse = ", ")
      )
    }
    if (needs_pbn_blocks(set)) {
      texts <- find_calls(exprs, is_txt_call)
      odd <- vapply(texts, txt_scenario, character(1))
      odd <- odd[odd != "\"real\""]
      if (length(odd) > 0) {
        found <- c(
          found,
          paste0(
            "pbn::txt() with an argument that it does not take: ",
            paste(unique(odd), collapse = ", ")
          )
        )
      }
    }
    if (length(found) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": ",
          paste(found, collapse = "; "),
          ". A manuscript reads only scenario \"real\""
        )
      )
    }
  }
  return(bind_rows(out))
}

# A file or a folder under manuscript/built/vNN/, or that folder itself.
built_version_pattern <- "^manuscript/built/(v[0-9]{2})(/.*)?$"

# The number NN of each version vNN at the start of `x`, such as 17 for
# "v17_run.R" or "v17_AB.docx".
version_number <- function(x) {
  return(as.integer(substr(x, 2, 3)))
}

# The rule built_reads_own_version: each read_results() call of a file in
# manuscript/built/vNN/ has the version argument "vNN", the string. A
# variable, another string and a call without the version fail.
# From the rule set house_2026_11_26 of `set`, each pbn::txt() call has the
# version argument "vNN" too.
rule_built_reads_own_version <- function(path, set = "house_2026_11_17") {
  rule <- "built_reads_own_version"
  files <- manuscript_files(path)
  files <- files[grepl(built_version_pattern, files)]
  out <- list()
  for (f in files) {
    exprs <- manuscript_code(file.path(path, f))
    if (is.null(exprs)) {
      out[[f]] <- project_row(rule, "FAIL", f, paste0(f, ": R cannot parse it"))
      next
    }
    own <- sub(built_version_pattern, "\\1", f)
    calls <- find_calls(exprs, is_read_results_call)
    versions <- lapply(calls, read_version)
    if (needs_pbn_blocks(set)) {
      texts <- find_calls(exprs, is_txt_call)
      calls <- c(calls, texts)
      versions <- c(versions, lapply(texts, txt_version))
    }
    reads_own <- vapply(
      versions,
      function(v) return(identical(v, own)),
      logical(1)
    )
    bad <- vapply(
      calls[!reads_own],
      function(cl) return(paste(deparse(cl), collapse = " ")),
      character(1)
    )
    if (length(bad) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": a manuscript in manuscript/built/",
          own,
          "/ reads only the version \"",
          own,
          "\", and these calls do not: ",
          paste(unique(bad), collapse = "; ")
        )
      )
    }
  }
  return(bind_rows(out))
}

# The version argument of one read_results() call, or NULL when the call has
# none or R cannot match it to read_results().
read_version <- function(cl) {
  matched <- tryCatch(
    match.call(read_results, cl),
    error = function(e) return(NULL)
  )
  return(matched[["version"]])
}

# The rule built_has_run: each folder manuscript/built/vNN/ has the run
# script vNN_run.R at the top level.
rule_built_has_run <- function(path) {
  rule <- "built_has_run"
  entries <- project_entries(path)
  folders <- unique(sub(
    built_version_pattern,
    "manuscript/built/\\1",
    entries[grepl(built_version_pattern, entries)]
  ))
  folders <- folders[dir.exists(file.path(path, folders))]
  run <- paste0(basename(folders), "_run.R", recycle0 = TRUE)
  missing <- folders[!run %in% run_scripts(path)]
  run <- run[!run %in% run_scripts(path)]
  return(project_row(
    rule,
    "FAIL",
    missing,
    paste0(missing, ": there is no ", run, recycle0 = TRUE)
  ))
}

# The files manuscript/manual/vNN_<INITIALS>.docx of the project in `path`
# whose names match manual_pattern. manual_manuscript_names fails the other
# names.
manual_files <- function(path) {
  entries <- project_entries(path)
  return(entries[
    dirname(entries) == "manuscript/manual" &
      grepl(manual_pattern, basename(entries)) &
      !dir.exists(file.path(path, entries))
  ])
}

# The versions vNN of the files of manual_files(), sorted.
manual_versions <- function(path) {
  versions <- unique(substr(basename(manual_files(path)), 1, 3))
  return(sort(versions, method = "radix"))
}

# The rule manual_not_ahead: each manual version NN is below the newest run
# version MM, the largest MM of a vMM_run.R. The versions of manual and built
# manuscripts use one counter, so a hand edit of vMM is v(MM+1). Without a
# run script there is nothing to compare.
rule_manual_not_ahead <- function(path) {
  rule <- "manual_not_ahead"
  runs <- run_scripts(path)
  if (length(runs) == 0) {
    return(project_row(
      rule,
      "PASS",
      "",
      "there is no vNN_run.R, so nothing to compare"
    ))
  }
  newest <- max(version_number(runs))
  files <- manual_files(path)
  n <- version_number(basename(files))
  ahead <- n >= newest
  return(project_row(
    rule,
    "FAIL",
    files[ahead],
    sprintf(
      paste0(
        "manual v%02d is ahead of the newest run v%02d: freeze v%02d and ",
        "start v%02d with pbn::new_version(\"v%02d\", to = \"v%02d\")"
      ),
      n[ahead],
      newest,
      newest,
      n[ahead] + 1L,
      newest,
      n[ahead] + 1L
    )
  ))
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

# TRUE when `cl` is a call of txt() or pbn::txt().
is_txt_call <- function(cl) {
  f <- cl[[1]]
  if (identical(f, quote(txt))) {
    return(TRUE)
  }
  return(
    is.call(f) &&
      length(f) == 3 &&
      as.character(f[[1]]) %in% c("::", ":::") &&
      identical(f[[2]], quote(pbn)) &&
      identical(f[[3]], quote(txt))
  )
}

# The version argument of one txt() call, or NULL when the call has none or
# R cannot match it to txt().
txt_version <- function(cl) {
  matched <- tryCatch(match.call(txt, cl), error = function(e) return(NULL))
  return(matched[["version"]])
}

# "\"real\"" for a txt() call that R matches to txt(), which reads the real
# blocks only, and else the call, deparsed.
txt_scenario <- function(cl) {
  matched <- tryCatch(match.call(txt, cl), error = function(e) return(NULL))
  if (is.null(matched)) {
    return(paste(deparse(cl), collapse = " "))
  }
  return("\"real\"")
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
  entries <- project_entries(path, ignore = character(0))
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

# The rule real_results_are_fake: a finding for each
# vNN_results/real/results.qs2 whose manifest has fake_real_data TRUE. The
# run of such results had initialize_project(fake_real_data = TRUE), so its
# numbers are not the numbers of the real data.
rule_real_results_are_fake <- function(path) {
  rule <- "real_results_are_fake"
  entries <- project_entries(path)
  files <- entries[grepl("^v[0-9]{2}_results/real/results\\.qs2$", entries)]
  out <- list()
  for (f in files) {
    m <- tryCatch(
      qs2::qs_read(file.path(path, f))$manifest,
      error = function(e) return(NULL)
    )
    if (isTRUE(m$fake_real_data)) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        f,
        paste0(
          f,
          ": the real results come from fake real data, because the run had ",
          "fake_real_data = TRUE. They are not the numbers of the real data"
        )
      )
    }
  }
  return(bind_rows(out))
}
