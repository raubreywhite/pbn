# The run script of the file of to_one_file(): the run script of the project
# with the call of initialize_project() for the server and the scenario
# "real" only, and the header and the end of the file.

# The lines of the run script `script` of `version` for the file of
# to_one_file(), and `workers`, the text of the argument workers of
# pbn::run_plan(), "1" without it, or NA when the script has not exactly one
# such call. `code` is the code of the functions folder.
one_file_script <- function(script, version, home, folders, code) {
  exprs <- parse(text = script, keep.source = TRUE)
  refs <- attr(exprs, "srcref")
  init <- which(vapply(
    as.list(exprs),
    function(e) {
      return(is.call(e) && identical(e[[1]], quote(pbn::initialize_project)))
    },
    logical(1)
  ))
  if (length(init) != 1) {
    stop(
      version,
      "_run.R has ",
      length(init),
      " top-level calls pbn::initialize_project(). pbn::to_one_file() needs ",
      "exactly one.",
      call. = FALSE
    )
  }
  matched <- match.call(initialize_project, exprs[[init]])
  if (!identical(matched$version, version)) {
    stop(
      "The call pbn::initialize_project() of ",
      version,
      "_run.R MUST give version = \"",
      version,
      "\" as a string.",
      call. = FALSE
    )
  }
  tokens <- utils::getParseData(exprs)
  sc <- scenarios_call(tokens, script, version)
  env_text <- if (is.null(matched$env)) {
    NULL
  } else {
    paste(deparse(matched$env), collapse = " ")
  }
  call_lines <- init_call_lines(version, home, folders, env_text, code)
  # The later edit first, so that the places of the earlier one hold.
  init_ref <- as.integer(refs[[init]][1:4])
  after <- sc[[1]] > init_ref[[3]] ||
    (sc[[1]] == init_ref[[3]] && sc[[2]] > init_ref[[4]])
  if (after) {
    script <- replace_span(script, sc, "pbn::scenarios(CFG, only = \"real\")")
    script <- replace_span(script, init_ref, call_lines)
  } else {
    script <- replace_span(script, init_ref, call_lines)
    script <- replace_span(script, sc, "pbn::scenarios(CFG, only = \"real\")")
  }
  return(list(lines = script, workers = run_plan_workers(tokens)))
}

# The ids of the parse data `tokens` of each call of pbn::<name>.
pbn_call_ids <- function(tokens, name) {
  rows <- which(tokens$token == "SYMBOL_FUNCTION_CALL" & tokens$text == name)
  rows <- rows[vapply(
    rows,
    function(i) return(identical(call_package(tokens, i), "pbn")),
    logical(1)
  )]
  return(tokens$parent[match(tokens$parent[rows], tokens$id)])
}

# The place of the one call pbn::scenarios(CFG) of the run script `script`,
# as the first line, the first byte, the last line and the last byte. Stops
# when the script has not exactly one call of pbn::scenarios(), or when the
# call has another argument than CFG.
scenarios_call <- function(tokens, script, version) {
  ids <- pbn_call_ids(tokens, "scenarios")
  texts <- vapply(
    ids,
    function(id) return(utils::getParseText(tokens, id)),
    character(1)
  )
  ok <- vapply(
    texts,
    function(x) {
      return(identical(str2lang(x), quote(pbn::scenarios(CFG))))
    },
    logical(1)
  )
  if (length(ids) != 1 || !all(ok)) {
    stop(
      version,
      "_run.R has ",
      length(ids),
      " calls of pbn::scenarios()",
      if (length(ids) > 0) {
        paste0(": ", paste(gsub("\\s+", " ", texts), collapse = ", "))
      },
      ". pbn::to_one_file() needs exactly one call, pbn::scenarios(CFG), ",
      "with no other argument, and makes it pbn::scenarios(CFG, only = ",
      "\"real\").",
      call. = FALSE
    )
  }
  row <- tokens[tokens$id == ids, , drop = FALSE]
  return(c(
    row$line1,
    col_to_byte(script[row$line1], row$col1),
    row$line2,
    col_to_byte(script[row$line2], row$col2)
  ))
}

# The byte of the line `line` at the column `col` of the parse data. The
# parser counts a TAB up to the next multiple of 8 columns. The line is
# ASCII, so a byte is a character.
col_to_byte <- function(line, col) {
  chars <- strsplit(line, "", fixed = TRUE)[[1]]
  cols <- integer(length(chars))
  now <- 0L
  for (i in seq_along(chars)) {
    now <- if (chars[i] == "\t") bitwAnd(now + 8L, bitwNot(7L)) else now + 1L
    cols[i] <- now
  }
  return(match(col, cols))
}

# The text of the argument workers of the one call pbn::run_plan() of the
# parse data `tokens`: "1" without it, or NA when there is not exactly one
# call.
run_plan_workers <- function(tokens) {
  ids <- pbn_call_ids(tokens, "run_plan")
  if (length(ids) != 1) {
    return(NA_character_)
  }
  call <- str2lang(utils::getParseText(tokens, ids))
  matched <- tryCatch(match.call(run_plan, call), error = function(e) {
    return(NULL)
  })
  if (is.null(matched$workers)) {
    return("1")
  }
  return(paste(deparse(matched$workers), collapse = " "))
}

# The lines of the call of pbn::initialize_project() of the file of
# to_one_file(): the version, the paths of the server, the argument `env` of
# the run script as `env_text` or NULL, and `generated` with the project,
# the md5 of the files of the project and the code `code`.
init_call_lines <- function(version, home, folders, env_text, code) {
  # The md5 of the manifest of the project, so that the server writes the
  # same manifest$source_md5.
  md5 <- state$manifest_md5
  comma <- function(n) return(c(rep(",", n - 1), ""))
  return(c(
    "pbn::initialize_project(",
    paste0("  version = ", quoted(version), ","),
    paste0("  home = ", quoted(home), ","),
    "  folders = c(",
    paste0(
      "    ",
      names(folders),
      " = ",
      quoted(folders),
      comma(length(folders))
    ),
    "  ),",
    if (!is.null(env_text)) paste0("  env = ", env_text, ","),
    "  generated = list(",
    paste0("    project = ", quoted(state$project), ","),
    "    source_md5 = c(",
    paste0(
      "      ",
      quoted(names(md5)),
      " = ",
      quoted(md5),
      comma(length(md5))
    ),
    "    ),",
    "    code = quote({",
    code,
    "    })",
    "  )",
    ")"
  ))
}

# Each string of `x` as an R string in double quotes.
quoted <- function(x) {
  return(encodeString(unname(x), quote = "\""))
}

# The header of the file of to_one_file(): what it is, where it comes from,
# the md5 of each file `included` in it, and the workers `workers`.
one_file_header <- function(version, home, included, workers) {
  width <- max(nchar(included))
  return(c(
    paste0(
      "# ",
      version,
      "_one_file.R: the run of the real data of the project ",
      state$project,
      ", version ",
      version,
      ","
    ),
    "# as one file for a computer without the project folder.",
    "#",
    paste0(
      "# pbn ",
      getNamespaceVersion("pbn"),
      " generated it on ",
      format(Sys.Date(), "%Y-%m-%d"),
      " from the project folder ",
      state$project,
      "."
    ),
    "# Do not edit: change the project and generate again with pbn::to_one_file().",
    "#",
    "# The md5 of each file in this file, as pbn computes it for the manifest:",
    paste0(
      "#   ",
      formatC(included, width = -width),
      "  ",
      unname(state$source_md5[included])
    ),
    "#",
    "# Paste the whole file into R on the server, and run it from the top to",
    paste0(
      "# the end. pbn writes the results to ",
      home,
      "/",
      version,
      "_results/."
    ),
    "#",
    workers_lines(workers),
    ""
  ))
}

# The lines of the header on the workers of pbn::run_plan().
workers_lines <- function(workers) {
  if (is.na(workers)) {
    return(paste(
      "# The run script has not exactly one call of pbn::run_plan(), so this",
      "header gives no workers."
    ))
  }
  if (identical(workers, "1")) {
    return(c(
      "# Workers: pbn::run_plan() below runs on 1 worker. To use more, add",
      "# workers = <n> to that call."
    ))
  }
  return(c(
    paste0(
      "# Workers: pbn::run_plan() below runs with workers = ",
      workers,
      "."
    ),
    "# To change the number of workers, change that argument."
  ))
}

# The end of the file of to_one_file(): where the results are, and what to
# type next.
one_file_footer <- function(home, version) {
  has_blocks <- exists("blocks", envir = state$env, inherits = FALSE)
  return(c(
    "# ---- The end of the run ----",
    paste0("# The real results are in ", home, "/", version, "_results/real/."),
    if (has_blocks) {
      c(
        "# To show the numbers on the screen, type:",
        "#   pbn::show_blocks()",
        "#   pbn::show_block(1)",
        "#   pbn::show_block(1, page = 2)"
      )
    } else {
      "# The project has no blocks. ?pbn::show_block says how to declare them."
    }
  ))
}
