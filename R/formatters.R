#' Declare one block of the manuscript, with its formatter and its items
#'
#' `block()` declares one element of the list `blocks` in
#' `<version>_functions/blocks.R`. The function of the block returns
#' numbers, and the formatter turns them into the text of the manuscript.
#'
#' @param fn A function of `results`. It returns the raw value of the block:
#'   a data.frame or a data.table of numbers and labels.
#' @param format The name of the formatter, one string. It is the name of a
#'   house formatter, or of a function in `<version>_functions/formatters.R`.
#' @param items The checklist items that the block answers, as a character
#'   vector of `"<id>:<item>"` strings, for example
#'   `c("strobe:13a", "strobe:14a")`. The default is no item.
#' @details
#' The rule set `house_2026_11_26` and the newer sets need `block()` for each
#' element of `blocks`:
#'
#' ```r
#' blocks <- list(
#'   "Results para 1: effect" = pbn::block(
#'     function(results) results$effect,
#'     format = "effect",
#'     items = c("strobe:17")
#'   )
#' )
#' ```
#'
#' The raw value MUST hold numbers as numbers. A character or factor column
#' holds labels only. No cell of such a column MAY match `[0-9][.,][0-9]`,
#' `[0-9]%`, `[0-9] \(` or `[=<>] ?[0-9]`. Each of these patterns is a
#' formatted number, such as `0.80`, `68.7%`, `3 (` and `n = 3`.
#' [save_results()], [update_blocks()], [show_blocks()], [show_block()] and
#' [txt()] stop on such a cell. The error names the block, the column and
#' the first cell that matches. The check reads the value only, so a block
#' MAY call any function, for example `paste0()` to make a name.
#'
#' The formatted value of a block is `formatter(value)`. A formatter is a
#' function of one argument, the raw value. It returns a character vector
#' with one string for each row, or a data.frame or a data.table. Its columns
#' MAY be of mixed types. pbn has five house formatters. Each of them uses
#' the minus sign U+2212 for a negative number:
#'
#' | name | columns | example |
#' |---|---|---|
#' | `ci` | `est`, `lo`, `hi`; `digits` (default 2) | `0.80 (95% CI 0.46 to 1.16)` |
#' | `p` | `p` | `p < 0.001`, `p = 0.047` |
#' | `effect` | `est`, `lo`, `hi`, `p`; `unit`, `digits` (default 2) | `(0.80 points, 95% CI 0.46 to 1.16, p < 0.001)` |
#' | `n_pct` | `n`, `total`; `digits` (default 1) | `1,037 (68.7%)` |
#' | `mean_sd` | `mean`, `sd`; `digits` (default 1) | `30.7 (4.8)` |
#'
#' `p` gives `p < 0.001` below 0.001, and else 3 decimals. A row with `NA`
#' in a column that the formatter needs gives `NA`.
#'
#' A project formatter is a function in `<version>_functions/formatters.R`.
#' It wins over a house formatter of the same name. That file MUST hold only
#' assignments of a name to `function(...)`. pbn leaves it out of
#' `manifest$source_md5` under the same conditions as `blocks.R`, see
#' [save_results()]. So a change of a formatter keeps the results current.
#' The rule `blocks_list` of [check_project()] fails a use of a name of
#' `formatters.R` in a file other than `blocks.R` and `formatters.R`.
#'
#' In `blocks.R`, `format` MUST be one string, and `items` MUST be a call
#' `c("<id>:<item>", ...)` of strings, so that [check_project()] reads them
#' without running code. An `<id>` holds lower-case letters, digits and `-`.
#' An `<item>` holds no space and no `:`.
#' @return An object of the class `pbn_block`: a list with `fn`, `format` and
#'   `items`.
#' @seealso [show_block()], [txt()], [update_blocks()].
#' @export
block <- function(fn, format, items = character(0)) {
  if (!is.function(fn)) {
    stop("`fn` must be a function of results.", call. = FALSE)
  }
  if (!is_formatter_name(format)) {
    stop(
      "`format` must be one string, the name of a formatter.",
      call. = FALSE
    )
  }
  problem <- items_problem(items)
  if (!is.null(problem)) {
    stop("`items` ", problem, ".", call. = FALSE)
  }
  return(structure(
    list(fn = fn, format = format, items = unname(items)),
    class = "pbn_block"
  ))
}

#' The formatted string of one block, for the manuscript
#'
#' `txt()` returns one string of the formatted value of a block, from the
#' real `blocks.qs2` of the version. A manuscript calls it inline, for
#' example `` `r pbn::txt("Results para 1: effect", version = "v02")` ``.
#'
#' @param name The name of the block in `blocks`, one string.
#' @param row The row of the formatted value, a whole number from 1.
#' @param col The column of the formatted value when the formatter returns a
#'   data.frame: a name or a number. `NULL` is right only for a character
#'   vector or a data.frame with one column.
#' @inheritParams show_blocks
#' @details
#' `txt()` reads the raw value as [read_blocks()] does:
#' `<home>/<version>_results/real/blocks.qs2` when it exists, which
#' [save_results()], [update_blocks()] and [import_blocks()] write. In a
#' one-file project without that file, it reads `fake/` with a warning, see
#' the section The folder fake/ of [initialize_project()]. It formats the
#' value at each call, so a change of a formatter needs no new
#' `blocks.qs2`.
#'
#' After [initialize_project()] of `version` and `home` in this R session,
#' `txt()` takes the block from that session. Else it reads
#' `<version>_functions/blocks.R` without running it. It then needs a call
#' `pbn::block()` with a string `format` for the block `name`, and it
#' evaluates only the function assignments of
#' `<version>_functions/formatters.R`. The default `home` is then the
#' working folder. The default `version` needs [initialize_project()].
#'
#' `txt()` stops when the block, the row, the column or `blocks.qs2` is
#' missing. It also stops when the block is not a `pbn::block()`, or when
#' the cell is not a string. It stops on a raw value that breaks the rule of [block()].
#' @return One string.
#' @seealso [block()], [show_block()].
#' @export
txt <- function(name, row = 1, col = NULL, version = NULL, home = NULL) {
  if (!is_formatter_name(name)) {
    stop("`name` must be one string, the name of a block.", call. = FALSE)
  }
  if (!is_count(row)) {
    stop("`row` must be one whole number of 1 or more.", call. = FALSE)
  }
  where <- txt_where(version, home)
  found <- txt_block(name, where$version, where$home)
  from <- blocks_source(where$version, where$home)
  file <- from$file
  saved <- from$values
  if (!name %in% names(saved)) {
    stop(
      file,
      " holds no block \"",
      name,
      "\". Write it again with pbn::update_blocks().",
      call. = FALSE
    )
  }
  value <- format_block(name, found$block, saved[[name]], found$formatters)
  return(txt_cell(value, name, row, col))
}

#' Write blocks.qs2 again from the saved results
#'
#' `update_blocks()` evaluates the blocks on the saved results of each
#' scenario of the version, and writes `blocks.qs2` of each one. It runs no
#' analysis.
#'
#' @inheritParams show_blocks
#' @details
#' It does what step 7 of [save_results()] does, for each folder
#' `<version>_results/<scenario>/` that holds a `results.qs2`. In a one-file
#' project, the folder of `"real"` is `fake/`, and `update_blocks()` does
#' not change `real/`, the results of the server. See the section The
#' folder fake/ of [initialize_project()]. It creates
#' `blocks.qs2` where it is absent. Call it after a change of the raw value
#' of a block, or to create the files without a run. A change of a
#' formatter needs no call, because [show_block()] and [txt()] format at
#' each call. `blocks.qs2` holds the raw value of each block.
#'
#' It needs [initialize_project()] of `version` in this R session. It stops
#' as [save_results()] does on a block that fails or that breaks the rule of
#' [block()]. Then it writes no file.
#' @return Invisibly, the names of the scenarios that it wrote.
#' @seealso [block()], [show_block()].
#' @export
update_blocks <- function(version = NULL, home = NULL) {
  if (is.null(state$results)) {
    stop(
      "Call pbn::initialize_project() before pbn::update_blocks(). The ",
      "blocks come from the functions folder that it sources.",
      call. = FALSE
    )
  }
  if (is.null(version)) {
    version <- state$version
  }
  check_version(version)
  if (!identical(version, state$version)) {
    stop(
      "The blocks in this R session are those of ",
      state$version,
      ", not of ",
      version,
      ". Call pbn::initialize_project() for ",
      version,
      " first.",
      call. = FALSE
    )
  }
  if (!is.null(home) && !same_folder(home, state$home)) {
    stop(
      "The blocks in this R session are those of ",
      state$home,
      ". Call pbn::initialize_project() for ",
      home,
      " first.",
      call. = FALSE
    )
  }
  blocks <- get0("blocks", envir = state$env, inherits = FALSE)
  check_blocks(blocks, state$version)
  dirs <- scenario_dirs(state)
  scenarios <- names(dirs)[file.exists(file.path(dirs, "results.qs2"))]
  if (length(scenarios) == 0) {
    stop(
      "There are no saved results in ",
      state$results,
      ". Run the project first.",
      call. = FALSE
    )
  }
  save_blocks(scenarios)
  cat(
    "pbn: wrote blocks.qs2 for ",
    paste(scenarios, collapse = ", "),
    ".\n",
    sep = ""
  )
  return(invisible(scenarios))
}

# The house formatters, by name. Each is a function of the raw value of a
# block, a data.frame, and returns one string for each row.
house_formatters <- function() {
  return(list(
    ci = format_ci,
    p = format_p_value,
    effect = format_effect,
    n_pct = format_n_pct,
    mean_sd = format_mean_sd
  ))
}

# The formatter `name`: the function `name` of the environment `env` of the
# project formatters when it has one, and else the house formatter. `env` is
# the value of formatters_of(), or NULL for the house formatters only.
# Stops on a name with no formatter. Phase 6 of the checklist work calls it
# to format the blocks of a checklist item.
find_formatter <- function(name, env = NULL) {
  if (!is_formatter_name(name)) {
    stop("A formatter is named by one string.", call. = FALSE)
  }
  if (is.environment(env)) {
    own <- get0(name, envir = env, mode = "function", inherits = FALSE)
    if (!is.null(own)) {
      return(own)
    }
  }
  house <- house_formatters()
  if (name %in% names(house)) {
    return(house[[name]])
  }
  stop(
    "There is no formatter ",
    name,
    ". The house formatters are ",
    paste(names(house), collapse = ", "),
    ". A project formatter is a function in vNN_functions/formatters.R.",
    call. = FALSE
  )
}

# The file of the project formatters in the functions folder of `version`,
# relative to the project folder.
formatters_file <- function(version) {
  return(paste0(version, "_functions/formatters.R"))
}

# The environment of the project formatters for show_block() and txt(). In
# the run of the file of to_one_file() there is no functions folder, so it
# is the environment `env` of the sourced code. Else it is a new
# environment with the functions of formatters_file() of `version` in
# `home`, read at this call, so that a change of a formatter shows without
# a new initialize_project(). The rule blocks_list makes the two the same
# for a project that passes it.
formatters_of <- function(home, version, env = NULL, generated = FALSE) {
  if (generated) {
    return(env)
  }
  return(formatter_env(file.path(home, formatters_file(version))))
}

# A new environment with the functions that the file `file` assigns at its
# top level, evaluated there and in no other place. Its parent is the base
# environment, so a formatter names the package of each other function.
# Empty when the file does not exist. Stops when the file does not parse or
# holds a top-level expression that is not an assignment of a name to
# function(...).
formatter_env <- function(file) {
  env <- new.env(parent = baseenv())
  if (!file.exists(file)) {
    return(env)
  }
  exprs <- tryCatch(
    parse(file, keep.source = FALSE, encoding = "UTF-8"),
    error = function(e) {
      stop(
        file,
        " does not parse: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  for (e in exprs) {
    if (!is_function_assignment(e) || !as.character(e[[1]]) %in% c("<-", "=")) {
      stop(
        file,
        " holds ",
        cut_code(e),
        ". It MUST hold only assignments of a name to function(...).",
        call. = FALSE
      )
    }
    eval(e, envir = env)
  }
  return(env)
}

# The formatted value of the block `block`, a pbn_block with the name
# `name`, for its raw value `value`, with the project formatters `env`.
# Stops on a raw value that breaks the rule of block(), on a formatter that
# stops, and on a formatted value of the wrong shape. Each error names the
# block.
format_block <- function(name, block, value, env) {
  problem <- raw_value_problem(value)
  if (!is.null(problem)) {
    stop("The block \"", name, "\" ", problem, call. = FALSE)
  }
  fn <- find_formatter(block$format, env)
  out <- tryCatch(fn(value), error = function(e) {
    stop(
      "The formatter ",
      block$format,
      " of the block \"",
      name,
      "\" failed: ",
      conditionMessage(e),
      call. = FALSE
    )
  })
  if (is.character(out) && length(out) == nrow(value)) {
    return(out)
  }
  if (is.data.frame(out)) {
    return(out)
  }
  stop(
    "The formatter ",
    block$format,
    " of the block \"",
    name,
    "\" MUST return a data.frame, or a character vector with one string ",
    "for each of the ",
    nrow(value),
    " rows of the raw value.",
    call. = FALSE
  )
}

# The patterns of a formatted number in a label cell of a raw value.
formatted_number_patterns <- c(
  "[0-9][.,][0-9]",
  "[0-9]%",
  "[0-9] \\(",
  "[=<>] ?[0-9]"
)

# What is wrong with the raw value `value` of a pbn_block, as the end of a
# sentence that starts with the block, or NULL. A raw value is a data.frame
# or a data.table. In a character or a factor column, no cell matches a
# pattern of formatted_number_patterns. The text names the column, the row
# and the cell of the first match, column by column.
raw_value_problem <- function(value) {
  if (!is.data.frame(value)) {
    return(paste0(
      "returned an object of the class ",
      class(value)[1],
      ", and a block of pbn::block() MUST return a data.frame or a ",
      "data.table of numbers"
    ))
  }
  for (col in names(value)) {
    cells <- value[[col]]
    if (!is.character(cells) && !is.factor(cells)) {
      next
    }
    cells <- as.character(cells)
    hit <- rep(FALSE, length(cells))
    for (pattern in formatted_number_patterns) {
      hit <- hit | grepl(pattern, cells)
    }
    hit[is.na(cells)] <- FALSE
    if (any(hit)) {
      i <- which(hit)[1]
      return(paste0(
        "holds a formatted number in the column ",
        col,
        ", row ",
        i,
        ": ",
        encodeString(cells[i], quote = "\""),
        ". A block of pbn::block() returns numbers, and its formatter ",
        "formats them"
      ))
    }
  }
  return(NULL)
}

# TRUE when `x` is one string that is not NA and not empty.
is_formatter_name <- function(x) {
  return(is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x))
}

# The pattern of one checklist tag "<id>:<item>". The id is that of the
# checklist YAML schema, and the item holds no space and no colon.
tag_pattern <- "^[a-z0-9-]+:[^[:space:]:]+$"

# What is wrong with the items `items` of block(), as the end of a sentence
# that starts with "`items`", or NULL. NULL is wrong: no item is
# character(0).
items_problem <- function(items) {
  if (!is.character(items) || !is.null(attributes(items))) {
    return(
      "must be a character vector of \"<id>:<item>\" strings, without names"
    )
  }
  bad <- is.na(items) | !grepl(tag_pattern, items)
  if (any(bad)) {
    return(paste0(
      "must be \"<id>:<item>\" strings, such as \"strobe:13a\", and ",
      if (is.na(items[bad][1])) {
        "NA"
      } else {
        encodeString(items[bad][1], quote = "\"")
      },
      " is not"
    ))
  }
  if (anyDuplicated(items) > 0) {
    return(paste0(
      "must name each item once, and ",
      encodeString(items[duplicated(items)][1], quote = "\""),
      " comes more than once"
    ))
  }
  return(NULL)
}

# The checklist tags `tags`, "<id>:<item>" strings that pass items_problem(),
# as a data.frame with the columns `id` and `item`.
parse_tags <- function(tags) {
  return(data.frame(
    id = sub(":.*$", "", tags),
    item = sub("^[^:]*:", "", tags),
    stringsAsFactors = FALSE
  ))
}

# The numbers `x` with `digits` decimals, one digits for each element or
# one for all, and the minus sign U+2212 for a negative number. A number
# that rounds to zero has no sign. `big` is the thousands mark. NA stays NA.
format_decimals <- function(x, digits, big = "") {
  digits <- rep_len(digits, length(x))
  out <- vapply(
    seq_along(x),
    function(i) {
      if (is.na(x[i]) || is.na(digits[i])) {
        return(NA_character_)
      }
      r <- round(x[i], digits[i])
      if (r == 0) {
        r <- 0
      }
      return(formatC(r, format = "f", digits = digits[i], big.mark = big))
    },
    character(1)
  )
  # intToUtf8() keeps the file ASCII, for R CMD check.
  return(sub("^-", intToUtf8(0x2212L), out))
}

# The numeric column `col` of the raw value `x` of the formatter `name`.
# Stops when it is missing or not numeric.
formatter_column <- function(x, col, name) {
  if (!is.data.frame(x)) {
    stop(
      "the formatter ",
      name,
      " needs a data.frame, not an object of the class ",
      class(x)[1],
      call. = FALSE
    )
  }
  if (!col %in% names(x)) {
    stop(
      "the formatter ",
      name,
      " needs the column ",
      col,
      ", and the value has the columns ",
      paste(names(x), collapse = ", "),
      call. = FALSE
    )
  }
  v <- x[[col]]
  if (!is.numeric(v)) {
    stop(
      "the column ",
      col,
      " of the formatter ",
      name,
      " MUST be numeric",
      call. = FALSE
    )
  }
  return(as.numeric(v))
}

# The decimals of the formatter `name` for each row of `x`: the column
# digits, whole numbers of 0 or more, or `default`.
formatter_digits <- function(x, name, default) {
  if (!"digits" %in% names(x)) {
    return(rep(default, nrow(x)))
  }
  d <- formatter_column(x, "digits", name)
  if (any(!is.na(d) & (d < 0 | d != round(d)))) {
    stop(
      "the column digits of the formatter ",
      name,
      " MUST hold whole numbers of 0 or more",
      call. = FALSE
    )
  }
  return(as.integer(d))
}

# The text of each row of `x` with `est`, `lo` and `hi` as
# "<est> (95% CI <lo> to <hi>)", each with the decimals `digits`.
ci_text <- function(x, name, digits) {
  est <- format_decimals(formatter_column(x, "est", name), digits)
  lo <- format_decimals(formatter_column(x, "lo", name), digits)
  hi <- format_decimals(formatter_column(x, "hi", name), digits)
  out <- paste0(est, " (95% CI ", lo, " to ", hi, ")")
  out[is.na(est) | is.na(lo) | is.na(hi)] <- NA_character_
  return(out)
}

# The house formatter ci: "0.80 (95% CI 0.46 to 1.16)".
format_ci <- function(x) {
  return(ci_text(x, "ci", formatter_digits(x, "ci", 2L)))
}

# The p-values of the column p of `x` as "p < 0.001", or "p = " and 3
# decimals.
p_text <- function(x, name) {
  p <- formatter_column(x, "p", name)
  out <- paste0("p = ", format_decimals(p, 3L))
  out[!is.na(p) & p < 0.001] <- "p < 0.001"
  out[is.na(p)] <- NA_character_
  return(out)
}

# The house formatter p: "p < 0.001" or "p = 0.047".
format_p_value <- function(x) {
  return(p_text(x, "p"))
}

# The house formatter effect: "(0.80 points, 95% CI 0.46 to 1.16, p <
# 0.001)". The optional column unit follows the estimate. An NA or an empty
# unit gives none.
format_effect <- function(x) {
  digits <- formatter_digits(x, "effect", 2L)
  est <- format_decimals(formatter_column(x, "est", "effect"), digits)
  lo <- format_decimals(formatter_column(x, "lo", "effect"), digits)
  hi <- format_decimals(formatter_column(x, "hi", "effect"), digits)
  p <- p_text(x, "effect")
  unit <- rep("", nrow(x))
  if ("unit" %in% names(x)) {
    u <- x[["unit"]]
    if (!is.character(u) && !is.factor(u)) {
      stop(
        "the column unit of the formatter effect MUST be text",
        call. = FALSE
      )
    }
    u <- as.character(u)
    has <- !is.na(u) & nzchar(u)
    unit[has] <- paste0(" ", u[has])
  }
  out <- paste0("(", est, unit, ", 95% CI ", lo, " to ", hi, ", ", p, ")")
  out[is.na(est) | is.na(lo) | is.na(hi) | is.na(p)] <- NA_character_
  return(out)
}

# The house formatter n_pct: "1,037 (68.7%)", the count n with the
# thousands mark, and 100 n / total.
format_n_pct <- function(x) {
  digits <- formatter_digits(x, "n_pct", 1L)
  n <- formatter_column(x, "n", "n_pct")
  total <- formatter_column(x, "total", "n_pct")
  count <- format_decimals(n, 0L, big = ",")
  pct <- format_decimals(100 * n / total, digits)
  out <- paste0(count, " (", pct, "%)")
  out[is.na(count) | is.na(pct)] <- NA_character_
  return(out)
}

# The house formatter mean_sd: "30.7 (4.8)".
format_mean_sd <- function(x) {
  digits <- formatter_digits(x, "mean_sd", 1L)
  m <- format_decimals(formatter_column(x, "mean", "mean_sd"), digits)
  s <- format_decimals(formatter_column(x, "sd", "mean_sd"), digits)
  out <- paste0(m, " (", s, ")")
  out[is.na(m) | is.na(s)] <- NA_character_
  return(out)
}

# The version and the project folder of txt(): `version`, or that of the
# last initialize_project(), and `home`, or that of the last
# initialize_project() of the version, or the working folder.
txt_where <- function(version, home) {
  if (is.null(version)) {
    if (is.null(state$results)) {
      stop(
        "Give pbn::txt() the version, as in version = \"v01\", or call ",
        "pbn::initialize_project() first.",
        call. = FALSE
      )
    }
    version <- state$version
  }
  check_version(version)
  if (is.null(home)) {
    home <- if (identical(version, state$version)) state$home else "."
  }
  if (!is_one_string(home) || !dir.exists(home)) {
    stop("`home` must be the path of one existing folder.", call. = FALSE)
  }
  return(list(version = version, home = home))
}

# TRUE when the folders `a` and `b` are one folder.
same_folder <- function(a, b) {
  if (!is_one_string(a) || !is_one_string(b)) {
    return(FALSE)
  }
  norm <- function(x) {
    return(normalizePath(x, winslash = "/", mustWork = FALSE))
  }
  return(identical(norm(a), norm(b)))
}

# The block `name` of txt() and the project formatters: from this R session
# after initialize_project() of `version` and `home`, and else from the
# files blocks.R and formatters.R, read without running blocks.R.
txt_block <- function(name, version, home) {
  if (
    !is.null(state$results) &&
      identical(version, state$version) &&
      same_folder(home, state$home)
  ) {
    blocks <- get0("blocks", envir = state$env, inherits = FALSE)
    check_blocks(blocks, state$version)
    if (!name %in% names(blocks)) {
      stop("There is no block \"", name, "\" in ", version, ".", call. = FALSE)
    }
    return(list(
      block = txt_pbn_block(blocks[[name]], name),
      formatters = formatters_of(
        state$home,
        version,
        state$env,
        isTRUE(state$generated)
      )
    ))
  }
  file <- file.path(home, blocks_file(version))
  elements <- blocks_elements_of_file(file)
  if (is.null(elements)) {
    stop(
      "pbn::txt() cannot read the list blocks of ",
      file,
      ". It needs one assignment blocks <- list(...) in a file that parses.",
      call. = FALSE
    )
  }
  if (!name %in% names(elements)) {
    stop("There is no block \"", name, "\" in ", file, ".", call. = FALSE)
  }
  e <- elements[[name]]
  if (!e$is_block || !is_formatter_name(e$format)) {
    stop(
      "The block \"",
      name,
      "\" of ",
      file,
      " is not a call pbn::block() with a string format.",
      call. = FALSE
    )
  }
  return(list(
    block = list(format = e$format),
    formatters = formatters_of(home, version)
  ))
}

# The block `b` with the name `name` when it is a pbn_block, and else a
# stop: txt() formats with the formatter of a pbn_block only.
txt_pbn_block <- function(b, name) {
  if (!inherits(b, "pbn_block")) {
    stop(
      "The block \"",
      name,
      "\" is a function and not pbn::block(), so it names no formatter.",
      call. = FALSE
    )
  }
  return(b)
}

# The cell of the row `row` and the column `col` of the formatted value
# `value` of the block `name`, as txt() returns it.
txt_cell <- function(value, name, row, col) {
  n <- if (is.data.frame(value)) nrow(value) else length(value)
  if (row > n) {
    stop(
      "The block \"",
      name,
      "\" has ",
      n,
      if (n == 1) " row" else " rows",
      ", so it has no row ",
      row,
      ".",
      call. = FALSE
    )
  }
  if (is.data.frame(value)) {
    if (is.null(col)) {
      if (ncol(value) != 1) {
        stop(
          "The formatter of the block \"",
          name,
          "\" returns ",
          ncol(value),
          " columns. Give the column as col.",
          call. = FALSE
        )
      }
      col <- 1L
    }
    ok <- (is_one_string(col) && col %in% names(value)) ||
      (is_count(col) && col <= ncol(value))
    if (!ok) {
      stop(
        "The formatted value of the block \"",
        name,
        "\" has no column ",
        format(col),
        ".",
        call. = FALSE
      )
    }
    value <- value[[col]]
  } else if (!is.null(col)) {
    stop(
      "The formatter of the block \"",
      name,
      "\" returns a character vector, so col MUST be NULL.",
      call. = FALSE
    )
  }
  cell <- value[[row]]
  if (!is_one_string(cell)) {
    stop(
      "The cell of row ",
      row,
      " of the block \"",
      name,
      "\" is not a string. A formatter formats each number as a string.",
      call. = FALSE
    )
  }
  return(cell)
}

# The elements of the list `blocks` that the file `file`, a blocks.R,
# assigns once at its top level, read without running code: a named list
# with one element for each block. Each holds `call`, the code of the
# element; `is_block`, TRUE for a call pbn::block(); `args`, the arguments
# of that call matched to block(), or NULL when they do not match; `format`,
# the string format or NULL; and `items`, the strings of a literal items, or
# NULL when items is not a literal c() of strings. NULL when the file does
# not parse or does not assign blocks to list(...) exactly once. Phase 6 of
# the checklist work reads the items of each block here.
blocks_elements_of_file <- function(file) {
  exprs <- if (file.exists(file)) parse_or_null(file) else NULL
  if (is.null(exprs)) {
    return(NULL)
  }
  found <- Filter(function(e) return(is_assignment_to(e, "blocks")), exprs)
  if (length(found) != 1) {
    return(NULL)
  }
  value <- found[[1]][[3]]
  if (!is.call(value) || !identical(value[[1]], as.name("list"))) {
    return(NULL)
  }
  parts <- as.list(value)[-1]
  if (length(parts) == 0 || !is.null(blocks_names_problem(names(parts)))) {
    return(NULL)
  }
  return(lapply(parts, block_element))
}

# The parts of the element `x` of the list blocks, as
# blocks_elements_of_file() returns them.
block_element <- function(x) {
  out <- list(
    call = x,
    is_block = is_block_call_of_pbn(x),
    args = NULL,
    format = NULL,
    items = NULL
  )
  if (!out$is_block) {
    return(out)
  }
  args <- tryCatch(
    as.list(match.call(block, x))[-1],
    error = function(e) return(NULL)
  )
  out["args"] <- list(args)
  if (is.null(args)) {
    return(out)
  }
  if (is_formatter_name(args[["format"]])) {
    out$format <- args[["format"]]
  }
  # An argument items = NULL is no literal c(), so it is not taken as absent.
  out["items"] <- list(
    if ("items" %in% names(args)) {
      literal_items(args[["items"]])
    } else {
      character(0)
    }
  )
  return(out)
}

# TRUE when `x` is a call of pbn::block().
is_block_call_of_pbn <- function(x) {
  return(is.call(x) && identical(x[[1]], quote(pbn::block)))
}

# The strings of the code `x` of the argument items of pbn::block(): the
# strings of a call c() whose arguments are each one string without a name,
# and else NULL. c() gives an empty vector.
literal_items <- function(x) {
  if (!is.call(x) || !identical(x[[1]], as.name("c"))) {
    return(NULL)
  }
  parts <- as.list(x)[-1]
  if (!is.null(names(parts)) && any(nzchar(names(parts)))) {
    return(NULL)
  }
  strings <- vapply(
    parts,
    function(p) return(is.character(p) && length(p) == 1 && !is.na(p)),
    logical(1)
  )
  if (!all(strings)) {
    return(NULL)
  }
  return(as.character(unlist(parts)))
}
