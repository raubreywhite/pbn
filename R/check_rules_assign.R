# The functions that the rule no_local_assign fails on a call of, each with
# the package that a call MAY name with `::` or `:::`. A call of the same
# name from another package passes.
local_assign_calls <- c(
  local = "base",
  assign = "base",
  delayedAssign = "base",
  assignInNamespace = "utils",
  assignInMyNamespace = "utils"
)

# The operators that the rule no_local_assign fails on. `<-`, `->`, `=` and
# the `:=` of data.table pass.
local_assign_operators <- c("<<-", "->>")

# The constructs of the rule no_local_assign, as its parameter forbid and its
# findings write them: each call with (), and each operator.
local_assign_constructs <- function() {
  return(c(paste0(names(local_assign_calls), "()"), local_assign_operators))
}

# The detail of the PASS line of no_local_assign for the constructs
# `forbid`, in the order of local_assign_constructs().
local_assign_pass_detail <- function(forbid) {
  forbid <- intersect(local_assign_constructs(), forbid)
  calls <- sub("\\(\\)$", "", forbid[endsWith(forbid, "()")])
  ops <- forbid[!endsWith(forbid, "()")]
  parts <- c(
    if (length(calls) > 0) paste("calls", or_list(calls)),
    if (length(ops) > 0) paste("uses", or_list(ops))
  )
  return(paste("no .R file", paste(parts, collapse = ", or ")))
}

# The strings `x` as one list in prose: "a", "a or b", "a, b or c".
or_list <- function(x) {
  n <- length(x)
  if (n == 1) {
    return(x)
  }
  return(paste(paste(x[-n], collapse = ", "), "or", x[n]))
}

# The rule no_local_assign: no .R file calls a function of
# local_assign_calls or uses an operator of local_assign_operators, at any
# depth, when `forbid` names it. The constructs `forbid` come from the rules
# of the project, see read_rules(). It reads the files of loc_per_file. It parses each file and
# evaluates nothing. A file that R cannot parse fails loc_per_file, so this
# rule skips it.
rule_no_local_assign <- function(path, forbid) {
  rule <- "no_local_assign"
  entries <- project_entries(path)
  files <- entries[
    grepl("\\.[rR]$", entries) & !dir.exists(file.path(path, entries))
  ]
  out <- list()
  for (f in sort(files, method = "radix")) {
    hits <- local_assign_hits(file.path(path, f))
    hits <- hits[hits$construct %in% forbid, , drop = FALSE]
    if (nrow(hits) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        rep(f, nrow(hits)),
        paste0(f, ":", hits$line, ": ", hits$construct)
      )
    }
  }
  return(bind_rows(out))
}

# The uses of local_assign_calls and local_assign_operators in the file
# `file`, as a data.frame with the columns `line` and `construct`, in the
# order of the file. The parse data finds them, so a word in a comment or a
# string, a variable and an argument name do not count. Empty for a file
# that R cannot parse.
local_assign_hits <- function(file) {
  exprs <- parse_or_null(file, keep_source = TRUE)
  tokens <- utils::getParseData(exprs)
  if (is.null(tokens) || nrow(tokens) == 0) {
    return(data.frame(line = integer(0), construct = character(0)))
  }
  tokens <- tokens[order(tokens$line1, tokens$col1), , drop = FALSE]
  # A call can quote its name in backticks, and the parser keeps them.
  quoted <- tokens$token == "SYMBOL_FUNCTION_CALL"
  tokens$text[quoted] <- gsub("`", "", tokens$text[quoted], fixed = TRUE)
  is_call <- tokens$token == "SYMBOL_FUNCTION_CALL" &
    tokens$text %in% names(local_assign_calls)
  package <- vapply(
    which(is_call),
    function(i) return(call_package(tokens, i)),
    character(1)
  )
  own <- is.na(package) |
    package == local_assign_calls[tokens$text[is_call]]
  is_call[which(is_call)[!own]] <- FALSE
  is_operator <- tokens$token %in%
    c("LEFT_ASSIGN", "RIGHT_ASSIGN") &
    tokens$text %in% local_assign_operators
  hit <- is_call | is_operator
  return(data.frame(
    line = tokens$line1[hit],
    construct = ifelse(
      is_call[hit],
      paste0(tokens$text[hit], "()"),
      tokens$text[hit]
    )
  ))
}

# The package that the call at row `i` of the parse data `tokens` names with
# `::` or `:::`, or NA when it names none.
call_package <- function(tokens, i) {
  package <- tokens$text[
    tokens$parent == tokens$parent[i] & tokens$token == "SYMBOL_PACKAGE"
  ]
  if (length(package) == 0) {
    return(NA_character_)
  }
  return(package[[1]])
}
