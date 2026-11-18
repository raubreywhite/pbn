# The md5 of the run script in manifest$source_md5. It leaves out the text
# that cannot change the results and that a project adds for the file of
# to_one_file(): the paths of the server, and the commented calls of
# to_one_file() and check_one_file(). So the results of a run before such an
# edit stay current. The section The md5 of the run script of
# initialize_project() gives the rule.

# A comment that only calls to_one_file() or check_one_file() with no
# argument, with or without pbn::, for example "# pbn::to_one_file()". The
# run script holds such a line, and the file of to_one_file() leaves it out.
one_file_comment_pattern <- paste0(
  "^#+[[:space:]]*(pbn[[:space:]]*::[[:space:]]*)?",
  "(to_one_file|check_one_file)[[:space:]]*\\([[:space:]]*\\)[[:space:]]*$"
)

# The arguments of pbn::initialize_project() with the paths of the server.
server_path_args <- c("one_file_home", "one_file_folders")

# The md5 of the run script `path` for manifest$source_md5. `raw` is the md5
# of its bytes. It is the md5 of the bytes without the text of
# run_script_skips(), and `raw` when there is no such text.
run_script_md5 <- function(path, raw) {
  bytes <- readBin(path, "raw", file.size(path))
  kept <- run_script_kept(bytes)
  if (is.null(kept)) {
    return(raw)
  }
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  writeBin(charToRaw(kept), file)
  return(unname(tools::md5sum(file)))
}

# The text of the bytes `bytes` of a run script without the ranges of
# run_script_skips(), or NULL when there is no such range. NULL also for a
# file that holds a NUL, that is not UTF-8 or that does not parse.
run_script_kept <- function(bytes) {
  if (length(bytes) == 0 || any(bytes == as.raw(0))) {
    return(NULL)
  }
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    return(NULL)
  }
  # Each line but the last ends in LF in `text`. strsplit() drops the empty
  # line after a final LF, so it comes back here.
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (endsWith(text, "\n")) {
    lines <- c(lines, "")
  }
  skips <- run_script_skips(lines)
  if (is.null(skips) || nrow(skips) == 0) {
    return(NULL)
  }
  # The place of the first character of each line in `text`.
  starts <- cumsum(c(1L, nchar(lines, type = "chars") + 1L))
  from <- starts[skips$line1] + skips$char1 - 1L
  to <- starts[skips$line2] + skips$char2 - 1L
  keep_from <- c(1L, to + 1L)
  keep_to <- c(from - 1L, nchar(text, type = "chars"))
  return(paste(substring(text, keep_from, keep_to), collapse = ""))
}

# The ranges that the md5 of the run script with the lines `lines` leaves
# out, as a data.frame with `line1`, `char1`, `line2` and `char2`, sorted and
# without overlaps. Each line but the last ends in LF in the file, and a line
# MAY end in CR. NULL when the lines do not parse.
# These are the ranges:
# - each argument one_file_home and one_file_folders of a top-level call
#   pbn::initialize_project(), from the comma before its name to the last
#   character of its value. An argument without a comma before it stays.
# - each line that holds only a comment of one_file_comment_pattern, with
#   its LF.
run_script_skips <- function(lines) {
  code <- sub("\r$", "", lines)
  exprs <- tryCatch(
    parse(text = code, keep.source = TRUE),
    error = function(e) return(NULL)
  )
  tokens <- if (is.null(exprs)) NULL else utils::getParseData(exprs)
  if (is.null(tokens)) {
    return(NULL)
  }
  args <- server_arg_ranges(exprs, tokens, code)
  comments <- one_file_comment_lines(tokens)
  # A comment line inside the range of an argument goes with it.
  inside <- vapply(
    comments,
    function(n) return(any(args$line1 < n & args$line2 > n)),
    logical(1)
  )
  comments <- comments[!inside]
  last <- length(lines)
  out <- rbind(
    args,
    data.frame(
      line1 = comments,
      char1 = rep(1L, length(comments)),
      # The LF of the line. The last line has no LF.
      line2 = comments,
      char2 = nchar(lines[comments], type = "chars") +
        as.integer(comments < last)
    )
  )
  return(out[order(out$line1, out$char1), , drop = FALSE])
}

# The ranges of the arguments one_file_home and one_file_folders of each
# top-level call pbn::initialize_project() of the expressions `exprs`, with
# the parse data `tokens` of the lines `code`. See run_script_skips().
server_arg_ranges <- function(exprs, tokens, code) {
  refs <- attr(exprs, "srcref")
  out <- list(data.frame(
    line1 = integer(0),
    char1 = integer(0),
    line2 = integer(0),
    char2 = integer(0)
  ))
  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    if (is.call(e) && identical(e[[1]], quote(pbn::initialize_project))) {
      kids <- call_children(tokens, as.integer(refs[[i]]))
      out <- c(out, list(kid_arg_ranges(kids, code, tokens)))
    }
  }
  return(do.call(rbind, out))
}

# The children in the parse data `tokens` of the top-level expression with
# the source reference `full`, in the order of the file.
call_children <- function(tokens, full) {
  id <- tokens$id[
    tokens$parent == 0 &
      tokens$line1 == full[1] &
      tokens$col1 == full[5] &
      tokens$line2 == full[3] &
      tokens$col2 == full[6]
  ]
  kids <- tokens[tokens$parent %in% id, , drop = FALSE]
  return(kids[order(kids$line1, kids$col1), , drop = FALSE])
}

# The ranges of the arguments one_file_home and one_file_folders among the
# children `kids` of one call, in the lines `code` with the parse data
# `tokens`: from the comma before the name to the end of the value. An
# argument without a comma before it, or with a value that is not literal
# strings, see is_literal_paths(), gives no range. Such a value is code that
# can change the results, so it stays in the md5.
kid_arg_ranges <- function(kids, code, tokens) {
  hits <- which(kids$token == "SYMBOL_SUB" & kids$text %in% server_path_args)
  ok <- vapply(
    hits,
    function(k) return(is_skipped_arg(kids, k, tokens)),
    logical(1)
  )
  hits <- hits[ok]
  return(data.frame(
    line1 = kids$line1[hits - 1],
    char1 = vapply(
      hits - 1,
      function(k) return(col_to_byte(code[kids$line1[k]], kids$col1[k])),
      integer(1)
    ),
    line2 = kids$line2[hits + 2],
    char2 = vapply(
      hits + 2,
      function(k) return(col_to_byte(code[kids$line2[k]], kids$col2[k])),
      integer(1)
    )
  ))
}

# The children of the node `id` of the parse data `tokens`, without
# comments, in the order of the file.
node_children <- function(tokens, id) {
  kids <- tokens[
    tokens$parent == id & tokens$token != "COMMENT",
    ,
    drop = FALSE
  ]
  return(kids[order(kids$line1, kids$col1), , drop = FALSE])
}

# TRUE when the node `id` of the parse data `tokens` is one string constant.
is_string_node <- function(tokens, id) {
  kids <- node_children(tokens, id)
  return(nrow(kids) == 1 && kids$token == "STR_CONST")
}

# TRUE when the name of an argument at the place `k` of the children `kids`
# of a call has a comma before it, then = and a value of literal strings in
# the parse data `tokens`.
is_skipped_arg <- function(kids, k, tokens) {
  around <- kids$token[k + c(-1, 1, 2)]
  if (k < 2 || anyNA(around)) {
    return(FALSE)
  }
  if (!identical(around, c("','", "EQ_SUB", "expr"))) {
    return(FALSE)
  }
  return(is_literal_paths(tokens, kids$id[k + 2], kids$text[k]))
}

# TRUE when the value node `id` of the argument `arg` is literal strings, as
# the parse data `tokens` gives it, without evaluating it: one string
# constant, or for one_file_folders also a call c(...) whose arguments are
# each one string constant, with or without a name.
is_literal_paths <- function(tokens, id, arg) {
  if (is_string_node(tokens, id)) {
    return(TRUE)
  }
  if (arg != "one_file_folders") {
    return(FALSE)
  }
  return(is_c_of_strings(tokens, id))
}

# TRUE when the node `id` of the parse data `tokens` is a call c(...) whose
# arguments are each one string constant, with or without a name.
is_c_of_strings <- function(tokens, id) {
  kids <- node_children(tokens, id)
  n <- nrow(kids)
  if (n < 4 || kids$token[2] != "'('" || kids$token[n] != "')'") {
    return(FALSE)
  }
  fn <- node_children(tokens, kids$id[1])
  is_c <- nrow(fn) == 1 &&
    fn$token == "SYMBOL_FUNCTION_CALL" &&
    fn$text == "c"
  if (!is_c) {
    return(FALSE)
  }
  # The arguments, split at the commas: each is a string, or a name, = and
  # a string.
  inner <- kids[3:(n - 1), , drop = FALSE]
  group <- cumsum(inner$token == "','")
  parts <- split(
    inner[inner$token != "','", , drop = FALSE],
    group[inner$token != "','"]
  )
  if (length(parts) != max(group) + 1) {
    return(FALSE)
  }
  ok <- vapply(
    parts,
    function(a) return(is_string_arg(tokens, a)),
    logical(1)
  )
  return(all(ok))
}

# TRUE when the tokens `a` of one argument of a call are a string constant,
# or a name, = and a string constant, in the parse data `tokens`.
is_string_arg <- function(tokens, a) {
  shape <- paste(a$token, collapse = " ")
  forms <- c("expr", "SYMBOL_SUB EQ_SUB expr", "STR_CONST EQ_SUB expr")
  return(shape %in% forms && is_string_node(tokens, a$id[nrow(a)]))
}

# The lines of the parse data `tokens` that hold only a comment of
# one_file_comment_pattern, and no other token.
one_file_comment_lines <- function(tokens) {
  terminal <- tokens[tokens$terminal, , drop = FALSE]
  hit <- terminal[
    terminal$token == "COMMENT" &
      grepl(one_file_comment_pattern, terminal$text),
    ,
    drop = FALSE
  ]
  alone <- vapply(
    hit$line1,
    function(n) {
      return(sum(terminal$line1 <= n & terminal$line2 >= n) == 1)
    },
    logical(1)
  )
  return(sort(unique(hit$line1[alone])))
}
