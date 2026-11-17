# The rules of check_project() that read the code of the .R files of a
# project: the selection of the files, and the rules no_absolute_paths and
# blocks_list. Each rule parses each file and runs no code of it.

# The file of to_one_file() at the top level of a project.
one_file_pattern <- "^v[0-9]{2}_one_file\\.R$"

# The .R files that the code rules read, relative to `path`: each .R file of
# the project but the file of to_one_file(), which holds the paths of the
# server by design and which pbn writes.
code_files <- function(path) {
  entries <- project_entries(path)
  keep <- grepl("\\.[rR]$", entries) &
    !grepl(one_file_pattern, entries) &
    !dir.exists(file.path(path, entries))
  return(entries[keep])
}

# The functions whose arguments MAY hold an absolute path: the places where a
# project declares its paths.
path_calls <- c("initialize_project", "to_one_file")

# TRUE for each string of `x` that is an absolute path:
# - "/" and a name of letters, digits, ".", "_" or "-" that starts with a
#   letter or "_", and then "/" again, for example "/home/a" or "/data/",
# - "~/" or "~\", the home folder,
# - a drive letter, ":" and "/" or "\", for example "C:/", "C:\" or "N:/",
# - two "\" and a letter or a digit, the start of a UNC path such as
#   "\\server\share".
# A lone "/" or "~", a regular expression such as "/$" or "/.*$", a URL, a
# format such as "%s/%s" or "%Y/%m/%d", a fraction such as "1/2", and a
# relative path such as "data/x.csv", "./x" or "../x" are not.
is_absolute_path <- function(x) {
  return(
    grepl("^/[A-Za-z_][A-Za-z0-9._-]*/", x) |
      grepl("^~[/\\\\]", x) |
      grepl("^[A-Za-z]:[/\\\\]", x) |
      grepl("^\\\\\\\\[A-Za-z0-9]", x)
  )
}

# The rule no_absolute_paths: no string constant of an .R file of
# code_files() is an absolute path, see is_absolute_path(), and no call of
# file.path() or fs::path() starts from a string constant. The arguments of
# a call of pbn::initialize_project() or pbn::to_one_file() are exempt. A
# file that R cannot parse fails loc_per_file, so this rule skips it.
rule_no_absolute_paths <- function(path) {
  rule <- "no_absolute_paths"
  out <- list()
  for (f in sort(code_files(path), method = "radix")) {
    hits <- absolute_path_hits(file.path(path, f))
    if (nrow(hits) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        rep(f, nrow(hits)),
        paste0(f, ":", hits$line, ": ", hits$shown)
      )
    }
  }
  return(bind_rows(out))
}

# The string constants of the file `file` that are absolute paths, outside
# the arguments of the calls of path_calls with the prefix pbn::, as a
# data.frame with the columns `line` and `shown`, the string in quotes cut at
# 40 characters, in the order of the file. The parse data finds them, so a
# comment does not count. A string that the parse data shortens, one of more
# than 1000 bytes, is skipped. Empty for a file that R cannot parse.
absolute_path_hits <- function(file) {
  none <- data.frame(line = integer(0), shown = character(0))
  exprs <- parse_or_null(file, keep_source = TRUE)
  tokens <- utils::getParseData(exprs)
  if (is.null(tokens) || nrow(tokens) == 0) {
    return(none)
  }
  tokens <- tokens[order(tokens$line1, tokens$col1), , drop = FALSE]
  strings <- tokens[tokens$token == "STR_CONST", , drop = FALSE]
  values <- vapply(strings$text, string_value, character(1), USE.NAMES = FALSE)
  hit <- !is.na(values) & is_absolute_path(values)
  exempt <- path_call_ids(tokens)
  parents <- stats::setNames(tokens$parent, tokens$id)
  inside <- vapply(
    strings$id,
    function(id) return(has_ancestor(id, parents, exempt)),
    logical(1)
  )
  hit <- hit & !inside
  found <- rbind(
    data.frame(
      line = strings$line1[hit],
      col = strings$col1[hit],
      shown = vapply(values[hit], shown_string, character(1), USE.NAMES = FALSE)
    ),
    literal_path_calls(tokens, exempt, parents)
  )
  found <- found[order(found$line, found$col), , drop = FALSE]
  return(data.frame(line = found$line, shown = found$shown))
}

# The calls of file.path(), base::file.path() and fs::path() in the parse
# data `tokens` whose first argument is a string constant, outside the
# calls `exempt`, as a data.frame with `line`, `col` and `shown`, such as
# file.path("data", ...). Such a path is absolute, or it depends on the
# working directory.
literal_path_calls <- function(tokens, exempt, parents) {
  out <- list(data.frame(line = integer(0), col = integer(0), shown = character(0)))
  rows <- which(
    tokens$token == "SYMBOL_FUNCTION_CALL" &
      tokens$text %in% c("file.path", "path")
  )
  for (i in rows) {
    pkg <- call_package(tokens, i)
    call_id <- tokens$parent[match(tokens$parent[i], tokens$id)]
    if (!is_path_call(tokens$text[i], pkg) || has_ancestor(call_id, parents, exempt)) {
      next
    }
    value <- first_string_argument(tokens, call_id)
    if (is.na(value)) {
      next
    }
    row <- match(call_id, tokens$id)
    name <- if (is.na(pkg)) tokens$text[i] else paste0(pkg, "::", tokens$text[i])
    out[[length(out) + 1]] <- data.frame(
      line = tokens$line1[row],
      col = tokens$col1[row],
      shown = paste0(name, "(", shown_string(value), ", ...)")
    )
  }
  return(do.call(rbind, out))
}

# TRUE for the function `name` of the package `pkg`, NA without one, when it
# is file.path() of base or path() of fs.
is_path_call <- function(name, pkg) {
  if (name == "file.path") {
    return(is.na(pkg) || pkg == "base")
  }
  return(identical(pkg, "fs"))
}

# The value of the first path argument of the call `call_id` of the parse
# data `tokens` when it is one string constant, or NA. The argument `fsep`
# is no part of the path, so it does not count.
first_string_argument <- function(tokens, call_id) {
  kids <- tokens[tokens$parent == call_id, , drop = FALSE]
  kids <- kids[order(kids$line1, kids$col1), , drop = FALSE]
  kids <- kids[-seq_len(which(kids$token == "'('")[1]), , drop = FALSE]
  group <- cumsum(kids$token == "','")
  for (g in unique(group)) {
    part <- kids[group == g & !kids$token %in% c("','", "')'"), , drop = FALSE]
    if (nrow(part) == 0 || identical(part$text[part$token == "SYMBOL_SUB"], "fsep")) {
      next
    }
    value <- part[part$token == "expr", , drop = FALSE]
    inner <- tokens[tokens$parent %in% value$id, , drop = FALSE]
    if (nrow(value) != 1 || nrow(inner) != 1 || inner$token != "STR_CONST") {
      return(NA_character_)
    }
    return(string_value(inner$text))
  }
  return(NA_character_)
}

# The value of the string constant `text` as the parser read it, or NA when
# it is not one string, for example a long string that the parse data
# shortens.
string_value <- function(text) {
  value <- tryCatch(str2lang(text), error = function(e) return(NULL))
  if (!is_one_string(value)) {
    return(NA_character_)
  }
  return(value)
}

# The string `x` in double quotes, cut at 40 characters, with "..." after
# the quotes when it is cut.
shown_string <- function(x) {
  if (nchar(x) <= 40) {
    return(encodeString(x, quote = "\""))
  }
  return(paste0(encodeString(substr(x, 1, 40), quote = "\""), "..."))
}

# The ids of the parse data `tokens` of each call of path_calls with the
# prefix pbn::. The name of the function and its package share a parent,
# the expression pbn::<name>, and the parent of that is the call.
path_call_ids <- function(tokens) {
  rows <- which(
    tokens$token == "SYMBOL_FUNCTION_CALL" & tokens$text %in% path_calls
  )
  rows <- rows[vapply(
    rows,
    function(i) return(identical(call_package(tokens, i), "pbn")),
    logical(1)
  )]
  inner <- tokens$parent[rows]
  return(tokens$parent[match(inner, tokens$id)])
}

# TRUE when one of the ids `ids` is an ancestor of the token `id`, with the
# parent of each token in `parents`, named by the id.
has_ancestor <- function(id, parents, ids) {
  if (length(ids) == 0) {
    return(FALSE)
  }
  up <- parents[[as.character(id)]]
  while (up > 0) {
    if (up %in% ids) {
      return(TRUE)
    }
    up <- parents[[as.character(up)]]
  }
  return(FALSE)
}

# The rule blocks_list: the functions folder MAY define the object `blocks`
# of show_block(). When it does, one top-level assignment of the folder
# assigns it, and its value is a call of list() in which each element has a
# name of its own, and each element is a function, or the name of a
# function that a top-level assignment of the folder defines.
rule_blocks_list <- function(path) {
  rule <- "blocks_list"
  found <- blocks_assignments(path)
  where <- found$where
  if (length(where) == 0) {
    return(project_row(rule, "FAIL", character(0), character(0)))
  }
  if (length(where) > 1) {
    return(project_row(
      rule,
      "FAIL",
      sub(":[0-9]+$", "", where[1]),
      paste0(
        "blocks is assigned ",
        length(where),
        " times, at ",
        paste(where, collapse = ", "),
        ". Assign it once"
      )
    ))
  }
  problem <- blocks_problem(found$values[[1]], found$functions)
  if (is.null(problem)) {
    return(project_row(rule, "FAIL", character(0), character(0)))
  }
  return(project_row(
    rule,
    "FAIL",
    sub(":[0-9]+$", "", where),
    paste0(where, ": ", problem)
  ))
}

# The top-level assignments of blocks in the files of the functions folders
# of `path`: `where`, each as <file>:<line>, `values`, the expression that
# each assigns, and `functions`, the names of the functions of the top-level
# assignments of those files.
blocks_assignments <- function(path) {
  out <- list(where = character(0), values = list(), functions = character(0))
  for (f in sort(functions_files(path), method = "radix")) {
    exprs <- parse_or_null(file.path(path, f), keep_source = TRUE)
    lines <- vapply(
      attr(exprs, "srcref"),
      function(r) return(as.integer(r[[1]])),
      integer(1)
    )
    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      if (is_function_assignment(e)) {
        out$functions <- c(out$functions, as.character(e[[2]]))
      }
      if (is_assignment_to(e, "blocks")) {
        out$where <- c(out$where, paste0(f, ":", lines[i]))
        out$values <- c(out$values, list(e[[3]]))
      }
    }
  }
  return(out)
}

# TRUE when `e` assigns to the name `name` with <-, = or <<-.
is_assignment_to <- function(e, name) {
  return(
    is.call(e) &&
      length(e) == 3 &&
      is.symbol(e[[1]]) &&
      as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
      identical(e[[2]], as.name(name))
  )
}

# What is wrong with the value `value` of blocks, or NULL. `functions` names
# the functions of the top-level assignments of the functions folder.
blocks_problem <- function(value, functions) {
  if (!is.call(value) || !identical(value[[1]], as.name("list"))) {
    return(paste(
      "blocks MUST be list(...), one function of results for each place in",
      "the manuscript"
    ))
  }
  parts <- as.list(value)[-1]
  if (length(parts) == 0) {
    return("blocks is an empty list")
  }
  nm <- names(parts)
  problem <- blocks_names_problem(nm)
  if (!is.null(problem)) {
    return(problem)
  }
  is_fn <- vapply(parts, is_block_function, logical(1), functions = functions)
  if (!all(is_fn)) {
    return(paste0(
      "the element ",
      encodeString(nm[!is_fn][1], quote = "\""),
      " of blocks is not a function, or the name of a function of the ",
      "functions folder"
    ))
  }
  return(NULL)
}

# What is wrong with the names `nm` of the elements of blocks, or NULL.
blocks_names_problem <- function(nm) {
  if (is.null(nm) || any(!nzchar(nm))) {
    i <- if (is.null(nm)) 1L else which(!nzchar(nm))[1]
    return(paste0("element ", i, " of blocks has no name"))
  }
  if (anyDuplicated(nm) > 0) {
    return(paste0(
      "the name ",
      encodeString(nm[duplicated(nm)][1], quote = "\""),
      " comes more than once in blocks"
    ))
  }
  return(NULL)
}

# TRUE when the element `x` of the list of blocks is a function, or the name
# of one of the functions `functions`.
is_block_function <- function(x, functions) {
  if (is.call(x)) {
    return(identical(x[[1]], as.name("function")))
  }
  return(is.symbol(x) && as.character(x) %in% functions)
}
