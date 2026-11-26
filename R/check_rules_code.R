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

# The functions whose arguments MAY hold an absolute path: the place where a
# project declares its paths, also those of the server.
path_calls <- "initialize_project"

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
# a call of pbn::initialize_project() are exempt. A
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
  out <- list(data.frame(
    line = integer(0),
    col = integer(0),
    shown = character(0)
  ))
  rows <- which(
    tokens$token == "SYMBOL_FUNCTION_CALL" &
      tokens$text %in% c("file.path", "path")
  )
  for (i in rows) {
    pkg <- call_package(tokens, i)
    call_id <- tokens$parent[match(tokens$parent[i], tokens$id)]
    if (
      !is_path_call(tokens$text[i], pkg) ||
        has_ancestor(call_id, parents, exempt)
    ) {
      next
    }
    value <- first_string_argument(tokens, call_id)
    if (is.na(value)) {
      next
    }
    row <- match(call_id, tokens$id)
    name <- if (is.na(pkg)) {
      tokens$text[i]
    } else {
      paste0(pkg, "::", tokens$text[i])
    }
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
    if (
      nrow(part) == 0 ||
        identical(part$text[part$token == "SYMBOL_SUB"], "fsep")
    ) {
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

# The rule blocks_list, for each functions folder vNN_functions/ of `path`:
# - only the file vNN_functions/blocks.R assigns `blocks`, at its top level,
#   and it assigns it once. pbn leaves that file out of manifest$source_md5,
#   so a change of the blocks does not make the results stale.
# - the value is a call of list() in which each element has a name of its
#   own, and each element is a function, or the name of a function that a
#   top-level assignment of the folder defines.
# - no other file of the folder and no vNN_run.R uses a name that blocks.R
#   assigns at its top level. The run then never depends on blocks.R.
# From the set house_2026_11_26 of `set`, also:
# - each element is a call pbn::block() whose format names a formatter and
#   whose items is a literal c() of "<id>:<item>" strings, see
#   pbn_block_problem().
# - vNN_functions/formatters.R has the shape and the name rules of blocks.R,
#   see formatters_file_problems().
# A folder without blocks.R and without blocks passes.
rule_blocks_list <- function(path, set = "house_2026_11_17") {
  rows <- list()
  for (folder in sort(functions_folders(path), method = "radix")) {
    found <- blocks_folder_problems(path, folder, set)
    rows <- c(
      rows,
      list(project_row(
        "blocks_list",
        "FAIL",
        sub(":[0-9]+$", "", found$where),
        paste0(found$where, ": ", found$problem, recycle0 = TRUE)
      ))
    )
  }
  return(bind_rows(rows))
}

# The problems of blocks_list in the functions folder `folder` of `path`
# under the rule set `set`: `where`, each as <file>:<line>, and `problem`,
# the text of each.
blocks_folder_problems <- function(path, folder, set = "house_2026_11_17") {
  home_file <- paste0(folder, "/blocks.R")
  found <- blocks_assignments(path, folder)
  where <- character(0)
  problem <- character(0)
  outside <- !startsWith(found$where, paste0(home_file, ":"))
  where <- c(where, found$where[outside])
  problem <- c(
    problem,
    rep(
      paste0(
        "blocks is assigned here. Assign it in ",
        home_file,
        " only, because pbn leaves that file out of manifest$source_md5"
      ),
      sum(outside)
    )
  )
  inside <- found$where[!outside]
  if (length(inside) > 1) {
    where <- c(where, inside[1])
    problem <- c(
      problem,
      paste0(
        "blocks is assigned ",
        length(inside),
        " times, at ",
        paste(inside, collapse = ", "),
        ". Assign it once"
      )
    )
  } else if (length(inside) == 1) {
    bad <- blocks_problem(
      found$values[!outside][[1]],
      found$functions,
      if (needs_pbn_blocks(set)) formatter_names_of(path, folder)
    )
    if (!is.null(bad)) {
      where <- c(where, inside)
      problem <- c(problem, bad)
    }
  }
  files <- functions_files(path)
  others <- setdiff(files[dirname(files) == folder], home_file)
  script <- sub("_functions$", "_run.R", folder)
  found <- blocks_file_problems(path, home_file, others, script)
  where <- c(where, found$where)
  problem <- c(problem, found$problem)
  if (needs_pbn_blocks(set)) {
    mine <- paste0(folder, "/formatters.R")
    found <- formatters_file_problems(
      path,
      mine,
      setdiff(others, mine),
      script
    )
    where <- c(where, found$where)
    problem <- c(problem, found$problem)
  }
  return(list(where = where, problem = problem))
}

# The names of the formatters of the functions folder `folder` of `path`,
# for the element checks of blocks_problem(): `own`, the names that
# formatters.R assigns at its top level, and `elsewhere`, the names that
# another file of the folder assigns at its top level.
formatter_names_of <- function(path, folder) {
  mine <- paste0(folder, "/formatters.R")
  files <- functions_files(path)
  others <- setdiff(files[dirname(files) == folder], mine)
  elsewhere <- unlist(lapply(others, function(f) {
    return(top_level_assignments(file.path(path, f))$name)
  }))
  return(list(
    own = blocks_file_names(file.path(path, mine)),
    elsewhere = unique(c(character(0), elsewhere))
  ))
}

# The problems of the file `home_file`, vNN_functions/formatters.R, of
# `path`, from the rule set house_2026_11_26. pbn leaves it out of
# manifest$source_md5, as blocks.R. `others` are the other .R files of its
# folder, without formatters.R, and `script` the run script. The value is
# that of blocks_file_problems(). The problems are:
# - a top-level expression other than an assignment with <- or = of a name
#   to function(...).
# - a name that formatters.R assigns and another file of the folder other
#   than blocks.R assigns too. blocks_list checks blocks.R against
#   formatters.R already.
# - a use of a name of formatters.R in a file other than blocks.R and
#   formatters.R, or in the run script.
formatters_file_problems <- function(path, home_file, others, script) {
  out <- list(where = character(0), problem = character(0), parsed = TRUE)
  file <- file.path(path, home_file)
  if (!file.exists(file)) {
    return(out)
  }
  exprs <- parse_or_null(file, keep_source = TRUE)
  if (is.null(exprs)) {
    out$parsed <- FALSE
    return(out)
  }
  lines <- vapply(
    attr(exprs, "srcref"),
    function(r) return(as.integer(r[[1]])),
    integer(1)
  )
  for (i in seq_along(exprs)) {
    if (!is_formatters_shape(exprs[[i]])) {
      out$where <- c(out$where, paste0(home_file, ":", lines[i]))
      out$problem <- c(
        out$problem,
        paste0(
          "the top level of formatters.R holds ",
          cut_code(exprs[[i]]),
          ". It MUST hold only assignments of a name to function(...), ",
          "because pbn leaves it out of manifest$source_md5"
        )
      )
    }
  }
  blocks <- paste0(dirname(home_file), "/blocks.R")
  twice <- blocks_redefinitions(
    path,
    exprs,
    lines,
    home_file,
    setdiff(others, blocks)
  )
  used <- blocks_names_used(path, home_file, c(script, setdiff(others, blocks)))
  out$where <- c(out$where, twice$where, used$where)
  out$problem <- c(out$problem, twice$problem, used$problem)
  return(out)
}

# TRUE when the top-level expression `e` of formatters.R assigns with <- or
# = a name to function(...).
is_formatters_shape <- function(e) {
  return(
    is_assignment_call(e) &&
      is.symbol(e[[2]]) &&
      !identical(e[[2]], as.name("blocks")) &&
      is_blocks_shape(e)
  )
}

# TRUE when the file vNN_functions/formatters.R of `version` in `home`
# cannot change the results under the rule set `set`: the set is
# house_2026_11_26 or newer, and the file does not exist, or it parses and
# has none of the problems of formatters_file_problems(). Under an older set
# it is FALSE, so the manifest holds the file as before.
formatters_file_inert <- function(home, version, set) {
  if (!needs_pbn_blocks(set)) {
    return(FALSE)
  }
  home_file <- formatters_file(version)
  if (!file.exists(file.path(home, home_file))) {
    return(TRUE)
  }
  others <- setdiff(
    sourcing_order(home, version),
    c(home_file, blocks_file(version))
  )
  script <- paste0(version, "_run.R")
  if (!file.exists(file.path(home, script))) {
    script <- character(0)
  }
  found <- formatters_file_problems(
    home,
    home_file,
    c(others, blocks_file(version)),
    script
  )
  return(found$parsed && length(found$where) == 0)
}

# The problems of the file `home_file`, vNN_functions/blocks.R, of `path`
# that let it change the results, although pbn leaves it out of
# manifest$source_md5. `others` are the other .R files of its folder and
# `script` the run script, relative to `path`. The value is a list:
# `where`, each as <file>:<line>, `problem`, and `parsed`, FALSE when
# blocks.R exists and does not parse. The problems are:
# - a top-level expression other than an assignment with <- or = of a name
#   to function(...), or of blocks to list(...).
# - a name that blocks.R assigns and another file of the folder assigns too.
# - a use of a name of blocks.R in another file or in the run script.
blocks_file_problems <- function(path, home_file, others, script) {
  out <- list(where = character(0), problem = character(0), parsed = TRUE)
  file <- file.path(path, home_file)
  if (!file.exists(file)) {
    return(out)
  }
  exprs <- parse_or_null(file, keep_source = TRUE)
  if (is.null(exprs)) {
    out$parsed <- FALSE
    return(out)
  }
  lines <- vapply(
    attr(exprs, "srcref"),
    function(r) return(as.integer(r[[1]])),
    integer(1)
  )
  for (i in seq_along(exprs)) {
    if (!is_blocks_shape(exprs[[i]])) {
      out$where <- c(out$where, paste0(home_file, ":", lines[i]))
      out$problem <- c(
        out$problem,
        paste0(
          "the top level of blocks.R holds ",
          cut_code(exprs[[i]]),
          ". It MUST hold only assignments of a name to function(...), and ",
          "blocks <- list(...), because pbn leaves it out of ",
          "manifest$source_md5"
        )
      )
    }
  }
  twice <- blocks_redefinitions(path, exprs, lines, home_file, others)
  used <- blocks_names_used(path, home_file, c(script, others))
  out$where <- c(out$where, twice$where, used$where)
  out$problem <- c(out$problem, twice$problem, used$problem)
  return(out)
}

# TRUE when the top-level expression `e` of blocks.R assigns with <- or =
# a name to function(...), or blocks to list(...).
is_blocks_shape <- function(e) {
  if (!is_assignment_call(e) || !is.symbol(e[[2]])) {
    return(FALSE)
  }
  if (!as.character(e[[1]]) %in% c("<-", "=") || !is.call(e[[3]])) {
    return(FALSE)
  }
  want <- if (identical(e[[2]], as.name("blocks"))) "list" else "function"
  return(identical(e[[3]][[1]], as.name(want)))
}

# The first line of the code of `e`, cut at 40 characters, in backticks.
cut_code <- function(e) {
  text <- deparse(e, width.cutoff = 60L)[1]
  if (nchar(text) > 40) {
    text <- paste0(substr(text, 1, 40), "...")
  }
  return(paste0("`", text, "`"))
}

# The names that blocks.R, with the expressions `exprs` on the lines
# `lines`, assigns at its top level and that a file of `others` assigns at
# its top level too: `where`, the place in blocks.R, and `problem`, which
# names the other place.
blocks_redefinitions <- function(path, exprs, lines, home_file, others) {
  out <- list(where = character(0), problem = character(0))
  mine <- vapply(
    exprs,
    function(e) {
      n <- assigned_name(e)
      return(if (is.null(n)) NA_character_ else n)
    },
    character(1)
  )
  # An assignment of blocks in another file has its own check.
  mine[mine %in% "blocks"] <- NA_character_
  for (f in sort(others, method = "radix")) {
    theirs <- top_level_assignments(file.path(path, f))
    for (i in which(mine %in% theirs$name)) {
      at <- theirs$line[match(mine[i], theirs$name)]
      out$where <- c(out$where, paste0(home_file, ":", lines[i]))
      out$problem <- c(
        out$problem,
        paste0(
          mine[i],
          " is assigned here and at ",
          f,
          ":",
          at,
          ". A name of ",
          basename(home_file),
          " MUST NOT be assigned in another file, because pbn leaves ",
          basename(home_file),
          " out of manifest$source_md5"
        )
      )
    }
  }
  return(out)
}

# The names that the file `file` assigns at its top level, with the line of
# each: a data.frame with the columns `name` and `line`. None for a file
# that does not parse.
top_level_assignments <- function(file) {
  exprs <- parse_or_null(file, keep_source = TRUE)
  if (length(exprs) == 0) {
    return(data.frame(name = character(0), line = integer(0)))
  }
  name <- vapply(
    exprs,
    function(e) {
      n <- assigned_name(e)
      return(if (is.null(n)) NA_character_ else n)
    },
    character(1)
  )
  line <- vapply(
    attr(exprs, "srcref"),
    function(r) return(as.integer(r[[1]])),
    integer(1)
  )
  keep <- !is.na(name)
  return(data.frame(name = name[keep], line = line[keep]))
}

# TRUE when the file vNN_functions/blocks.R of `version` in `home` cannot
# change the results: it does not exist, or it parses and has none of the
# problems of blocks_file_problems(). The other files are those that org
# sources. initialize_project() leaves blocks.R out of manifest$source_md5
# only then.
blocks_file_inert <- function(home, version) {
  home_file <- blocks_file(version)
  if (!file.exists(file.path(home, home_file))) {
    return(TRUE)
  }
  others <- setdiff(sourcing_order(home, version), home_file)
  script <- paste0(version, "_run.R")
  if (!file.exists(file.path(home, script))) {
    script <- character(0)
  }
  found <- blocks_file_problems(home, home_file, others, script)
  return(found$parsed && length(found$where) == 0)
}

# The top-level assignments of blocks in the files of the functions folder
# `folder` of `path`: `where`, each as <file>:<line>, `values`, the
# expression that each assigns, and `functions`, the names of the functions
# of the top-level assignments of those files.
blocks_assignments <- function(path, folder) {
  out <- list(where = character(0), values = list(), functions = character(0))
  files <- functions_files(path)
  for (f in sort(files[dirname(files) == folder], method = "radix")) {
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

# The uses of the names that `home_file`, vNN_functions/blocks.R, assigns
# at its top level, in the files `others` of `path`, such as the other
# files of its folder and its run script: `where`, each as <file>:<line>,
# and `problem`. A use is a
# name in the code, and not the name of an argument or of an element after
# `$` or `@`. A file that does not parse fails loc_per_file, so this rule
# skips it.
blocks_names_used <- function(path, home_file, others) {
  out <- list(where = character(0), problem = character(0))
  defined <- blocks_file_names(file.path(path, home_file))
  others <- others[file.exists(file.path(path, others))]
  if (length(defined) == 0) {
    return(out)
  }
  for (f in sort(others, method = "radix")) {
    found <- name_uses(file.path(path, f), defined)
    out$where <- c(out$where, paste0(f, ":", found$line, recycle0 = TRUE))
    out$problem <- c(
      out$problem,
      paste0(
        "uses ",
        found$name,
        ", which ",
        home_file,
        " assigns. pbn leaves that file out of manifest$source_md5, so the ",
        "run MUST NOT use it. Move ",
        found$name,
        " to another file",
        recycle0 = TRUE
      )
    )
  }
  return(out)
}

# The names that the file `file` assigns at its top level, also blocks.
# None for a file that does not exist or does not parse.
blocks_file_names <- function(file) {
  if (!file.exists(file)) {
    return(character(0))
  }
  exprs <- parse_or_null(file)
  return(unique(as.character(unlist(lapply(exprs, assigned_name)))))
}

# The uses of the names `defined` in the file `file`: a data.frame with the
# columns `name` and `line`, in the order of the file. A name after `$` or
# `@` and the name of an argument do not count.
name_uses <- function(file, defined) {
  none <- data.frame(name = character(0), line = integer(0))
  parsed <- parse_or_null(file, keep_source = TRUE)
  tokens <- if (is.null(parsed)) NULL else utils::getParseData(parsed)
  if (is.null(tokens)) {
    return(none)
  }
  tokens <- tokens[tokens$terminal, , drop = FALSE]
  tokens <- tokens[order(tokens$line1, tokens$col1), , drop = FALSE]
  before <- c("", tokens$token[-nrow(tokens)])
  after <- c(tokens$token[-1], "")
  # The target of an assignment is no use. blocks_list checks an assignment
  # of blocks, and of another name of blocks.R, on its own.
  hit <- tokens$token %in%
    c("SYMBOL", "SYMBOL_FUNCTION_CALL") &
    tokens$text %in% defined &
    !before %in% c("'$'", "'@'") &
    !after %in% c("LEFT_ASSIGN", "EQ_ASSIGN")
  return(data.frame(name = tokens$text[hit], line = tokens$line1[hit]))
}

# The name that the top-level expression `e` assigns with <-, =, <<-, -> or
# ->>, or NULL. For x$a <- 1 and names(x) <- y it is x.
assigned_name <- function(e) {
  if (!is_assignment_call(e)) {
    return(NULL)
  }
  target <- e[[2]]
  while (is.call(target) && length(target) >= 2) {
    target <- target[[2]]
  }
  return(if (is.symbol(target)) as.character(target) else NULL)
}

# TRUE when `e` is a call of <-, = or <<-. R parses -> and ->> as <- and
# <<-.
is_assignment_call <- function(e) {
  return(
    is.call(e) &&
      length(e) == 3 &&
      is.symbol(e[[1]]) &&
      as.character(e[[1]]) %in% c("<-", "=", "<<-")
  )
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
# `formatters` is NULL before the set house_2026_11_26, and else the value
# of formatter_names_of(): then each element MUST pass pbn_block_problem().
blocks_problem <- function(value, functions, formatters = NULL) {
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
  if (!is.null(formatters)) {
    return(pbn_blocks_problem(parts, nm, functions, formatters))
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

# TRUE when the element `x` of the list of blocks is a function, the name
# of one of the functions `functions`, or a call pbn::block().
is_block_function <- function(x, functions) {
  if (is.call(x)) {
    return(
      identical(x[[1]], as.name("function")) || is_block_call_of_pbn(x)
    )
  }
  return(is.symbol(x) && as.character(x) %in% functions)
}

# The first problem of pbn_block_problem() of the elements `parts` with the
# names `nm` of the list of blocks, or NULL.
pbn_blocks_problem <- function(parts, nm, functions, formatters) {
  for (i in seq_along(parts)) {
    bad <- pbn_block_problem(parts[[i]], nm[i], functions, formatters)
    if (!is.null(bad)) {
      return(bad)
    }
  }
  return(NULL)
}

# What is wrong with the element `x` with the name `name` of the list of
# blocks, from the set house_2026_11_26, or NULL. It MUST be a call
# pbn::block() whose arguments match block(). Its fn is a function, or the
# name of one of the functions `functions`. Its format is one string: the
# name of a function of formatters.R, or of a house formatter that no other
# file of the folder assigns, see formatter_names_of() for `formatters`.
# Its items is absent or a literal c() of "<id>:<item>" strings.
pbn_block_problem <- function(x, name, functions, formatters) {
  head <- paste0("the element ", encodeString(name, quote = "\""), " of blocks")
  e <- block_element(x)
  problem <- block_call_problem(e, functions)
  if (is.null(problem)) {
    problem <- block_format_problem(e$format, formatters)
  }
  if (is.null(problem)) {
    problem <- block_items_problem(e$items)
  }
  if (is.null(problem)) {
    return(NULL)
  }
  return(paste0(head, problem))
}

# What is wrong with the call of the element `e` of block_element(), as the
# end of a sentence that starts with the element, or NULL. `functions` are
# the functions of the folder.
block_call_problem <- function(e, functions) {
  if (!e$is_block) {
    return(paste0(
      " is not a call pbn::block(fn, format, items). The rule set ",
      "house_2026_11_26 needs it for each block"
    ))
  }
  if (is.null(e$args) || !"fn" %in% names(e$args)) {
    return(paste0(
      " is a call pbn::block() without fn, or with an argument that ",
      "pbn::block() does not take"
    ))
  }
  fn <- e$args[["fn"]]
  is_fn <- is.call(fn) && identical(fn[[1]], as.name("function"))
  if (!is_fn && !(is.symbol(fn) && as.character(fn) %in% functions)) {
    return(paste0(
      " has a fn that is not a function, or the name of a function of the ",
      "functions folder"
    ))
  }
  return(NULL)
}

# What is wrong with the format `format` of block_element(), NULL when it is
# not one string, as the end of a sentence that starts with the element, or
# NULL. `formatters` is the value of formatter_names_of().
block_format_problem <- function(format, formatters) {
  if (is.null(format)) {
    return(" has a format that is not one string")
  }
  if (format %in% formatters$elsewhere) {
    return(paste0(
      " has the format ",
      format,
      ", which a file other than formatters.R assigns. A project formatter ",
      "MUST be in formatters.R only"
    ))
  }
  if (!format %in% c(formatters$own, names(house_formatters()))) {
    return(paste0(
      " has the format ",
      format,
      ", which is no house formatter and no function of formatters.R"
    ))
  }
  return(NULL)
}

# What is wrong with the items `items` of block_element(), NULL when they
# are not literal, as the end of a sentence that starts with the element, or
# NULL.
block_items_problem <- function(items) {
  if (is.null(items)) {
    return(
      " has items that are not a literal c(\"<id>:<item>\", ...) of strings"
    )
  }
  problem <- items_problem(items)
  if (!is.null(problem)) {
    return(paste0(": items ", problem))
  }
  return(NULL)
}
