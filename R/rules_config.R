# The rules of a project: the file pbn.yml in its top folder, and the dated
# house rule sets in inst/rules/ that it extends. check_project() and
# initialize_project() read them with read_rules().

# The one name of the file. pbn reads no other file and takes no path to one.
rules_file <- "pbn.yml"

# The top-level keys of pbn.yml.
rules_keys <- c("extends", "rules", "allow")

# The levels of a rule.
rule_levels <- c("fail", "warn", "off")

# The rules of check_results(), which the rule results_contract runs. pbn.yml
# cannot name them, because results_contract is locked.
results_rule_names <- c(
  "element_names",
  "exclusions_table",
  "allowed_objects",
  "no_row_per_person",
  "manifest_scenario",
  "manifest_data",
  "manifest_source"
)

# Why each locked rule of the house sets is locked.
locked_reasons <- c(
  results_contract = paste(
    "It keeps data for each person out of the results files that git",
    "tracks."
  ),
  gitexclude_not_tracked = "It keeps the files with data for each person out of git.",
  fake_data_names = paste(
    "It keeps the output of fake data apart from the output of real data."
  )
)

# The names of the rule sets that this pbn ships, oldest first. A set is
# inst/rules/house_YYYY_MM_DD.yml, and pbn never changes a set after its
# release.
house_sets <- function() {
  files <- list.files(
    system.file("rules", package = "pbn"),
    pattern = "^house_[0-9]{4}_[0-9]{2}_[0-9]{2}\\.yml$"
  )
  return(sort(sub("\\.yml$", "", files), method = "radix"))
}

# The one line of a new pbn.yml.
starter_line <- function() {
  sets <- house_sets()
  return(paste0("extends: ", sets[length(sets)]))
}

# Reads a YAML file. A word that YAML reads as a boolean, such as off, yes,
# no, on, y, true and false, stays its text with the class pbn_yaml_bool. So
# the level off is "off" and not FALSE, and a path MUST NOT be such a word.
# A sequence stays a list, so that each element keeps its class. An error of
# the yaml package names no path, because pbn reads one file only.
read_yaml_file <- function(file) {
  bool <- function(x) return(structure(x, class = "pbn_yaml_bool"))
  text <- readLines(file, warn = FALSE, encoding = "UTF-8")
  return(yaml::yaml.load(
    paste(text, collapse = "\n"),
    handlers = list(
      "bool#yes" = bool,
      "bool#no" = bool,
      seq = function(x) return(x)
    )
  ))
}

# TRUE when YAML read `x` as a boolean word.
is_yaml_bool <- function(x) {
  return(inherits(x, "pbn_yaml_bool"))
}

# A value of pbn.yml as text for a message: null for a missing value.
show_value <- function(x) {
  if (is.null(x)) {
    return("null")
  }
  return(paste(format(unlist(x)), collapse = " "))
}

# The rule set `name` of inst/rules/: one entry for each rule, in the order
# of check_project(). Each entry is a list with `level`, `locked` and the
# parameters of the rule.
read_house_set <- function(name) {
  x <- read_yaml_file(system.file(
    "rules",
    paste0(name, ".yml"),
    package = "pbn"
  ))
  return(lapply(x, function(entry) {
    if (!is.list(entry)) {
      entry <- list(level = entry)
    }
    entry$locked <- identical(as.vector(entry$locked), "true")
    if (!is.null(entry$forbid)) {
      entry$forbid <- as.character(unlist(entry$forbid))
    }
    return(entry)
  }))
}

# The rules of the project in the folder `home`: the set that pbn.yml
# extends, with the changes and the exceptions of pbn.yml. Stops on a
# missing or invalid pbn.yml, on a file pbn.yaml and on a file
# pbn_allow.txt. Returns a list with `set`, `rules` (one merged entry for
# each rule), `overrides` (the names of the rules that pbn.yml changes) and
# `allow` (a data.frame with the columns `rule`, `path` and `written`).
read_rules <- function(home) {
  if (file.exists(file.path(home, "pbn.yaml"))) {
    stop(
      "Rename pbn.yaml to pbn.yml. pbn reads the rules of a project from ",
      "pbn.yml only.",
      call. = FALSE
    )
  }
  file <- file.path(home, rules_file)
  stop_on_allow_txt(home, file.exists(file))
  if (!file.exists(file)) {
    stop(
      "There is no pbn.yml in ",
      home,
      ". Create it with this one line:\n\n",
      starter_line(),
      "\n\nThe line names the house rules of check_project(). ",
      "?check_project tells how pbn.yml changes them.",
      call. = FALSE
    )
  }
  x <- tryCatch(read_yaml_file(file), error = function(e) {
    stop(
      "pbn.yml is not valid YAML. The yaml package says: ",
      trimws(conditionMessage(e), which = "right"),
      call. = FALSE
    )
  })
  return(parse_rules(x))
}

# The rules of the parsed pbn.yml `x`, as read_rules() returns them.
parse_rules <- function(x) {
  if (is.null(x)) {
    x <- list()
  }
  if (!is.list(x) || (length(x) > 0 && is.null(names(x)))) {
    stop(
      "pbn.yml MUST be a map with the keys extends, rules and allow.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(x), rules_keys)
  if (length(unknown) > 0) {
    stop(
      "pbn.yml: the key ",
      unknown[1],
      " is not known. The keys are extends, rules and allow.",
      call. = FALSE
    )
  }
  set <- check_extends(x[["extends"]])
  rules <- read_house_set(set)
  overrides <- check_map(x[["rules"]], "rules")
  for (rule in names(overrides)) {
    rules[[rule]] <- merge_override(rule, overrides[[rule]], rules[[rule]])
  }
  allow <- parse_allow(check_map(x[["allow"]], "allow"), rules)
  return(list(
    set = set,
    rules = rules,
    overrides = names(overrides),
    allow = allow
  ))
}

# The name of the set of the key extends, after the checks.
check_extends <- function(extends) {
  if (is.null(extends)) {
    stop(
      "pbn.yml has no key extends. Add this line:\n\n",
      starter_line(),
      call. = FALSE
    )
  }
  sets <- house_sets()
  if (!is_one_string(extends) || !extends %in% sets) {
    stop(
      "pbn.yml: extends: ",
      show_value(extends),
      " is not a rule set of pbn ",
      utils::packageVersion("pbn"),
      ". The sets are: ",
      paste(sets, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(extends)
}

# The value of the key `key`, a map of rule names. NULL gives an empty map.
# Stops on an unknown rule name, with the nearest known name.
check_map <- function(x, key) {
  if (is.null(x)) {
    return(list())
  }
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(
      "pbn.yml: ",
      key,
      ": MUST be a map from the names of rules, for example ",
      key,
      ":\n  lower_case_names: ...",
      call. = FALSE
    )
  }
  for (rule in names(x)) {
    check_rule_name(rule, key)
  }
  return(x)
}

# Stops when `rule` is not a rule of check_project().
check_rule_name <- function(rule, key) {
  if (rule %in% results_rule_names) {
    stop(
      "pbn.yml: ",
      key,
      ": ",
      rule,
      " is a rule of check_results(), which the rule results_contract ",
      "runs. results_contract is locked, and pbn.yml cannot change it. ",
      locked_reasons[["results_contract"]],
      call. = FALSE
    )
  }
  known <- names(project_rules())
  if (!rule %in% known) {
    # A name that is part of a rule name, as cyclomatic, comes first.
    nearest <- known[order(
      utils::adist(rule, known, partial = TRUE),
      utils::adist(rule, known)
    )[1]]
    stop(
      "pbn.yml: ",
      key,
      ": ",
      rule,
      " is not a rule of check_project(). The nearest rule is ",
      nearest,
      ".",
      call. = FALSE
    )
  }
  return(invisible(rule))
}

# The entry of `rule` in the set, `house`, with the override `value` of
# pbn.yml merged into it.
merge_override <- function(rule, value, house) {
  where <- paste0("pbn.yml: rules: ", rule)
  if (is_one_string(value)) {
    value <- list(level = value)
  }
  if (!is.list(value) || is.null(names(value))) {
    stop(
      where,
      " MUST be a level, fail, warn or off, or a map such as ",
      "{level: warn}.",
      call. = FALSE
    )
  }
  check_override_keys(value, house, where)
  for (name in names(value)) {
    value[[name]] <- check_param(name, value[[name]], where)
  }
  if (
    isTRUE(house$locked) &&
      !is.null(value$level) &&
      value$level != house$level
  ) {
    stop(
      where,
      " is locked, so pbn.yml cannot set it to ",
      value$level,
      ". ",
      locked_reasons[[rule]],
      call. = FALSE
    )
  }
  return(utils::modifyList(house, value))
}

# Stops when the override `value` sets locked, or a parameter that the
# entry `house` of the set does not have.
check_override_keys <- function(value, house, where) {
  if ("locked" %in% names(value)) {
    stop(
      where,
      ": locked is a field of the rule set, and pbn.yml cannot set it.",
      call. = FALSE
    )
  }
  params <- setdiff(names(house), c("level", "locked"))
  unknown <- setdiff(names(value), c("level", params))
  if (length(unknown) > 0) {
    stop(
      where,
      " has no parameter ",
      unknown[1],
      ". ",
      if (length(params) == 0) {
        "It has no parameter."
      } else {
        paste0("Its parameters are: ", paste(params, collapse = ", "), ".")
      },
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The value `value` of the parameter `name`, after its check.
check_param <- function(name, value, where) {
  if (name == "level") {
    if (!is_one_string(value) || !value %in% rule_levels) {
      stop(
        where,
        ": level is ",
        show_value(value),
        ", and it MUST be fail, warn or off.",
        call. = FALSE
      )
    }
    return(as.vector(value))
  }
  if (name == "limit") {
    if (!is_count(value)) {
      stop(
        where,
        ": limit is ",
        show_value(value),
        ", and it MUST be a whole number of 1 or more.",
        call. = FALSE
      )
    }
    return(as.integer(value))
  }
  return(check_forbid(value, where))
}

# The value of the parameter forbid of no_local_assign, after its check.
check_forbid <- function(value, where) {
  known <- local_assign_constructs()
  if (length(value) == 0) {
    stop(
      where,
      ": forbid is empty. To turn the rule off, set its level to off.",
      call. = FALSE
    )
  }
  if (!is.list(value)) {
    value <- list(value)
  }
  text <- vapply(
    value,
    function(v) return(is_one_string(v) && !is_yaml_bool(v)),
    logical(1)
  )
  value <- unlist(value)
  if (!all(text)) {
    stop(
      where,
      ": forbid MUST be a list of constructs. They are: ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  bad <- setdiff(value, known)
  if (length(bad) > 0) {
    stop(
      where,
      ": forbid holds ",
      bad[1],
      ", which is not a construct of the rule. The constructs are: ",
      paste(known, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(unique(value))
}

# The exceptions of the section allow of pbn.yml, as a data.frame with the
# columns `rule`, `path` (the path that a finding MUST have) and `written`
# (the path as pbn.yml writes it). A final / of a folder is dropped from
# `path`, because a finding gives a folder without it.
parse_allow <- function(allow, rules) {
  out <- list()
  for (rule in names(allow)) {
    if (isTRUE(rules[[rule]]$locked)) {
      stop(
        "pbn.yml: allow: ",
        rule,
        " is locked, so pbn.yml cannot allow a path for it. ",
        locked_reasons[[rule]],
        call. = FALSE
      )
    }
    paths <- allow_paths(rule, allow[[rule]])
    check_allow_paths(rule, paths)
    out[[rule]] <- data.frame(
      rule = rep(rule, length(paths)),
      path = sub("/$", "", as.character(paths)),
      written = as.character(paths)
    )
  }
  if (length(out) == 0) {
    return(data.frame(
      rule = character(0),
      path = character(0),
      written = character(0)
    ))
  }
  return(do.call(rbind, unname(out)))
}

# The paths of the rule `rule` under allow, as a character vector. Stops
# when `value` is not a list of strings that are not empty.
allow_paths <- function(rule, value) {
  if (is_one_string(value)) {
    value <- list(value)
  }
  ok <- is.list(value) &&
    is.null(names(value)) &&
    all(vapply(
      value,
      function(v) return(is_one_string(v) && nzchar(v) && !is_yaml_bool(v)),
      logical(1)
    ))
  if (!ok) {
    stop(
      "pbn.yml: allow: ",
      rule,
      " MUST be a path or a list of paths, for example [adhoc/notes.R]. ",
      "A path that YAML reads as a word such as off, yes or no needs ",
      "quotes. To turn the rule off, write ",
      rule,
      ": off under rules:.",
      call. = FALSE
    )
  }
  return(as.character(unlist(value)))
}

# Stops on an entry of `paths` under allow: `rule` that is no path inside
# the project: empty after its final /, ".", a path that starts with / or
# ./, or a path with a part "." or "..". Such an entry would cover each
# finding of the rule, or none.
check_allow_paths <- function(rule, paths) {
  path <- sub("/+$", "", paths)
  parts <- strsplit(path, "/", fixed = TRUE)
  bad <- !nzchar(path) |
    startsWith(paths, "/") |
    vapply(
      parts,
      function(p) return(any(p %in% c(".", "..", ""))),
      logical(1)
    )
  if (any(bad)) {
    stop(
      "pbn.yml: allow: ",
      rule,
      ": ",
      encodeString(paths[bad][1], quote = "\""),
      " is not a path inside the project. Write a path relative to the ",
      "project folder, for example adhoc or adhoc/notes.R. An entry covers ",
      "its path and each path under it.",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# Stops when a file that org sources from `<version>_functions/` in `home`
# is a FAIL of loc_per_file under the rules `rules`. The level, the limit and
# the section allow of pbn.yml act as in check_project(), because the check
# uses loc_findings(), rule_findings() and apply_allow() of check_project().
# The files are the .R files at the top of the folder, hidden files too:
# org sources no subfolder. initialize_project() then gives org
# max_loc_per_file = Inf.
stop_on_long_files <- function(home, version, rules) {
  folder <- paste0(version, "_functions")
  files <- file.path(
    folder,
    list.files(file.path(home, folder), pattern = "\\.[rR]$", all.files = TRUE)
  )
  files <- files[!dir.exists(file.path(home, files))]
  def <- list(
    function(path, limit) return(loc_findings(path, files, limit)),
    ""
  )
  found <- apply_allow(
    rule_findings(home, "loc_per_file", def, rules$rules$loc_per_file),
    rules
  )
  fail <- found$detail[found$rule == "loc_per_file" & found$status == "FAIL"]
  if (length(fail) == 0) {
    return(invisible(NULL))
  }
  stop(
    "These files of ",
    folder,
    "/ fail the rule loc_per_file of pbn.yml, so the project does not ",
    "start:\n",
    paste0("  ", fail, collapse = "\n"),
    "\nSplit each file. A file that MUST stay long needs its path under ",
    "allow: loc_per_file: in pbn.yml, for example allow: {loc_per_file: [",
    sub(":[^:]*$", "", fail[1]),
    "]}.",
    call. = FALSE
  )
}

# The first line of check_project(): the set, and the counts of the
# overrides and the allowed paths of pbn.yml.
rules_line <- function(rules) {
  n_over <- length(rules$overrides)
  n_allow <- nrow(rules$allow)
  return(paste0(
    "Rules: ",
    rules$set,
    ", ",
    n_over,
    if (n_over == 1) " override, " else " overrides, ",
    n_allow,
    if (n_allow == 1) " allowed path" else " allowed paths",
    " (pbn.yml)"
  ))
}

# Stops when the project in `home` has the file pbn_allow.txt of pbn
# 2026.11.23 before pbn.yml. The error gives the YAML of its lines, so that
# the move is copy and paste. `has_yml` is TRUE when pbn.yml exists.
stop_on_allow_txt <- function(home, has_yml) {
  file <- file.path(home, "pbn_allow.txt")
  if (!file.exists(file)) {
    return(invisible(NULL))
  }
  yaml <- allow_txt_yaml(readLines(file, warn = FALSE))
  # R cuts an error at 1000 bytes by default, and the YAML MUST be whole.
  old <- options(warning.length = 8170)
  on.exit(options(old), add = TRUE)
  if (!has_yml) {
    yaml <- c(starter_line(), yaml)
  }
  stop(
    "pbn reads no pbn_allow.txt. Move its lines into the section allow: of ",
    if (has_yml) "pbn.yml" else "a new file pbn.yml",
    ", and delete pbn_allow.txt. ",
    if (has_yml) "These lines are the section:" else "This is the file:",
    "\n\n",
    paste(yaml, collapse = "\n"),
    call. = FALSE
  )
}

# The section allow: of pbn.yml for the lines `lines` of a pbn_allow.txt,
# each rule<TAB>path. A line without a TAB becomes a comment.
allow_txt_yaml <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  has_tab <- grepl("\t", lines, fixed = TRUE)
  rule <- sub("\t.*$", "", lines[has_tab])
  path <- sub("^[^\t]*\t", "", lines[has_tab])
  out <- "allow:"
  for (r in unique(rule)) {
    out <- c(
      out,
      paste0("  ", r, ":"),
      paste0("    - ", yaml_quote(path[rule == r]))
    )
  }
  bad <- lines[!has_tab]
  if (length(bad) > 0) {
    out <- c(
      out,
      "  # These lines of pbn_allow.txt have no TAB, so they allowed nothing:",
      paste0("  # ", bad)
    )
  }
  return(out)
}

# Each string of `x` in double quotes, as YAML reads it.
yaml_quote <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  return(paste0("\"", x, "\""))
}
