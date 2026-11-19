# The rules of check_project() that keep the explanations of a project in
# README.md and CHANGELOG.md: readme_and_changelog, changelog_with_code and
# comment_share. The set house_2026_11_19 brought them.

# The versions of the project in `path`: each vNN of a vNN_run.R or a
# vNN_functions/ at the top level, sorted.
project_versions <- function(path) {
  versions <- unique(substr(
    c(run_scripts(path), functions_folders(path)),
    1,
    3
  ))
  return(sort(versions, method = "radix"))
}

# The rule readme_and_changelog: README.md and CHANGELOG.md are at the top
# level, and CHANGELOG.md has a line "## vNN" for each version.
rule_readme_and_changelog <- function(path) {
  rule <- "readme_and_changelog"
  entries <- project_entries(path)
  files <- c("README.md", "CHANGELOG.md")
  missing <- files[!files %in% entries | dir.exists(file.path(path, files))]
  out <- list(project_row(
    rule,
    "FAIL",
    missing,
    paste0(missing, ": missing", recycle0 = TRUE)
  ))
  if (!"CHANGELOG.md" %in% missing) {
    lines <- readLines(
      file.path(path, "CHANGELOG.md"),
      warn = FALSE,
      encoding = "UTF-8"
    )
    versions <- project_versions(path)
    has <- vapply(
      versions,
      function(v) {
        return(any(grepl(paste0("^## ", v, "([[:space:]]|$)"), lines)))
      },
      logical(1)
    )
    absent <- versions[!has]
    out[[2]] <- project_row(
      rule,
      "FAIL",
      rep("CHANGELOG.md", length(absent)),
      paste0("CHANGELOG.md: no section \"## ", absent, "\"", recycle0 = TRUE)
    )
  }
  return(bind_rows(out))
}

# The rule changelog_with_code: in a git repository, a run script or a file
# under a functions folder with an uncommitted change needs a change to
# CHANGELOG.md too. Outside git, it has nothing to compare.
rule_changelog_with_code <- function(path) {
  rule <- "changelog_with_code"
  git <- git_binary()
  if (!nzchar(git)) {
    return(project_row(
      rule,
      "PASS",
      "",
      "git is not installed, so nothing to compare"
    ))
  }
  if (!in_git_repository(git, path)) {
    return(project_row(
      rule,
      "PASS",
      "",
      "the project is not a git repository, so nothing to compare"
    ))
  }
  changed <- uncommitted_files(git, path)
  code <- changed[grepl("^v[0-9]{2}_run\\.R$|^v[0-9]{2}_functions/", changed)]
  if ("CHANGELOG.md" %in% changed) {
    code <- character(0)
  }
  return(project_row(
    rule,
    "WARN",
    code,
    paste0(
      code,
      ": changed since the last commit, and CHANGELOG.md did not change",
      recycle0 = TRUE
    )
  ))
}

# The files of the project in `path` that differ from the last commit,
# relative to `path` and sorted: the staged changes, the changes that are not
# staged, and the untracked files that git does not ignore. A deleted file
# counts. In a repository without a commit, every staged file counts.
uncommitted_files <- function(git, path) {
  args <- c("-c", "core.quotepath=off", "-C", shQuote(path))
  diff <- c("--name-only", "--relative", "--no-renames", "--", ".")
  files <- c(
    git_lines(git, c(args, "diff", "--cached", diff), path),
    git_lines(git, c(args, "diff", diff), path),
    git_lines(
      git,
      c(args, "ls-files", "--others", "--exclude-standard", "--", "."),
      path
    )
  )
  return(sort(unique(files), method = "radix"))
}

# The lines of the output of git with the arguments `args`. Stops when git
# ends with a status other than 0.
git_lines <- function(git, args, path) {
  out <- suppressWarnings(system2(git, args, stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(out, "status"))) {
    stop("git ", args[5], " failed in ", path, call. = FALSE)
  }
  return(out)
}

# TRUE for each comment of `text` that is a section marker: text, and then
# four or more of = or - at the end, as in "# PART 1 ====".
is_section_marker <- function(text) {
  marker <- "[=-]{4,}[[:space:]]*$"
  before <- sub(marker, "", sub("^#+", "", text))
  return(grepl(marker, text) & grepl("[[:alnum:]]", before))
}

# The comment lines and the lines that are not blank of the file `file`, or
# NULL when R cannot parse it. The parse data decides what is a comment, so
# a "#" in a string does not count. A line with code and a comment counts as
# a comment line. A section marker does not.
comment_counts <- function(file) {
  exprs <- parse_or_null(file, keep_source = TRUE)
  if (is.null(exprs)) {
    return(NULL)
  }
  tokens <- utils::getParseData(exprs)
  comments <- tokens[tokens$token == "COMMENT", , drop = FALSE]
  comments <- comments[!is_section_marker(comments$text), , drop = FALSE]
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  return(c(
    comments = length(unique(comments$line1)),
    lines = sum(nzchar(trimws(lines)))
  ))
}

# A share as a percent for a detail, for example 25 or 26.7.
percent_text <- function(x) {
  return(paste0(format(round(100 * x, 1)), "%"))
}

# The rule comment_share: in a run script or an .R file at the top of a
# functions folder, the comment lines are at most the share `limit` of the
# lines that are not blank. A file that R cannot parse fails loc_per_file,
# so this rule skips it.
rule_comment_share <- function(path, limit) {
  rule <- "comment_share"
  out <- list()
  for (f in version_files(path)) {
    n <- comment_counts(file.path(path, f))
    if (is.null(n) || n[["lines"]] == 0) {
      next
    }
    share <- n[["comments"]] / n[["lines"]]
    if (share > limit) {
      out[[f]] <- project_row(
        rule,
        "WARN",
        f,
        paste0(
          f,
          ": ",
          n[["comments"]],
          " of ",
          n[["lines"]],
          " lines that are not blank are comments, ",
          percent_text(share),
          ", above the limit of ",
          percent_text(limit)
        )
      )
    }
  }
  return(bind_rows(out))
}

# The detail of the PASS line of comment_share, for the entry `entry` of the
# rules of the project.
comment_share_pass_detail <- function(entry) {
  return(paste0(
    "no run script or .R file of a functions folder has more than ",
    percent_text(entry$limit),
    " comment lines"
  ))
}
