# The rules readme_and_changelog, changelog_with_code and comment_share of
# the set house_2026_11_19. The projects of house_2026_11_17 see none of
# them.

# A synthetic project of local_project() on the set house_2026_11_19.
local_project_changelog <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_19")
  return(p)
}

# The findings of the rule `rule` in the project in `home`.
changelog_rows <- function(home, rule) {
  found <- project_findings(home)
  return(found[found$rule == rule, c("status", "path", "detail")])
}

# Makes the project in `home` a git repository with one commit of every
# file.
git_commit_all <- function(home) {
  git_in(home, "init", "-q")
  git_in(home, "add", "-A")
  git_in(
    home,
    "-c",
    "user.name=pbn",
    "-c",
    "user.email=test@example.org",
    "-c",
    "commit.gpgsign=false",
    "commit",
    "-q",
    "-m",
    "first"
  )
  return(invisible(home))
}

test_that("readme_and_changelog passes README.md and CHANGELOG.md with a section for every version", {
  p <- local_project_changelog()
  write_lines(
    p$home,
    "CHANGELOG.md",
    c("# Changelog", "", "## v02 (2026-11-20)", "", "## v01", "- First.")
  )
  write_lines(
    p$home,
    "v02_run.R",
    "pbn::initialize_project(version = \"v02\", home = \".\", folders = c(main = \"x\"))"
  )
  write_lines(p$home, "v02_functions/f.R", "f <- function() return(1)")
  freeze_version("v01", p$home)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    changelog_rows(p$home, "readme_and_changelog")$detail,
    paste(
      "README.md and CHANGELOG.md exist, and CHANGELOG.md has a section",
      "## vNN for every version"
    )
  )
})

test_that("readme_and_changelog fails a missing README.md, a missing CHANGELOG.md and a version without its section", {
  p <- local_project_changelog()
  unlink(file.path(p$home, "README.md"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "readme_and_changelog")
  expect_identical(
    changelog_rows(p$home, "readme_and_changelog"),
    data.frame(
      status = "FAIL",
      path = "README.md",
      detail = with_fix("readme_and_changelog", "README.md: missing")
    ),
    ignore_attr = TRUE
  )

  write_lines(p$home, "README.md", "# A synthetic project")
  write_lines(
    p$home,
    "CHANGELOG.md",
    c("# Changelog", "", "## v011", "### v01", "- v01 is the first.")
  )
  write_lines(p$home, "v02_functions/f.R", "f <- function() return(1)")
  rows <- changelog_rows(p$home, "readme_and_changelog")
  expect_identical(rows$path, c("CHANGELOG.md", "CHANGELOG.md"))
  expect_identical(
    rows$detail,
    with_fix(
      "readme_and_changelog",
      c(
        "CHANGELOG.md: no section \"## v01\"",
        "CHANGELOG.md: no section \"## v02\""
      )
    )
  )

  unlink(file.path(p$home, "CHANGELOG.md"))
  expect_identical(
    changelog_rows(p$home, "readme_and_changelog")$detail,
    with_fix("readme_and_changelog", "CHANGELOG.md: missing")
  )
})

test_that("changelog_with_code passes outside git and without git, with a detail that says nothing to compare", {
  p <- local_project_changelog()
  expect_identical(
    changelog_rows(p$home, "changelog_with_code")$detail,
    "the project is not a git repository, so nothing to compare"
  )
  local_mocked_bindings(git_binary = function() return(""))
  expect_identical(
    changelog_rows(p$home, "changelog_with_code")$detail,
    "git is not installed, so nothing to compare"
  )
})

test_that("changelog_with_code passes in git without a change, and with a change to CHANGELOG.md", {
  skip_if_not(nzchar(Sys.which("git")), "git is not installed")
  p <- local_project_changelog()
  git_commit_all(p$home)
  pass <- paste(
    "every uncommitted change to a vNN_run.R or a file under",
    "vNN_functions/ comes with a change to CHANGELOG.md"
  )
  expect_identical(
    changelog_rows(p$home, "changelog_with_code")$detail,
    pass
  )
  cat("x <- 2\n", file = file.path(p$home, "v01_run.R"), append = TRUE)
  write_lines(p$home, "v01_functions/new.R", "n <- function() return(1)")
  write_lines(p$home, "adhoc/notes.R", "x <- 1")
  cat("- Added n().\n", file = file.path(p$home, "CHANGELOG.md"), append = TRUE)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found[found$rule == "changelog_with_code", "detail"],
    pass
  )
})

test_that("changelog_with_code warns on each changed, staged, untracked or deleted code file when CHANGELOG.md did not change", {
  skip_if_not(nzchar(Sys.which("git")), "git is not installed")
  p <- local_project_changelog()
  git_commit_all(p$home)
  cat("x <- 2\n", file = file.path(p$home, "v01_run.R"), append = TRUE)
  cat(
    "# a change\n",
    file = file.path(p$home, "v01_functions", "_cfg.R"),
    append = TRUE
  )
  git_in(p$home, "add", "v01_functions/_cfg.R")
  write_lines(p$home, "v01_functions/new.R", "n <- function() return(1)")
  unlink(file.path(p$home, "v01_functions", "double_it.R"))
  # A change outside the versions needs no line in CHANGELOG.md.
  write_lines(p$home, "adhoc/notes.R", "x <- 1")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  rows <- found[found$rule == "changelog_with_code", ]
  expect_identical(rows$status, rep("WARN", 4))
  files <- c(
    "v01_functions/_cfg.R",
    "v01_functions/double_it.R",
    "v01_functions/new.R",
    "v01_run.R"
  )
  expect_identical(rows$path, files)
  expect_identical(
    rows$detail,
    with_fix(
      "changelog_with_code",
      paste0(
        files,
        ": changed since the last commit, and CHANGELOG.md did not change"
      )
    )
  )
})

test_that("changelog_with_code reads the project folder only, when it is a folder inside a git repository", {
  skip_if_not(nzchar(Sys.which("git")), "git is not installed")
  p <- local_project_changelog()
  # git looks for the repository in p$root, the folder above the project.
  withr::local_envvar(GIT_CEILING_DIRECTORIES = dirname(p$root))
  git_commit_all(p$root)
  cat("x <- 2\n", file = file.path(p$home, "v01_run.R"), append = TRUE)
  # A CHANGELOG.md outside the project folder does not count.
  write_lines(p$root, "CHANGELOG.md", "# Another changelog")
  write_lines(p$root, "other_run.R", "x <- 1")
  expect_identical(
    changelog_rows(p$home, "changelog_with_code"),
    data.frame(
      status = "WARN",
      path = "v01_run.R",
      detail = with_fix(
        "changelog_with_code",
        "v01_run.R: changed since the last commit, and CHANGELOG.md did not change"
      )
    ),
    ignore_attr = TRUE
  )
  cat("- Changed.\n", file = file.path(p$home, "CHANGELOG.md"), append = TRUE)
  expect_identical(
    changelog_rows(p$home, "changelog_with_code")$status,
    "PASS"
  )
})

test_that("comment_share passes a file at the limit, a section marker, a # in a string, and a file outside the versions", {
  p <- local_project_changelog()
  write_lines(
    p$home,
    "v01_functions/notes.R",
    c(
      "# PART 1 ====",
      "# One comment.",
      "f <- function() {",
      "  x <- \"# not a comment",
      "  # still not a comment\"",
      "",
      "  # Section two ----------",
      "  return(x)",
      "}"
    )
  )
  # 1 comment line of 4 is the limit 25%, and a share above it fails.
  write_lines(
    p$home,
    "v01_functions/limit.R",
    c("# a", "x <- 1", "y <- 2", "z <- 3")
  )
  write_lines(p$home, "adhoc/explained.R", c("# a", "# b", "x <- 1"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found[found$rule == "comment_share", c("status", "detail")],
    data.frame(
      status = "PASS",
      detail = paste(
        "no run script or .R file of a functions folder has more than",
        "25% comment lines"
      )
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    comment_counts(file.path(p$home, "v01_functions", "notes.R")),
    c(comments = 1L, lines = 8L)
  )
})

test_that("comment_share warns on a run script and a functions file above the limit, and counts a comment after code", {
  p <- local_project_changelog()
  write_lines(
    p$home,
    "v01_functions/explained.R",
    c(
      "# Why this function exists.",
      "f <- function(x) {",
      "  y <- x + 1 # one more",
      "  return(y)",
      "}"
    )
  )
  cat(
    "# a\n# b\n# c\n# d\n# e\n# f\n# g\n# h\n# i\n",
    file = file.path(p$home, "v01_run.R"),
    append = TRUE
  )
  n_run <- sum(nzchar(trimws(readLines(file.path(p$home, "v01_run.R")))))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  rows <- found[found$rule == "comment_share", ]
  expect_identical(rows$status, c("WARN", "WARN"))
  expect_identical(rows$path, c("v01_functions/explained.R", "v01_run.R"))
  expect_identical(
    rows$detail,
    with_fix(
      "comment_share",
      c(
        paste0(
          "v01_functions/explained.R: 2 of 5 lines that are not blank are ",
          "comments, 40%, above the limit of 25%"
        ),
        paste0(
          "v01_run.R: 9 of ",
          n_run,
          " lines that are not blank are comments, ",
          format(round(100 * 9 / n_run, 1)),
          "%, above the limit of 25%"
        )
      )
    )
  )
})

test_that("comment_share takes a limit from pbn.yml, and stops on a limit that is not a share", {
  p <- local_project_changelog()
  write_lines(
    p$home,
    "v01_functions/explained.R",
    c("# Why.", "f <- function() return(1)", "g <- function() return(2)")
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_19", "rules:", "  comment_share: {limit: 0.5}")
  )
  expect_identical(read_rules(p$home)$rules$comment_share$limit, 0.5)
  expect_identical(
    changelog_rows(p$home, "comment_share")$detail,
    paste(
      "no run script or .R file of a functions folder has more than",
      "50% comment lines"
    )
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_19", "rules:", "  comment_share: {limit: 0.2}")
  )
  expect_identical(
    changelog_rows(p$home, "comment_share")$detail,
    with_fix(
      "comment_share",
      paste0(
        "v01_functions/explained.R: 1 of 3 lines that are not blank are ",
        "comments, 33.3%, above the limit of 20%"
      )
    )
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_19", "rules:", "  comment_share: {limit: 2}")
  )
  expect_error(
    read_rules(p$home),
    paste0(
      "pbn.yml: rules: comment_share: limit is 2, and it MUST be a number ",
      "from 0 to 1, such as 0.25."
    ),
    fixed = TRUE
  )
  # The limit of another rule stays a count.
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_19", "rules:", "  loc_per_file: {limit: 0.5}")
  )
  expect_error(
    read_rules(p$home),
    "limit is 0.5, and it MUST be a whole number of 1 or more.",
    fixed = TRUE
  )
})

test_that("a project on house_2026_11_17 runs none of the three rules", {
  p <- local_project()
  unlink(file.path(p$home, "README.md"))
  write_lines(p$home, "v01_functions/explained.R", c("# a", "# b", "x <- 1"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_false(any(
    c("readme_and_changelog", "changelog_with_code", "comment_share") %in%
      found$rule
  ))
})
