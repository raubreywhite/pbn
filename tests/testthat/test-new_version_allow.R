# new_version() adds the entries of the new functions folder to the lists
# under allow of pbn.yml.

# The lines of pbn.yml after house_line, for the project of local_project().
# Returns the project folder.
yml_project <- function(..., env = parent.frame()) {
  p <- local_project(env = env)
  write_pbn_yml(p$home, ...)
  return(p$home)
}

yml_lines <- function(home) {
  return(readLines(file.path(home, "pbn.yml")))
}

# Calls new_version("v01") on `home`, and fails the test, without an error,
# when the call stops. Returns the messages of the call.
new_version_ok <- function(home) {
  msgs <- character(0)
  expect_no_error(msgs <- testthat::capture_messages(new_version("v01", home = home)))
  return(msgs)
}

allow_of <- function(home) {
  a <- read_rules(home)$allow
  return(paste(a$rule, a$written, sep = ": "))
}

test_that("a flow list on one line gets the entry of v02_functions at its end", {
  home <- yml_project(
    "allow:",
    "  loc_per_file: [adhoc, v01_functions/big.R]"
  )
  msgs <- new_version_ok(home)
  expect_identical(
    yml_lines(home),
    c(
      house_line,
      "allow:",
      "  loc_per_file: [adhoc, v01_functions/big.R, v02_functions/big.R]"
    )
  )
  expect_identical(
    allow_of(home),
    c(
      "loc_per_file: adhoc",
      "loc_per_file: v01_functions/big.R",
      "loc_per_file: v02_functions/big.R"
    )
  )
  expect_true(
    "new_version(): added v02_functions/big.R to allow: loc_per_file in pbn.yml.\n" %in%
      msgs
  )
})

test_that("a block list gets the entry of v02_functions on the line after the old entry", {
  home <- yml_project(
    "allow:",
    "  loc_per_file:",
    "    - v01_functions/big.R",
    "    - adhoc",
    "  lower_case_names:",
    "  - v01_functions/Old/",
    "  - v01_functions"
  )
  msgs <- new_version_ok(home)
  expect_identical(
    yml_lines(home),
    c(
      house_line,
      "allow:",
      "  loc_per_file:",
      "    - v01_functions/big.R",
      "    - v02_functions/big.R",
      "    - adhoc",
      "  lower_case_names:",
      "  - v01_functions/Old/",
      "  - v02_functions/Old/",
      "  - v01_functions"
    )
  )
  expect_identical(
    sort(allow_of(home)),
    sort(c(
      "loc_per_file: v01_functions/big.R",
      "loc_per_file: v02_functions/big.R",
      "loc_per_file: adhoc",
      "lower_case_names: v01_functions/Old/",
      "lower_case_names: v02_functions/Old/",
      "lower_case_names: v01_functions"
    ))
  )
  expect_identical(
    sum(grepl("^new_version\\(\\): added ", msgs)),
    2L
  )
})

test_that("quoted entries keep their quotes in a flow list and in a block list", {
  home <- yml_project(
    "allow:",
    "  loc_per_file: ['v01_functions/big.R', \"v01_functions/b c.R\"]",
    "  lower_case_names:",
    "    - 'v01_functions/A.R'",
    "    - \"v01_functions/B.R\""
  )
  new_version_ok(home)
  expect_identical(
    yml_lines(home),
    c(
      house_line,
      "allow:",
      paste0(
        "  loc_per_file: ['v01_functions/big.R', \"v01_functions/b c.R\", ",
        "'v02_functions/big.R', \"v02_functions/b c.R\"]"
      ),
      "  lower_case_names:",
      "    - 'v01_functions/A.R'",
      "    - 'v02_functions/A.R'",
      "    - \"v01_functions/B.R\"",
      "    - \"v02_functions/B.R\""
    )
  )
  expect_true(all(
    c(
      "loc_per_file: v02_functions/big.R",
      "loc_per_file: v02_functions/b c.R",
      "lower_case_names: v02_functions/A.R",
      "lower_case_names: v02_functions/B.R"
    ) %in%
      allow_of(home)
  ))
})

test_that("an inline comment stays on the line of a flow list and of a block entry", {
  home <- yml_project(
    "allow: # the exceptions",
    "  loc_per_file: [v01_functions/big.R]   # one big file",
    "  lower_case_names:  # names",
    "    - v01_functions/A.R # a capital"
  )
  new_version_ok(home)
  expect_identical(
    yml_lines(home),
    c(
      house_line,
      "allow: # the exceptions",
      "  loc_per_file: [v01_functions/big.R, v02_functions/big.R]   # one big file",
      "  lower_case_names:  # names",
      "    - v01_functions/A.R # a capital",
      "    - v02_functions/A.R"
    )
  )
  expect_identical(length(allow_of(home)), 4L)
})

test_that("an entry whose destination is there already is not added twice", {
  home <- yml_project(
    "allow:",
    "  loc_per_file: [v01_functions/big.R, v02_functions/big.R]",
    "  lower_case_names:",
    "    - v01_functions/A.R",
    "    - v02_functions/A.R/",
    "    - v01_functions/C.R"
  )
  msgs <- new_version_ok(home)
  expect_identical(
    yml_lines(home),
    c(
      house_line,
      "allow:",
      "  loc_per_file: [v01_functions/big.R, v02_functions/big.R]",
      "  lower_case_names:",
      "    - v01_functions/A.R",
      "    - v02_functions/A.R/",
      "    - v01_functions/C.R",
      "    - v02_functions/C.R"
    )
  )
  expect_identical(
    grep("^new_version\\(\\): added ", msgs, value = TRUE),
    "new_version(): added v02_functions/C.R to allow: lower_case_names in pbn.yml.\n"
  )
})

test_that("comments and unrelated lines stay byte-identical, with CRLF line ends too", {
  p <- local_project()
  old <- c(
    "# The rules of this project.",
    "extends: house_2026_11_17",
    "",
    "rules:",
    "  loc_per_file:",
    "    limit: 900 # v01_functions/big.R is long",
    "allow:",
    "  # loc_per_file: [v01_functions/x.R]",
    "  loc_per_file: [v01_functions/big.R]",
    "  lower_case_names:",
    "    - v01_functions/A.R # a capital",
    "",
    "ignore: [archive/] # old files"
  )
  for (eol in c("\n", "\r\n")) {
    home <- if (eol == "\n") p$home else local_project()$home
    file <- file.path(home, "pbn.yml")
    writeBin(charToRaw(paste0(paste(old, collapse = eol), eol)), file)
    new_version_ok(home)
    new <- append(old, "    - v02_functions/A.R", after = 11)
    new[9] <- "  loc_per_file: [v01_functions/big.R, v02_functions/big.R]"
    expect_identical(
      readBin(file, "raw", file.size(file)),
      charToRaw(paste0(paste(new, collapse = eol), eol))
    )
  }
})

test_that("an entry that does not name v01_functions/ stays unchanged", {
  home <- yml_project(
    "allow:",
    "  loc_per_file: [archive, osf/, v01_functions/big.R]",
    "  lower_case_names: [archive, osf]",
    "  no_local_copy_of_pbn_function:",
    "    - osf/x.R",
    "    - v01_functionsx/a.R",
    "    - v01_run.R"
  )
  old <- yml_lines(home)
  old_allow <- allow_of(home)
  new_version_ok(home)
  new <- yml_lines(home)
  expect_identical(new[-3], old[-3])
  expect_identical(
    new[3],
    "  loc_per_file: [archive, osf/, v01_functions/big.R, v02_functions/big.R]"
  )
  expect_identical(
    setdiff(allow_of(home), "loc_per_file: v02_functions/big.R"),
    old_allow
  )
})

test_that("a project with no allow list in pbn.yml, or no pbn.yml, keeps it as it is", {
  home <- yml_project("# no exceptions")
  file <- file.path(home, "pbn.yml")
  before <- file_states(home)
  msgs <- new_version_ok(home)
  expect_false(any(grepl("pbn.yml", msgs, fixed = TRUE)))
  after <- file_states(home)
  expect_identical(after[after$file == "pbn.yml", ], before[before$file == "pbn.yml", ])
  expect_true(file.exists(file.path(home, "v02_run.R")))

  home <- local_project()$home
  unlink(file.path(home, "pbn.yml"))
  new_version_ok(home)
  expect_false(file.exists(file.path(home, "pbn.yml")))
  expect_true(dir.exists(file.path(home, "v02_functions")))
})

test_that("a flow list on more than one line stops, and leaves no v02 and pbn.yml as it was", {
  home <- yml_project(
    "allow:",
    "  loc_per_file: [adhoc,",
    "    v01_functions/big.R]"
  )
  before <- file_states(home)
  expect_error(
    new_version("v01", home = home),
    paste0(
      "new_version() could not add these entries under allow: in pbn.yml. ",
      "pbn.yml MAY have a form that new_version() does not edit, such as a ",
      "flow list on more than one line. new_version() wrote pbn.yml back ",
      "and removed what it created. Add the entries by hand, and call ",
      "new_version() again:\n",
      "  loc_per_file: v02_functions/big.R"
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(home, "v02_run.R")))
  expect_false(dir.exists(file.path(home, "v02_functions")))
  expect_false(file.exists(file.path(home, "v01_frozen.md5")))
  # The undo writes CHANGELOG.md back, so its time changes.
  expect_identical(
    file_states(home)[, c("file", "md5")],
    before[, c("file", "md5")]
  )

  # The entry added by hand, a second call passes and adds nothing.
  write_pbn_yml(
    home,
    "allow:",
    "  loc_per_file: [adhoc,",
    "    v01_functions/big.R, v02_functions/big.R]"
  )
  old <- yml_lines(home)
  msgs <- new_version_ok(home)
  expect_false(any(grepl("pbn.yml", msgs, fixed = TRUE)))
  expect_identical(yml_lines(home), old)
})

test_that("a stop after an edit of one list writes pbn.yml back as it was", {
  home <- yml_project(
    "allow:",
    "  lower_case_names: [v01_functions/A.R] # one capital",
    "  loc_per_file: [adhoc,",
    "    v01_functions/big.R]"
  )
  file <- file.path(home, "pbn.yml")
  old <- readBin(file, "raw", file.size(file))
  before <- file_states(home)
  expect_error(
    new_version("v01", home = home),
    paste0(
      "new_version() again:\n",
      "  lower_case_names: v02_functions/A.R\n",
      "  loc_per_file: v02_functions/big.R"
    ),
    fixed = TRUE
  )
  expect_identical(readBin(file, "raw", file.size(file)), old)
  expect_false(file.exists(file.path(home, "v02_run.R")))
  expect_false(dir.exists(file.path(home, "v02_functions")))
  expect_identical(
    file_states(home)[, c("file", "md5")],
    before[, c("file", "md5")]
  )
})

# The steps of the edit, each on its own, so that a defect in one step fails
# an expectation of that step.

test_that("allow_to_add() takes only the entries that start with v01_functions/", {
  home <- yml_project(
    "allow:",
    "  loc_per_file: [v01_functionsx/a.R, v01_functions/big.R, v01_run.R]"
  )
  add <- pbn:::allow_to_add(home, "v01", "v02")$add
  expect_identical(add$written, "v01_functions/big.R")
  expect_identical(add$new, "v02_functions/big.R")
})

block_add <- data.frame(
  rule = "lower_case_names",
  written = "v01_functions/A.R",
  new = "v02_functions/A.R"
)

test_that("allow_lines_with() puts a block entry on the line after the old entry", {
  out <- pbn:::allow_lines_with(
    c(house_line, "allow:", "  lower_case_names:", "    - v01_functions/A.R", "    - adhoc"),
    block_add
  )
  expect_identical(
    out$lines,
    c(
      house_line,
      "allow:",
      "  lower_case_names:",
      "    - v01_functions/A.R",
      "    - v02_functions/A.R",
      "    - adhoc"
    )
  )
  expect_identical(out$inserted, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("allow_lines_with() finds a block entry that has a trailing comment", {
  out <- pbn:::allow_lines_with(
    c(house_line, "allow:", "  lower_case_names:", "    - v01_functions/A.R # a capital"),
    block_add
  )
  expect_identical(
    out$lines,
    c(
      house_line,
      "allow:",
      "  lower_case_names:",
      "    - v01_functions/A.R # a capital",
      "    - v02_functions/A.R"
    )
  )
})

test_that("allow_lines_with() adds the entries at the end of a flow list on one line", {
  out <- pbn:::allow_lines_with(
    c(house_line, "allow:", "  loc_per_file: [adhoc, 'v01_functions/big.R'] # big"),
    data.frame(
      rule = "loc_per_file",
      written = "v01_functions/big.R",
      new = "v02_functions/big.R"
    )
  )
  expect_identical(
    out$lines,
    c(
      house_line,
      "allow:",
      "  loc_per_file: [adhoc, 'v01_functions/big.R', 'v02_functions/big.R'] # big"
    )
  )
  expect_identical(out$inserted, c(FALSE, FALSE, FALSE))
})

# A pbn.yml with a flow list under allow, and a line under rules that names
# v01_functions/ in a comment. Returns the folder, the old lines, the lines
# after a correct edit, and the argument `allow` of allow_edit_ok().
edit_case <- function(env = parent.frame()) {
  home <- local_project(env = env)$home
  old <- c(
    house_line,
    "# the exceptions",
    "rules:",
    "  loc_per_file:",
    "    limit: 900 # v01_functions/big.R",
    "allow:",
    "  loc_per_file: [v01_functions/big.R]"
  )
  writeLines(old, file.path(home, "pbn.yml"))
  good <- old
  good[7] <- "  loc_per_file: [v01_functions/big.R, v02_functions/big.R]"
  allow <- list(
    old = pbn:::read_rules(home)$allow,
    add = data.frame(
      rule = "loc_per_file",
      written = "v01_functions/big.R",
      new = "v02_functions/big.R"
    )
  )
  return(list(home = home, old = old, good = good, allow = allow))
}

edit_ok <- function(x, lines) {
  writeLines(lines, file.path(x$home, "pbn.yml"))
  return(pbn:::allow_edit_ok(x$home, x$old, rep(FALSE, length(x$old)), x$allow, "v01", "\n"))
}

test_that("allow_edit_ok() is FALSE when a comment line changed", {
  x <- edit_case()
  expect_true(edit_ok(x, x$good))
  bad <- x$good
  bad[2] <- "# the exceptions, changed"
  expect_false(edit_ok(x, bad))
})

test_that("allow_edit_ok() is FALSE when a line outside allow that names v01_functions/ changed", {
  x <- edit_case()
  bad <- x$good
  bad[5] <- "    limit: 900 # v01_functions/big.R is long"
  expect_false(edit_ok(x, bad))
})

test_that("flow_list_with() adds each matching entry with the quote of its match, and leaves a list it cannot read as it was", {
  add <- data.frame(written = c("x", "y", "z"), new = c("X", "Y", "Z"))
  expect_identical(
    flow_list_with("a: [x, 'y', \"z\"] # c", 4, add),
    list(line = "a: [x, 'y', \"z\", X, 'Y', \"Z\"] # c", used = c(TRUE, TRUE, TRUE))
  )
  for (line in c("a: [x, y", "a: [x, {y: 1}]", "a: []", "a: [q]", "a: [x]  junk")) {
    expect_identical(
      flow_list_with(line, 4, add),
      list(line = line, used = c(FALSE, FALSE, FALSE)),
      info = line
    )
  }
})
