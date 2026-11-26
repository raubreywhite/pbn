# create_project() and new_version().

# An empty folder for a new project, deleted when the calling test ends. git
# does not look above it.
local_empty_home <- function(env = parent.frame()) {
  root <- tempfile("pbnnew")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE), envir = env)
  root <- normalizePath(root, winslash = "/")
  withr::local_envvar(GIT_CEILING_DIRECTORIES = root, .local_envir = env)
  home <- file.path(root, "my_study")
  dir.create(home)
  return(home)
}

# The files that create_project() writes, sorted in the C locale.
new_project_files <- c(
  ".gitignore",
  "AGENTS.md",
  "CHANGELOG.md",
  "README.md",
  "check_project.R",
  "pbn.yml",
  "v01_functions/_cfg.R",
  "v01_functions/data_clean.R",
  "v01_functions/data_cohort.R",
  "v01_functions/data_raw.R",
  "v01_functions/expect_known.R",
  "v01_functions/fit_risk_difference.R",
  "v01_functions/output.R",
  "v01_run.R"
)

test_that("create_project writes the files of a project that passes every rule of house_2026_11_26 without a WARN", {
  home <- local_empty_home()
  written <- suppressMessages(create_project(
    home = home,
    folders = c(main = "../data", lookup = "../codes")
  ))
  expect_setequal(written, new_project_files)
  expect_identical(
    sort(
      list.files(home, recursive = TRUE, all.files = TRUE),
      method = "radix"
    ),
    new_project_files
  )
  found <- project_findings(home)
  expect_identical(unique(found$status), "PASS")
  expect_identical(
    unique(found$rule),
    names(read_house_set("house_2026_11_26"))
  )
  expect_identical(
    readLines(file.path(home, "pbn.yml")),
    "extends: house_2026_11_26"
  )
  expect_identical(readLines(file.path(home, ".gitignore")), "*gitexclude*")
  expect_identical(
    readLines(file.path(home, "check_project.R")),
    "pbn::check_project()"
  )
  expect_identical(
    readLines(file.path(home, "AGENTS.md")),
    readLines(system.file("templates", "AGENTS.md", package = "pbn"))
  )
  expect_identical(
    readLines(file.path(home, "v01_run.R"))[1],
    paste0(
      "pbn::initialize_project(version = \"v01\", home = \".\", ",
      "folders = c(main = \"../data\", lookup = \"../codes\"))"
    )
  )
  expect_identical(readLines(file.path(home, "README.md"))[1], "# my_study")
  expect_true("## v01" %in% readLines(file.path(home, "CHANGELOG.md")))
  for (f in c("v01_run.R", grep("^v01_functions/", written, value = TRUE))) {
    expect_identical(
      comment_counts(file.path(home, f))[["comments"]],
      0L,
      info = f
    )
  }
  bytes <- readBin(file.path(home, "v01_run.R"), "raw", 10000)
  expect_false(as.raw(13) %in% bytes)
})

test_that("the AGENTS.md template holds the rules for an agent", {
  expect_identical(
    readLines(system.file("templates", "AGENTS.md", package = "pbn")),
    c(
      "# AGENTS.md",
      "",
      paste(
        "This project uses the R package pbn. Before you change any code,",
        "read how to use pbn: https://www.rwhite.no/pbn/articles/pbn.md"
      ),
      "",
      "## Before you report a change",
      "",
      paste(
        "- Run `pbn::verify(\"vNN\")` on the newest version. Continue until",
        "the verdict is PASS. Each finding of a rule names its fix."
      ),
      paste(
        "- A PASS without the scenario `real` covers the fake scenarios only.",
        "Say so when you report."
      ),
      paste(
        "- Add what changed and why to `CHANGELOG.md`, under the section of",
        "the version."
      ),
      "",
      "## What a person decides",
      "",
      paste(
        "- Propose the fake scenarios and their expected values. A person",
        "confirms them before they count. After that, do not change an",
        "expected value in an `expect_` function, or add or remove a fake",
        "scenario, without asking."
      ),
      "- Do not change `pbn.yml`. Ask.",
      "- Do not freeze a version or start a new one. Ask.",
      "",
      "## Results",
      "",
      paste(
        "- In a one-file project, the run on this computer writes the",
        "scenario `real` to `vNN_results/fake/`. A one-file project is one",
        "whose `vNN_run.R` gives `one_file_home` to",
        "`pbn::initialize_project()`."
      ),
      paste(
        "- `fake/` stands in for the real results, and its numbers are fake.",
        "It is not one of the fake scenarios, which have planted effects."
      ),
      paste(
        "- `vNN_results/real/` holds only the real results that come back",
        "from the server. Do not write into it."
      ),
      "",
      "## Manuscripts",
      "",
      paste(
        "- Save your own draft as `manuscript/draft/vNN_<INITIALS>.docx`, and",
        "its supporting information as",
        "`manuscript/draft/vNN_<INITIALS>_si.docx`. NN is the version it",
        "starts from. A draft is outside the version counter, so you MAY",
        "overwrite it."
      ),
      paste(
        "- A hand edit, by a person or by an agent for a person, is saved as",
        "`manuscript/manual/v(NN+1)_<INITIALS>.docx`. NN is the version it",
        "starts from. INITIALS are those of the person responsible."
      ),
      paste(
        "- Never edit or overwrite a file that is already in",
        "`manuscript/manual/` or `manuscript/built/`. Save a new file."
      ),
      paste(
        "- Add a `## v(NN+1)` section to `CHANGELOG.md` with the file name,",
        "who edited it, the version it starts from, and what changed."
      ),
      paste(
        "- `verify()` then fails on `manual_not_ahead` until a person starts",
        "the next run version. Do not start it; ask."
      ),
      "- A built manuscript reads `read_results()` of its own version only."
    )
  )
})

test_that("create_project stops before it writes anything, and names every file that is already there", {
  home <- local_empty_home()
  write_lines(home, "README.md", "# My own README")
  write_lines(home, "v01_functions/data_raw.R", "x <- 1")
  write_lines(home, "pbn.yml", "extends: house_2026_11_17")
  before <- file_states(home)
  expect_error(
    create_project(home = home),
    paste0(
      "create_project() writes no file that is already there. These files ",
      "of the new project exist in ",
      home,
      ": pbn.yml, README.md, v01_functions/data_raw.R. Move them, or give ",
      "another folder as `home`."
    ),
    fixed = TRUE
  )
  expect_identical(file_states(home), before)
  expect_error(
    create_project(home = file.path(home, "absent")),
    paste0(
      "The folder ",
      file.path(home, "absent"),
      " does not exist. Create it, and run create_project() again."
    ),
    fixed = TRUE
  )
  expect_error(
    create_project(home = home, folders = c(main = "")),
    "`folders` must be one or more paths of folders",
    fixed = TRUE
  )
  expect_identical(file_states(home), before)
})

test_that("verify() gives the verdict PASS on a new project without real data, and the run with real fails with the message to read the data", {
  skip_if_pbn_not_installed()
  skip_if_not_installed("cohort")
  home <- local_empty_home()
  suppressMessages(create_project(home = home))
  out <- utils::capture.output(res <- verify_result("v01", home))
  expect_identical(res$status, 0L)
  expect_identical(attr(res, "verdict"), "PASS")
  expect_identical(
    attr(res, "lines"),
    c(
      "verify(v01): v01_run.R ended with status 0.",
      "check.csv files of this run: 1.",
      "Failed assertions: 0.",
      "check_project(): 0 FAIL, 0 WARN.",
      paste(
        "Verdict: PASS on the fake scenarios only. The scenario `real` did",
        "not run."
      )
    )
  )
  check <- utils::read.csv(file.path(home, "v01_results", "known", "check.csv"))
  expect_identical(check$expected, c(1, 0.35))
  expect_identical(check$pass, c(TRUE, TRUE))
  expect_false(dir.exists(file.path(home, "v01_results", "real")))

  # Without `only`, the run includes real, and data_raw() stops for it.
  run <- file.path(home, "v01_run.R")
  writeLines(
    sub(
      "pbn::scenarios(CFG, only = setdiff(CFG$scenarios$scenario, \"real\"))",
      "pbn::scenarios(CFG)",
      readLines(run),
      fixed = TRUE
    ),
    run
  )
  expect_true("scenarios <- pbn::scenarios(CFG)" %in% readLines(run))
  out <- utils::capture.output(res <- verify_result("v01", home))
  expect_identical(res$status, 1L)
  expect_identical(attr(res, "verdict"), "FAIL")
  expect_true(any(grepl(
    "Replace this stop() in data_raw() with code that reads the real data",
    out,
    fixed = TRUE
  )))
})

test_that("new_version freezes the old version, copies it, changes the version of the run script, adds the section of CHANGELOG.md, and the project passes", {
  home <- local_empty_home()
  suppressMessages(create_project(home = home))
  write_lines(home, "v01_functions/sub/.hidden", "x")
  expect_message(
    to <- new_version("v01", home = home),
    paste0(
      "new_version(): v01 is now frozen in v01_frozen.md5. Wrote v02_run.R ",
      "and v02_functions/. Added the section ## v02 to CHANGELOG.md."
    ),
    fixed = TRUE
  )
  expect_identical(to, "v02")
  v01 <- list.files(
    file.path(home, "v01_functions"),
    recursive = TRUE,
    all.files = TRUE
  )
  expect_identical(
    list.files(
      file.path(home, "v02_functions"),
      recursive = TRUE,
      all.files = TRUE
    ),
    v01
  )
  expect_identical(
    unname(tools::md5sum(file.path(home, "v02_functions", v01))),
    unname(tools::md5sum(file.path(home, "v01_functions", v01)))
  )
  old_run <- readLines(file.path(home, "v01_run.R"))
  new_run <- readLines(file.path(home, "v02_run.R"))
  expect_identical(
    new_run[1],
    sub("\"v01\"", "\"v02\"", old_run[1], fixed = TRUE)
  )
  expect_identical(new_run[-1], old_run[-1])
  expect_identical(
    readLines(file.path(home, "CHANGELOG.md")),
    c(
      "# Changelog",
      "",
      "## v02",
      "",
      "- Started from v01 with `pbn::new_version()`.",
      "",
      "## v01",
      "",
      "- Created the project with `pbn::create_project()`."
    )
  )
  found <- project_findings(home)
  expect_identical(unique(found$status), "PASS")

  # A frozen version without a change stays as it is.
  md5_v01 <- md5_of(file.path(home, "v01_frozen.md5"))
  freeze_version("v02", home)
  md5_v02 <- md5_of(file.path(home, "v02_frozen.md5"))
  expect_message(
    new_version("v02", home = home),
    "v02 stays frozen in v02_frozen.md5",
    fixed = TRUE
  )
  expect_identical(md5_of(file.path(home, "v01_frozen.md5")), md5_v01)
  expect_identical(md5_of(file.path(home, "v02_frozen.md5")), md5_v02)
  expect_identical(
    readLines(file.path(home, "v03_run.R"))[1],
    sub("\"v01\"", "\"v03\"", old_run[1], fixed = TRUE)
  )
  expect_identical(unique(project_findings(home)$status), "PASS")
  # A new version starts from the newest version only.
  expect_error(
    new_version("v02", "v05", home = home),
    paste0(
      "new_version() starts a new version from the newest version, v03, ",
      "and v02 is older. Give from = \"v03\"."
    ),
    fixed = TRUE
  )
  expect_false(file.exists(file.path(home, "v05_run.R")))
})

test_that("new_version stops before it writes anything on a target that exists, a changed frozen version, an older target and a run script without the call", {
  home <- local_empty_home()
  suppressMessages(create_project(home = home))
  suppressMessages(new_version("v01", home = home))
  before <- file_states(home)
  expect_error(
    new_version("v01", home = home),
    paste0(
      "new_version() writes no file that is already there. These files and ",
      "folders of v02 exist in ",
      home,
      ": v02_run.R, v02_functions. Give another version as `to`."
    ),
    fixed = TRUE
  )
  expect_error(
    new_version("v02", "v01", home = home),
    "These files and folders of v01 exist in ",
    fixed = TRUE
  )
  write_lines(home, "v02_frozen.md5", "x")
  expect_error(
    new_version("v01", "v02", home = home),
    ": v02_run.R, v02_functions, v02_frozen.md5. Give",
    fixed = TRUE
  )
  unlink(file.path(home, "v02_frozen.md5"))
  expect_error(
    new_version("v05", home = home),
    paste0(
      "new_version() needs v05_run.R and v05_functions/ in ",
      home,
      ". Give the folder of the project as `home`, or give a version that ",
      "exists as `from`."
    ),
    fixed = TRUE
  )

  # v02 is the newest version. A new version starts from it, and it MUST be
  # newer.
  expect_error(
    new_version("v01", "v03", home = home),
    paste0(
      "new_version() starts a new version from the newest version, v02, ",
      "and v01 is older. Give from = \"v02\"."
    ),
    fixed = TRUE
  )
  expect_error(
    new_version("v02", "v00", home = home),
    paste0(
      "new_version() makes the newest version, and v00 is not newer than ",
      "v02. Give a newer version as `to`."
    ),
    fixed = TRUE
  )

  freeze_version("v02", home)
  cat("x <- 2\n", file = file.path(home, "v02_run.R"), append = TRUE)
  write_lines(home, "v02_functions/new.R", "n <- function() return(1)")
  expect_error(
    new_version("v02", home = home),
    paste0(
      "v02 is frozen, and v02_frozen.md5 does not match the files of v02:\n",
      "  v02_functions/new.R: added\n",
      "  v02_run.R: changed\n",
      "Put the files back as they were, or run pbn::freeze_version(\"v02\") ",
      "to record the change as intended."
    ),
    fixed = TRUE
  )
  unlink(file.path(home, c("v02_functions/new.R", "v02_frozen.md5")))
  writeLines(
    utils::head(readLines(file.path(home, "v02_run.R")), -1),
    file.path(home, "v02_run.R")
  )

  write_lines(home, "v05_run.R", "x <- 1")
  write_lines(home, "v05_functions/f.R", "f <- function() return(1)")
  expect_error(
    new_version("v05", home = home),
    paste0(
      "new_version() needs one call pbn::initialize_project(version = ",
      "\"v05\") in v05_run.R, and it found 0."
    ),
    fixed = TRUE
  )
  unlink(file.path(home, c("v05_run.R", "v05_functions")), recursive = TRUE)
  # The test changed v02_run.R and put it back, so its time changed.
  expect_identical(
    file_states(home)[, c("file", "md5")],
    before[, c("file", "md5")]
  )
  expect_error(new_version("v1", home = home), "`version` must be")
  expect_error(
    new_version("v99", home = home),
    "There is no version after v99.",
    fixed = TRUE
  )
})

test_that("new_version changes the version argument by its name or its place, and no other string", {
  home <- local_empty_home()
  write_lines(
    home,
    "v01_run.R",
    c(
      "pbn::initialize_project(",
      "  home = \".\",",
      "  folders = c(main = \"../v01\"),",
      "  version = 'v01'",
      ")",
      "x <- \"v01\""
    )
  )
  write_lines(home, "v01_functions/f.R", "f <- function() return(1)")
  suppressMessages(new_version("v01", home = home))
  expect_identical(
    readLines(file.path(home, "v02_run.R")),
    c(
      "pbn::initialize_project(",
      "  home = \".\",",
      "  folders = c(main = \"../v01\"),",
      "  version = \"v02\"",
      ")",
      "x <- \"v01\""
    )
  )
  write_lines(
    home,
    "v03_run.R",
    "pbn::initialize_project(\"v03\", home = \".\", folders = c(main = \"x\"))"
  )
  write_lines(home, "v03_functions/f.R", "f <- function() return(1)")
  freeze_version("v02", home)
  suppressMessages(new_version("v03", home = home))
  expect_identical(
    readLines(file.path(home, "v04_run.R")),
    "pbn::initialize_project(\"v04\", home = \".\", folders = c(main = \"x\"))"
  )
  # The CHANGELOG.md that new_version() created has a section for each new
  # version.
  expect_identical(
    readLines(file.path(home, "CHANGELOG.md")),
    c(
      "# Changelog",
      "",
      "## v04",
      "",
      "- Started from v03 with `pbn::new_version()`.",
      "",
      "## v02",
      "",
      "- Started from v01 with `pbn::new_version()`."
    )
  )
})

test_that("new_version and verify() run the new version of a new project", {
  skip_if_pbn_not_installed()
  skip_if_not_installed("cohort")
  home <- local_empty_home()
  suppressMessages(create_project(home = home))
  suppressMessages(new_version("v01", home = home))
  out <- utils::capture.output(res <- verify_result("v02", home))
  expect_identical(attr(res, "verdict"), "PASS")
  expect_true(file.exists(file.path(home, "v02_results", "known", "check.csv")))
})

test_that("create_project stops before it writes anything when the package cohort is not installed, with the line that installs it", {
  home <- local_empty_home()
  local_mocked_bindings(cohort_installed = function() return(FALSE))
  expect_error(
    create_project(home = home),
    paste0(
      "create_project() writes a project whose data step cohort needs the ",
      "package cohort, and cohort is not installed. Install it with ",
      "pak::pak(c(\"raubreywhite/pbn\", \"raubreywhite/cohort\")), and run ",
      "create_project() again."
    ),
    fixed = TRUE
  )
  expect_identical(
    list.files(home, all.files = TRUE, no.. = TRUE),
    character(0)
  )
})

test_that("create_project removes each file and folder that it created when a write stops, and a second call works", {
  home <- local_empty_home()
  write_lines(home, "adhoc/notes.txt", "mine")
  dir.create(file.path(home, "v01_functions"))
  real_write <- write_text
  seen <- new.env()
  failing_write <- function(file, lines) {
    if (basename(file) == "fit_risk_difference.R") {
      seen$files <- list.files(home, recursive = TRUE, all.files = TRUE)
      stop("the disk is full")
    }
    return(real_write(file, lines))
  }
  with_mocked_bindings(
    expect_error(create_project(home = home), "the disk is full", fixed = TRUE),
    write_text = failing_write
  )
  # The call wrote files before the error.
  expect_true(all(
    c("pbn.yml", "AGENTS.md", "v01_functions/data_raw.R") %in% seen$files
  ))
  # It removed them, and kept the file and the folder that were there.
  expect_identical(
    sort(list.files(
      home,
      recursive = TRUE,
      all.files = TRUE,
      include.dirs = TRUE
    )),
    c("adhoc", "adhoc/notes.txt", "v01_functions")
  )
  suppressMessages(create_project(home = home))
  expect_identical(unique(project_findings(home)$status), "PASS")
})

test_that("new_version removes each file and folder that it created, also the vNN_frozen.md5, and puts CHANGELOG.md back when a step stops", {
  home <- local_empty_home()
  suppressMessages(create_project(home = home))
  before <- file_states(home)[, c("file", "md5")]
  half_copy <- function(from, to) {
    dir.create(to)
    file.copy(file.path(from, "_cfg.R"), to)
    stop("the disk is full")
  }
  with_mocked_bindings(
    expect_error(
      new_version("v01", home = home),
      "the disk is full",
      fixed = TRUE
    ),
    copy_folder = half_copy
  )
  expect_identical(file_states(home)[, c("file", "md5")], before)

  half_changelog <- function(file, from, to) {
    writeLines("half", file)
    stop("the disk is full")
  }
  with_mocked_bindings(
    expect_error(
      new_version("v01", home = home),
      "the disk is full",
      fixed = TRUE
    ),
    add_changelog_section = half_changelog
  )
  expect_identical(file_states(home)[, c("file", "md5")], before)

  # A vNN_frozen.md5 that was there before the call stays.
  freeze_version("v01", home)
  frozen <- file_states(home)[, c("file", "md5")]
  with_mocked_bindings(
    expect_error(
      new_version("v01", home = home),
      "the disk is full",
      fixed = TRUE
    ),
    copy_folder = half_copy
  )
  expect_identical(file_states(home)[, c("file", "md5")], frozen)

  expect_message(
    new_version("v01", home = home),
    "v01 stays frozen in v01_frozen.md5",
    fixed = TRUE
  )
  expect_identical(unique(project_findings(home)$status), "PASS")
})

test_that("new_version says that CHANGELOG.md already has the section of the new version, and leaves the file as it is", {
  home <- local_empty_home()
  suppressMessages(create_project(home = home))
  log <- file.path(home, "CHANGELOG.md")
  writeLines(c(readLines(log), "", "## v02", "", "- Planned."), log)
  before <- md5_of(log)
  expect_message(
    new_version("v01", home = home),
    paste0(
      "new_version(): v01 is now frozen in v01_frozen.md5. Wrote v02_run.R ",
      "and v02_functions/. CHANGELOG.md already has the section ## v02."
    ),
    fixed = TRUE
  )
  expect_identical(md5_of(log), before)
})
