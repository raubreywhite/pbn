# The set house_2026_11_20: the rules built_reads_own_version, built_has_run
# and manual_not_ahead of the one version counter of the manuscripts, and
# the section of each manual version that readme_and_changelog needs. The
# projects of house_2026_11_19 see none of it.

# The rules that the set house_2026_11_20 adds to house_2026_11_19.
rules_2026_11_20 <- c(
  "built_reads_own_version",
  "built_has_run",
  "manual_not_ahead"
)

# A synthetic project of local_project() on the set house_2026_11_20. The
# manual manuscript v01_AB.docx of local_project() is at the newest run v01,
# so manual_not_ahead fails it. This project has no manual manuscript.
local_project_20 <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_20")
  unlink(file.path(p$home, "manuscript", "manual", "v01_AB.docx"))
  return(p)
}

# The findings of the rule `rule` in the project in `home`.
rows_20 <- function(home, rule) {
  found <- project_findings(home)
  return(found[found$rule == rule, c("status", "path", "detail")])
}

# A run version `version` of the project in `home`: vNN_run.R, the functions
# file vNN_functions/f.R and the section "## vNN" of CHANGELOG.md. Each
# older version gets frozen, because frozen_versions_unchanged needs it.
add_run <- function(home, version) {
  for (old in project_versions(home)) {
    freeze_version(old, home)
  }
  write_lines(
    home,
    paste0(version, "_run.R"),
    paste0(
      "pbn::initialize_project(version = \"",
      version,
      "\", home = \".\", folders = c(main = \"x\"))"
    )
  )
  write_lines(
    home,
    file.path(paste0(version, "_functions"), "f.R"),
    "f <- function() return(1)"
  )
  cat(
    "\n## ",
    version,
    "\n",
    file = file.path(home, "CHANGELOG.md"),
    append = TRUE,
    sep = ""
  )
  return(invisible(home))
}

test_that("house_2026_11_20 is house_2026_11_19 with three new rules at fail, and a new pbn.yml and create_project() name the newest set", {
  old <- read_house_set("house_2026_11_19")
  new <- read_house_set("house_2026_11_20")
  expect_identical(new[names(old)], old)
  expect_identical(
    names(new),
    intersect(names(project_rules()), c(names(old), rules_2026_11_20))
  )
  expect_identical(setdiff(names(new), names(old)), rules_2026_11_20)
  expect_identical(
    vapply(new[rules_2026_11_20], function(r) return(r$level), character(1)),
    c(
      built_reads_own_version = "fail",
      built_has_run = "fail",
      manual_not_ahead = "fail"
    )
  )
  expect_false(any(vapply(
    new[rules_2026_11_20],
    function(r) return(r$locked),
    logical(1)
  )))
  expect_identical(starter_line(), "extends: house_2026_11_26")
  expect_identical(create_project_set, "house_2026_11_26")
})

test_that("a project on house_2026_11_20 without a manual manuscript passes every rule, and the new rules print their PASS lines", {
  p <- local_project_20()
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found$detail[found$rule %in% rules_2026_11_20],
    c(
      "every read_results() call in manuscript/built/vNN/ reads the version vNN",
      "every folder manuscript/built/vNN/ has a vNN_run.R",
      paste(
        "every manuscript/manual/vNN_<INITIALS>.docx is older than the newest",
        "run version"
      )
    )
  )
})

test_that("built_reads_own_version fails each read_results() call in manuscript/built/vNN/ that does not name vNN", {
  p <- local_project_20()
  write_lines(
    p$home,
    "manuscript/built/v01/numbers.R",
    c(
      "a <- read_results(\"v01\")",
      "b <- pbn::read_results(\"v02\")",
      "v <- \"v01\"",
      "c <- pbn::read_results(version = v)",
      "d <- read_results(scenario = \"real\")"
    )
  )
  write_lines(
    p$home,
    "manuscript/built/v01/text.qmd",
    c(
      "```{r}",
      "r <- pbn::read_results(\"v01\")",
      "```",
      "",
      "The count is `r nrow(pbn::read_results(\"v03\")$table1)`."
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "built_reads_own_version")
  expect_identical(
    rows_20(p$home, "built_reads_own_version"),
    data.frame(
      status = c("FAIL", "FAIL"),
      path = c(
        "manuscript/built/v01/numbers.R",
        "manuscript/built/v01/text.qmd"
      ),
      detail = with_fix(
        "built_reads_own_version",
        c(
          paste0(
            "manuscript/built/v01/numbers.R: a manuscript in ",
            "manuscript/built/v01/ reads only the version \"v01\", and these ",
            "calls do not: pbn::read_results(\"v02\"); ",
            "pbn::read_results(version = v); ",
            "read_results(scenario = \"real\")"
          ),
          paste0(
            "manuscript/built/v01/text.qmd: a manuscript in ",
            "manuscript/built/v01/ reads only the version \"v01\", and these ",
            "calls do not: pbn::read_results(\"v03\")"
          )
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("built_reads_own_version passes the calls of the own version, a file without a call, and a file outside a folder vNN/", {
  p <- local_project_20()
  write_lines(
    p$home,
    "manuscript/built/v01/numbers.R",
    c(
      "a <- read_results(\"v01\")",
      "b <- pbn::read_results(\"v01\", scenario = \"real\")",
      "c <- pbn::read_results(version = \"v01\")"
    )
  )
  write_lines(
    p$home,
    "manuscript/built/v01/text.Rmd",
    c("```{r}", "x <- 1", "```", "", "`r pbn::read_results(\"v01\")$n`")
  )
  write_lines(p$home, "manuscript/built/notes.R", "r <- read_results(\"v02\")")
  write_lines(
    p$home,
    "manuscript/built/draft/a.R",
    "r <- read_results(\"v02\")"
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    rows_20(p$home, "built_reads_own_version")$status,
    "PASS"
  )
})

test_that("built_has_run fails a folder manuscript/built/vNN/ without vNN_run.R, on the disk and in git", {
  p <- local_project_20()
  write_lines(
    p$home,
    "manuscript/built/v02/numbers.R",
    "r <- read_results(\"v02\")"
  )
  dir.create(file.path(p$home, "manuscript", "built", "v03"))
  want <- data.frame(
    status = c("FAIL", "FAIL"),
    path = c("manuscript/built/v02", "manuscript/built/v03"),
    detail = with_fix(
      "built_has_run",
      c(
        "manuscript/built/v02: there is no v02_run.R",
        "manuscript/built/v03: there is no v03_run.R"
      )
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "built_has_run")
  expect_identical(
    rows_20(p$home, "built_has_run"),
    want,
    ignore_attr = TRUE
  )
  # git tracks no empty folder, so in git only v02 is a folder of the list.
  git_in(p$home, "init", "-q")
  expect_identical(
    rows_20(p$home, "built_has_run"),
    want[1, ],
    ignore_attr = TRUE
  )
})

test_that("built_has_run passes each folder manuscript/built/vNN/ with its vNN_run.R", {
  p <- local_project_20()
  write_lines(
    p$home,
    "manuscript/built/v01/numbers.R",
    "r <- read_results(\"v01\")"
  )
  add_run(p$home, "v02")
  write_lines(
    p$home,
    "manuscript/built/v02/numbers.R",
    "r <- read_results(\"v02\")"
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(rows_20(p$home, "built_has_run")$status, "PASS")

  # A project without manuscript/built/ has no folder to check.
  unlink(file.path(p$home, "manuscript", "built"), recursive = TRUE)
  expect_identical(
    rows_20(p$home, "built_has_run"),
    data.frame(
      status = "PASS",
      path = "",
      detail = "every folder manuscript/built/vNN/ has a vNN_run.R"
    ),
    ignore_attr = TRUE
  )
})

test_that("manual_not_ahead fails a manual version at or above the newest run version, and names the call that starts the next run", {
  p <- local_project_20()
  add_run(p$home, "v02")
  write_lines(p$home, "manuscript/manual/v01_AB.docx", "")
  write_lines(p$home, "manuscript/manual/v02_AB.docx", "")
  write_lines(p$home, "manuscript/manual/v03_CD.docx", "")
  cat(
    "\n## v03\n",
    file = file.path(p$home, "CHANGELOG.md"),
    append = TRUE
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "manual_not_ahead")
  expect_identical(
    rows_20(p$home, "manual_not_ahead"),
    data.frame(
      status = c("FAIL", "FAIL"),
      path = c(
        "manuscript/manual/v02_AB.docx",
        "manuscript/manual/v03_CD.docx"
      ),
      detail = with_fix(
        "manual_not_ahead",
        c(
          paste0(
            "manual v02 is ahead of the newest run v02: freeze v02 and start ",
            "v03 with pbn::new_version(\"v02\", to = \"v03\")"
          ),
          paste0(
            "manual v03 is ahead of the newest run v02: freeze v02 and start ",
            "v04 with pbn::new_version(\"v02\", to = \"v04\")"
          )
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("manual_not_ahead passes manual versions below the newest run, a name of another pattern, and a project without a run script", {
  p <- local_project_20()
  add_run(p$home, "v03")
  write_lines(p$home, "manuscript/manual/v02_AB.docx", "")
  write_lines(p$home, "manuscript/manual/v01_AB.docx", "")
  cat(
    "\n## v02\n",
    file = file.path(p$home, "CHANGELOG.md"),
    append = TRUE
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(rows_20(p$home, "manual_not_ahead")$status, "PASS")

  # manual_manuscript_names fails the name, and manual_not_ahead skips it.
  write_lines(p$home, "manuscript/manual/v9_AB.docx", "")
  expect_identical(rows_20(p$home, "manual_not_ahead")$status, "PASS")
  unlink(file.path(p$home, "manuscript", "manual", "v9_AB.docx"))

  q <- local_project_20()
  unlink(file.path(q$home, "v01_run.R"))
  write_lines(q$home, "manuscript/manual/v05_AB.docx", "")
  expect_identical(
    rows_20(q$home, "manual_not_ahead"),
    data.frame(
      status = "PASS",
      path = "",
      detail = "there is no vNN_run.R, so nothing to compare"
    ),
    ignore_attr = TRUE
  )
})

test_that("readme_and_changelog of house_2026_11_20 fails a manual version without its section in CHANGELOG.md", {
  p <- local_project_20()
  add_run(p$home, "v03")
  write_lines(p$home, "manuscript/manual/v02_AB.docx", "")
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), "readme_and_changelog")
  expect_identical(
    rows_20(p$home, "readme_and_changelog"),
    data.frame(
      status = "FAIL",
      path = "CHANGELOG.md",
      detail = with_fix(
        "readme_and_changelog",
        "CHANGELOG.md: no section \"## v02\""
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("readme_and_changelog of house_2026_11_20 passes a manual version with its section in CHANGELOG.md", {
  p <- local_project_20()
  add_run(p$home, "v03")
  write_lines(p$home, "manuscript/manual/v02_AB.docx", "")
  cat(
    "\n## v02 (2026-11-20)\n",
    file = file.path(p$home, "CHANGELOG.md"),
    append = TRUE
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(rows_20(p$home, "readme_and_changelog")$status, "PASS")
})

test_that("a project on house_2026_11_19 runs none of the three rules, needs no section for a manual version, and its pbn.yml cannot name a new rule", {
  p <- local_project_20()
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_19")
  add_run(p$home, "v03")
  write_lines(p$home, "manuscript/manual/v02_AB.docx", "")
  write_lines(p$home, "manuscript/manual/v07_AB.docx", "")
  write_lines(
    p$home,
    "manuscript/built/v09/numbers.R",
    "r <- read_results(\"v01\")"
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_false(any(rules_2026_11_20 %in% found$rule))
  expect_identical(
    unique(found$rule),
    names(read_house_set("house_2026_11_19"))
  )
  expect_identical(
    found[found$rule == "readme_and_changelog", "status"],
    "PASS"
  )
  # The same project on house_2026_11_20 fails each of them.
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_20")
  expect_identical(
    failed_rules(project_findings(p$home)),
    c(
      "readme_and_changelog",
      "built_reads_own_version",
      "built_has_run",
      "manual_not_ahead"
    )
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_19", "rules:", "  manual_not_ahead: warn")
  )
  err <- tryCatch(read_rules(p$home), error = function(e) return(e))
  expect_identical(
    conditionMessage(err),
    paste0(
      "pbn.yml: rules: manual_not_ahead is not a rule of the set ",
      "house_2026_11_19. The oldest set with it is house_2026_11_20. To use ",
      "it, change extends: to that set or a newer one."
    )
  )
})

test_that("a manual manuscript MAY have a lower-case suffix, such as v02_AB_si.docx, and it counts as its version", {
  p <- local_project_20()
  add_run(p$home, "v03")
  write_lines(p$home, "manuscript/manual/v02_AB.docx", "")
  write_lines(p$home, "manuscript/manual/v02_AB_si.docx", "")
  write_lines(p$home, "manuscript/manual/v02_CD2_letter2.docx", "")
  cat("\n## v02\n", file = file.path(p$home, "CHANGELOG.md"), append = TRUE)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))

  q <- local_project_20()
  write_lines(q$home, "manuscript/manual/v02_AB.docx", "")
  write_lines(q$home, "manuscript/manual/v02_AB_si.docx", "")
  found <- project_findings(q$home)
  expect_identical(
    sort(found$path[found$rule == "manual_not_ahead" & found$status == "FAIL"]),
    c("manuscript/manual/v02_AB.docx", "manuscript/manual/v02_AB_si.docx")
  )
  expect_true(any(
    found$rule == "readme_and_changelog" &
      grepl("## v02", found$detail, fixed = TRUE)
  ))
})

test_that("a manual manuscript with a suffix that is not one lower-case word fails its name", {
  for (name in c(
    "v02_AB_SI.docx",
    "v02_AB_.docx",
    "v02_AB_s-i.docx",
    "v02_AB_si_2.docx",
    "v02_AB-si.docx"
  )) {
    p <- local_project_20()
    write_lines(p$home, file.path("manuscript", "manual", name), "")
    found <- project_findings(p$home)
    expect_true(
      file.path("manuscript", "manual", name) %in%
        found$path[
          found$rule == "manual_manuscript_names" & found$status == "FAIL"
        ],
      info = name
    )
  }
})

test_that("under house_2026_11_26 a pbn::txt() call in manuscript/built/v02/ with another version fails built_reads_own_version, and under house_2026_11_20 it is not read", {
  p <- local_project_20()
  write_lines(
    p$home,
    "manuscript/built/v02/text.qmd",
    c(
      "```{r}",
      "a <- pbn::txt(\"Effect\", version = \"v02\")",
      "```",
      "",
      "The effect is `r pbn::txt(\"x\", version = \"v01\")`, `r txt(\"y\", 2, NULL, \"v02\")`."
    )
  )
  expect_identical(
    rows_20(p$home, "built_reads_own_version")$status,
    "PASS"
  )
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  expect_identical(
    rows_20(p$home, "built_reads_own_version"),
    data.frame(
      status = "FAIL",
      path = "manuscript/built/v02/text.qmd",
      detail = with_fix(
        "built_reads_own_version",
        paste0(
          "manuscript/built/v02/text.qmd: a manuscript in ",
          "manuscript/built/v02/ reads only the version \"v02\", and these ",
          "calls do not: pbn::txt(\"x\", version = \"v01\")"
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("under house_2026_11_26 manuscript_reads_real fails a pbn::txt() call with an argument that txt() does not take, and passes the others", {
  p <- local_project_20()
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  write_lines(
    p$home,
    "manuscript/built/v01/text.R",
    c(
      "a <- pbn::txt(\"Effect\", version = \"v01\")",
      "b <- pbn::txt(\"Effect\", scenario = \"null\", version = \"v01\")"
    )
  )
  expect_identical(
    rows_20(p$home, "manuscript_reads_real"),
    data.frame(
      status = "FAIL",
      path = "manuscript/built/v01/text.R",
      detail = with_fix(
        "manuscript_reads_real",
        paste0(
          "manuscript/built/v01/text.R: pbn::txt() with an argument that it ",
          "does not take: pbn::txt(\"Effect\", scenario = \"null\", version = ",
          "\"v01\"). A manuscript reads only scenario \"real\""
        )
      )
    ),
    ignore_attr = TRUE
  )
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_25")
  expect_identical(rows_20(p$home, "manuscript_reads_real")$status, "PASS")
})
