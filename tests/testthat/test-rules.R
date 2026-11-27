# The rules of a project: pbn.yml, the house rule sets it extends, and what
# they change in check_project() and initialize_project().

# The error of read_rules() on a project whose pbn.yml holds `lines`.
rules_error <- function(lines) {
  p <- local_project()
  write_lines(p$home, "pbn.yml", lines)
  err <- tryCatch(read_rules(p$home), error = function(e) return(e))
  if (!inherits(err, "error")) {
    stop("read_rules() did not stop")
  }
  return(conditionMessage(err))
}

test_that("each shipped set names every rule of check_project() in its order, and house_2026_11_17 holds the levels and the parameters of pbn 2026.11.23", {
  expect_identical(
    house_sets(),
    c(
      "house_2026_11_17",
      "house_2026_11_19",
      "house_2026_11_20",
      "house_2026_11_25",
      "house_2026_11_26"
    )
  )
  for (set in house_sets()) {
    rules <- read_house_set(set)
    expect_identical(
      names(rules),
      intersect(names(project_rules()), names(rules)),
      info = set
    )
    levels <- vapply(rules, function(r) return(r$level), character(1))
    expect_true(all(levels %in% rule_levels), info = set)
  }
  expect_identical(
    names(read_house_set("house_2026_11_26")),
    names(project_rules())
  )
  expect_identical(
    names(read_house_set("house_2026_11_17")),
    setdiff(
      names(project_rules()),
      c(
        rules_2026_11_19,
        "built_reads_own_version",
        "built_has_run",
        "manual_not_ahead",
        rules_2026_11_25,
        rules_2026_11_26
      )
    )
  )
  rules <- read_house_set("house_2026_11_17")
  levels <- vapply(rules, function(r) return(r$level), character(1))
  expect_identical(
    unname(levels),
    c(rep("fail", 16), "warn", "warn")
  )
  expect_identical(
    names(levels)[15:18],
    c(
      "no_absolute_paths",
      "blocks_list",
      "real_results_are_fake",
      "unknown_path"
    )
  )
  expect_identical(rules$loc_per_file$limit, 1000L)
  expect_identical(rules$cyclomatic_complexity$limit, 15L)
  expect_identical(rules$no_local_assign$forbid, local_assign_constructs())
  expect_identical(
    local_assign_constructs(),
    c(
      "local()",
      "assign()",
      "delayedAssign()",
      "assignInNamespace()",
      "assignInMyNamespace()",
      "<<-",
      "->>"
    )
  )
  locked <- names(rules)[vapply(
    rules,
    function(r) return(r$locked),
    logical(1)
  )]
  expect_identical(
    locked,
    c("results_contract", "gitexclude_not_tracked", "fake_data_names")
  )
  expect_identical(sort(names(locked_reasons)), sort(locked))
})

test_that("the bytes of each shipped set are pinned", {
  # Add the md5 of each new set here. NEVER change an md5 of this list. A
  # project names its set in pbn.yml, so that a new version of pbn does not
  # change its rules. A change to a released set would change the rules of
  # every project that names it. A change to the house rules is a new set.
  # The md5 is of the bytes without CR, so that a checkout with CR LF line
  # ends, as git can make on Windows, gives the same md5.
  # house_2026_11_17 got the rules no_absolute_paths and blocks_list, and then
  # the file.path() part of no_absolute_paths, and then the rule
  # real_results_are_fake, the file blocks.R of blocks_list and the shape of
  # that file, before the
  # first release of pbn, which had no tag and no build. Those were the
  # changes of its md5.
  # house_2026_11_19 got the rules readme_and_changelog, changelog_with_code
  # and comment_share in pbn 2026.11.28, hours after its release and before
  # a project used it. The maintainer approved that change of its md5.
  # house_2026_11_26 got the words "of any initials" in the comment of
  # manual_has_supplement. The maintainer approved that change of its md5
  # on 2026-11-26.
  pinned <- c(
    house_2026_11_17 = "ac42b7368bc90e2735bf4cb47e4a8e0e",
    house_2026_11_19 = "37f18daf1accecb9c35963a69d2b6097",
    house_2026_11_20 = "66615d03431f9ae95a8102a200d81831",
    house_2026_11_25 = "91998c756abbf55915449ce3b14892a2",
    house_2026_11_26 = "382e8b0470122a169de81319d346d31c"
  )
  folder <- system.file("rules", package = "pbn")
  files <- list.files(folder, pattern = "\\.yml$")
  md5 <- vapply(
    files,
    function(f) {
      bytes <- readBin(
        file.path(folder, f),
        "raw",
        file.size(file.path(folder, f))
      )
      tmp <- tempfile()
      on.exit(unlink(tmp))
      writeBin(bytes[bytes != as.raw(13)], tmp)
      return(unname(tools::md5sum(tmp)))
    },
    character(1)
  )
  names(md5) <- sub("\\.yml$", "", files)
  expect_identical(md5[names(pinned)], pinned)
  expect_setequal(names(md5), names(pinned))
})

test_that("check_results() has the rules that pbn.yml cannot name", {
  expect_identical(check_results(list())$rule, results_rule_names)
})

test_that("a project without pbn.yml stops check_project() and initialize_project() with the one line to write", {
  p <- local_project()
  unlink(file.path(p$home, "pbn.yml"))
  want <- paste0(
    "There is no pbn.yml in ",
    p$home,
    ". Create it with this one line:\n\n",
    "extends: house_2026_11_26\n\n",
    "The line names the house rules of check_project(). ",
    "?check_project tells how pbn.yml changes them."
  )
  expect_error(capture.output(check_project(p$home)), want, fixed = TRUE)
  expect_error(project_findings(p$home), want, fixed = TRUE)
  local_clean_state()
  expect_error(
    suppressMessages(initialize_project(
      "v01",
      p$home,
      p$data_default,
      env = new.env()
    )),
    paste0(
      "There is no pbn.yml in ",
      normalizePath(p$home, winslash = "/"),
      ". Create it with this one line:\n\nextends: house_2026_11_26\n\n"
    ),
    fixed = TRUE
  )
  expect_null(state$results)
})

test_that("a file pbn.yaml stops with the advice to rename it", {
  p <- local_project()
  write_lines(p$home, "pbn.yaml", house_line)
  want <- paste0(
    "Rename pbn.yaml to pbn.yml. pbn reads the rules of a project from ",
    "pbn.yml only."
  )
  expect_error(project_findings(p$home), want, fixed = TRUE)
  unlink(file.path(p$home, "pbn.yml"))
  expect_error(project_findings(p$home), want, fixed = TRUE)
})

test_that("a file pbn_allow.txt stops with its lines as the YAML of the section allow, and that YAML gives the same findings", {
  p <- local_project()
  write_lines(p$home, "adhoc/Notes.txt", "x")
  write_lines(p$home, "notes.txt", "x")
  write_lines(
    p$home,
    "pbn_allow.txt",
    c(
      "lower_case_names\tadhoc/Notes.txt",
      "unknown_path\tnotes.txt",
      "",
      "unknown_path\ta \"quoted\" name.txt",
      "no tab here"
    )
  )
  section <- c(
    "allow:",
    "  lower_case_names:",
    "    - \"adhoc/Notes.txt\"",
    "  unknown_path:",
    "    - \"notes.txt\"",
    "    - \"a \\\"quoted\\\" name.txt\"",
    "  # These lines of pbn_allow.txt have no TAB, so they allowed nothing:",
    "  # no tab here"
  )
  err <- tryCatch(project_findings(p$home), error = function(e) return(e))
  expect_identical(
    conditionMessage(err),
    paste0(
      "pbn reads no pbn_allow.txt. Move its lines into the section allow: ",
      "of pbn.yml, and delete pbn_allow.txt. These lines are the section:\n\n",
      paste(section, collapse = "\n")
    )
  )
  unlink(file.path(p$home, "pbn.yml"))
  err <- tryCatch(project_findings(p$home), error = function(e) return(e))
  expect_identical(
    conditionMessage(err),
    paste0(
      "pbn reads no pbn_allow.txt. Move its lines into the section allow: ",
      "of a new file pbn.yml, and delete pbn_allow.txt. This is the file:\n\n",
      paste(c("extends: house_2026_11_26", section), collapse = "\n")
    )
  )

  # Copy and paste: the file of the error gives the old findings.
  writeLines(
    sub("^.*This is the file:\n\n", "", conditionMessage(err)),
    file.path(p$home, "pbn.yml")
  )
  unlink(file.path(p$home, "pbn_allow.txt"))
  # In the newest set, manual_not_ahead fails the manual v01_AB.docx of
  # local_project(), which is at the newest run v01.
  unlink(file.path(p$home, "manuscript", "manual", "v01_AB.docx"))
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found$detail[found$status == "WARN"],
    c(
      with_fix(
        "lower_case_names",
        "adhoc/Notes.txt: the name has a capital letter (allowed)"
      ),
      with_fix(
        "unknown_path",
        "notes.txt: the house layout does not name it (allowed)"
      ),
      with_fix(
        "allow",
        "pbn.yml: the entry allow: unknown_path: a \"quoted\" name.txt matches no finding"
      )
    )
  )
})

test_that("extends MUST name a set of pbn, and is never the newest set by default", {
  sets <- paste0(
    " is not a rule set of pbn ",
    utils::packageVersion("pbn"),
    paste0(
      ". The sets are: house_2026_11_17, house_2026_11_19, house_2026_11_20, ",
      "house_2026_11_25, house_2026_11_26."
    )
  )
  expect_identical(
    rules_error("extends: house_2026.11.17"),
    paste0("pbn.yml: extends: house_2026.11.17", sets)
  )
  expect_identical(
    rules_error("extends: [house_2026_11_17, house_2026_11_17]"),
    paste0("pbn.yml: extends: house_2026_11_17 house_2026_11_17", sets)
  )
  no_extends <- paste0(
    "pbn.yml has no key extends. Add this line:\n\n",
    "extends: house_2026_11_26"
  )
  expect_identical(rules_error("rules:\n  loc_per_file: off"), no_extends)
  expect_identical(rules_error(""), no_extends)
  expect_identical(rules_error("# only a comment"), no_extends)
})

test_that("pbn.yml stops on invalid YAML, a duplicated key and a tab, with the message of the yaml package", {
  expect_identical(
    rules_error(c(house_line, "rules: [a")),
    paste0(
      "pbn.yml is not valid YAML. The yaml package says: Parser error: ",
      "while parsing a flow sequence at line 2, column 8 did not find ",
      "expected ',' or ']' at line 3, column 1"
    )
  )
  expect_identical(
    rules_error(c(
      house_line,
      "rules:",
      "  loc_per_file: warn",
      "  loc_per_file: off"
    )),
    paste0(
      "pbn.yml is not valid YAML. The yaml package says: ",
      "Duplicate map key: 'loc_per_file'"
    )
  )
  expect_identical(
    rules_error(c(house_line, "rules:", "\tloc_per_file: off")),
    paste0(
      "pbn.yml is not valid YAML. The yaml package says: Scanner error: ",
      "while scanning for the next token at line 3, column 1 found ",
      "character that cannot start any token at line 3, column 1"
    )
  )
})

test_that("pbn.yml stops on an unknown key, an unknown rule with the nearest rule, and a value that is not a map", {
  expect_identical(
    rules_error(c(house_line, "rule:", "  loc_per_file: off")),
    "pbn.yml: the key rule is not known. The keys are extends, rules, allow and ignore."
  )
  expect_identical(
    rules_error(c(house_line, "rules:", "  lower_case_name: warn")),
    paste0(
      "pbn.yml: rules: lower_case_name is not a rule of check_project(). ",
      "The nearest rule is lower_case_names."
    )
  )
  expect_identical(
    rules_error(c(house_line, "allow:", "  cyclomatic: [a.R]")),
    paste0(
      "pbn.yml: allow: cyclomatic is not a rule of check_project(). ",
      "The nearest rule is cyclomatic_complexity."
    )
  )
  expect_identical(
    rules_error("- extends: house_2026_11_17"),
    "pbn.yml MUST be a map with the keys extends, rules, allow and ignore."
  )
  expect_identical(
    rules_error(c(house_line, "rules: [loc_per_file]")),
    paste0(
      "pbn.yml: rules: MUST be a map from the names of rules, for example ",
      "rules:\n  lower_case_names: ..."
    )
  )
  expect_identical(
    rules_error(c(house_line, "allow:", "  lower_case_names: {a: 1}")),
    paste0(
      "pbn.yml: allow: lower_case_names MUST be a path or a list of paths, ",
      "for example [adhoc/notes.R]. A path that YAML reads as a word such as ",
      "off, yes or no needs quotes. To turn the rule off, write ",
      "lower_case_names: off under rules:."
    )
  )
})

test_that("pbn.yml stops on an empty forbid, on a word that YAML reads as a boolean under allow, and on such a word as a level other than off", {
  for (empty in c("{forbid: null}", "{forbid: []}", "\n    forbid:")) {
    expect_identical(
      rules_error(c(
        house_line,
        "rules:",
        paste0("  no_local_assign: ", empty)
      )),
      paste0(
        "pbn.yml: rules: no_local_assign: forbid is empty. To turn the rule ",
        "off, set its level to off."
      ),
      info = empty
    )
  }
  expect_identical(
    rules_error(c(
      house_line,
      "rules:",
      "  no_local_assign: {forbid: [assign(), yes]}"
    )),
    paste0(
      "pbn.yml: rules: no_local_assign: forbid MUST be a list of constructs. ",
      "They are: local(), assign(), delayedAssign(), assignInNamespace(), ",
      "assignInMyNamespace(), <<-, ->>."
    )
  )
  hint <- paste0(
    "pbn.yml: allow: unknown_path MUST be a path or a list of paths, for ",
    "example [adhoc/notes.R]. A path that YAML reads as a word such as off, ",
    "yes or no needs quotes. To turn the rule off, write unknown_path: off ",
    "under rules:."
  )
  for (value in c(
    "off",
    "[off, notes.txt]",
    "[y]",
    "yes",
    "true",
    "1",
    "",
    "{a: b}"
  )) {
    expect_identical(
      rules_error(c(house_line, "allow:", paste0("  unknown_path: ", value))),
      hint,
      info = value
    )
  }
  for (word in c("on", "yes", "no", "true", "false", "y", "Off", "OFF")) {
    expect_identical(
      rules_error(c(house_line, "rules:", paste0("  loc_per_file: ", word))),
      paste0(
        "pbn.yml: rules: loc_per_file: level is ",
        word,
        ", and it MUST be fail, warn or off."
      ),
      info = word
    )
  }
  expect_identical(
    rules_error(c(house_line, "rules:", "  loc_per_file: {limit: null}")),
    paste0(
      "pbn.yml: rules: loc_per_file: limit is null, and it MUST be a whole ",
      "number of 1 or more."
    )
  )

  # The level off, as YAML reads it or in quotes, and a path in quotes pass.
  p <- local_project()
  for (off in c("off", "\"off\"", "{level: off}")) {
    write_pbn_yml(p$home, "rules:", paste0("  loc_per_file: ", off))
    expect_identical(
      read_rules(p$home)$rules$loc_per_file$level,
      "off",
      info = off
    )
  }
  write_pbn_yml(
    p$home,
    "rules:",
    "  no_local_assign: {forbid: local()}",
    "allow:",
    "  unknown_path: [notes.txt, \"off\"]",
    "  lower_case_names: adhoc/Notes.txt"
  )
  rules <- read_rules(p$home)
  expect_identical(rules$rules$no_local_assign$forbid, "local()")
  expect_identical(
    rules$allow$path,
    c("notes.txt", "off", "adhoc/Notes.txt")
  )
  expect_identical(
    read_house_set("house_2026_11_17")$results_contract$locked,
    TRUE
  )
})

test_that("pbn.yml stops on an unknown parameter, a bad level, a bad limit and an unknown construct", {
  expect_identical(
    rules_error(c(house_line, "rules:", "  lower_case_names: {limit: 3}")),
    paste0(
      "pbn.yml: rules: lower_case_names has no parameter limit. ",
      "It has no parameter."
    )
  )
  expect_identical(
    rules_error(c(house_line, "rules:", "  loc_per_file: {max: 3}")),
    paste0(
      "pbn.yml: rules: loc_per_file has no parameter max. ",
      "Its parameters are: limit."
    )
  )
  for (level in c("no", "FAIL", "true", "[warn, off]")) {
    expect_identical(
      rules_error(c(house_line, "rules:", paste0("  loc_per_file: ", level))),
      paste0(
        "pbn.yml: rules: loc_per_file",
        if (level == "[warn, off]") {
          paste0(
            " MUST be a level, fail, warn or off, or a map such as ",
            "{level: warn}."
          )
        } else {
          paste0(": level is ", level, ", and it MUST be fail, warn or off.")
        }
      ),
      info = level
    )
  }
  for (limit in c("0", "-1", "1.5", "1e3", "yes")) {
    expect_identical(
      rules_error(c(
        house_line,
        "rules:",
        paste0("  cyclomatic_complexity: {limit: ", limit, "}")
      )),
      paste0(
        "pbn.yml: rules: cyclomatic_complexity: limit is ",
        limit,
        ", and it MUST be a whole number of 1 or more."
      ),
      info = limit
    )
  }
  expect_identical(
    rules_error(c(
      house_line,
      "rules:",
      "  no_local_assign: {forbid: [assign(), sapply()]}"
    )),
    paste0(
      "pbn.yml: rules: no_local_assign: forbid holds sapply(), which is not ",
      "a construct of the rule. The constructs are: local(), assign(), ",
      "delayedAssign(), assignInNamespace(), assignInMyNamespace(), <<-, ->>."
    )
  )
  expect_identical(
    rules_error(c(house_line, "rules:", "  no_local_assign: {forbid: [1, 2]}")),
    paste0(
      "pbn.yml: rules: no_local_assign: forbid MUST be a list of constructs. ",
      "They are: local(), assign(), delayedAssign(), assignInNamespace(), ",
      "assignInMyNamespace(), <<-, ->>."
    )
  )
})

test_that("pbn.yml cannot weaken or allow a locked rule, or a rule of check_results()", {
  for (rule in c(
    "results_contract",
    "gitexclude_not_tracked",
    "fake_data_names"
  )) {
    for (level in c("warn", "off")) {
      expect_identical(
        rules_error(c(house_line, "rules:", paste0("  ", rule, ": ", level))),
        paste0(
          "pbn.yml: rules: ",
          rule,
          " is locked, so pbn.yml cannot set it to ",
          level,
          ". ",
          locked_reasons[[rule]]
        ),
        info = paste(rule, level)
      )
    }
    expect_identical(
      rules_error(c(house_line, "allow:", paste0("  ", rule, ": [a.R]"))),
      paste0(
        "pbn.yml: allow: ",
        rule,
        " is locked, so pbn.yml cannot allow a path for it. ",
        locked_reasons[[rule]]
      ),
      info = rule
    )
  }
  expect_identical(
    locked_reasons[["results_contract"]],
    "It keeps data for each person out of the results files that git tracks."
  )
  expect_identical(
    rules_error(c(house_line, "rules:", "  fake_data_names: {locked: false}")),
    paste0(
      "pbn.yml: rules: fake_data_names: locked is a field of the rule set, ",
      "and pbn.yml cannot set it."
    )
  )
  for (key in c("rules", "allow")) {
    expect_identical(
      rules_error(c(
        house_line,
        paste0(key, ":"),
        if (key == "rules") {
          "  no_row_per_person: off"
        } else {
          "  no_row_per_person: [a]"
        }
      )),
      paste0(
        "pbn.yml: ",
        key,
        ": no_row_per_person is a rule of check_results(), which the rule ",
        "results_contract runs. results_contract is locked, and pbn.yml ",
        "cannot change it. ",
        locked_reasons[["results_contract"]]
      ),
      info = key
    )
  }
  # The level fail of a locked rule changes nothing, so it passes.
  p <- local_project()
  write_pbn_yml(p$home, "rules:", "  results_contract: fail")
  expect_identical(read_rules(p$home)$rules$results_contract$level, "fail")
})

# The rules of the house set that came after the output of the fixture
# check_project_before_rules.txt.
rules_after_fixture <- c(
  "no_absolute_paths",
  "blocks_list",
  "real_results_are_fake"
)

test_that("a project with only extends gives the output of pbn before pbn.yml, with the line of the rules and the count of OFF", {
  fixture <- test_path("fixtures", "check_project_before_rules.txt")
  for (name in project_variant_names) {
    if (name == "git") {
      skip_if_not(nzchar(Sys.which("git")), "git is not installed")
    }
    p <- local_project()
    allow <- make_project_variant(p, name)
    if (length(allow) > 0) {
      write_pbn_yml(
        p$home,
        "allow:",
        paste0("  ", names(allow), ": [", unlist(allow), "]")
      )
    }
    out <- project_report(p$home)
    # The fix "Run git rm --cached on it" of gitexclude_not_tracked moved
    # from the detail to the fix at the end of the line.
    before <- sub(
      "\\. Run git rm --cached on it$",
      "",
      fixture_lines(fixture, name)
    )
    # manual_manuscript_names names the suffixed form too since pbn 2026.12.11.
    before <- sub(
      "is named vNN_<INITIALS>\\.docx$",
      "is named vNN_<INITIALS>.docx or vNN_<INITIALS>_<suffix>.docx",
      before
    )
    before <- sub(
      "the name must be vNN_<INITIALS>\\.docx$",
      "the name must be vNN_<INITIALS>.docx or vNN_<INITIALS>_<suffix>.docx",
      before
    )
    n <- length(before)
    expect_identical(
      out[1],
      paste0(
        "Rules: house_2026_11_17, 0 overrides, ",
        length(allow),
        " allowed paths (pbn.yml)"
      ),
      info = name
    )
    # The rules that came after the fixture pass on each variant, and the
    # other lines are those of the fixture.
    # The fixture is older than the fix at the end of each WARN and FAIL.
    body <- sub("\\. Fix: .*$", "", as.character(out[-c(1, length(out))]))
    later <- sub("^[A-Z]+  ([a-z_]+)  .*$", "\\1", body) %in%
      rules_after_fixture
    expect_identical(body[!later], before[2:(n - 1)], info = name)
    expect_identical(
      body[later],
      paste0(
        "PASS  ",
        rules_after_fixture,
        "  ",
        vapply(
          project_rules()[rules_after_fixture],
          function(d) return(d[[2]]),
          character(1),
          USE.NAMES = FALSE
        )
      ),
      info = name
    )
    passed <- as.integer(sub(" PASS.*$", "", before[n]))
    expect_identical(
      out[length(out)],
      paste0(
        passed + length(rules_after_fixture),
        sub("^[0-9]+", "", before[n]),
        ", 0 OFF"
      ),
      info = name
    )
    status <- if (attr(out, "counts")[["FAIL"]] > 0) 1 else 0
    expect_identical(paste("# exit status", status), before[1], info = name)
  }
})

test_that("off runs no rule and prints OFF, and warn turns each FAIL of a rule into WARN", {
  p <- local_project()
  write_lines(p$home, "adhoc/Notes.txt", "x")
  write_lines(p$home, "adhoc/long.R", paste0("x", seq_len(1001), " <- 1"))
  write_pbn_yml(
    p$home,
    "rules:",
    "  lower_case_names: warn",
    "  loc_per_file: off"
  )
  out <- project_report(p$home)
  found <- attr(out, "findings")
  expect_identical(failed_rules(found), character(0))
  expect_identical(found$rule, names(read_house_set("house_2026_11_17")))
  expect_identical(
    found[found$rule == "loc_per_file", c("status", "path", "detail")],
    data.frame(status = "OFF", path = "", detail = "off in pbn.yml"),
    ignore_attr = TRUE
  )
  expect_identical(
    found$status[found$rule == "lower_case_names"],
    "WARN"
  )
  expect_identical(
    out[1],
    "Rules: house_2026_11_17, 2 overrides, 0 allowed paths (pbn.yml)"
  )
  expect_identical(out[12], "OFF  loc_per_file  off in pbn.yml")
  expect_identical(out[length(out)], "16 PASS, 1 WARN, 0 FAIL, 1 OFF")

  # A rule that is off does not run: its function is never called.
  calls <- 0
  testthat::local_mocked_bindings(rule_loc_per_file = function(path, limit) {
    calls <<- calls + 1
    return(project_row("loc_per_file", "FAIL", "x.R", "x.R: ran"))
  })
  project_findings(p$home)
  expect_identical(calls, 0)

  # fail on a rule of the level warn turns its WARN into FAIL.
  write_lines(p$home, "notes.txt", "x")
  write_pbn_yml(p$home, "rules:", "  unknown_path: fail")
  expect_identical(
    failed_rules(project_findings(p$home)),
    c("lower_case_names", "loc_per_file", "unknown_path")
  )
})

test_that("an override merges into the house entry, and each parameter changes its rule", {
  p <- local_project()
  write_lines(p$home, "adhoc/branchy.R", branchy_lines("tidy", 16))
  write_lines(p$home, "adhoc/wide.R", branchy_lines("wide", 30))
  write_lines(p$home, "adhoc/short.R", paste0("x", seq_len(41), " <- 1"))
  write_lines(
    p$home,
    "adhoc/early.R",
    c("a <- 1", "assign(\"b\", a)", "c <<- 2")
  )
  write_pbn_yml(
    p$home,
    "rules:",
    "  cyclomatic_complexity: {limit: 25}",
    "  loc_per_file: {limit: 40, level: warn}",
    "  no_local_assign:",
    "    forbid: [\"<<-\"]"
  )
  rules <- read_rules(p$home)
  expect_identical(
    rules$rules$cyclomatic_complexity,
    list(level = "fail", limit = 25L, locked = FALSE)
  )
  found <- project_findings(p$home)
  expect_identical(
    found[found$rule == "cyclomatic_complexity", c("status", "detail")],
    data.frame(
      status = "FAIL",
      detail = with_fix(
        "cyclomatic_complexity",
        "adhoc/wide.R: wide scores 31 (limit 25)"
      )
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    found$detail[found$rule == "loc_per_file"],
    with_fix("loc_per_file", "adhoc/short.R: 41 code lines, more than 40")
  )
  expect_identical(found$status[found$rule == "loc_per_file"], "WARN")
  expect_identical(
    found$detail[found$rule == "no_local_assign"],
    with_fix("no_local_assign", "adhoc/early.R:3: <<-")
  )

  # The PASS lines name the limits and the constructs of pbn.yml.
  unlink(file.path(p$home, "adhoc"), recursive = TRUE)
  found <- project_findings(p$home)
  expect_identical(
    found$detail[
      found$rule %in%
        c("loc_per_file", "cyclomatic_complexity", "no_local_assign")
    ],
    c(
      "no .R file has more than 40 code lines",
      "no function of an .R file has a cyclomatic complexity above 25",
      "no .R file uses <<-"
    )
  )
  expect_identical(
    local_assign_pass_detail(c("assign()", "local()")),
    "no .R file calls local or assign"
  )
})

test_that("an allow entry of a folder covers each finding at any depth under it, written with or without a final /, and not a folder whose name starts with it", {
  p <- local_project()
  write_lines(p$home, "archive/Old.R", "x <- 1")
  write_lines(p$home, "archive/deep/Er/Data.txt", "x")
  write_lines(p$home, "archive2/X.txt", "x")
  for (entry in c("archive", "archive/")) {
    write_pbn_yml(p$home, "allow:", paste0("  lower_case_names: [", entry, "]"))
    found <- project_findings(p$home)
    rows <- found[found$rule == "lower_case_names", ]
    expect_identical(
      rows$path,
      c(
        "archive/Old.R",
        "archive/deep/Er",
        "archive/deep/Er/Data.txt",
        "archive2/X.txt"
      ),
      info = entry
    )
    expect_identical(
      rows$status,
      c("WARN", "WARN", "WARN", "FAIL"),
      info = entry
    )
    expect_false(any(found$rule == "allow"), info = entry)
  }
  # A file entry covers that file only.
  write_pbn_yml(p$home, "allow:", "  lower_case_names: [archive/Old.R]")
  found <- project_findings(p$home)
  rows <- found[found$rule == "lower_case_names", ]
  expect_identical(rows$status, c("WARN", "FAIL", "FAIL", "FAIL"))
  # An entry under which there is no finding is stale.
  write_pbn_yml(
    p$home,
    "allow:",
    "  lower_case_names: [archive/deep/Er/Data.txt/x]"
  )
  found <- project_findings(p$home)
  expect_identical(
    found$detail[found$rule == "allow"],
    with_fix(
      "allow",
      paste0(
        "pbn.yml: the entry allow: lower_case_names: ",
        "archive/deep/Er/Data.txt/x matches no finding"
      )
    )
  )
})

test_that("allow covers a path and each path under it, with or without a final /, and a stale entry warns", {
  p <- local_project()
  dir.create(file.path(p$home, "adhoc", "Old"))
  write_lines(p$home, "adhoc/Notes.txt", "x")
  write_lines(p$home, "adhoc/Other.txt", "x")
  write_pbn_yml(
    p$home,
    "allow:",
    "  lower_case_names:",
    "    - adhoc/Old/",
    "    - adhoc/Notes.txt",
    "    - adhoc",
    "  loc_per_file: [adhoc/gone.R]"
  )
  out <- project_report(p$home)
  found <- attr(out, "findings")
  rows <- found[found$rule == "lower_case_names", ]
  expect_identical(
    rows$path,
    c("adhoc/Notes.txt", "adhoc/Old", "adhoc/Other.txt")
  )
  # The entry adhoc covers adhoc/Other.txt, which no other entry covers.
  expect_identical(rows$status, c("WARN", "WARN", "WARN"))
  expect_identical(
    found$detail[found$rule == "allow"],
    with_fix(
      "allow",
      "pbn.yml: the entry allow: loc_per_file: adhoc/gone.R matches no finding"
    )
  )
  expect_identical(
    out[1],
    "Rules: house_2026_11_17, 0 overrides, 4 allowed paths (pbn.yml)"
  )

  # An entry of a rule that is off matches nothing, and says why.
  write_pbn_yml(
    p$home,
    "rules:",
    "  loc_per_file: off",
    "allow:",
    "  loc_per_file: [a.R]"
  )
  found <- project_findings(p$home)
  expect_identical(
    found$detail[found$rule == "allow"],
    with_fix(
      "allow",
      "pbn.yml: the entry allow: loc_per_file: a.R matches no finding, because the rule is off"
    )
  )
  expect_identical(
    rules_line(read_rules(p$home)),
    "Rules: house_2026_11_17, 1 override, 1 allowed path (pbn.yml)"
  )
})

test_that("check_project.R ends with status 0 when pbn.yml turns each FAIL into WARN", {
  skip_if_pbn_not_installed()
  p <- local_project()
  write_lines(p$home, "adhoc/Notes.txt", "x")
  write_pbn_yml(p$home, "rules:", "  lower_case_names: warn")
  res <- run_script(p$home, "check_project.R")
  expect_identical(res$status, 0L)
  expect_match(
    res$stdout,
    "WARN  lower_case_names  adhoc/Notes.txt",
    fixed = TRUE
  )
  expect_match(res$stdout, "17 PASS, 1 WARN, 0 FAIL, 0 OFF", fixed = TRUE)
})

test_that("an allow entry that is not a path inside the project stops: empty, /, ., a path from / and a path with . or ..", {
  p <- local_project()
  for (entry in c(
    "\"\"",
    "\"/\"",
    "\".\"",
    "\"./\"",
    "/adhoc",
    "../adhoc",
    "adhoc/./x",
    "adhoc//x"
  )) {
    write_pbn_yml(p$home, "allow:", paste0("  lower_case_names: [", entry, "]"))
    expect_error(read_rules(p$home), "lower_case_names", info = entry)
  }
  write_pbn_yml(p$home, "allow:", "  lower_case_names: [\"/\"]")
  expect_error(
    read_rules(p$home),
    paste0(
      "pbn.yml: allow: lower_case_names: \"/\" is not a path inside the ",
      "project. Write a path relative to the project folder, for example ",
      "adhoc or adhoc/notes.R. An entry covers its path and each path under it."
    ),
    fixed = TRUE
  )
  # A folder entry of a locked rule stops, as any entry of it.
  write_pbn_yml(p$home, "allow:", "  results_contract: [v01_results]")
  expect_error(
    read_rules(p$home),
    "is locked, so pbn.yml cannot allow a path for it.",
    fixed = TRUE
  )
})
