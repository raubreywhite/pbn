# The set house_2026_11_26: the rules manual_has_supplement,
# manual_si_has_main, guidelines_valid and guidelines_covered, and the folder
# guidelines/ in the house layout. A
# project keeps its reporting checklists in guidelines/, one YAML file per
# guideline. The projects of house_2026_11_25 and older sets see none of it.

# The two supplement rules of house_2026_11_26. rules_2026_11_26 of
# helper-project.R has them too.
supplement_rules <- c("manual_has_supplement", "manual_si_has_main")

# A synthetic project of local_project() on the set house_2026_11_26, without
# the manual manuscript that manual_not_ahead fails.
local_project_26 <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  unlink(file.path(p$home, "manuscript", "manual", "v01_AB.docx"))
  return(p)
}

# Copies the checklist fixtures `names` into guidelines/ of `home`. They are
# byte copies of reporting-guidelines/<id>/<id>.yml of the methods
# repository.
copy_guidelines <- function(home, names = c("strobe", "agrema")) {
  dir.create(file.path(home, "guidelines"), showWarnings = FALSE)
  file.copy(
    test_path("fixtures", "guidelines", paste0(names, ".yml")),
    file.path(home, "guidelines")
  )
  return(invisible(home))
}

# The lines of a small checklist file with the id `id`. `items` holds one
# string per entry, the lines of the entry after "  - ", joined by "\n".
guideline_lines <- function(id, items, top = character(0)) {
  entry <- function(x) {
    parts <- strsplit(x, "\n", fixed = TRUE)[[1]]
    return(c(paste0("  - ", parts[1]), paste0("    ", parts[-1])))
  }
  return(c(
    paste0("id: ", id),
    paste0("title: ", toupper(id)),
    top,
    "items:",
    unlist(lapply(items, entry))
  ))
}

# One valid entry with the item id `item`, the kind `kind` and the extra
# lines `extra`.
entry_text <- function(item, kind = "prose", extra = character(0)) {
  return(paste(
    c(
      paste0("item: \"", item, "\""),
      "section: Methods",
      "topic: Setting",
      paste0("kind: ", kind),
      extra,
      paste0("text: \"Describe item ", item, "\"")
    ),
    collapse = "\n"
  ))
}

# The status, the path and the detail of each finding of the rule `rule`.
rows_26 <- function(home, rule) {
  found <- project_findings(home)
  return(found[found$rule == rule, c("status", "path", "detail")])
}

# The message of the error of `code`, or "no error" when it gives none. A
# test compares this text, so a missing or a changed error fails the
# assertion itself.
error_text <- function(code) {
  return(tryCatch(
    {
      force(code)
      "no error"
    },
    error = function(e) return(conditionMessage(e))
  ))
}

# The number of rows of project_findings() in `home`, and the md5 of those
# rows as text: one line "rule TAB status TAB path TAB detail" per row,
# joined by LF, with the folder of the project as <home>. When
# project_findings() stops, the two values are "error" and its message, so
# that the comparison with the expected values fails, and not the call.
findings_md5 <- function(home) {
  f <- tryCatch(project_findings(home), error = function(e) return(e))
  if (inherits(f, "error")) {
    return(c(n = "error", md5 = conditionMessage(f)))
  }
  lines <- paste(
    f$rule,
    f$status,
    f$path,
    gsub(home, "<home>", f$detail, fixed = TRUE),
    sep = "\t"
  )
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeBin(charToRaw(enc2utf8(paste(lines, collapse = "\n"))), tmp)
  return(c(n = as.character(nrow(f)), md5 = unname(tools::md5sum(tmp))))
}

test_that("the checklist fixtures are the files of the methods repository", {
  expect_identical(
    unname(tools::md5sum(test_path(
      "fixtures",
      "guidelines",
      c("strobe.yml", "agrema.yml")
    ))),
    c("f410f3f2ea24a2d8c81a58b75634e3d1", "35c61dd4e1bf94246348e69676a02f04")
  )
})

test_that("house_2026_11_26 is house_2026_11_25 with built_reads_source, manual_has_supplement, manual_si_has_main and guidelines_valid at fail and guidelines_covered at warn, and a new pbn.yml names it", {
  old <- read_house_set("house_2026_11_25")
  new <- read_house_set("house_2026_11_26")
  expect_identical(new[names(old)], old)
  expect_identical(
    names(new),
    intersect(
      names(project_rules()),
      c(names(old), rules_2026_11_26)
    )
  )
  expect_identical(setdiff(names(new), names(old)), rules_2026_11_26)
  expect_identical(
    vapply(
      new[rules_2026_11_26],
      function(r) return(if (is.null(r$level)) "missing" else r$level),
      character(1)
    ),
    c(
      built_reads_source = "fail",
      manual_has_supplement = "fail",
      manual_si_has_main = "fail",
      guidelines_valid = "fail",
      guidelines_covered = "warn"
    )
  )
  expect_true(all(vapply(
    new[rules_2026_11_26],
    function(r) return(identical(r$locked, FALSE)),
    logical(1)
  )))
  expect_identical(starter_line(), "extends: house_2026_11_26")
  # create_project() names house_2026_11_26: its template passes the set.
  expect_identical(create_project_set, "house_2026_11_26")
})

test_that("valid checklist files pass guidelines_valid, guidelines_covered WARNs each result item without a block, and guidelines/ is a known path", {
  p <- local_project_26()
  copy_guidelines(p$home)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found[found$rule == "guidelines_valid", c("status", "detail")],
    data.frame(
      status = "PASS",
      detail = "every guidelines/*.yml follows the checklist schema"
    ),
    ignore_attr = TRUE
  )
  # The project has no blocks.R, so each result item of the two files gives
  # one WARN: 5 of AGReMA and 11 of STROBE. STROBE has 13 result entries,
  # and its item 15 comes 3 times, once per design.
  covered <- found[found$rule == "guidelines_covered", ]
  expect_identical(nrow(covered), 16L)
  expect_identical(unique(covered$status), "WARN")
  expect_identical(
    as.vector(table(covered$path)[c(
      "guidelines/agrema.yml",
      "guidelines/strobe.yml"
    )]),
    c(5L, 11L)
  )
  expect_identical(
    covered$detail[1],
    with_fix(
      "guidelines_covered",
      paste0(
        "guidelines/agrema.yml: no block of v01_functions/blocks.R names ",
        "the result item agrema:17 in its items"
      )
    )
  )
  # No unknown_path row names guidelines/.
  expect_false(any(found$rule == "unknown_path" & found$path == "guidelines"))
  expect_identical(
    found[found$rule == "unknown_path", c("status", "detail")],
    data.frame(
      status = "PASS",
      detail = "the house layout names every top-level path"
    ),
    ignore_attr = TRUE
  )
})

test_that("read_guidelines() gives one row per entry of every file, and design: keeps the entries of that design", {
  p <- local_project_26()
  expect_identical(read_guidelines(p$home), empty_guidelines())
  expect_identical(names(empty_guidelines()), guideline_columns)
  copy_guidelines(p$home)
  g <- read_guidelines(p$home)
  expect_identical(
    names(g),
    c(
      "id",
      "item",
      "section",
      "topic",
      "kind",
      "text",
      "designs",
      "file",
      "file_md5"
    )
  )
  expect_true(all(vapply(g, is.character, logical(1))))
  expect_identical(
    as.vector(table(g$id)[c("agrema", "strobe")]),
    c(34L, 41L)
  )
  expect_identical(
    unique(g$file),
    c("guidelines/agrema.yml", "guidelines/strobe.yml")
  )
  expect_identical(
    unique(g$file_md5),
    c("35c61dd4e1bf94246348e69676a02f04", "f410f3f2ea24a2d8c81a58b75634e3d1")
  )
  expect_identical(unique(g$designs[g$id == "agrema"]), "")
  s15 <- g[g$id == "strobe" & g$item == "15", ]
  expect_identical(s15$designs, c("cohort", "case-control", "cross-sectional"))
  expect_identical(
    g$designs[g$id == "strobe" & g$item == "1a"],
    "cohort, case-control, cross-sectional"
  )

  # A project copy with design: cohort keeps 34 of the 41 entries of STROBE:
  # the entries without designs and the entries of cohort.
  strobe <- file.path(p$home, "guidelines", "strobe.yml")
  lines <- readLines(strobe, encoding = "UTF-8")
  writeLines(append(lines, "design: cohort", after = 6), strobe)
  g <- read_guidelines(p$home)
  s <- g[g$id == "strobe", ]
  expect_identical(nrow(s), 34L)
  expect_identical(unique(s$designs), "cohort")
  expect_identical(
    s$text[s$item == "15"],
    "Cohort study—Report numbers of outcome events or summary measures over time"
  )
  expect_identical(failed_rules(project_findings(p$home)), character(0))
})

test_that("a file with kind: maybe fails guidelines_valid", {
  p <- local_project_26()
  write_lines(
    p$home,
    "guidelines/mini.yml",
    guideline_lines("mini", c(entry_text("1"), entry_text("2", kind = "maybe")))
  )
  expect_identical(
    rows_26(p$home, "guidelines_valid"),
    data.frame(
      status = "FAIL",
      path = "guidelines/mini.yml",
      detail = with_fix(
        "guidelines_valid",
        "guidelines/mini.yml: item 2: the kind maybe is not result, prose or author"
      )
    ),
    ignore_attr = TRUE
  )
  err <- error_text(read_guidelines(p$home))
  expect_identical(
    err,
    paste0(
      "These checklist files break the schema of ?check_project:\n",
      "- guidelines/mini.yml: item 2: the kind maybe is not result, prose or author"
    )
  )
})

test_that("guidelines_valid gives one FAIL for each file that breaks the schema, also a file that does not parse", {
  p <- local_project_26()
  copy_guidelines(p$home)
  write_lines(p$home, "guidelines/a_parse.yml", c("id: a", "items: [a"))
  write_lines(
    p$home,
    "guidelines/b_keys.yml",
    c(
      "id: b",
      "desing: cohort",
      "items:",
      "  - item: \"1\"",
      "    section: Methods",
      "    topic: Setting",
      "    kind: prose"
    )
  )
  write_lines(
    p$home,
    "guidelines/c_design.yml",
    guideline_lines(
      "c",
      entry_text("1"),
      top = c("designs: [cohort]", "design: case-control")
    )
  )
  write_lines(
    p$home,
    "guidelines/d_dup.yml",
    guideline_lines("d", c(entry_text("1"), entry_text("1")))
  )
  write_lines(
    p$home,
    "guidelines/e_dup_design.yml",
    guideline_lines(
      "e",
      c(
        entry_text("15", extra = "designs: [cohort]"),
        entry_text("15", extra = "designs: [cohort, case-control]")
      ),
      top = "designs: [cohort, case-control]"
    )
  )
  write_lines(
    p$home,
    "guidelines/f_entry.yml",
    c(
      "id: F",
      "title: F",
      "items:",
      "  - item: 2",
      "    section: Methods",
      "    topic: Setting",
      "    kind: prose",
      "    text: x",
      "  - item: \"3\"",
      "    section: Methods",
      "    topic: Setting",
      "    kind: prose",
      "    designs: [cohort]",
      "    text: x",
      "  - just a string"
    )
  )
  write_lines(
    p$home,
    "guidelines/g_many.yml",
    c(
      "id: g",
      "title: G",
      "items:",
      paste0(
        "  - {item: \"",
        1:5,
        "\", section: M, topic: T, text: x}"
      )
    )
  )
  write_lines(p$home, "guidelines/h_empty.yml", character(0))
  write_lines(
    p$home,
    "guidelines/z_copy.yml",
    guideline_lines("strobe", entry_text("1"))
  )
  found <- rows_26(p$home, "guidelines_valid")
  expect_identical(
    found$path,
    c(
      "guidelines/a_parse.yml",
      "guidelines/b_keys.yml",
      "guidelines/c_design.yml",
      "guidelines/d_dup.yml",
      "guidelines/e_dup_design.yml",
      "guidelines/f_entry.yml",
      "guidelines/g_many.yml",
      "guidelines/h_empty.yml",
      "guidelines/z_copy.yml"
    )
  )
  expect_identical(unique(found$status), "FAIL")
  expect_match(
    found$detail[1],
    "^guidelines/a_parse.yml: it is not valid YAML: .+\\. Fix: correct the file to the checklist schema of \\?check_project\\.$"
  )
  expect_identical(
    found$detail[-1],
    with_fix(
      "guidelines_valid",
      c(
        paste0(
          "guidelines/b_keys.yml: the key desing is not a key of the schema; ",
          "the key title is missing or not one string; ",
          "item 1: the key text is missing or not one string"
        ),
        "guidelines/c_design.yml: the key design is not one of the key designs",
        "guidelines/d_dup.yml: item 1: the item id comes more than once",
        paste0(
          "guidelines/e_dup_design.yml: item 15: the item id comes more than ",
          "once for the design cohort"
        ),
        paste0(
          "guidelines/f_entry.yml: the id F has a character other than a-z, ",
          "0-9 and -; entry 1: the key item is missing or not one string; ",
          "item 3: it has the key designs, and the file has none; and 1 more"
        ),
        paste0(
          "guidelines/g_many.yml: item 1: the key kind is missing or not one ",
          "string; item 2: the key kind is missing or not one string; item 3: ",
          "the key kind is missing or not one string; and 2 more"
        ),
        "guidelines/h_empty.yml: it is not a YAML map with the keys id, title and items",
        "guidelines/z_copy.yml: the id strobe is also the id of guidelines/strobe.yml"
      )
    )
  )
  # The valid files give no finding, and read_guidelines() stops on the
  # broken files only.
  err <- error_text(read_guidelines(p$home))
  expect_identical(
    sub(
      "^- ([^:]+):.*$",
      "\\1",
      strsplit(err, "\n")[[1]][-1]
    ),
    found$path
  )
})

test_that("designs MUST be a YAML sequence of strings: a scalar string or a map fails, at the top level and in an entry", {
  p <- local_project_26()
  write_lines(
    p$home,
    "guidelines/a_top_scalar.yml",
    guideline_lines("a", entry_text("1"), top = "designs: cohort")
  )
  write_lines(
    p$home,
    "guidelines/b_top_map.yml",
    guideline_lines("b", entry_text("1"), top = "designs: {x: cohort}")
  )
  write_lines(
    p$home,
    "guidelines/c_entry_scalar.yml",
    guideline_lines(
      "c",
      entry_text("1", extra = "designs: cohort"),
      top = "designs: [cohort]"
    )
  )
  write_lines(
    p$home,
    "guidelines/d_entry_map.yml",
    guideline_lines(
      "d",
      entry_text("1", extra = "designs: {x: cohort}"),
      top = "designs: [cohort]"
    )
  )
  write_lines(
    p$home,
    "guidelines/e_sequence.yml",
    guideline_lines(
      "e",
      c(
        entry_text("1", extra = "designs: [cohort]"),
        entry_text("2", extra = "designs:\n  - cohort")
      ),
      top = c("designs:", "  - cohort")
    )
  )
  expect_identical(
    rows_26(p$home, "guidelines_valid"),
    data.frame(
      status = rep("FAIL", 4),
      path = c(
        "guidelines/a_top_scalar.yml",
        "guidelines/b_top_map.yml",
        "guidelines/c_entry_scalar.yml",
        "guidelines/d_entry_map.yml"
      ),
      detail = with_fix(
        "guidelines_valid",
        c(
          "guidelines/a_top_scalar.yml: the key designs is not a list of strings",
          "guidelines/b_top_map.yml: the key designs is not a list of strings",
          "guidelines/c_entry_scalar.yml: item 1: the key designs is not a list of strings",
          "guidelines/d_entry_map.yml: item 1: the key designs is not a list of strings"
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("items MUST be a YAML sequence: a map of entries fails with one message", {
  p <- local_project_26()
  write_lines(
    p$home,
    "guidelines/a_map.yml",
    c("id: a", "title: A", "items: {a: b}")
  )
  write_lines(
    p$home,
    "guidelines/b_entries.yml",
    c(
      "id: b",
      "title: B",
      "items:",
      "  one:",
      "    item: \"1\"",
      "    section: Methods",
      "    topic: Setting",
      "    kind: prose",
      "    text: x"
    )
  )
  expect_identical(
    rows_26(p$home, "guidelines_valid"),
    data.frame(
      status = c("FAIL", "FAIL"),
      path = c("guidelines/a_map.yml", "guidelines/b_entries.yml"),
      detail = with_fix(
        "guidelines_valid",
        c(
          "guidelines/a_map.yml: the key items is not a YAML sequence of entries",
          "guidelines/b_entries.yml: the key items is not a YAML sequence of entries"
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("an optional key that is there with no value fails: version: null, source: and citation: ~", {
  p <- local_project_26()
  write_lines(
    p$home,
    "guidelines/a_empty.yml",
    guideline_lines(
      "a",
      entry_text("1"),
      top = c("version: null", "source:", "citation: ~")
    )
  )
  write_lines(
    p$home,
    "guidelines/b_designs.yml",
    guideline_lines(
      "b",
      entry_text("1", extra = "designs: ~"),
      top = c("designs:", "design:")
    )
  )
  expect_identical(
    rows_26(p$home, "guidelines_valid"),
    data.frame(
      status = c("FAIL", "FAIL"),
      path = c("guidelines/a_empty.yml", "guidelines/b_designs.yml"),
      detail = with_fix(
        "guidelines_valid",
        c(
          paste0(
            "guidelines/a_empty.yml: the key version is present but empty; ",
            "the key source is present but empty; the key citation is ",
            "present but empty"
          ),
          paste0(
            "guidelines/b_designs.yml: the key designs is present but empty; ",
            "the key design is present but empty; item 1: the key designs is ",
            "present but empty"
          )
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("an item id MAY come twice when design: keeps one of the two", {
  p <- local_project_26()
  write_lines(
    p$home,
    "guidelines/e.yml",
    guideline_lines(
      "e",
      c(
        entry_text("15", extra = "designs: [cohort]"),
        entry_text("15", extra = "designs: [case-control]"),
        entry_text("15", extra = "designs: [case-control]")
      ),
      top = c("designs: [cohort, case-control]", "design: cohort")
    )
  )
  expect_identical(rows_26(p$home, "guidelines_valid")$status, "PASS")
  expect_identical(read_guidelines(p$home)$designs, "cohort")
})

test_that("pbn.yml turns guidelines_valid off, and allows a path for it; ignore: cannot name guidelines/", {
  p <- local_project_26()
  write_lines(
    p$home,
    "guidelines/mini.yml",
    guideline_lines("mini", entry_text("1", kind = "maybe"))
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_26", "rules:", "  guidelines_valid: off")
  )
  expect_identical(
    rows_26(p$home, "guidelines_valid")[, c("status", "detail")],
    data.frame(status = "OFF", detail = "off in pbn.yml"),
    ignore_attr = TRUE
  )
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_26",
      "allow:",
      "  guidelines_valid: [guidelines/mini.yml]"
    )
  )
  expect_identical(rows_26(p$home, "guidelines_valid")$status, "WARN")
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_26", "ignore: [guidelines/]")
  )
  err <- error_text(read_rules(p$home))
  expect_identical(
    err,
    paste0(
      "pbn.yml: the entry ignore: guidelines is a name of the house layout, ",
      "which the rules exist to check."
    )
  )
})

test_that("under house_2026_11_25 a folder guidelines/ gives an unknown_path WARN and no guidelines_valid row", {
  p <- local_project_26()
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_25")
  write_lines(
    p$home,
    "guidelines/mini.yml",
    guideline_lines("mini", entry_text("1", kind = "maybe"))
  )
  found <- project_findings(p$home)
  expect_false(any(rules_2026_11_26 %in% found$rule))
  expect_identical(
    found[found$rule == "unknown_path", c("status", "path", "detail")],
    data.frame(
      status = "WARN",
      path = "guidelines",
      detail = with_fix(
        "unknown_path",
        "guidelines: the house layout does not name it"
      )
    ),
    ignore_attr = TRUE
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_25", "rules:", "  guidelines_valid: off")
  )
  err <- error_text(read_rules(p$home))
  expect_identical(
    err,
    paste0(
      "pbn.yml: rules: guidelines_valid is not a rule of the set ",
      "house_2026_11_25. The oldest set with it is house_2026_11_26. To use ",
      "it, change extends: to that set or a newer one."
    )
  )
})

test_that("under house_2026_11_17, _19, _20 and _25 the findings of the standard project are those of pbn at 78488c5, with and without guidelines/", {
  # Measured at 78488c5, before the set house_2026_11_26: the number of rows
  # and findings_md5() of local_project() on each set, then with
  # guidelines/strobe.yml and a guidelines/bad.yml that does not parse, then
  # with ignore: [guidelines/] in pbn.yml.
  at_head <- list(
    house_2026_11_17 = c(
      "18",
      "b91d09aded305de86550a07c73e66179",
      "18",
      "ddb83faee711c9b4fbd2f43994dc4084",
      "18",
      "b91d09aded305de86550a07c73e66179"
    ),
    house_2026_11_19 = c(
      "24",
      "074a64464d08dda2c9d36e922c5fa053",
      "24",
      "46889a36f006b695714f55c173c80afb",
      "24",
      "074a64464d08dda2c9d36e922c5fa053"
    ),
    house_2026_11_20 = c(
      "27",
      "d826563f98f81b407c111fdb671cfa88",
      "27",
      "8b54a80a7e10b9b5d0f4d71bf904164d",
      "27",
      "d826563f98f81b407c111fdb671cfa88"
    ),
    house_2026_11_25 = c(
      "30",
      "b4916f8dcaf72f89d8e947620186c07f",
      "30",
      "cbeb1fbf5fac0eaf8f3f21ec5a7ce575",
      "30",
      "b4916f8dcaf72f89d8e947620186c07f"
    )
  )
  for (set in names(at_head)) {
    p <- local_project()
    write_lines(p$home, "pbn.yml", paste0("extends: ", set))
    plain <- findings_md5(p$home)
    copy_guidelines(p$home, "strobe")
    write_lines(p$home, "guidelines/bad.yml", c("id: bad", "items: [a"))
    with_guidelines <- findings_md5(p$home)
    write_lines(
      p$home,
      "pbn.yml",
      c(paste0("extends: ", set), "ignore: [guidelines/]")
    )
    ignored <- findings_md5(p$home)
    expect_identical(
      unname(c(plain, with_guidelines, ignored)),
      at_head[[set]],
      info = set
    )
    # At 78488c5 the key ignore stopped on vNN_frozen.md5 for every set.
    write_lines(
      p$home,
      "pbn.yml",
      c(paste0("extends: ", set), "ignore: [v03_frozen.md5]")
    )
    err <- error_text(read_rules(p$home))
    expect_identical(
      err,
      paste0(
        "pbn.yml: the entry ignore: v03_frozen.md5 is a name of the house ",
        "layout, which the rules exist to check."
      ),
      info = set
    )
  }
})

test_that("is_one_string() has one definition, which passes the empty string, and guidelines_valid still fails an empty title", {
  # R/guidelines.R once defined a second is_one_string() that failed "",
  # and by collation it masked the one of R/check_rules_results.R.
  expect_true(is_one_string(""))
  expect_false(is_nonempty_string(""))
  p <- local_project_26()
  dir.create(file.path(p$home, "guidelines"))
  lines <- guideline_lines("e", entry_text("1"))
  lines[2] <- "title: \"\""
  write_lines(p$home, "guidelines/e.yml", lines)
  expect_identical(
    rows_26(p$home, "guidelines_valid"),
    data.frame(
      status = "FAIL",
      path = "guidelines/e.yml",
      detail = with_fix(
        "guidelines_valid",
        "guidelines/e.yml: the key title is missing or not one string"
      )
    ),
    ignore_attr = TRUE
  )
})

# The checklist guidelines/mini.yml with the result items 1 and 2 and the
# prose item 3, and v01_functions/blocks.R with one block whose items are
# the strings `items`.
write_mini_coverage <- function(home, items) {
  write_lines(
    home,
    "guidelines/mini.yml",
    guideline_lines(
      "mini",
      c(
        entry_text("1", kind = "result"),
        entry_text("2", kind = "result"),
        entry_text("3", kind = "prose")
      )
    )
  )
  write_lines(
    home,
    "v01_functions/blocks.R",
    c(
      "blocks <- list(",
      "  \"Effect\" = pbn::block(",
      "    function(results) results$effect,",
      "    format = \"ci\",",
      paste0(
        "    items = c(",
        paste0("\"", items, "\"", collapse = ", "),
        ")"
      ),
      "  )",
      ")"
    )
  )
  return(invisible(home))
}

test_that("a result item that no block names gives a WARN guidelines_covered, and a prose item never does", {
  p <- local_project_26()
  write_mini_coverage(p$home, "mini:1")
  expect_identical(
    rows_26(p$home, "guidelines_covered"),
    data.frame(
      status = "WARN",
      path = "guidelines/mini.yml",
      detail = with_fix(
        "guidelines_covered",
        paste0(
          "guidelines/mini.yml: no block of v01_functions/blocks.R names ",
          "the result item mini:2 in its items"
        )
      )
    ),
    ignore_attr = TRUE
  )
  write_mini_coverage(p$home, c("mini:1", "mini:2"))
  expect_identical(
    rows_26(p$home, "guidelines_covered")[, c("status", "detail")],
    data.frame(
      status = "PASS",
      detail = paste(
        "every result item of guidelines/*.yml has a block, and every item",
        "of a block is in guidelines/*.yml"
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("a tag that names no item gives a WARN guidelines_covered, also a tag of a checklist that is not there", {
  p <- local_project_26()
  write_mini_coverage(
    p$home,
    c("mini:1", "mini:2", "mini:3", "mini:99", "strobe:99")
  )
  expect_identical(
    rows_26(p$home, "guidelines_covered"),
    data.frame(
      status = c("WARN", "WARN"),
      path = c("v01_functions/blocks.R", "v01_functions/blocks.R"),
      detail = with_fix(
        "guidelines_covered",
        c(
          paste0(
            "v01_functions/blocks.R: the block \"Effect\" names mini:99, ",
            "and no file guidelines/*.yml has that item"
          ),
          paste0(
            "v01_functions/blocks.R: the block \"Effect\" names strobe:99, ",
            "and no file guidelines/*.yml has that item"
          )
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("guidelines_covered reads the blocks.R of the newest version only, and a broken checklist gives it no finding", {
  p <- local_project_26()
  write_mini_coverage(p$home, c("mini:1", "mini:2", "mini:99"))
  # v02 names mini:1 only. The tags of v01 count no more.
  dir.create(file.path(p$home, "v02_functions"))
  file.copy(
    file.path(p$home, "v01_functions", "blocks.R"),
    file.path(p$home, "v02_functions")
  )
  write_lines(
    p$home,
    "v02_functions/blocks.R",
    sub(
      "c(\"mini:1\", \"mini:2\", \"mini:99\")",
      "c(\"mini:1\")",
      readLines(file.path(p$home, "v02_functions", "blocks.R")),
      fixed = TRUE
    )
  )
  expect_identical(
    rows_26(p$home, "guidelines_covered")$detail,
    with_fix(
      "guidelines_covered",
      paste0(
        "guidelines/mini.yml: no block of v02_functions/blocks.R names ",
        "the result item mini:2 in its items"
      )
    )
  )
  # A file that breaks the schema gives guidelines_valid a FAIL, and
  # guidelines_covered nothing for its items or its tags.
  unlink(file.path(p$home, "v02_functions"), recursive = TRUE)
  write_lines(
    p$home,
    "guidelines/mini.yml",
    guideline_lines("mini", entry_text("1", kind = "maybe"))
  )
  expect_identical(rows_26(p$home, "guidelines_covered")$status, "PASS")
  expect_identical(rows_26(p$home, "guidelines_valid")$status, "FAIL")
})

# The manual manuscripts `files` in manuscript/manual/ of `home`, and no
# other file there.
write_manual <- function(home, files) {
  unlink(file.path(home, "manuscript", "manual"), recursive = TRUE)
  for (f in files) {
    write_lines(home, file.path("manuscript", "manual", f), "")
  }
  return(invisible(home))
}

# The rule, the status, the path and the detail of the rows of the two
# supplement rules. When project_findings() stops, one row with the rule
# "error" and the message, so that the comparison fails, and not the call.
supplement_rows <- function(home) {
  found <- tryCatch(project_findings(home), error = function(e) return(e))
  if (inherits(found, "error")) {
    return(data.frame(
      rule = "error",
      status = "error",
      path = "",
      detail = conditionMessage(found)
    ))
  }
  keep <- found$rule %in% supplement_rules
  return(found[keep, c("rule", "status", "path", "detail")])
}

test_that("manual_has_supplement fails a main manual file with no _si file at or below its version, and names the file to add", {
  p <- local_project_26()
  write_manual(p$home, "v01_AB.docx")
  expect_identical(
    supplement_rows(p$home),
    data.frame(
      rule = c("manual_has_supplement", "manual_si_has_main"),
      status = c("FAIL", "PASS"),
      path = c("manuscript/manual/v01_AB.docx", ""),
      detail = c(
        with_fix(
          "manual_has_supplement",
          paste0(
            "manuscript/manual/v01_AB.docx: there is no supplement ",
            "manuscript/manual/vMM_<INITIALS>_si.docx of any initials with MM ",
            "at or below 01"
          )
        ),
        paste(
          "every manuscript/manual/vNN_<INITIALS>_si.docx has the main file",
          "manuscript/manual/vNN_<INITIALS>.docx"
        )
      )
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    project_rules()$manual_has_supplement[[3]],
    paste(
      "add the supplement manuscript/manual/vNN_<INITIALS>_si.docx, or",
      "turn the rule off with the line manual_has_supplement: off under",
      "rules: in pbn.yml"
    )
  )
  # A file with another suffix is not a supplement.
  write_manual(p$home, c("v01_AB.docx", "v01_AB_cover.docx"))
  expect_identical(
    supplement_rows(p$home)$status,
    c("FAIL", "PASS")
  )
})

test_that("v02_AB.docx, v02_AB_si.docx and v03_CD.docx pass both rules, because the _si file of v02 covers v03 whatever its initials", {
  p <- local_project_26()
  write_manual(p$home, c("v02_AB.docx", "v02_AB_si.docx", "v03_CD.docx"))
  found <- project_findings(p$home)
  expect_identical(intersect(failed_rules(found), supplement_rules), character(0))
  expect_identical(
    supplement_rows(p$home)$status,
    c("PASS", "PASS")
  )
  # The _si file of v02 does not cover v01.
  write_manual(p$home, c("v01_CD.docx", "v02_AB.docx", "v02_AB_si.docx"))
  expect_identical(
    supplement_rows(p$home),
    data.frame(
      rule = c("manual_has_supplement", "manual_si_has_main"),
      status = c("FAIL", "PASS"),
      path = c("manuscript/manual/v01_CD.docx", ""),
      detail = c(
        with_fix(
          "manual_has_supplement",
          paste0(
            "manuscript/manual/v01_CD.docx: there is no supplement ",
            "manuscript/manual/vMM_<INITIALS>_si.docx of any initials with MM ",
            "at or below 01"
          )
        ),
        paste(
          "every manuscript/manual/vNN_<INITIALS>_si.docx has the main file",
          "manuscript/manual/vNN_<INITIALS>.docx"
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("manual_si_has_main fails v02_AB_si.docx beside v03_AB.docx without v02_AB.docx, and an _si file with no main file at all", {
  p <- local_project_26()
  write_manual(p$home, c("v02_AB_si.docx", "v03_AB.docx"))
  expect_identical(
    supplement_rows(p$home),
    data.frame(
      rule = c("manual_has_supplement", "manual_si_has_main"),
      status = c("PASS", "FAIL"),
      path = c("", "manuscript/manual/v02_AB_si.docx"),
      detail = c(
        paste(
          "every manuscript/manual/vNN_<INITIALS>.docx has a supplement",
          "manuscript/manual/vMM_<INITIALS>_si.docx of any initials with MM at",
          "or below NN"
        ),
        with_fix(
          "manual_si_has_main",
          paste0(
            "manuscript/manual/v02_AB_si.docx: there is no main file ",
            "manuscript/manual/v02_AB.docx"
          )
        )
      )
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    project_rules()$manual_si_has_main[[3]],
    paste(
      "add the main file manuscript/manual/vNN_<INITIALS>.docx with the",
      "version and the initials of the _si file, or rename the _si file"
    )
  )
  # Other initials of the same version are not the main file.
  write_manual(p$home, c("v02_AB_si.docx", "v02_CD.docx"))
  expect_identical(
    supplement_rows(p$home)[, c("status", "path")],
    data.frame(
      status = c("PASS", "FAIL"),
      path = c("", "manuscript/manual/v02_AB_si.docx")
    ),
    ignore_attr = TRUE
  )
  write_manual(p$home, "v01_AB_si.docx")
  expect_identical(
    supplement_rows(p$home)[, c("status", "path")],
    data.frame(
      status = c("PASS", "FAIL"),
      path = c("", "manuscript/manual/v01_AB_si.docx")
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    intersect(failed_rules(project_findings(p$home)), supplement_rules),
    "manual_si_has_main"
  )
})

test_that("rules: manual_has_supplement: off gives OFF, and an allow entry of the path of the main file leaves no FAIL", {
  p <- local_project_26()
  write_manual(p$home, "v01_AB.docx")
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_26", "rules:", "  manual_has_supplement: off")
  )
  expect_identical(
    supplement_rows(p$home)[1, c("rule", "status", "path", "detail")],
    data.frame(
      rule = "manual_has_supplement",
      status = "OFF",
      path = "",
      detail = "off in pbn.yml"
    ),
    ignore_attr = TRUE
  )
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_26",
      "allow:",
      "  manual_has_supplement: [manuscript/manual/v01_AB.docx]"
    )
  )
  expect_false("FAIL" %in% supplement_rows(p$home)$status)
  # apply_allow() makes an allowed finding a WARN that ends in "(allowed)".
  expect_identical(
    supplement_rows(p$home)[1, c("status", "path", "detail")],
    data.frame(
      status = "WARN",
      path = "manuscript/manual/v01_AB.docx",
      detail = with_fix(
        "manual_has_supplement",
        paste0(
          "manuscript/manual/v01_AB.docx: there is no supplement ",
          "manuscript/manual/vMM_<INITIALS>_si.docx of any initials with MM ",
          "at or below 01 ",
          "(allowed)"
        )
      )
    ),
    ignore_attr = TRUE
  )
})

test_that("the supplement rules ignore manuscript/draft/: a draft main without _si and a draft _si without main give no finding", {
  p <- local_project_26()
  write_manual(p$home, c("v02_AB.docx", "v02_AB_si.docx"))
  write_lines(p$home, "manuscript/draft/v03_CD.docx", "")
  write_lines(p$home, "manuscript/draft/v03_EF_si.docx", "")
  rows <- supplement_rows(p$home)
  expect_identical(
    rows[, c("rule", "status", "path")],
    data.frame(
      rule = supplement_rules,
      status = c("PASS", "PASS"),
      path = c("", "")
    ),
    ignore_attr = TRUE
  )
  expect_false(any(startsWith(rows$path, "manuscript/draft/")))
})

test_that("under house_2026_11_25 the same projects give no row of manual_has_supplement or manual_si_has_main", {
  p <- local_project_26()
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_25")
  for (files in list("v01_AB.docx", c("v02_AB_si.docx", "v03_AB.docx"))) {
    write_manual(p$home, files)
    # supplement_rows() gives one row "error" when project_findings() stops.
    expect_identical(nrow(supplement_rows(p$home)), 0L)
  }
  expect_false(any(supplement_rules %in% names(read_house_set("house_2026_11_25"))))
})

# An entry of items with the item id `item`, the kind `kind` and the other
# keys `...`.
schema_entry <- function(item, kind = "result", ...) {
  return(list(
    item = item,
    section = "S",
    topic = "T",
    kind = kind,
    text = "x",
    ...
  ))
}

test_that("guideline_top_problems() lists the problems of the top-level keys in the order of the schema checks", {
  bad <- list(
    id = "Bad_ID",
    extra = 1,
    version = 2,
    citation = NULL,
    designs = list("a", 1),
    design = "z",
    items = list(a = 1)
  )
  expect_identical(
    guideline_top_problems(bad),
    c(
      "the key extra is not a key of the schema",
      "the key title is missing or not one string",
      "the id Bad_ID has a character other than a-z, 0-9 and -",
      "the key citation is present but empty",
      "the key version is not one string",
      "the key designs is not a list of strings",
      "the key design is not one of the key designs",
      "the key items is not a YAML sequence of entries"
    )
  )
  expect_identical(
    guideline_top_problems(list(id = "ok", title = "T", items = list())),
    "the key items is missing or empty"
  )
  ok <- list(
    id = "ok",
    title = "T",
    designs = list("a", "b"),
    design = "a",
    items = list(schema_entry("1"))
  )
  expect_identical(guideline_top_problems(ok), character(0))
})

test_that("guideline_entries() labels each problem of an entry, keeps the valid entries of the design, and finds an item id that comes twice", {
  y <- list(
    id = "ok",
    title = "T",
    designs = list("a", "b"),
    items = list(
      schema_entry("1"),
      schema_entry("2", kind = "maybe", foo = 1),
      "scalar",
      schema_entry("4", designs = list("c")),
      schema_entry("5", designs = NULL),
      schema_entry("6", designs = "b"),
      schema_entry("1"),
      schema_entry("7", designs = list("b"))
    )
  )
  problems <- c(
    "item 2: the key foo is not a key of an item",
    "item 2: the kind maybe is not result, prose or author",
    "entry 3: it is not a YAML map",
    "item 4: the key designs holds a value that the file does not",
    "item 5: the key designs is present but empty",
    "item 6: the key designs is not a list of strings"
  )
  rows <- function(designs) {
    n <- length(designs)
    return(data.frame(
      item = c("1", "1", "7")[seq_len(n)],
      section = rep("S", n),
      topic = rep("T", n),
      kind = rep("result", n),
      text = rep("x", n),
      designs = designs
    ))
  }
  expect_identical(
    guideline_entries(y),
    list(
      problems = c(
        problems,
        "item 1: the item id comes more than once for the design a",
        "item 1: the item id comes more than once for the design b"
      ),
      items = rows(c("a, b", "a, b", "b"))
    )
  )
  y$design <- "a"
  expect_identical(
    guideline_entries(y),
    list(
      problems = c(
        problems,
        "item 1: the item id comes more than once for the design a"
      ),
      items = rows(c("a", "a"))
    )
  )
  none <- guideline_entries(list(
    id = "ok",
    title = "T",
    items = list(schema_entry("1", designs = list("a")))
  ))
  expect_identical(
    none$problems,
    "item 1: it has the key designs, and the file has none"
  )
  expect_identical(nrow(none$items), 0L)
  expect_identical(
    names(none$items),
    c("item", "section", "topic", "kind", "text", "designs")
  )
})
