# The proof of a Table 1 style: each installed style reproduces the real
# published tables of its reference folder, and every cell that differs is a
# listed exception. The fixture style fixtures/table1_proof/ is made up, and
# so are its two reference tables. It has one exception of each rule.

proof_dir <- test_path("fixtures", "table1_proof")
proof_name <- "fixture_proof_2026_12_04"

# A copy of the fixture style and its folder in a temporary folder, with
# `change` applied to the folder. It returns the path of the style file.
proof_copy <- function(
  change = function(dir) return(NULL),
  env = parent.frame()
) {
  root <- withr::local_tempdir(.local_envir = env)
  file.copy(file.path(proof_dir, paste0(proof_name, ".yml")), root)
  file.copy(file.path(proof_dir, proof_name), root, recursive = TRUE)
  change(file.path(root, proof_name))
  return(file.path(root, paste0(proof_name, ".yml")))
}

# The line `from` of the file `file` changed to `to`. It stops when `from` is
# not a line of the file, so a change that does not apply cannot pass.
change_line <- function(file, from, to) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  stopifnot(sum(x == from) == 1)
  x[x == from] <- to
  writeLines(x, file, useBytes = TRUE)
  return(invisible(NULL))
}

# The fixture style file with the YAML `change` applied, beside a copy of its
# folder.
style_copy <- function(change, env = parent.frame()) {
  f <- proof_copy(env = env)
  s <- yaml::read_yaml(f)
  yaml::write_yaml(change(s), f)
  return(f)
}

# exceptions.yml of a copy of the folder, with `change` applied to its list.
exceptions_copy <- function(change, env = parent.frame()) {
  return(proof_copy(
    function(dir) {
      f <- file.path(dir, "exceptions.yml")
      x <- yaml::read_yaml(f)
      x$exceptions <- change(x$exceptions)
      yaml::write_yaml(x, f)
      return(NULL)
    },
    env = env
  ))
}

with_name <- function(x) {
  return(paste0(proof_name, ": ", x))
}

# The failure of each of the four cells of the fixture that differ, when no
# exception lists it.
unlisted <- c(
  paste0(
    "reference_1, row 2 (Smoker), column \"Group B (n = 10)\": printed ",
    "\"4 (45.0%)\", the style gives \"4 (40.0%)\", and exceptions.yml does ",
    "not list the cell."
  ),
  paste0(
    "reference_1, row 4 (Age, years), column \"P\": printed \"0.512\", the ",
    "style gives \"\", and exceptions.yml does not list the cell."
  ),
  paste0(
    "reference_1, row 5 (Parity), column \"P\": printed \"0.62\", the style ",
    "gives \"0.621\", and exceptions.yml does not list the cell."
  ),
  paste0(
    "reference_2, row 2 (Epidural), column \"P value\": printed \"<.001\", ",
    "the style gives \"<0.001\", and exceptions.yml does not list the cell."
  )
)

test_that("every installed style reproduces its reference tables, or lists each differing cell as an exception", {
  expect_identical(style_proofs(), character(0))
})

test_that("the fixture style is proven: each cell of its two reference tables matches or is a listed exception", {
  local_mocked_bindings(table1_style_dir = function() return(proof_dir))
  expect_identical(table1_styles(), proof_name)
  expect_identical(style_proofs(), character(0))
  # 9 printed rows and 5 columns in reference_1, 3 rows and 4 columns in
  # reference_2, the label columns included, so 57 cells are compared.
  dir <- file.path(proof_dir, proof_name)
  file <- paste0(dir, ".yml")
  n <- vapply(
    1:2,
    function(k) return(nrow(proof_cells(read_proof_inputs(dir, k), file))),
    integer(1)
  )
  expect_identical(n, c(45L, 12L))
})

test_that("a changed reference cell that no exception lists fails, and names the style, reference, row, column and both strings", {
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "reference_1.csv"),
      "\"Smoker\",\"6 (30.0%)\",\"2 (20.0%)\",\"4 (45.0%)\",\"0.628\"",
      "\"Smoker\",\"6 (30%)\",\"2 (20.0%)\",\"4 (45.0%)\",\"0.628\""
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_1, row 2 (Smoker), column \"All (n = 20)\": printed ",
      "\"6 (30%)\", the style gives \"6 (30.0%)\", and exceptions.yml does ",
      "not list the cell."
    ))
  )
})

test_that("a listed exception whose strings no longer match the cell is stale and fails", {
  f <- exceptions_copy(function(x) {
    x[[1]]$style <- "4 (41.0%)"
    return(x)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_1, row 2 (Smoker), column \"Group B (n = 10)\": ",
      "exceptions.yml lists printed \"4 (45.0%)\" and style \"4 (41.0%)\", ",
      "but the cell is printed \"4 (45.0%)\" and the style gives ",
      "\"4 (40.0%)\"."
    ))
  )
  # A cell that now matches keeps no exception.
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "reference_1.csv"),
      "\"Smoker\",\"6 (30.0%)\",\"2 (20.0%)\",\"4 (45.0%)\",\"0.628\"",
      "\"Smoker\",\"6 (30.0%)\",\"2 (20.0%)\",\"4 (40.0%)\",\"0.628\""
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_1, row 2 (Smoker), column \"Group B (n = 10)\": ",
      "exceptions.yml lists printed \"4 (45.0%)\" and style \"4 (40.0%)\", ",
      "but the cell is printed \"4 (40.0%)\" and the style gives ",
      "\"4 (40.0%)\"."
    ))
  )
})

test_that("an exception that names no cell, or one cell twice, fails", {
  f <- exceptions_copy(function(x) {
    x[[1]]$row <- 30L
    return(x)
  })
  expect_identical(
    style_proof(f),
    with_name(c(
      paste0(
        "reference_1, row 2 (Smoker), column \"Group B (n = 10)\": printed ",
        "\"4 (45.0%)\", the style gives \"4 (40.0%)\", and exceptions.yml ",
        "does not list the cell."
      ),
      paste0(
        "exceptions.yml names reference_1, row 30, column \"Group B (n = ",
        "10)\", which is not a cell of reference_1.csv."
      )
    ))
  )
  f <- exceptions_copy(function(x) {
    x[[5]] <- x[[1]]
    x[[5]]$reference <- "reference_9"
    return(x)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "exceptions.yml names reference_9, row 2, column \"Group B (n = 10)\", ",
      "which is not a cell of reference_9.csv."
    ))
  )
  f <- exceptions_copy(function(x) return(c(x, x[1])))
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "exceptions.yml lists the cell reference_1, row 2, column ",
      "\"Group B (n = 10)\" more than once."
    ))
  )
})

test_that("a style with no reference folder, or a folder that lacks a file, fails", {
  f <- proof_copy(function(dir) return(unlink(dir, recursive = TRUE)))
  expect_identical(
    style_proof(f),
    with_name(paste0("there is no reference folder ", proof_name, "/."))
  )
  for (file in c(
    "reference_2.csv",
    "source_2.yml",
    "fake_2.csv",
    "vars_2.csv",
    "layout_2.yml"
  )) {
    f <- proof_copy(function(dir) return(file.remove(file.path(dir, file))))
    expect_identical(
      style_proof(f),
      with_name(paste0("the reference folder lacks the file ", file, "."))
    )
  }
  # Without exceptions.yml, each of the four cells that differ is unlisted.
  f <- proof_copy(function(dir) {
    return(file.remove(file.path(dir, "exceptions.yml")))
  })
  expect_identical(
    style_proof(f),
    with_name(c(
      "the reference folder lacks the file exceptions.yml.",
      unlisted
    ))
  )
  f <- proof_copy(function(dir) {
    return(file.copy(
      file.path(dir, "fake_2.csv"),
      file.path(dir, "fake_2.CSV")
    ))
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "the reference folder holds the file fake_2.CSV, which is not a file ",
      "of a proof."
    ))
  )
})

test_that("a style field whose source says editorial fails", {
  f <- style_copy(function(s) {
    s$bin$template$source <- "editorial choice"
    s$cont$mean_digits$source <- paste0(
      "An Editorial reading of the Fixture guide for authors, p. 2."
    )
    s$footnotes[[1]]$source <- "Fixturesen 2026, Table 1. Editorial."
    return(s)
  })
  expect_identical(
    style_proof(f),
    with_name(c(
      "the source of the field bin.template says editorial.",
      "the source of the field cont.mean_digits says editorial.",
      "the source of the field footnotes[1] says editorial."
    ))
  )
})

test_that("a style field whose source cites no reference cell, no author and table and no guideline page fails", {
  nothing <- c(
    "2/2 papers give one decimal.",
    "reference_3, row 2, column \"P\".",
    "reference_1, row 99, column \"P\".",
    "reference_1, row 2, column \"Q\".",
    "reference_1, row 2.",
    "The Fixture guide for authors.",
    "Fixturesen 2026.",
    "Table 1.",
    "Fixturesen 2026, Table 2."
  )
  for (src in nothing) {
    f <- style_copy(function(s) {
      s$bin$pct_digits$source <- src
      return(s)
    })
    expect_identical(
      style_proof(f),
      with_name(paste0(
        "the source of the field bin.pct_digits cites no reference cell, no ",
        "author and table, and no guideline page."
      )),
      label = src
    )
  }
  # Each of the three forms of a citation passes.
  cites <- c(
    "reference_2, row 3, column \"P value\".",
    "Testberg 2026, Table 2.",
    "Fixture guideline, page 7."
  )
  for (src in cites) {
    f <- style_copy(function(s) {
      s$bin$pct_digits$source <- src
      return(s)
    })
    expect_identical(style_proof(f), character(0), label = src)
  }
})

test_that("an exception with an empty source or justification fails", {
  f <- exceptions_copy(function(x) {
    x[[1]]$justification <- ""
    x[[2]]$source <- "  "
    return(x)
  })
  expect_identical(
    style_proof(f),
    with_name(c(
      "exceptions.yml entry 1 has an empty justification.",
      "exceptions.yml entry 2 has an empty source."
    ))
  )
})

test_that("an exception with a rule outside the four fails", {
  f <- exceptions_copy(function(x) {
    x[[1]]$rule <- "editorial"
    return(x)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "exceptions.yml entry 1 has a rule that is not one of guideline, ",
      "other_reference, not_computable, published_error."
    ))
  )
})

test_that("an exception by guideline cites a page, and one by other_reference cites a cell of another reference", {
  f <- exceptions_copy(function(x) {
    x[[3]]$source <- "The Fixture guide for authors."
    x[[4]]$source <- "reference_2, row 1, column \"P value\" prints \"0.642\"."
    return(x)
  })
  expect_identical(
    style_proof(f),
    with_name(c(
      paste0(
        "exceptions.yml entry 3 has the rule guideline, and its source cites ",
        "no guideline page."
      ),
      paste0(
        "exceptions.yml entry 4 has the rule other_reference, and its source ",
        "cites no cell of another reference."
      )
    ))
  )
  f <- exceptions_copy(function(x) {
    x[[1]]$printed <- NULL
    x[[2]]$note <- "x"
    x[[3]]$row <- "5"
    return(x)
  })
  # An entry whose cell fields are bad lists no cell, so its cell is
  # unlisted too.
  expect_identical(
    style_proof(f),
    with_name(c(
      "exceptions.yml entry 1 lacks the field printed.",
      "exceptions.yml entry 2 has the unknown field note.",
      paste0(
        "exceptions.yml entry 3 needs reference as reference_<k>, row as a ",
        "whole number, and column, printed and style as quoted strings."
      ),
      unlisted[1:3]
    ))
  )
})

test_that("the layout, vars and source of a reference must match it", {
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "layout_2.yml"),
      "  - {var: smoker, row: value}",
      "  - {var: smoker, row: value, label: Smoker}"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_2: layout_2.yml row 1 has the key label, and a layout row ",
      "has only the keys var, row and level."
    ))
  )
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "layout_2.yml"),
      "  - {column: 'P value', cell: p}",
      "  - {column: 'P', cell: p}"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_2: layout_2.yml lists the columns \"Cases (n = 8)\", ",
      "\"Controls (n = 12)\", \"P\", but reference_2.csv has the columns ",
      "\"Cases (n = 8)\", \"Controls (n = 12)\", \"P value\"."
    ))
  )
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "fake_2.csv"),
      "case,1,1,26",
      "cases,1,1,26"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_2: fake_2.csv has values of group that vars_2.csv does not ",
      "list: cases."
    ))
  )
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "layout_2.yml"),
      "  - {column: 'Cases (n = 8)', cell: value, grp: case}",
      "  - {column: 'Cases (n = 8)', cell: value, grp: cases}"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name(paste0(
      "reference_2: layout_2.yml row 1 and column \"Cases (n = 8)\" name the ",
      "variable smoker, the row value, the level NA and the group cases, and ",
      "table1() gives 0 such rows."
    ))
  )
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "source_2.yml"),
      "doi: '10.0000/pbn.fixture.b'",
      "doi: ''"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    with_name("reference_2: source_2.yml lacks the field doi, or it is empty.")
  )
})

# The failure of a label cell that no exception lists.
label_failure <- function(reference, row, printed, style) {
  return(with_name(paste0(
    reference,
    ", row ",
    row,
    " (",
    printed,
    "), column \"\": printed \"",
    printed,
    "\", the style gives \"",
    style,
    "\", and exceptions.yml does not list the cell."
  )))
}

ref1_level_1 <- "\"- 1\",\"7 (36.8%)\",\"4 (40.0%)\",\"3 (33.3%)\",\"\""
ref2_smoker <- "\"Smoker\",\"3 (37.5%)\",\"3 (25.0%)\",\"0.642\""

test_that("a changed printed label of a variable row fails, and an exception can list it", {
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "reference_2.csv"),
      ref2_smoker,
      sub("^\"Smoker\"", "\"Smokers\"", ref2_smoker)
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_2", 1, "Smokers", "Smoker")
  )
  # The label of the style comes from vars_2.csv.
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "vars_2.csv"),
      "smoker,bin,Smoker,,",
      "smoker,bin,Smoking,,"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_2", 1, "Smoker", "Smoking")
  )
  # A label cell follows the exception rules: listed, it passes; with other
  # strings, it is stale.
  for (style in c("Smoker", "Smoke")) {
    f <- proof_copy(function(dir) {
      change_line(
        file.path(dir, "reference_2.csv"),
        ref2_smoker,
        sub("^\"Smoker\"", "\"Smokers\"", ref2_smoker)
      )
      e <- file.path(dir, "exceptions.yml")
      x <- yaml::read_yaml(e)
      x$exceptions[[5]] <- list(
        reference = "reference_2",
        row = 1L,
        column = "",
        printed = "Smokers",
        style = style,
        rule = "other_reference",
        source = "reference_1, row 2, column \"\" prints \"Smoker\".",
        justification = "The style follows the singular of reference_1."
      )
      yaml::write_yaml(x, e)
      return(NULL)
    })
    expect_identical(
      style_proof(f),
      if (style == "Smoker") {
        character(0)
      } else {
        with_name(paste0(
          "reference_2, row 1 (Smokers), column \"\": exceptions.yml lists ",
          "printed \"Smokers\" and style \"Smoke\", but the cell is printed ",
          "\"Smokers\" and the style gives \"Smoker\"."
        ))
      },
      label = style
    )
  }
})

test_that("a changed printed level label fails, and so does a prefix that the style does not declare", {
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "reference_1.csv"),
      ref1_level_1,
      sub("^\"- 1\"", "\"- one\"", ref1_level_1)
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_1", 7, "- one", "- 1")
  )
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "reference_1.csv"),
      ref1_level_1,
      sub("^\"- 1\"", "\"1\"", ref1_level_1)
    )
    return(NULL)
  })
  expect_identical(style_proof(f), label_failure("reference_1", 7, "1", "- 1"))
})

test_that("a Missing label that differs from missing.row_label of the style fails", {
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "reference_1.csv"),
      "\"- Missing\",\"1 (5.0%)\",\"0 (0.0%)\",\"1 (10.0%)\",\"\"",
      "\"- Not known\",\"1 (5.0%)\",\"0 (0.0%)\",\"1 (10.0%)\",\"\""
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_1", 9, "- Not known", "- Missing")
  )
  f <- style_copy(function(s) {
    s$missing$row_label$value <- "Missing"
    return(s)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_1", 9, "- Missing", "Missing")
  )
})

test_that("a printed section row fails when table1() puts no section row before its variable, or one with another label", {
  f <- proof_copy(function(dir) {
    for (v in c("smoker,bin,Smoker", "epidural,bin,Epidural")) {
      change_line(
        file.path(dir, "vars_1.csv"),
        paste0(v, ",,Characteristics of the mother"),
        paste0(v, ",,")
      )
    }
    change_line(
      file.path(dir, "vars_1.csv"),
      "age,cont,\"Age, years\",,Characteristics of the mother",
      "age,cont,\"Age, years\",,"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_1", 1, "Characteristics of the mother", "")
  )
  # The label of the section row comes from vars_1.csv.
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "vars_1.csv"),
      "smoker,bin,Smoker,,Characteristics of the mother",
      "smoker,bin,Smoker,,Characteristics of mothers"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure(
      "reference_1",
      1,
      "Characteristics of the mother",
      "Characteristics of mothers"
    )
  )
  # table1() puts the section row before epidural, the first variable of the
  # section, so no section row comes before smoker.
  f <- proof_copy(function(dir) {
    change_line(
      file.path(dir, "vars_1.csv"),
      "smoker,bin,Smoker,,Characteristics of the mother",
      "smoker,bin,Smoker,,"
    )
    return(NULL)
  })
  expect_identical(
    style_proof(f),
    label_failure("reference_1", 1, "Characteristics of the mother", "")
  )
})

test_that("a printed row of none with cells is an exception with the rule not_computable, or it fails", {
  # reference_2 gets the printed row Total, which table1() does not compute.
  add_total <- function(dir) {
    cat(
      "\"Total\",\"8\",\"12\",\"\"\n",
      file = file.path(dir, "reference_2.csv"),
      append = TRUE
    )
    cat(
      "  - {row: none}\n",
      file = file.path(dir, "layout_2.yml"),
      append = TRUE
    )
    return(NULL)
  }
  total <- function(column, printed) {
    return(with_name(paste0(
      "reference_2, row 4 (Total), column \"",
      column,
      "\": printed \"",
      printed,
      "\", the style gives \"\", and exceptions.yml does not list the cell."
    )))
  }
  f <- proof_copy(add_total)
  expect_identical(
    style_proof(f),
    c(
      total("", "Total"),
      total("Cases (n = 8)", "8"),
      total("Controls (n = 12)", "12")
    )
  )
  # Listed with not_computable, the three cells pass. With another rule, a
  # cell fails.
  for (rule in c("not_computable", "published_error")) {
    f <- proof_copy(function(dir) {
      add_total(dir)
      e <- file.path(dir, "exceptions.yml")
      x <- yaml::read_yaml(e)
      cols <- c("", "Cases (n = 8)", "Controls (n = 12)")
      printed <- c("Total", "8", "12")
      for (i in 1:3) {
        x$exceptions[[4 + i]] <- list(
          reference = "reference_2",
          row = 4L,
          column = cols[i],
          printed = printed[i],
          style = "",
          rule = if (i == 2) rule else "not_computable",
          source = "?pbn::table1: table1() gives no row of totals.",
          justification = "The reference prints the group sizes in a row."
        )
      }
      yaml::write_yaml(x, e)
      return(NULL)
    })
    expect_identical(
      style_proof(f),
      if (rule == "not_computable") {
        character(0)
      } else {
        with_name(paste0(
          "reference_2, row 4 (Total), column \"Cases (n = 8)\": the row is ",
          "of none, so its exception MUST have the rule not_computable."
        ))
      },
      label = rule
    )
  }
})
