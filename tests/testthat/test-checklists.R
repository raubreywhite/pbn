# pbn::make_checklists(): the filled-in checklist of each guideline, from
# the formatted strings of the blocks and the text of the manuscript.

# A fake project for make_checklists() in a temporary folder: one checklist
# guidelines/mini.yml with the result items 1 and 2 and the prose item 3,
# three blocks in v01_functions/blocks.R with a project formatter, their raw
# values in v01_results/real/blocks.qs2, and the built manuscript
# manuscript/built/v01/paper.md with the lines `md`.
checklist_project <- function(md = paper_md, env = parent.frame()) {
  home <- tempfile("pbnchecklist")
  dir.create(home)
  withr::defer(unlink(home, recursive = TRUE), envir = env)
  home <- normalizePath(home, winslash = "/")
  write_lines(
    home,
    "guidelines/mini.yml",
    c(
      "id: mini",
      "title: MINI",
      "items:",
      "  - item: \"1\"",
      "    section: Results",
      "    topic: Main results",
      "    kind: result",
      "    text: \"Give the adjusted estimate | with its 95% CI\"",
      "  - item: \"2\"",
      "    section: Results",
      "    topic: Participants",
      "    kind: result",
      "    text: \"Report the numbers of participants\"",
      "  - item: \"3\"",
      "    section: Discussion",
      "    topic: Limitations",
      "    kind: prose",
      "    text: \"Discuss limitations\""
    )
  )
  write_lines(home, "v01_functions/blocks.R", mini_blocks)
  write_lines(home, "v01_functions/formatters.R", mini_formatters)
  dir.create(file.path(home, "v01_results", "real"), recursive = TRUE)
  qs2::qs_save(
    list(
      "Effect" = data.frame(est = 0.8, lo = 0.46, hi = 1.16, p = 0.0002),
      "Sample" = data.frame(n = 1037, total = 1509),
      "Table 1" = data.frame(
        variable = c("Maternal age", "Body mass index"),
        mean = c(30.66, 24.12),
        sd = c(4.81, 3.18)
      )
    ),
    file.path(home, "v01_results", "real", "blocks.qs2")
  )
  write_lines(home, "manuscript/built/v01/paper.md", md)
  return(home)
}

mini_blocks <- c(
  "blocks <- list(",
  "  \"Effect\" = pbn::block(",
  "    function(results) results$effect,",
  "    format = \"effect\",",
  "    items = c(\"mini:1\")",
  "  ),",
  "  \"Sample\" = pbn::block(",
  "    function(results) results$sample,",
  "    format = \"n_pct\",",
  "    items = c(\"mini:2\")",
  "  ),",
  "  \"Table 1\" = pbn::block(",
  "    function(results) results$table1,",
  "    format = \"cells\",",
  "    items = c(\"mini:2\")",
  "  )",
  ")"
)

# A project formatter that returns a data.frame of two columns.
mini_formatters <- c(
  "cells <- function(x) {",
  "  return(data.frame(",
  "    variable = x$variable,",
  "    value = sprintf(\"%.1f (%.1f)\", x$mean, x$sd)",
  "  ))",
  "}"
)

# The built manuscript: it prints every formatted string, the p-value with
# the backslash of pandoc Markdown.
paper_md <- c(
  "# A paper",
  "",
  "## Methods",
  "",
  "We describe the cohort.",
  "",
  "## Results",
  "",
  "The effect was (0.80, 95% CI 0.46 to 1.16, p \\< 0.001) in all.",
  "Of the people, 1,037 (68.7%) took part.",
  "",
  "## Table 1 {#tbl-1}",
  "",
  "| Variable | Mean (SD) |",
  "|---|---|",
  "| Maternal age | 30.7 (4.8) |",
  "| Body mass index | 24.1 (3.2) |"
)

# The status of each item of the table `x` of make_checklists().
statuses <- function(x) {
  return(stats::setNames(x$Status, x$Item))
}

# A .docx file `file` with only word/document.xml, whose body is the XML
# `body`. Needs the program zip.
write_docx <- function(file, body) {
  dir <- tempfile("pbndocx")
  dir.create(file.path(dir, "word"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      paste0(
        "<w:document xmlns:w=\"http://schemas.openxmlformats.org/",
        "wordprocessingml/2006/main\"><w:body>",
        body,
        "</w:body></w:document>"
      )
    ),
    file.path(dir, "word", "document.xml")
  )
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  withr::with_dir(dir, utils::zip(file, "word/document.xml", flags = "-q"))
  return(invisible(file))
}

# A paragraph of WordprocessingML with the runs `runs` and the style
# `style`, or no style.
w_p <- function(runs, style = NULL) {
  props <- if (is.null(style)) {
    ""
  } else {
    paste0("<w:pPr><w:pStyle w:val=\"", style, "\"/></w:pPr>")
  }
  return(paste0(
    "<w:p>",
    props,
    paste0(
      "<w:r><w:t xml:space=\"preserve\">",
      runs,
      "</w:t></w:r>",
      collapse = ""
    ),
    "</w:p>"
  ))
}

# The body of the .docx manuscript. The count 1,037 is split across two
# runs, as Word often writes it. `sample` replaces that paragraph.
docx_body <- function(
  sample = w_p(c("Of the people, 1,0", "37 (68.7%) took part."))
) {
  return(paste0(
    w_p("Results", "Heading1"),
    w_p("The effect was (0.80, 95% CI 0.46 to 1.16, p &lt; 0.001) in all."),
    sample,
    w_p("Table 1", "Heading2"),
    "<w:tbl><w:tr><w:tc>",
    w_p("Maternal age"),
    "</w:tc><w:tc>",
    w_p(c("30.7 ", "(4.8)")),
    "</w:tc></w:tr><w:tr><w:tc>",
    w_p("Body mass index"),
    "</w:tc><w:tc>",
    w_p("24.1 (3.2)"),
    "</w:tc></w:tr></w:tbl>"
  ))
}

test_that("norm_text() gives the output of norm() of the methods checker on its self-tests, and a<br>b gives a b", {
  # The self-tests of norm() in reporting-guidelines/check_checklists.R of
  # the methods repository, one for one.
  expect_identical(norm_text("a<br>b"), "a b")
  expect_identical(norm_text("<td>x</td>"), "x")
  expect_identical(norm_text("p \\< 0.001 and x > y"), "p < 0.001 and x > y")
  expect_identical(norm_text("a<br/>b<br />c"), "a b c")
  expect_identical(norm_text("<td rowspan=\"2\">a</td>"), "a")
  expect_identical(norm_text("a &amp; b &lt;c&gt;&nbsp;d"), "a & b <c> d")
  expect_identical(
    norm_text("13\\* \\_x\\_ \\[y\\] p \\< 1, q \\> 2"),
    "13* _x_ [y] p < 1, q > 2"
  )
  expect_identical(norm_text("a \\<z\\> b"), "a <z> b")
  expect_identical(norm_text("a\u2009b\u00a0c"), "a b c")
  expect_identical(norm_text("  a \t\n b  "), "a b")
  expect_identical(norm_text("\u22120.07"), "\u22120.07")
  expect_identical(norm_text("Ab"), "Ab")
})

test_that("a manuscript that prints every string gives numbers found with the headings, and to fill for a prose item", {
  local_clean_state()
  home <- checklist_project()
  before <- list.files(home, recursive = TRUE, all.files = TRUE)
  out <- withVisible(make_checklists("v01", home = home))
  expect_false(out$visible)
  expect_identical(
    out$value,
    data.frame(
      Guideline = c("mini", "mini", "mini"),
      Section = c("Results", "Results", "Discussion"),
      Item = c("1", "2", "3"),
      Recommendation = c(
        "Give the adjusted estimate | with its 95% CI",
        "Report the numbers of participants",
        "Discuss limitations"
      ),
      `Reported in` = c("Results", "Results; Table 1", ""),
      Status = c("numbers found", "numbers found", "to fill"),
      check.names = FALSE
    )
  )
  # It writes checklist_mini.md and no other file.
  after <- list.files(home, recursive = TRUE, all.files = TRUE)
  expect_identical(
    setdiff(after, before),
    "manuscript/built/v01/checklist_mini.md"
  )
  expect_identical(
    readLines(file.path(home, "manuscript/built/v01/checklist_mini.md")),
    c(
      paste(
        "`numbers found` means that each formatted string of the blocks of",
        "the item occurs in the manuscript text. A reader confirms each item."
      ),
      "",
      paste0(
        "Checklist: guidelines/mini.yml. Manuscript: ",
        "manuscript/built/v01/paper.md. Blocks: v01_functions/blocks.R and ",
        "v01_results/real/blocks.qs2."
      ),
      "",
      "| Section | Item | Recommendation | Reported in | Status |",
      "|---|---|---|---|---|",
      "| Results | 1 | Give the adjusted estimate \\| with its 95% CI | Results | numbers found |",
      "| Results | 2 | Report the numbers of participants | Results; Table 1 | numbers found |",
      "| Discussion | 3 | Discuss limitations |  | to fill |"
    )
  )
})

test_that("a manuscript without one number gives numbers missing with that string for its item only", {
  local_clean_state()
  home <- checklist_project(
    md = sub(
      "1,037 (68.7%)",
      "most",
      paper_md,
      fixed = TRUE
    )
  )
  out <- make_checklists("v01", home = home)
  expect_identical(
    statuses(out),
    c(
      "1" = "numbers found",
      "2" = "numbers missing: 1,037 (68.7%)",
      "3" = "to fill"
    )
  )
  # Reported in still names the headings of the strings that it found.
  expect_identical(out$`Reported in`, c("Results", "Table 1", ""))
})

test_that("each cell of each row of a formatter that returns a data.frame is a string to find, also a label", {
  local_clean_state()
  home <- checklist_project(
    md = sub(
      "| Body mass index |",
      "| BMI |",
      paper_md,
      fixed = TRUE
    )
  )
  expect_identical(
    statuses(make_checklists("v01", home = home)),
    c(
      "1" = "numbers found",
      "2" = "numbers missing: Body mass index",
      "3" = "to fill"
    )
  )
  # The last cell of the last row counts too.
  writeLines(
    sub("24.1 (3.2)", "24.1", paper_md, fixed = TRUE),
    file.path(home, "manuscript/built/v01/paper.md")
  )
  expect_identical(
    statuses(make_checklists("v01", home = home))[["2"]],
    "numbers missing: 24.1 (3.2)"
  )
})

test_that("the manuscript p \\< 0.001 matches the string p < 0.001 of the formatter", {
  local_clean_state()
  home <- checklist_project(
    md = c(
      "## Results",
      "",
      "The effect was (0.80, 95% CI 0.46 to 1.16, p \\< 0.001).",
      "1,037 (68.7%), Maternal age 30.7 (4.8), Body mass index 24.1 (3.2)."
    )
  )
  expect_identical(
    statuses(make_checklists("v01", home = home))[["1"]],
    "numbers found"
  )
})

test_that("a second run with one number absent still gives numbers missing: the checklist files are never part of the text", {
  local_clean_state()
  home <- checklist_project(
    md = sub(
      "1,037 (68.7%)",
      "most",
      paper_md,
      fixed = TRUE
    )
  )
  first <- make_checklists("v01", home = home)
  # The first run wrote the missing string into checklist_mini.md.
  expect_true(any(grepl(
    "1,037 (68.7%)",
    readLines(file.path(home, "manuscript/built/v01/checklist_mini.md")),
    fixed = TRUE
  )))
  second <- make_checklists("v01", home = home)
  expect_identical(statuses(second)[["2"]], "numbers missing: 1,037 (68.7%)")
  expect_identical(second, first)
})

test_that("a result item that no block names gives no block", {
  local_clean_state()
  home <- checklist_project()
  writeLines(
    sub("c(\"mini:2\")", "c()", mini_blocks, fixed = TRUE),
    file.path(home, "v01_functions/blocks.R")
  )
  out <- make_checklists("v01", home = home)
  expect_identical(
    statuses(out),
    c("1" = "numbers found", "2" = "no block", "3" = "to fill")
  )
  expect_identical(out$`Reported in`, c("Results", "", ""))
})

test_that("a change of a formatter changes the strings with no new blocks.qs2", {
  local_clean_state()
  home <- checklist_project()
  qs2 <- file.path(home, "v01_results", "real", "blocks.qs2")
  md5 <- unname(tools::md5sum(qs2))
  writeLines(
    sub("%.1f (%.1f)", "%.2f (%.2f)", mini_formatters, fixed = TRUE),
    file.path(home, "v01_functions/formatters.R")
  )
  expect_identical(
    statuses(make_checklists("v01", home = home))[["2"]],
    "numbers missing: 30.66 (4.81)"
  )
  expect_identical(unname(tools::md5sum(qs2)), md5)
})

test_that("a .docx manuscript gives numbers found when a number is split across two runs, and numbers missing without it", {
  skip_if(!nzchar(Sys.which("zip")))
  local_clean_state()
  home <- checklist_project()
  docx <- write_docx(file.path(tempdir(), "pbn_paper.docx"), docx_body())
  withr::defer(unlink(docx))
  out <- make_checklists("v01", manuscript = docx, home = home)
  expect_identical(
    statuses(out),
    c("1" = "numbers found", "2" = "numbers found", "3" = "to fill")
  )
  expect_identical(out$`Reported in`, c("Results", "Results; Table 1", ""))
  write_docx(docx, docx_body(sample = w_p("Of the people, most took part.")))
  expect_identical(
    statuses(make_checklists("v01", manuscript = docx, home = home))[["2"]],
    "numbers missing: 1,037 (68.7%)"
  )
})

test_that("a pretty-printed .docx, with each paragraph, run and text on its own line, gives numbers found with the heading", {
  skip_if(!nzchar(Sys.which("zip")))
  local_clean_state()
  home <- checklist_project()
  # Each <w:p>, <w:pPr>, <w:r> and <w:t> on its own line. The count 1,037
  # is split across two runs on different lines.
  pretty_p <- function(runs, style = NULL) {
    return(paste(
      c(
        "<w:p>",
        if (!is.null(style)) {
          c(
            "  <w:pPr>",
            paste0("    <w:pStyle w:val=\"", style, "\"/>"),
            "  </w:pPr>"
          )
        },
        unlist(lapply(runs, function(r) {
          return(c(
            "  <w:r>",
            paste0("    <w:t xml:space=\"preserve\">", r, "</w:t>"),
            "  </w:r>"
          ))
        })),
        "</w:p>"
      ),
      collapse = "\n"
    ))
  }
  body <- paste(
    c(
      "",
      pretty_p("Results", "Heading1"),
      pretty_p(c(
        "The effect was (0.80, 95% CI 0.46 to 1.16, p &lt; 0.001) in all. ",
        "Of the people, 1,0",
        "37 (68.7%) took part. Maternal age 30.7 (4.8), ",
        "Body mass index 24.1 (3.2)."
      )),
      ""
    ),
    collapse = "\n"
  )
  docx <- write_docx(file.path(tempdir(), "pbn_pretty.docx"), body)
  withr::defer(unlink(docx))
  out <- make_checklists("v01", manuscript = docx, home = home)
  expect_identical(
    statuses(out),
    c("1" = "numbers found", "2" = "numbers found", "3" = "to fill")
  )
  expect_identical(out$`Reported in`, c("Results", "Results", ""))
})

test_that("make_checklists() checks its arguments, and NULL is the default only", {
  local_clean_state()
  home <- checklist_project()
  expect_error(
    make_checklists(home = home),
    "Give pbn::make_checklists() the version",
    fixed = TRUE
  )
  expect_error(
    make_checklists("1", home = home),
    "`version` must be \"v\" and two digits",
    fixed = TRUE
  )
  expect_error(
    make_checklists(NA_character_, home = home),
    "`version` must be \"v\" and two digits",
    fixed = TRUE
  )
  for (bad in list(NA_character_, "", c(home, home), 1)) {
    expect_error(
      make_checklists("v01", home = bad),
      "`home` must be the path of one existing folder.",
      fixed = TRUE
    )
  }
  for (bad in list(NA_character_, "", c("a.md", "b.md"), 1, list("a.md"))) {
    expect_error(
      make_checklists("v01", manuscript = bad, home = home),
      "`manuscript` must be NULL or one path of a .md or a .docx file.",
      fixed = TRUE
    )
  }
  checklist <- file.path(home, "manuscript/built/v01/checklist_mini.md")
  writeLines("x", checklist)
  expect_error(
    make_checklists("v01", manuscript = checklist, home = home),
    "a file checklist_*.md that pbn::make_checklists() writes",
    fixed = TRUE
  )
  expect_error(
    make_checklists(
      "v01",
      manuscript = file.path(home, "paper.txt"),
      home = home
    ),
    "`manuscript` must be a .md or a .docx file.",
    fixed = TRUE
  )
  expect_error(
    make_checklists(
      "v01",
      manuscript = file.path(home, "none.md"),
      home = home
    ),
    "There is no file ",
    fixed = TRUE
  )
  # Without a built .md other than checklist_*.md, the default stops.
  unlink(file.path(home, "manuscript/built/v01/paper.md"))
  expect_error(
    make_checklists("v01", home = home),
    "other than checklist_*.md",
    fixed = TRUE
  )
})

test_that("make_checklists() stops on a missing blocks.qs2, a missing block, and a block whose items it cannot read", {
  local_clean_state()
  home <- checklist_project()
  qs2 <- file.path(home, "v01_results", "real", "blocks.qs2")
  saved <- qs2::qs_read(qs2)
  qs2::qs_save(saved[c("Effect", "Sample")], qs2)
  expect_error(
    make_checklists("v01", home = home),
    "holds no block \"Table 1\". Write it again with pbn::update_blocks().",
    fixed = TRUE
  )
  unlink(qs2)
  expect_error(
    make_checklists("v01", home = home),
    "blocks.qs2. Write it with pbn::update_blocks() or with a run.",
    fixed = TRUE
  )
  qs2::qs_save(saved, qs2)
  writeLines(
    sub("c(\"mini:2\")", "tags", mini_blocks, fixed = TRUE),
    file.path(home, "v01_functions/blocks.R")
  )
  expect_error(
    make_checklists("v01", home = home),
    "The block \"Sample\" of ",
    fixed = TRUE
  )
})

test_that("the NAMESPACE file exports make_checklists", {
  ns <- readLines(system.file("NAMESPACE", package = "pbn"))
  expect_true("export(make_checklists)" %in% ns)
  expect_true("make_checklists" %in% pbn_exports)
  expect_identical(new_exports[["make_checklists"]], "house_2026_11_26")
})
