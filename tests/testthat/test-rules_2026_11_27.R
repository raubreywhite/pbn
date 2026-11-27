# The set house_2026_11_27: the rule display_numbering. The numbered figures
# and tables of the outputs, of the manuscript and of its citations agree.
# The projects of house_2026_11_26 and older sets see none of it.

# A synthetic project of local_project() on the set house_2026_11_27,
# without the manual manuscript, so that it has no manuscript.
local_project_27 <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_27")
  unlink(file.path(p$home, "manuscript", "manual", "v01_AB.docx"))
  return(p)
}

# The CRC-32 of the raw vector `bytes`, as a double. R has no unsigned
# 32-bit integer, so each step works on the two 16-bit halves.
crc32 <- function(bytes) {
  xor32 <- function(a, b) {
    return(
      bitwXor(a %/% 65536, b %/% 65536) *
        65536 +
        bitwXor(a %% 65536, b %% 65536)
    )
  }
  table <- vapply(
    0:255,
    function(n) {
      c <- n
      for (k in 1:8) {
        c <- if (c %% 2 == 1) xor32(3988292384, c %/% 2) else c %/% 2
      }
      return(c)
    },
    numeric(1)
  )
  c <- 4294967295
  for (b in as.integer(bytes)) {
    c <- xor32(table[bitwXor(c %% 256, b) + 1], c %/% 256)
  }
  return(xor32(c, 4294967295))
}

# A zip file `file` that stores the one member `name` with the text `text`,
# without compression. The tests need no zip program and no package.
write_zip <- function(file, name, text) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  data <- charToRaw(enc2utf8(paste0(paste(text, collapse = "\n"), "\n")))
  name_raw <- charToRaw(name)
  u32 <- function(x) {
    return(writeBin(
      as.integer(if (x >= 2^31) x - 2^32 else x),
      raw(),
      4,
      endian = "little"
    ))
  }
  u16 <- function(x) {
    return(writeBin(as.integer(x), raw(), 2, endian = "little"))
  }
  crc <- crc32(data)
  # version, flags, method, time, date (1980-01-01), crc, sizes, name length
  common <- c(
    u16(20),
    u16(0),
    u16(0),
    u16(0),
    u16(33),
    u32(crc),
    u32(length(data)),
    u32(length(data)),
    u16(length(name_raw))
  )
  local <- c(u32(67324752), common, u16(0), name_raw)
  central <- c(
    u32(33639248),
    u16(20),
    common,
    u16(0),
    u16(0),
    u16(0),
    u16(0),
    u32(0),
    u32(0),
    name_raw
  )
  end <- c(
    u32(101010256),
    u16(0),
    u16(0),
    u16(1),
    u16(1),
    u32(length(central)),
    u32(length(local) + length(data)),
    u16(0)
  )
  writeBin(c(local, data, central, end), file)
  return(invisible(file))
}

# An .xlsx file `rel` of `home` with only xl/workbook.xml, whose sheets have
# the names `sheets`. The rule reads the sheet names only.
write_xlsx_sheets <- function(home, rel, sheets) {
  xml <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    paste0(
      "<workbook xmlns=\"http://schemas.openxmlformats.org/",
      "spreadsheetml/2006/main\"><sheets>",
      paste0(
        "<sheet name=\"",
        sheets,
        "\" sheetId=\"",
        seq_along(sheets),
        "\"/>",
        collapse = ""
      ),
      "</sheets></workbook>"
    )
  )
  return(write_zip(file.path(home, rel), "xl/workbook.xml", xml))
}

# A .docx file `rel` of `home` with only word/document.xml, whose body is
# the XML `body`.
write_docx_body <- function(home, rel, body) {
  xml <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
    paste0(
      "<w:document xmlns:w=\"http://schemas.openxmlformats.org/",
      "wordprocessingml/2006/main\"><w:body>",
      body,
      "</w:body></w:document>"
    )
  )
  return(write_zip(file.path(home, rel), "word/document.xml", xml))
}

# One paragraph of WordprocessingML with the text `x` in one run.
w_par <- function(x) {
  return(paste0(
    "<w:p><w:r><w:t xml:space=\"preserve\">",
    x,
    "</w:t></w:r></w:p>"
  ))
}

real_out <- "v01_results/real/output"

# The real output of the items `figures` and `tables`, as keys such as
# fig_1 and tab_s1: one .png and one sheet of data_behind_figures.xlsx for
# each figure, and one sheet of tables.xlsx for each table.
write_real_items <- function(
  home,
  figures = character(0),
  tables = character(0)
) {
  for (f in figures) {
    write_lines(home, file.path(real_out, paste0(f, "_x.png")), "")
  }
  if (length(figures) > 0) {
    write_xlsx_sheets(
      home,
      file.path(real_out, "data_behind_figures.xlsx"),
      paste0(figures, "_x")
    )
  }
  if (length(tables) > 0) {
    write_xlsx_sheets(home, file.path(real_out, "tables.xlsx"), tables)
  }
  return(invisible(home))
}

# The built manuscript v01 of `home` with the lines `lines`.
write_paper <- function(home, lines) {
  return(write_lines(home, "manuscript/built/v01/paper.md", lines))
}

# The status, the path and the detail of each row of display_numbering.
display_rows <- function(home) {
  found <- project_findings(home)
  out <- found[found$rule == "display_numbering", c("status", "path", "detail")]
  rownames(out) <- NULL
  return(out)
}

display_fix <- paste0(
  ". Fix: rename the output file or the sheet by the scheme of ",
  "?check_project, or renumber the figures and the tables of the output ",
  "function and of the manuscript in the order of their first citation."
)

# The FAIL rows of display_numbering with the details `details`, each with
# the path before its first ": ".
fail_rows <- function(details) {
  return(data.frame(
    status = rep("FAIL", length(details)),
    path = sub(": .*$", "", details),
    detail = paste0(details, display_fix, recycle0 = TRUE)
  ))
}

# The PASS row of display_numbering of a project without a manuscript.
pass_names <- data.frame(
  status = "PASS",
  path = "",
  detail = paste(
    "every output name and sheet name has a number or nonum_, and there is",
    "no manuscript to match"
  )
)

# The PASS row of display_numbering of a project whose manuscript matches
# the output folder `folder`.
pass_match <- function(folder = paste0(real_out, "/")) {
  return(data.frame(
    status = "PASS",
    path = "",
    detail = paste0(
      "every output name and sheet name has a number or nonum_, the ",
      "manuscript cites each series in the order 1, 2, 3, and its items are ",
      "the numbered items of ",
      folder
    )
  ))
}

test_that("house_2026_11_27 is house_2026_11_26 with display_numbering at fail, and a new pbn.yml names it", {
  old <- read_house_set("house_2026_11_26")
  new <- read_house_set("house_2026_11_27")
  expect_identical(new[names(old)], old)
  expect_identical(setdiff(names(new), names(old)), "display_numbering")
  expect_identical(
    names(new),
    intersect(names(project_rules()), c(names(old), "display_numbering"))
  )
  expect_identical(new$display_numbering$level, "fail")
  expect_identical(new$display_numbering$locked, FALSE)
  expect_identical(starter_line(), "extends: house_2026_11_27")
  # create_project() names house_2026_11_27: its template passes the set.
  expect_identical(create_project_set, "house_2026_11_27")
})

test_that("a project on house_2026_11_26 gives no display_numbering line, and a project on house_2026_11_27 without outputs and a manuscript passes it", {
  p <- local_project_27()
  write_lines(p$home, file.path(real_out, "figure3.png"), "")
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  expect_false("display_numbering" %in% project_findings(p$home)$rule)

  q <- local_project_27()
  expect_identical(display_rows(q$home), pass_names)
})

test_that("display_numbering passes numbered and nonum_ output names, and fails other names and a tab_N_ file", {
  p <- local_project_27()
  fake <- "v01_results/known/output_fake_data"
  for (f in c(
    "fig_3_gap.png",
    "nonum_sana.xlsx",
    "fig_s1_flow_external.txt",
    "tab_s1_x_external.txt"
  )) {
    write_lines(p$home, file.path(real_out, f), "")
  }
  write_xlsx_sheets(
    p$home,
    file.path(real_out, "data_behind_figures.xlsx"),
    "fig_3_gap"
  )
  write_lines(p$home, file.path(fake, "fake_data_fig_1_x.png"), "")
  write_xlsx_sheets(
    p$home,
    file.path(fake, "fake_data_data_behind_figures.xlsx"),
    "fig_1_x"
  )
  expect_identical(display_rows(p$home), pass_names)

  for (f in c("figure3.png", "sana.xlsx", "tab_1_x.csv")) {
    write_lines(p$home, file.path(real_out, f), "")
  }
  expect_identical(
    display_rows(p$home),
    fail_rows(c(
      paste0(
        real_out,
        "/tab_1_x.csv: a numbered table MUST be a sheet tab_N or tab_sN of ",
        "tables.xlsx, or an item tab_N_<name>_external.txt"
      ),
      paste0(
        real_out,
        c("/figure3.png", "/sana.xlsx"),
        ": an output name MUST start with fig_N_, fig_sN_ or nonum_, end ",
        "with _external.txt after tab_N_ or tab_sN_, or be tables.xlsx or ",
        "data_behind_figures.xlsx"
      )
    ))
  )
})

test_that("display_numbering passes the sheets tab_N and tab_sN of tables.xlsx, and fails the sheet Table 1", {
  p <- local_project_27()
  write_xlsx_sheets(
    p$home,
    file.path(real_out, "tables.xlsx"),
    c("tab_1", "tab_s1", "nonum_notes")
  )
  expect_identical(display_rows(p$home), pass_names)

  write_xlsx_sheets(
    p$home,
    file.path(real_out, "tables.xlsx"),
    c("tab_1", "Table 1")
  )
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      real_out,
      "/tables.xlsx: the sheet \"Table 1\" MUST be named tab_N or tab_sN or ",
      "nonum_<name>"
    ))
  )
})

test_that("display_numbering needs a sheet in data_behind_figures.xlsx for each figure file, and a figure file for each sheet, and an external figure needs none", {
  p <- local_project_27()
  write_lines(p$home, file.path(real_out, "fig_1_x.png"), "")
  write_lines(p$home, file.path(real_out, "fig_s1_flow_external.txt"), "")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      real_out,
      "/fig_1_x.png: ",
      real_out,
      "/ has no data_behind_figures.xlsx with a sheet fig_1_<name> with the ",
      "data of the figure"
    ))
  )

  book <- file.path(real_out, "data_behind_figures.xlsx")
  write_xlsx_sheets(p$home, book, "fig_2_x")
  expect_identical(
    display_rows(p$home),
    fail_rows(c(
      paste0(
        real_out,
        "/fig_1_x.png: data_behind_figures.xlsx of ",
        real_out,
        "/ has no sheet fig_1_<name> with the data of the figure"
      ),
      paste0(
        book,
        ": the sheet fig_2_x has no figure file fig_2_<name> in ",
        real_out,
        "/"
      )
    ))
  )

  write_xlsx_sheets(p$home, book, c("fig_1_cutoffs", "fig_1_centiles"))
  expect_identical(display_rows(p$home), pass_names)

  write_xlsx_sheets(p$home, book, c("fig_1_cutoffs", "fig_4_x", "Figure 1"))
  expect_identical(
    display_rows(p$home),
    fail_rows(c(
      paste0(
        book,
        ": the sheet \"Figure 1\" MUST be named fig_N_<name> or nonum_<name>"
      ),
      paste0(
        book,
        ": the sheet fig_4_x has no figure file fig_4_<name> in ",
        real_out,
        "/"
      )
    ))
  )
})

test_that("display_numbering passes first citations in the order 1, 2, 3 of each series, with lists and ranges", {
  p <- local_project_27()
  write_real_items(
    p$home,
    figures = c("fig_1", "fig_2", "fig_s1"),
    tables = c("tab_1", "tab_2", "tab_3")
  )
  write_paper(
    p$home,
    c(
      "Figure S1 shows the flow. Figure 1 and then",
      "Figure 2 show the gaps.",
      "",
      "Tables 1–3 hold the numbers."
    )
  )
  expect_identical(display_rows(p$home), pass_match())
  write_paper(p$home, c("Figures 1 and 2, Figure S1, Tables 1-3."))
  expect_identical(display_rows(p$home), pass_match())
  expect_identical(
    display_cited_keys("Tables 1–3, Figures S1, S2 and S4, Fig. 5"),
    c("tab_1", "tab_2", "tab_3", "fig_s1", "fig_s2", "fig_s4", "fig_5")
  )
})

test_that("display_numbering fails first citations out of order and a gap", {
  p <- local_project_27()
  write_real_items(p$home, figures = c("fig_1", "fig_2"))
  paper <- "manuscript/built/v01/paper.md"
  write_paper(p$home, "Figure 2 comes before Figure 1.")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      paper,
      ": the first citations of the figures come in the order Figure 2, ",
      "Figure 1. They MUST come in the order 1, 2, 3, ... with no gap, so ",
      "Figure 2 MUST be Figure 1"
    ))
  )

  unlink(file.path(p$home, real_out), recursive = TRUE)
  write_real_items(p$home, figures = c("fig_1", "fig_3"))
  write_paper(p$home, "Figure 1, and then Figure 3.")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      paper,
      ": the first citations of the figures come in the order Figure 1, ",
      "Figure 3. They MUST come in the order 1, 2, 3, ... with no gap, so ",
      "Figure 3 MUST be Figure 2"
    ))
  )
})

test_that("a citation in a tracked deletion does not count, a citation in a tracked insertion counts, and a caption needs a citation outside it", {
  p <- local_project_27()
  write_real_items(p$home, figures = c("fig_1", "fig_2"))
  main <- "manuscript/manual/v02_AB.docx"
  deleted <- paste0(
    "<w:p><w:r><w:t xml:space=\"preserve\">See </w:t></w:r>",
    "<w:del w:id=\"1\" w:author=\"A\"><w:r>",
    "<w:delText>Figure 2 and </w:delText></w:r></w:del>",
    "<w:ins w:id=\"2\" w:author=\"A\"><w:r>",
    "<w:t xml:space=\"preserve\">Figure 1, then Figure 2</w:t></w:r></w:ins>",
    "</w:p>"
  )
  write_docx_body(p$home, main, deleted)
  expect_identical(display_rows(p$home), pass_match())
  # The text of a deletion and of moved text in <w:t> does not count either.
  # A self-closing <w:del/> of a deleted paragraph mark holds no text, so the
  # text after it counts.
  in_t <- paste0(
    "<w:p><w:pPr><w:rPr><w:del w:id=\"3\" w:author=\"A\"/></w:rPr></w:pPr>",
    "<w:r><w:t xml:space=\"preserve\">See </w:t></w:r>",
    "<w:del w:id=\"1\" w:author=\"A\"><w:r>",
    "<w:t xml:space=\"preserve\">Figure 2 and </w:t></w:r></w:del>",
    "<w:moveFrom w:id=\"4\" w:author=\"A\"><w:r>",
    "<w:t xml:space=\"preserve\">Figure 2, </w:t></w:r></w:moveFrom>",
    "<w:r><w:t xml:space=\"preserve\">Figure 1, then Figure 2</w:t></w:r>",
    "</w:p>"
  )
  write_docx_body(p$home, main, in_t)
  expect_identical(
    docx_lines(file.path(p$home, main))$text,
    "See Figure 1, then Figure 2"
  )
  expect_identical(display_rows(p$home), pass_match())
  # Without the deletion, Figure 2 comes first.
  write_docx_body(
    p$home,
    main,
    w_par("See Figure 2 and Figure 1, then Figure 2")
  )
  expect_identical(
    display_rows(p$home)$detail,
    paste0(
      main,
      ": the first citations of the figures come in the order Figure 2, ",
      "Figure 1. They MUST come in the order 1, 2, 3, ... with no gap, so ",
      "Figure 2 MUST be Figure 1",
      display_fix
    )
  )

  write_docx_body(
    p$home,
    main,
    paste0(
      w_par("Figure 1 shows the gap."),
      w_par("Figure 1. The gap."),
      w_par("Figure 2. The centiles, as in Figure 2 and Figure 1.")
    )
  )
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      main,
      ": Figure 2 has a caption, but the text cites it nowhere outside its ",
      "caption"
    ))
  )
})

test_that("display_numbering matches the items of the manuscript with the numbered items of the real output", {
  p <- local_project_27()
  paper <- "manuscript/built/v01/paper.md"
  write_real_items(p$home, figures = c("fig_1", "fig_2", "fig_3"))
  write_paper(p$home, "Figure 1 and Figure 2.")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      real_out,
      "/fig_3_x.png: Figure 3 of the output is not in the manuscript ",
      paper
    ))
  )

  unlink(file.path(p$home, real_out), recursive = TRUE)
  write_lines(p$home, file.path(real_out, "fig_s1_flow_external.txt"), "")
  write_paper(p$home, "Figure S1 shows the flow.")
  expect_identical(display_rows(p$home), pass_match())

  write_paper(p$home, "Figure S1 shows the flow. Table 1 holds the numbers.")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      paper,
      ": Table 1 of the manuscript has no sheet tab_1 of tables.xlsx or ",
      "file tab_1_<name>_external.txt in ",
      real_out,
      "/"
    ))
  )
})

test_that("a project without a manuscript checks the names only", {
  p <- local_project_27()
  write_real_items(p$home, figures = c("fig_3"))
  expect_identical(display_rows(p$home), pass_names)
  paper <- "manuscript/built/v01/paper.md"
  write_paper(p$home, "Figure 1 shows the gap.")
  expect_identical(
    display_rows(p$home),
    fail_rows(c(
      paste0(
        real_out,
        "/fig_3_x.png: Figure 3 of the output is not in the manuscript ",
        paper
      ),
      paste0(
        paper,
        ": Figure 1 of the manuscript has no figure file fig_1_<name> in ",
        real_out,
        "/"
      )
    ))
  )
})

test_that("Supplementary Figure N and Supplementary Table N count as Figure SN and Table SN", {
  expect_identical(
    display_cited_keys(
      "Supplementary Figures 1 and 2, supplementary Table 3, Figure S4"
    ),
    c("fig_s1", "fig_s2", "tab_s3", "fig_s4")
  )
  p <- local_project_27()
  write_real_items(p$home, figures = c("fig_1", "fig_s1", "fig_s2"))
  write_lines(p$home, file.path(real_out, "tab_s1_x_external.txt"), "")
  write_paper(
    p$home,
    c(
      "Supplementary Figure 1 shows the flow, and Figure 1 the gaps.",
      "Figure S2 and Supplementary Table 1 hold the rest.",
      "",
      "Supplementary Figure 2. The centiles."
    )
  )
  expect_identical(display_rows(p$home), pass_match())

  # "Supplementary Figure 2" is Figure S2, so it comes before Figure S1.
  write_paper(p$home, "Supplementary Figure 2, then Figure S1 and Figure 1.")
  expect_identical(
    display_rows(p$home),
    fail_rows(c(
      paste0(
        "manuscript/built/v01/paper.md: the first citations of the ",
        "supplementary figures come in the order Figure S2, Figure S1. They ",
        "MUST come in the order 1, 2, 3, ... with no gap, so Figure S2 MUST ",
        "be Figure S1"
      ),
      paste0(
        real_out,
        "/tab_s1_x_external.txt: Table S1 of the output is not in the ",
        "manuscript manuscript/built/v01/paper.md"
      )
    ))
  )
})

test_that("a list needs the plural word, and a number of a list with % or a decimal part is not a citation", {
  expect_identical(
    display_cited_keys("Figures 1 and 2, 95% of them. Table 1, 25 persons."),
    c("fig_1", "fig_2", "tab_1")
  )
  expect_identical(
    display_cited_keys("Figure 1-3, Figs. 4 & 5, Tables 1, 2.5 and 3"),
    c("fig_1", "fig_2", "fig_3", "fig_4", "fig_5", "tab_1")
  )
})

test_that("a list of citations follows the Oxford comma: two items with and, or three or more that end in , and", {
  expect_identical(
    display_cited_keys("Figures 1 and 2, 25 and 30 participants were enrolled"),
    c("fig_1", "fig_2")
  )
  expect_identical(
    display_cited_keys("Figures 1, 2, and 3. Figures S1, S2, and S4."),
    c("fig_1", "fig_2", "fig_3", "fig_s1", "fig_s2", "fig_s4")
  )
  expect_identical(
    display_cited_keys("Figures 1 and 2 show it. Tables 1-2, 4, & 6\u20137."),
    c("fig_1", "fig_2", "tab_1", "tab_2", "tab_4", "tab_6", "tab_7")
  )
  # Without the Oxford comma the items still count.
  expect_identical(
    display_cited_keys("Figures 1, 2 and 3, and Tables 1, 2, 3 & 4"),
    c("fig_1", "fig_2", "fig_3", "tab_1", "tab_2", "tab_3", "tab_4")
  )
})

test_that("a list without the Oxford comma gives one finding, and its items count as citations", {
  p <- local_project_27()
  paper <- "manuscript/built/v01/paper.md"
  write_real_items(p$home, figures = c("fig_1", "fig_2", "fig_3"))
  write_paper(p$home, "Figures 1, 2 and 3 show the gaps.")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      paper,
      ": the list \"Figures 1, 2 and 3\" MUST use the Oxford comma: ",
      "\"Figures 1, 2, and 3\""
    ))
  )
  write_paper(p$home, "Figures 1, 2, and 3 show the gaps.")
  expect_identical(display_rows(p$home), pass_match())
})

test_that("a comma after one item that starts no Oxford list ends the citation, and is no finding", {
  expect_identical(
    display_cited_keys("Figures 1, 25 participants were seen."),
    "fig_1"
  )
  p <- local_project_27()
  write_real_items(p$home, figures = "fig_1")
  write_paper(p$home, "Figures 1, 25 participants were seen.")
  expect_identical(display_rows(p$home), pass_match())
})

test_that("a caption in Markdown emphasis or in the text of an image is a caption", {
  for (x in c(
    "**Figure 1.** The gap.",
    "*Figure 1.* The gap.",
    "__Figure 1__. The gap.",
    "![Figure 1. The gap](fig_1_gap.png)"
  )) {
    expect_identical(display_caption_key(x), "fig_1", info = x)
  }
  expect_identical(
    display_caption_key("Figure 1 shows the gap."),
    NA_character_
  )

  p <- local_project_27()
  write_real_items(p$home, figures = c("fig_1", "fig_2"))
  paper <- "manuscript/built/v01/paper.md"
  for (caption in c(
    "**Figure 2.** The centiles.",
    "![Figure 2. The centiles](fig_2_x.png)"
  )) {
    write_paper(p$home, c("Figure 1 shows the gap.", "", caption))
    expect_identical(
      display_rows(p$home),
      fail_rows(paste0(
        paper,
        ": Figure 2 has a caption, but the text cites it nowhere outside its ",
        "caption"
      )),
      info = caption
    )
    write_paper(p$home, c("Figures 1 and 2 show the gap.", "", caption))
    expect_identical(display_rows(p$home), pass_match(), info = caption)
  }
})

test_that("without vNN_results/real/output/ the manuscript matches the fake output, and the PASS line names its folder", {
  p <- local_project_27()
  fake <- "v01_results/known/output_fake_data"
  write_lines(p$home, file.path(fake, "fake_data_fig_1_x.png"), "")
  write_xlsx_sheets(
    p$home,
    file.path(fake, "fake_data_data_behind_figures.xlsx"),
    "fig_1_x"
  )
  write_xlsx_sheets(p$home, file.path(fake, "fake_data_tables.xlsx"), "tab_1")
  write_paper(p$home, "Figure 1 and Table 1.")
  expect_identical(display_rows(p$home), pass_match(paste0(fake, "/")))

  write_paper(p$home, "Figure 1, Figure 2 and Table 1.")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      "manuscript/built/v01/paper.md: Figure 2 of the manuscript has no ",
      "figure file fig_2_<name> in ",
      fake,
      "/"
    ))
  )

  # A real output folder wins over the fake output.
  write_paper(p$home, "Figure 1 and Table 1.")
  write_real_items(p$home, figures = "fig_1")
  expect_identical(
    display_rows(p$home),
    fail_rows(paste0(
      "manuscript/built/v01/paper.md: Table 1 of the manuscript has no ",
      "sheet tab_1 of tables.xlsx or file tab_1_<name>_external.txt in ",
      real_out,
      "/"
    ))
  )
})

test_that("in a one-file project the manuscript matches fake/output_fake_data/", {
  p <- local_project_27()
  fake <- "v01_results/fake/output_fake_data"
  write_lines(p$home, file.path(fake, "fake_data_fig_s1_flow_external.txt"), "")
  write_paper(p$home, "Figure S1 shows the flow.")
  expect_identical(display_rows(p$home), pass_match(paste0(fake, "/")))
})
