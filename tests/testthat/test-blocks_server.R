# export_blocks() on the server, import_blocks() and read_blocks() on this
# computer, the FAKE DATA line of a knitr document, and the rule
# built_reads_source of house_2026_11_26.

# The blocks of the round trip: each kind of column, numbers with 17
# significant digits, all-NA columns, a zero-row data.frame, a column order
# that is not alphabetical, a character vector, a data.table, a block of
# more than one page and a block of the real results.
server_blocks_lines <- c(
  "blocks <- list(",
  "  \"Numbers\" = function(results) data.frame(",
  "    z = c(0.1234567890123, 1e-17, 1 / 3, -2.5),",
  "    a = c(1L, NA, 3L, 4L),",
  "    chr = c(\"a\", NA, \"\\u2265 18\", \"d\"),",
  "    lgl = c(TRUE, FALSE, NA, TRUE)",
  "  ),",
  "  \"All NA\" = function(results) data.frame(",
  "    num = c(NA_real_, NA_real_), chr = c(NA_character_, NA_character_),",
  "    lgl = c(NA, NA)",
  "  ),",
  "  \"No rows\" = function(results) data.frame(y = numeric(0), x = character(0)),",
  "  \"Text\" = function(results) c(\"Line one\", NA, \"One third\"),",
  "  \"Table\" = function(results) data.table::data.table(g = c(\"b\", \"a\"), n = c(2L, 1L)),",
  "  \"Long\" = function(results) data.frame(i = 1:150, x = (1:150) / 7),",
  "  \"Models\" = function(results) results$models",
  ")"
)

# A project of local_project() with the blocks of server_blocks_lines, run
# and saved in this R session. The session stays initialised for the test.
# `one_file = TRUE` makes it a one-file project, whose real results are in
# fake/ with fake_real_data TRUE.
server_project <- function(one_file = FALSE, env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "v01_functions/blocks.R", server_blocks_lines)
  if (one_file) {
    make_one_file_script(p)
    res <- save_run(
      p,
      env = env,
      one_file_home = "N:/x",
      one_file_folders = c(main = "N:/d")
    )
  } else {
    res <- save_run(p, env = env)
  }
  if (inherits(res$err, "error")) {
    stop(conditionMessage(res$err))
  }
  return(p)
}

# The path `...` under v01_results/ of the project `p`.
results_path <- function(p, ...) {
  return(file.path(p$home, "v01_results", ...))
}

# The values `x` of a blocks.qs2, each data.table as a data.frame.
as_frames <- function(x) {
  return(lapply(x, function(v) {
    if (data.table::is.data.table(v)) {
      return(as.data.frame(v))
    }
    return(v)
  }))
}

# Writes the lines `lines` as v01_results/real/export_blocks.txt of `p`.
write_export <- function(p, lines) {
  dir.create(results_path(p, "real"), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, results_path(p, "real", "export_blocks.txt"))
  return(invisible(lines))
}

# The error message of import_blocks("v01") for the project `p`, or
# "no error".
import_error <- function(p) {
  err <- tryCatch(
    {
      utils::capture.output(import_blocks("v01", home = p$home))
      NULL
    },
    error = function(e) return(e)
  )
  if (is.null(err)) {
    return("no error")
  }
  return(conditionMessage(err))
}

# v01_results/real/blocks.qs2 of `p`, or NULL when it is not there.
real_blocks <- function(p) {
  file <- results_path(p, "real", "blocks.qs2")
  if (!file.exists(file)) {
    return(NULL)
  }
  return(qs2::qs_read(file))
}

# Saves `lines` as the export of `p`, deletes real/blocks.qs2, runs
# import_blocks(), and returns read_blocks() of the new real/blocks.qs2, or
# NULL when the import wrote none.
round_trip <- function(p, lines) {
  write_export(p, lines)
  unlink(results_path(p, "real", "blocks.qs2"))
  import_error(p)
  if (!file.exists(results_path(p, "real", "blocks.qs2"))) {
    return(NULL)
  }
  return(read_blocks("v01", home = p$home))
}

# The positions of the header lines of `lines`.
header_at <- function(lines) {
  return(grep("^pbn-blocks ", lines))
}

# The fields of the header lines of `lines`, as regexec() of
# page_header_pattern gives them.
header_fields <- function(lines) {
  h <- lines[header_at(lines)]
  return(regmatches(h, regexec(page_header_pattern, h)))
}

# The positions of the text lines of the page that starts at the header
# line `h` of `lines`: up to the empty line that ends the page.
page_text_at <- function(lines, h) {
  end <- h
  while (end < length(lines) && nzchar(lines[end + 1])) {
    end <- end + 1
  }
  return(seq_len(end - h) + h)
}

# Makes the project `p` of local_project() a one-file project: the call
# pbn::initialize_project() of its run script gives the paths of the server.
make_one_file_script <- function(p) {
  file <- file.path(p$home, "v01_run.R")
  lines <- readLines(file)
  i <- init_line(lines)
  lines[i] <- sub(
    "\\)\\)$",
    "), one_file_home = \"N:/x\", one_file_folders = c(main = \"N:/d\"))",
    lines[i]
  )
  writeLines(lines, file)
  return(invisible(p))
}

# The pages of export_blocks() for the one value `value` with the name
# "a", as the server prints them, with the token fake when `fake` is TRUE.
pages_of <- function(value, fake = FALSE) {
  ctx <- list(version = "v01", run_date = "2026-11-27", fake_real_data = fake)
  return(export_pages(export_text(list(a = value)), ctx, 1L, 1L))
}

test_that("read_blocks() after import_blocks() of the pages of export_blocks() is identical to blocks.qs2 of the run", {
  p <- server_project()
  run <- real_blocks(p)
  expect_true(data.table::is.data.table(run$Table))
  expect_identical(class(run$Text), "character")
  got <- round_trip(p, utils::capture.output(export_blocks()))
  expect_identical(as_frames(got), as_frames(run))
  expect_identical(got$Numbers$z[1:3], c(0.1234567890123, 1e-17, 1 / 3))
  expect_null(attr(got, "pbn_fake"))
})

test_that("export_blocks() prints every block, the character vector too", {
  p <- server_project()
  lines <- utils::capture.output(export_blocks())
  fields <- header_fields(lines)
  blocks <- unique(vapply(fields, function(f) return(f[4]), character(1)))
  expect_identical(blocks, as.character(1:7))
})

test_that("each page of export_blocks() has a header line and at most 34 text lines, and the md5 of the header is that of its text lines", {
  p <- server_project()
  lines <- utils::capture.output(out <- export_blocks())
  expect_identical(out, lines)
  expect_match(
    lines[1],
    "^pbn-blocks v01 run [0-9]{4}-[0-9]{2}-[0-9]{2} block 1/7 page 1/1 md5 [0-9a-f]{8}$"
  )
  heads <- header_at(lines)
  fields <- header_fields(lines)
  run_date <- qs2::qs_read(results_path(p, "real", "results.qs2"))$manifest$run_date
  for (j in seq_along(heads)) {
    text <- lines[page_text_at(lines, heads[j])]
    expect_true(length(text) >= 1 && length(text) <= 34, info = lines[heads[j]])
    tmp <- tempfile()
    writeBin(charToRaw(paste(text, collapse = "\n")), tmp)
    expect_identical(
      fields[[j]][8],
      substr(unname(tools::md5sum(tmp)), 1, 8),
      info = lines[heads[j]]
    )
    unlink(tmp)
    expect_identical(fields[[j]][2:3], c("v01", run_date))
  }
  blocks <- vapply(fields, function(f) return(f[4]), character(1))
  expect_true(all(vapply(fields, function(f) return(f[5]), character(1)) == "7"))
  # The block Long, block 6, has more than one page.
  long <- fields[blocks == "6"]
  expect_gt(length(long), 1)
  expect_identical(
    vapply(long, function(f) return(paste0(f[6], "/", f[7])), character(1)),
    paste0(seq_along(long), "/", length(long))
  )
})

test_that("each header line of the pages of real data has no token fake", {
  p <- server_project()
  lines <- utils::capture.output(export_blocks())
  heads <- lines[header_at(lines)]
  expect_gt(length(heads), 7)
  expect_identical(sum(endsWith(heads, " fake")), 0L)
})

test_that("each header line of the pages of fake data ends with the token fake", {
  p <- server_project(one_file = TRUE)
  lines <- utils::capture.output(export_blocks())
  heads <- lines[header_at(lines)]
  expect_gt(length(heads), 7)
  expect_true(all(grepl(
    "^pbn-blocks v01 run [0-9-]+ block [0-9]+/7 page [0-9]+/[0-9]+ md5 [0-9a-f]{8} fake$",
    heads
  )))
})

test_that("import_blocks() of fake pages keeps the values and gives the list the attribute pbn_fake TRUE", {
  p <- server_project(one_file = TRUE)
  run <- qs2::qs_read(results_path(p, "fake", "blocks.qs2"))
  write_export(p, utils::capture.output(export_blocks()))
  import_error(p)
  got <- real_blocks(p)
  expect_identical(attr(got, "pbn_fake"), TRUE)
  attr(got, "pbn_fake") <- NULL
  expect_identical(as_frames(got), as_frames(run))
})

test_that("a file whose header lines disagree on the token fake stops import_blocks()", {
  p <- server_project(one_file = TRUE)
  lines <- utils::capture.output(export_blocks())
  h <- header_at(lines)[1]
  lines[h] <- sub(" fake$", "", lines[h])
  write_export(p, lines)
  expect_identical(
    import_error(p),
    paste0(
      "The header lines disagree on the token fake: some end with it and ",
      "some do not. Keep the pages of one export only."
    )
  )
})

test_that("a changed digit on a page stops import_blocks(), and the error names the block and the page", {
  p <- server_project()
  lines <- utils::capture.output(export_blocks())
  heads <- header_at(lines)
  long <- heads[grepl(" block 6/7 page 2/", lines[heads])]
  i <- page_text_at(lines, long)[1]
  expect_match(lines[i], "[0-9]")
  changed <- lines
  changed[i] <- sub("([0-9])", "\\1\\1", lines[i])
  expect_false(identical(changed[i], lines[i]))
  write_export(p, changed)
  expect_match(
    import_error(p),
    "^The block 6, page 2 of [0-9]+: the md5 of its lines is [0-9a-f]{8}, and its header line says"
  )
})

test_that("a change to the text of any page stops import_blocks(), and names that block and page", {
  p <- server_project()
  lines <- utils::capture.output(export_blocks())
  for (h in header_at(lines)) {
    f <- regmatches(lines[h], regexec(page_header_pattern, lines[h]))[[1]]
    i <- utils::tail(page_text_at(lines, h), 1)
    changed <- lines
    changed[i] <- sub(".$", "#", lines[i])
    expect_false(identical(changed[i], lines[i]))
    write_export(p, changed)
    expect_match(
      import_error(p),
      paste0("^The block ", f[4], ", page ", f[6], " of ", f[7], ": the md5 "),
      info = lines[h]
    )
  }
})

test_that("a missing page or a missing block stops import_blocks(), and the error names the block and the page", {
  p <- server_project()
  lines <- utils::capture.output(export_blocks())
  heads <- header_at(lines)
  first <- heads[grepl(" block 6/7 page 1/", lines[heads])]
  write_export(p, lines[-c(first, page_text_at(lines, first))])
  expect_match(import_error(p), "^The block 6, page 1 of [0-9]+ is missing from the file\\.$")
  text <- heads[grepl(" block 4/7 ", lines[heads])]
  write_export(p, lines[-c(text, page_text_at(lines, text))])
  expect_identical(
    import_error(p),
    "The block 4, page 1 of its pages is missing from the file."
  )
})

test_that("a page twice with the same text counts once, and a page twice with other text stops import_blocks()", {
  p <- server_project()
  lines <- utils::capture.output(export_blocks())
  run <- real_blocks(p)
  heads <- header_at(lines)
  page <- c(heads[1], page_text_at(lines, heads[1]))
  got <- round_trip(p, c(lines, lines[page]))
  expect_identical(as_frames(got), as_frames(run))
  # The same page from another export: other text, and its own right md5.
  other <- lines[page]
  other[2] <- sub("0.1234567890123", "0.2234567890123", other[2], fixed = TRUE)
  expect_false(identical(other[2], lines[page][2]))
  other[1] <- sub(
    "md5 [0-9a-f]{8}$",
    paste0("md5 ", page_md5(other[-1])),
    other[1]
  )
  write_export(p, c(lines, other))
  expect_identical(
    import_error(p),
    paste0(
      "The block 1, page 1 comes twice in the file, with other lines. Keep ",
      "the page of one export only."
    )
  )
})

test_that("import_blocks() ignores trailing white space, empty lines and the lines before the first header line", {
  p <- local_project()
  pages <- pages_of(c(0.1, 1 / 3))
  write_export(p, c("> pbn::export_blocks()", "", paste0(pages, "  \t"), "", ""))
  import_error(p)
  expect_identical(real_blocks(p), list(a = c(0.1, 1 / 3)))
})

test_that("import_blocks() evaluates the text in baseenv(), so it does not see an object of the global environment", {
  p <- local_project()
  assign("pbn_test_object", 1, envir = globalenv())
  withr::defer(rm("pbn_test_object", envir = globalenv()))
  pages <- pages_of(1)
  pages[2] <- "list(a = pbn_test_object)"
  pages[1] <- sub("md5 [0-9a-f]{8}$", paste0("md5 ", page_md5(pages[2])), pages[1])
  write_export(p, pages)
  expect_match(
    import_error(p),
    "^The block 1: R cannot read its lines: object 'pbn_test_object' not found$"
  )
})

test_that("import_blocks() of a one-file project writes real/blocks.qs2 and not fake/, and overwrites it", {
  p <- local_project()
  make_one_file_script(p)
  expect_identical(basename(scenario_dir("v01", "real", p$home)), "fake")
  write_export(p, pages_of(c(1.5, 2)))
  import_error(p)
  expect_identical(real_blocks(p), list(a = c(1.5, 2)))
  expect_false(file.exists(results_path(p, "fake", "blocks.qs2")))
  write_export(p, pages_of("b"))
  import_error(p)
  expect_identical(real_blocks(p), list(a = "b"))
})

test_that("read_blocks() without real/blocks.qs2 returns fake/blocks.qs2 of a one-file project with a warning", {
  p <- local_project()
  make_one_file_script(p)
  dir.create(results_path(p, "fake"), recursive = TRUE)
  qs2::qs_save(list(a = "fake"), results_path(p, "fake", "blocks.qs2"))
  expect_warning(
    got <- read_blocks("v01", home = p$home),
    paste0(
      "^The numbers of v01 are fake: there is no .*v01_results/real/",
      "blocks\\.qs2, because pbn::import_blocks\\(\\) has not run\\. ",
      "pbn returns .*v01_results/fake/blocks\\.qs2\\.$"
    )
  )
  expect_identical(got, list(a = "fake"))
  expect_identical(names(formals(read_blocks)), c("version", "home"))
})

test_that("read_blocks() with real/blocks.qs2 and fake/blocks.qs2 returns real/ and gives no warning", {
  p <- local_project()
  make_one_file_script(p)
  dir.create(results_path(p, "fake"), recursive = TRUE)
  dir.create(results_path(p, "real"), recursive = TRUE)
  qs2::qs_save(list(a = "fake"), results_path(p, "fake", "blocks.qs2"))
  qs2::qs_save(list(a = "real"), results_path(p, "real", "blocks.qs2"))
  expect_no_warning(got <- read_blocks("v01", home = p$home))
  expect_identical(got, list(a = "real"))
})

# Renders the .Rmd of the lines `chunk` in the folder of `p` with
# rmarkdown::render() to Markdown, and returns the lines of the output.
render_md <- function(p, chunk) {
  rmd <- file.path(p$root, "doc.Rmd")
  writeLines(
    c("---", "title: \"Doc\"", "---", "", "```{r, warning = FALSE}", chunk, "```", "", "Text."),
    rmd
  )
  out <- rmarkdown::render(
    rmd,
    output_format = rmarkdown::md_document(),
    quiet = TRUE,
    envir = new.env()
  )
  return(readLines(out))
}

# One line of R that reads the blocks of `p` with read_blocks().
read_line <- function(p) {
  return(paste0("x <- pbn::read_blocks(\"v01\", home = \"", p$home, "\")"))
}

test_that("a rendered .Rmd that reads fake values shows FAKE DATA once at the top", {
  skip_if_not(rmarkdown::pandoc_available())
  p <- local_project()
  make_one_file_script(p)
  dir.create(results_path(p, "fake"), recursive = TRUE)
  qs2::qs_save(list(a = 1), results_path(p, "fake", "blocks.qs2"))
  out <- render_md(p, c(read_line(p), read_line(p)))
  expect_identical(out[nzchar(out)][1], "**FAKE DATA**")
  expect_identical(sum(grepl("FAKE DATA", out, fixed = TRUE)), 1L)
  # An import of fake pages is fake too.
  dir.create(results_path(p, "real"))
  qs2::qs_save(
    structure(list(a = 1), pbn_fake = TRUE),
    results_path(p, "real", "blocks.qs2")
  )
  out <- render_md(p, read_line(p))
  expect_identical(out[nzchar(out)][1], "**FAKE DATA**")
})

test_that("a rendered .Rmd that reads only real values shows no FAKE DATA", {
  skip_if_not(rmarkdown::pandoc_available())
  p <- local_project()
  make_one_file_script(p)
  dir.create(results_path(p, "real"), recursive = TRUE)
  qs2::qs_save(list(a = 1), results_path(p, "real", "blocks.qs2"))
  out <- render_md(p, read_line(p))
  expect_identical(sum(grepl("FAKE DATA", out, fixed = TRUE)), 0L)
  expect_true("Text." %in% out)
})

# The project of local_project() on the set house_2026_11_26, with the
# manuscript manuscript/built/v01/paper.qmd of the lines `qmd`.
rule_project <- function(qmd, one_file = TRUE, env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_26")
  if (one_file) {
    make_one_file_script(p)
  }
  write_lines(p$home, "manuscript/built/v01/paper.qmd", qmd)
  return(p)
}

# The rows of the rule `rule` of check_project() for the project `p`.
rule_rows <- function(p, rule = "built_reads_source") {
  found <- project_findings(p$home)
  return(found[found$rule == rule, c("status", "path", "detail")])
}

# A qmd file with the R chunk `code`.
qmd_with <- function(code) {
  return(c("# Results", "", "```{r}", code, "```"))
}

# Writes the raw values `x` as v01_results/real/blocks.qs2 of `p`.
save_real <- function(p, x = list(a = 1)) {
  dir.create(results_path(p, "real"), recursive = TRUE, showWarnings = FALSE)
  qs2::qs_save(x, results_path(p, "real", "blocks.qs2"))
  return(invisible(p))
}

# The FAIL row of built_reads_source for paper.qmd with the detail `x`.
source_fail <- function(x) {
  return(data.frame(
    status = "FAIL",
    path = "manuscript/built/v01/paper.qmd",
    detail = with_fix(
      "built_reads_source",
      paste0("manuscript/built/v01/paper.qmd: ", x)
    )
  ))
}

# The detail of built_reads_source for a read_results("v01") call.
results_detail <- paste0(
  "v01 is a one-file version, so a manuscript reads read_blocks() or ",
  "txt(), and these calls do not: pbn::read_results(\"v01\")"
)

test_that("built_reads_source fails a one-file built .qmd that calls read_results()", {
  p <- rule_project(qmd_with(c(
    "b <- pbn::read_blocks(\"v01\")",
    "r <- pbn::read_results(\"v01\")"
  )))
  save_real(p)
  expect_identical(rule_rows(p), source_fail(results_detail), ignore_attr = TRUE)
})

test_that("built_reads_source fails a one-file built .qmd that calls neither read_blocks() nor txt()", {
  p <- rule_project(c("# Results", "", "No numbers."))
  save_real(p)
  expect_identical(
    rule_rows(p),
    source_fail("it calls neither read_blocks() nor txt()"),
    ignore_attr = TRUE
  )
})

test_that("built_reads_source fails a one-file built .qmd that calls read_blocks(\"v01\") when real/blocks.qs2 is absent", {
  p <- rule_project(qmd_with("b <- pbn::read_blocks(\"v01\")"))
  dir.create(results_path(p, "fake"), recursive = TRUE)
  qs2::qs_save(list(a = 1), results_path(p, "fake", "blocks.qs2"))
  expect_identical(
    rule_rows(p),
    source_fail(paste0(
      "there is no v01_results/real/blocks.qs2, so the numbers of v01 are ",
      "fake and the manuscript is a draft"
    )),
    ignore_attr = TRUE
  )
})

test_that("built_reads_source fails a one-file built .qmd when real/blocks.qs2 has pbn_fake TRUE", {
  p <- rule_project(qmd_with("b <- pbn::read_blocks(\"v01\")"))
  save_real(p, structure(list(a = 1), pbn_fake = TRUE))
  expect_identical(
    rule_rows(p),
    source_fail(paste0(
      "v01_results/real/blocks.qs2 comes from the pages of fake data of ",
      "pbn::export_blocks(), so the numbers of v01 are fake and the ",
      "manuscript is a draft"
    )),
    ignore_attr = TRUE
  )
})

test_that("built_reads_source passes a one-file built .qmd that calls read_blocks(\"v01\") or txt() when real/blocks.qs2 exists", {
  p <- rule_project(qmd_with("b <- pbn::read_blocks(\"v01\")"))
  save_real(p)
  expect_identical(rule_rows(p)$status, "PASS")
  expect_identical(rule_rows(p, "built_reads_own_version")$status, "PASS")
  write_lines(
    p$home,
    "manuscript/built/v01/paper.qmd",
    c("The effect is `r pbn::txt(\"Effect\", version = \"v01\")`.")
  )
  expect_identical(rule_rows(p)$status, "PASS")
})

test_that("built_reads_source parses the run script and runs nothing: a run script that stops when it runs is still one-file", {
  p <- rule_project(qmd_with(c(
    "b <- pbn::read_blocks(\"v01\")",
    "r <- pbn::read_results(\"v01\")"
  )), one_file = FALSE)
  make_one_file_script(p)
  script <- file.path(p$home, "v01_run.R")
  writeLines(c("stop(\"this run script MUST NOT run\")", readLines(script)), script)
  expect_error(
    source(file.path(p$home, "v01_run.R"), local = new.env()),
    "this run script MUST NOT run",
    fixed = TRUE
  )
  save_real(p)
  expect_identical(rule_rows(p), source_fail(results_detail), ignore_attr = TRUE)
})

test_that("built_reads_source fails the built .qmd of a project that is not one-file when it calls read_blocks()", {
  p <- rule_project(qmd_with("b <- read_blocks(\"v01\")"), one_file = FALSE)
  save_real(p)
  expect_identical(
    rule_rows(p),
    source_fail(paste0(
      "v01 is not a one-file version, so a manuscript reads read_results(), ",
      "and these calls do not: read_blocks(\"v01\")"
    )),
    ignore_attr = TRUE
  )
  write_lines(p$home, "manuscript/built/v01/paper.qmd", qmd_with("r <- pbn::read_results(\"v01\")"))
  expect_identical(rule_rows(p)$status, "PASS")
})

test_that("under house_2026_11_26 built_reads_own_version fails a read_blocks() call with another version or no version", {
  p <- rule_project(qmd_with(c(
    "a <- pbn::read_blocks(\"v01\")",
    "b <- pbn::read_blocks(\"v02\")",
    "c <- read_blocks()"
  )))
  expect_identical(
    rule_rows(p, "built_reads_own_version"),
    data.frame(
      status = "FAIL",
      path = "manuscript/built/v01/paper.qmd",
      detail = with_fix(
        "built_reads_own_version",
        paste0(
          "manuscript/built/v01/paper.qmd: a manuscript in ",
          "manuscript/built/v01/ reads only the version \"v01\", and these ",
          "calls do not: pbn::read_blocks(\"v02\"); read_blocks()"
        )
      )
    ),
    ignore_attr = TRUE
  )
})
