# The set house_2026_11_25: the rules no_plot_title, no_plot_subtitle and
# no_plot_caption. A figure has no title, no subtitle and no caption, because
# its legend belongs in the manuscript. The projects of house_2026_11_20 see
# none of it.

# A synthetic project of local_project() on the set house_2026_11_25. The
# manual manuscript v01_AB.docx of local_project() is at the newest run v01,
# so manual_not_ahead fails it. This project has no manual manuscript.
local_project_25 <- function(env = parent.frame()) {
  p <- local_project(env = env)
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_25")
  unlink(file.path(p$home, "manuscript", "manual", "v01_AB.docx"))
  return(p)
}

# The status and the detail of each finding of the rule `rule` in `home`.
rows_25 <- function(home, rule) {
  found <- project_findings(home)
  return(found[found$rule == rule, c("status", "path", "detail")])
}

# A functions file with a figure that has a title, a subtitle and a
# caption, in each way that the rules know.
figure_lines <- c(
  "make_figure <- function(d, dir) {",
  "  q <- ggplot2::ggplot(d) + ggplot2::ggtitle(\"Risk\")",
  "  q <- q + ggtitle(\"Risk\", \"By group\")",
  "  q <- q + ggplot2::labs(title = \"Risk\", subtitle = \"By group\", caption = \"Fake\")",
  "  q <- q + labs(x = \"Group\", caption = \"Fake\")",
  "  q <- patchwork::wrap_plots(q, q) + patchwork::plot_annotation(\"Risk\", caption = \"Fake\")",
  "  q <- q + plot_annotation(subtitle = \"By group\", tag_levels = \"A\")",
  "  q$labels$title <- \"Risk\"",
  "  \"By group\" -> q$labels$subtitle",
  "  q$labels$caption = \"Fake\"",
  "  return(q)",
  "}"
)

# A functions file with figures that the rules pass.
figure_pass_lines <- c(
  "make_figure <- function(d, dir) {",
  "  q <- ggplot2::ggplot(d)",
  "  q <- q + ggplot2::labs(x = \"Group\", y = \"Risk\", colour = \"Sex\", tag = \"A\")",
  "  q <- q + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)",
  "  q <- q + ggtitle(NULL)",
  "  q <- q + patchwork::plot_annotation(tag_levels = \"A\", title = NULL)",
  "  q <- q + ggplot2::theme(",
  "    plot.title = ggplot2::element_text(),",
  "    plot.subtitle = ggplot2::element_blank(),",
  "    plot.caption = ggplot2::element_blank()",
  "  )",
  "  q$labels$title <- NULL",
  "  q$labels$x <- \"Group\"",
  "  # labs(title = \"Risk\") in a comment does not count.",
  "  note <- \"ggtitle('Risk')\"",
  "  tab <- knitr::kable(d, caption = \"Table 1\")",
  "  other <- gt::tab_header(gt::gt(d), title = \"Risk\", subtitle = \"By group\")",
  "  mine <- mypkg::labs(title = \"Risk\", caption = \"Fake\")",
  "  return(list(q, note, tab, other, mine))",
  "}"
)

test_that("house_2026_11_25 is house_2026_11_20 with three new rules at fail, and a new pbn.yml and create_project() name the newest set", {
  old <- read_house_set("house_2026_11_20")
  new <- read_house_set("house_2026_11_25")
  expect_identical(new[names(old)], old)
  expect_identical(
    names(new),
    intersect(names(project_rules()), c(names(old), rules_2026_11_25))
  )
  expect_identical(setdiff(names(new), names(old)), rules_2026_11_25)
  expect_identical(
    vapply(new[rules_2026_11_25], function(r) return(r$level), character(1)),
    c(
      no_plot_title = "fail",
      no_plot_subtitle = "fail",
      no_plot_caption = "fail"
    )
  )
  expect_false(any(vapply(
    new[rules_2026_11_25],
    function(r) return(r$locked),
    logical(1)
  )))
  expect_identical(starter_line(), "extends: house_2026_11_26")
  expect_identical(create_project_set, "house_2026_11_26")
})

test_that("a project on house_2026_11_25 without a figure text passes every rule, and the new rules print their PASS lines", {
  p <- local_project_25()
  write_lines(p$home, "v01_functions/figure.R", figure_pass_lines)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found$detail[found$rule %in% rules_2026_11_25],
    c(
      "no .R file gives a figure a title",
      "no .R file gives a figure a subtitle",
      "no .R file gives a figure a caption"
    )
  )
})

test_that("the rules match the arguments of a call as R does: by name, by a part of a name and by position", {
  p <- local_project_25()
  write_lines(
    p$home,
    "adhoc/sketch.R",
    c(
      "a <- patchwork::plot_annotation(tit = \"Risk\")",
      "b <- ggplot2::ggtitle(sub = \"By group\", \"Risk\")",
      "c <- ggplot2::labs(tit = \"Risk\", \"Risk\")",
      "d <- plot_annotation(NULL, \"By group\", \"Fake\")",
      "e <- ggplot2::labs(..., caption = \"Fake\")",
      "f <- ggplot2::labs(title = \"Risk\", no_such = 1, title = 2)"
    )
  )
  expect_identical(
    rows_25(p$home, "no_plot_title"),
    data.frame(
      status = c("FAIL", "FAIL"),
      path = c("adhoc/sketch.R", "adhoc/sketch.R"),
      detail = with_fix(
        "no_plot_title",
        c(
          "adhoc/sketch.R:1: patchwork::plot_annotation(title = ...)",
          "adhoc/sketch.R:2: ggplot2::ggtitle(label = ...)"
        )
      )
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    rows_25(p$home, "no_plot_subtitle")$detail,
    with_fix(
      "no_plot_subtitle",
      c(
        "adhoc/sketch.R:2: ggplot2::ggtitle(subtitle = ...)",
        "adhoc/sketch.R:4: plot_annotation(subtitle = ...)"
      )
    )
  )
  expect_identical(
    rows_25(p$home, "no_plot_caption")$detail,
    with_fix(
      "no_plot_caption",
      c(
        "adhoc/sketch.R:4: plot_annotation(caption = ...)",
        "adhoc/sketch.R:5: ggplot2::labs(caption = ...)"
      )
    )
  )
})

test_that("no_plot_title, no_plot_subtitle and no_plot_caption fail each place that sets their element, and pass NULL, other arguments, theme elements and other functions", {
  p <- local_project_25()
  write_lines(p$home, "v01_functions/figure.R", figure_lines)
  found <- project_findings(p$home)
  expect_identical(
    failed_rules(found),
    c("no_plot_title", "no_plot_subtitle", "no_plot_caption")
  )
  detail <- function(rule) {
    return(sub("\\. Fix: .*$", "", found$detail[found$rule == rule]))
  }
  expect_identical(
    detail("no_plot_title"),
    c(
      "v01_functions/figure.R:2: ggplot2::ggtitle(label = ...)",
      "v01_functions/figure.R:3: ggtitle(label = ...)",
      "v01_functions/figure.R:4: ggplot2::labs(title = ...)",
      "v01_functions/figure.R:6: patchwork::plot_annotation(title = ...)",
      "v01_functions/figure.R:8: q$labels$title <- ..."
    )
  )
  expect_identical(
    detail("no_plot_subtitle"),
    c(
      "v01_functions/figure.R:3: ggtitle(subtitle = ...)",
      "v01_functions/figure.R:4: ggplot2::labs(subtitle = ...)",
      "v01_functions/figure.R:7: plot_annotation(subtitle = ...)",
      "v01_functions/figure.R:9: ... -> q$labels$subtitle"
    )
  )
  expect_identical(
    detail("no_plot_caption"),
    c(
      "v01_functions/figure.R:4: ggplot2::labs(caption = ...)",
      "v01_functions/figure.R:5: labs(caption = ...)",
      "v01_functions/figure.R:6: patchwork::plot_annotation(caption = ...)",
      "v01_functions/figure.R:10: q$labels$caption = ..."
    )
  )
  expect_identical(
    unique(found$status[found$rule %in% rules_2026_11_25]),
    "FAIL"
  )
  # The same figure without its texts passes each rule.
  write_lines(p$home, "v01_functions/figure.R", figure_pass_lines)
  found <- project_findings(p$home)
  expect_identical(
    found$status[found$rule %in% rules_2026_11_25],
    rep("PASS", 3)
  )
})

test_that("each rule reads the .R files of no_absolute_paths: not the file of to_one_file(), and not a file that R cannot parse", {
  p <- local_project_25()
  write_lines(p$home, "v01_one_file.R", "q <- ggplot2::labs(title = \"Risk\")")
  write_lines(p$home, "adhoc/broken.R", c("labs(title = \"Risk\"", "x <- ("))
  write_lines(p$home, "manuscript/built/v01/figure.R", "q <- ggtitle(\"Risk\")")
  found <- project_findings(p$home)
  expect_identical(
    found$detail[found$rule == "no_plot_title"],
    with_fix(
      "no_plot_title",
      "manuscript/built/v01/figure.R:1: ggtitle(label = ...)"
    )
  )
})

test_that("pbn.yml turns each rule off on its own, and allows a path for each", {
  p <- local_project_25()
  write_lines(p$home, "v01_functions/figure.R", figure_lines)
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_25",
      "rules:",
      "  no_plot_title: off",
      "  no_plot_subtitle: warn",
      "allow:",
      "  no_plot_caption: [v01_functions/figure.R]"
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found[found$rule == "no_plot_title", c("status", "detail")],
    data.frame(status = "OFF", detail = "off in pbn.yml"),
    ignore_attr = TRUE
  )
  expect_identical(
    unique(found$status[found$rule == "no_plot_subtitle"]),
    "WARN"
  )
  caption <- found$detail[found$rule == "no_plot_caption"]
  expect_identical(
    unique(found$status[found$rule == "no_plot_caption"]),
    "WARN"
  )
  expect_identical(
    caption[1],
    with_fix(
      "no_plot_caption",
      "v01_functions/figure.R:4: ggplot2::labs(caption = ...) (allowed)"
    )
  )
  # An allowed path of a rule that is off matches no finding.
  write_lines(
    p$home,
    "pbn.yml",
    c(
      "extends: house_2026_11_25",
      "rules:",
      "  no_plot_title: off",
      "  no_plot_subtitle: off",
      "  no_plot_caption: off",
      "allow:",
      "  no_plot_title: [v01_functions/figure.R]"
    )
  )
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_identical(
    found$status[found$rule %in% rules_2026_11_25],
    rep("OFF", 3)
  )
  expect_identical(
    found$detail[found$rule == "allow"],
    with_fix(
      "allow",
      paste0(
        "pbn.yml: the entry allow: no_plot_title: v01_functions/figure.R ",
        "matches no finding, because the rule is off"
      )
    )
  )
})

test_that("a project on house_2026_11_20 runs none of the three rules, and its pbn.yml cannot name one", {
  p <- local_project_25()
  write_lines(p$home, "pbn.yml", "extends: house_2026_11_20")
  write_lines(p$home, "v01_functions/figure.R", figure_lines)
  found <- project_findings(p$home)
  expect_identical(failed_rules(found), character(0))
  expect_false(any(rules_2026_11_25 %in% found$rule))
  expect_identical(
    unique(found$rule),
    names(read_house_set("house_2026_11_20"))
  )
  write_lines(
    p$home,
    "pbn.yml",
    c("extends: house_2026_11_20", "rules:", "  no_plot_caption: off")
  )
  err <- tryCatch(read_rules(p$home), error = function(e) return(e))
  expect_identical(
    conditionMessage(err),
    paste0(
      "pbn.yml: rules: no_plot_caption is not a rule of the set ",
      "house_2026_11_20. The oldest set with it is house_2026_11_25. To use ",
      "it, change extends: to that set or a newer one."
    )
  )
})
