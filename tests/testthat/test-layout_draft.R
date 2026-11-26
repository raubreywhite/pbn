# manuscript/draft/ holds the drafts of an agent, vNN_<INITIALS>.docx and
# vNN_<INITIALS>_si.docx. They are outside the version counter.

test_that("check_project() gives no finding on a project with drafts in manuscript/draft/", {
  p <- local_project()
  write_lines(p$home, "manuscript/draft/v02_AB.docx", "")
  write_lines(p$home, "manuscript/draft/v02_AB_si.docx", "")
  # project_report() gives the lines and the findings of check_project(),
  # which quits a non-interactive R session on a FAIL.
  report <- project_report(p$home)
  found <- attr(report, "findings")
  expect_identical(
    found[found$status != "PASS", c("rule", "status", "detail")],
    data.frame(
      rule = character(0),
      status = character(0),
      detail = character(0)
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    attr(report, "counts")[c("WARN", "FAIL")],
    c(WARN = 0L, FAIL = 0L)
  )
  expect_true(all(found$status == "PASS"))
})
