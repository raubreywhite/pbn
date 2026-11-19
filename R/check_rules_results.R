# The rule functions behind check_results(). Each rule returns one row: rule,
# status, detail.

exclusions_columns <- c("step", "n_before", "n_excluded", "n_after")
model_classes <- c("lm", "glm", "gam", "merMod", "brmsfit", "coxph", "lavaan")
# The classes that a list MAY carry. The set is the one of the results
# contract. A data.frame MAY carry any class that inherits from data.frame,
# because each such class (data.table, tbl_df and others) holds columns. An
# atomic vector MAY carry any class, because a class changes how R prints the
# values and not what the vector holds. A real fit table holds a column of
# the class lavaan.vector, for example.
allowed_list_classes <- c(
  "list",
  "AsIs",
  "table",
  "noquote",
  "difftime",
  "Date",
  "POSIXct",
  "factor",
  "integer64"
)

# The attributes that the walk of the results does not go into. R sets names,
# dim, dimnames, row.names, class, levels, tzone, units and comment, and
# each holds names, sizes or labels. data.table sets .internal.selfref, an
# external pointer to the table itself that holds no code, sorted, the key
# columns, and index, a row order with one entry for each row. Each other
# attribute is walked, for example label and format.spss on a column read
# from an SPSS file.
skipped_attributes <- c(
  "names",
  "dim",
  "dimnames",
  "row.names",
  "class",
  "levels",
  "tzone",
  "units",
  "comment",
  "sorted",
  "index",
  ".internal.selfref"
)

# The advice in the detail of allowed_objects for an object of each class.
class_advice <- c(
  htest = "save data.frame(statistic = , p_value = , ...) of the test",
  POSIXlt = "convert it with as.POSIXct()",
  package_version = "convert it with as.character()",
  numeric_version = "convert it with as.character()",
  sf = "drop the geometry with sf::st_drop_geometry()",
  sfc = "drop the geometry with sf::st_drop_geometry()",
  formula = "save it as text with deparse()"
)

result_row <- function(rule, failures, pass_detail) {
  if (length(failures) == 0) {
    return(data.frame(rule = rule, status = "PASS", detail = pass_detail))
  }
  return(data.frame(
    rule = rule,
    status = "FAIL",
    detail = paste(failures, collapse = "; ")
  ))
}

rule_element_names <- function(x) {
  rule <- "element_names"
  if (!is.list(x) || is.data.frame(x)) {
    return(result_row(rule, "the object is not a list", ""))
  }
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep("", length(x))
  }
  nm[is.na(nm)] <- ""
  failures <- character(0)
  if (sum(nm == "manifest") != 1) {
    failures <- "manifest MUST be there exactly once"
  }
  # results_gitexclude.qs2 MAY hold every kind.
  if (!any(nm != "manifest") && length(gitexclude_kinds(x)) == 0) {
    failures <- c(failures, "there is no element other than manifest")
  }
  failures <- c(failures, name_failures(nm))
  return(result_row(
    rule,
    failures,
    "manifest, and each other element with a unique syntactic name"
  ))
}

# The failures of the names `nm` of the results: an element without a name,
# a name that is not syntactic, and a name that is there more than once. A
# missing name is "".
name_failures <- function(nm) {
  failures <- character(0)
  unnamed <- which(!nzchar(nm))
  if (length(unnamed) > 0) {
    failures <- paste(
      "these elements have no name:",
      paste(unnamed, collapse = ", ")
    )
  }
  bad <- unique(nm[nzchar(nm) & make.names(nm) != nm])
  if (length(bad) > 0) {
    failures <- c(
      failures,
      paste(
        "these names are not syntactic:",
        paste(dQuote(bad, FALSE), collapse = ", ")
      )
    )
  }
  twice <- unique(nm[nzchar(nm) & duplicated(nm)])
  if (length(twice) > 0) {
    failures <- c(
      failures,
      paste("more than once:", paste(twice, collapse = ", "))
    )
  }
  return(failures)
}

rule_exclusions_table <- function(x) {
  rule <- "exclusions_table"
  if (!is.list(x) || !"exclusions" %in% names(x)) {
    return(result_row(
      rule,
      character(0),
      "there is no element exclusions, so nothing to check"
    ))
  }
  e <- x[["exclusions"]]
  if (!is.data.frame(e)) {
    return(result_row(rule, "exclusions is not a data.frame", ""))
  }
  want <- exclusions_want(x, e)
  if (!setequal(names(e), want) || ncol(e) != length(want)) {
    return(result_row(
      rule,
      paste0(
        "the columns are ",
        paste(names(e), collapse = ", "),
        "; they must be ",
        paste(want, collapse = ", ")
      ),
      ""
    ))
  }
  counts <- e[, exclusions_columns[-1], drop = FALSE]
  if (!all(vapply(counts, is.numeric, logical(1)))) {
    return(result_row(
      rule,
      "n_before, n_excluded and n_after must be numeric",
      ""
    ))
  }
  ok <- e$n_before - e$n_excluded == e$n_after
  bad <- which(is.na(ok) | !ok)
  failures <- if (length(bad) > 0) {
    paste0(
      "n_before - n_excluded != n_after on these rows: ",
      paste(bad, collapse = ", ")
    )
  } else {
    character(0)
  }
  return(result_row(
    rule,
    failures,
    paste0("the counts add up on every row; the number of rows is ", nrow(e))
  ))
}

# The columns that the exclusion table `e` of the results `x` MUST have.
# save_results() adds the column sim when it stacks simulations. The column
# data, which names the data of a cohort, is optional.
exclusions_want <- function(x, e) {
  want <- exclusions_columns
  if (is_stacked(x)) {
    want <- c("sim", want)
  }
  if ("data" %in% names(e)) {
    want <- c(want, "data")
  }
  return(want)
}

rule_allowed_objects <- function(x) {
  found <- find_paths(x, "x", disallowed_class)
  failures <- if (length(found) > 0) {
    paste0(
      "not allowed: ",
      paste0(
        names(found),
        " (class ",
        found,
        "): ",
        vapply(found, advice_for, character(1)),
        collapse = "; "
      )
    )
  } else {
    character(0)
  }
  return(result_row(
    "allowed_objects",
    failures,
    paste(
      "each element is a data.frame, an atomic vector, NULL or a plain list",
      "of these"
    )
  ))
}

# The advice for an object of the classes `cls`, written as "a/b".
advice_for <- function(cls) {
  cls <- strsplit(cls, "/", fixed = TRUE)[[1]]
  if (any(cls %in% model_classes)) {
    return("save its table of coefficients as a data.frame")
  }
  known <- intersect(cls, names(class_advice))
  if (length(known) > 0) {
    return(unname(class_advice[known[1]]))
  }
  return("convert it to a data.frame or a plain list")
}

# The class of `o`, when no element MAY be `o`. NULL when an element MAY be
# `o`. This checks `o` itself. find_paths() checks what `o` holds.
disallowed_class <- function(o) {
  if (can_hold_code(o) || !is_values(o)) {
    return(paste(class(o), collapse = "/"))
  }
  return(NULL)
}

# TRUE for a function, an environment, an S4 object, an R6 object and a
# fitted model of a known class.
can_hold_code <- function(o) {
  return(
    is.function(o) ||
      is.environment(o) ||
      isS4(o) ||
      inherits(o, c("R6", model_classes))
  )
}

# TRUE for NULL, a data.frame, an atomic vector, and a list with no class or
# only allowed classes. A formula, a call, a name and an external pointer are
# none of these.
is_values <- function(o) {
  if (is.null(o) || is.data.frame(o) || is.atomic(o)) {
    return(TRUE)
  }
  return(is.list(o) && all(oldClass(o) %in% allowed_list_classes))
}

rule_no_row_per_person <- function(x) {
  rule <- "no_row_per_person"
  m <- if (is.list(x)) x[["manifest"]] else NULL
  # The manifest is pbn's. A vector in it, such as packages, can have n
  # values by chance. So is exclusions, with one row for each step: k steps
  # and k people would fail it. exclusions_table checks it, and
  # add_analyses() refuses a kind of that name.
  results <- if (is.list(x)) {
    x[!names(x) %in% c("manifest", "exclusions")]
  } else {
    list()
  }
  if (is_stacked(x) && is.data.frame(m[["n_rows_by_sim"]])) {
    return(no_row_per_person_stacked(results, m))
  }
  # The rows of the data, and for a cohort also the rows given to the
  # pipeline before its exclusions and the rows after them.
  n <- if (is.list(m)) {
    c(
      m[["n_rows_data"]],
      m[["n_rows_before_exclusions"]],
      m[["n_rows_after_exclusions"]],
      list_rows(m[["n_rows_lists"]])
    )
  } else {
    NULL
  }
  # One number, or one for each data element. An element that is not a
  # data.frame has NA.
  n <- if (is.numeric(n)) unique(n[!is.na(n)]) else numeric(0)
  if (length(n) == 0) {
    return(result_row(
      rule,
      character(0),
      "n_rows_data is NA, so nothing to check"
    ))
  }
  # One value is not a value for each person, so 0 and 1 check nothing.
  small <- n[n < 2]
  n <- n[n >= 2]
  if (length(n) == 0) {
    return(result_row(
      rule,
      character(0),
      paste0(
        "n_rows_data is ",
        paste(small, collapse = ", "),
        ", less than 2, so nothing to check"
      )
    ))
  }
  where <- find_paths(results, "x", function(o) return(person_size(o, n)))
  sizes <- paste(n, collapse = " or ")
  return(result_row(
    rule,
    person_failures(where),
    paste0(
      "no data.frame has ",
      sizes,
      " rows, and no vector or matrix has ",
      sizes,
      " values in one dimension"
    )
  ))
}

# The rows of each data.frame of the lists of data.frames in
# manifest$n_rows_lists `x`, as one integer vector, or NULL.
list_rows <- function(x) {
  if (!is.list(x) || length(x) == 0) {
    return(NULL)
  }
  n <- unlist(x, use.names = FALSE)
  return(if (is.numeric(n)) n else NULL)
}

# The failure of no_row_per_person for the paths `where`, or character(0).
person_failures <- function(where) {
  if (length(where) == 0) {
    return(character(0))
  }
  return(paste0(
    paste0(names(where), " (", where, ")", collapse = ", "),
    " has one value for each row of the data. Save a summary of it ",
    "instead. If it is an aggregate that has this size by chance, change ",
    "its shape so that the size differs, for example to a long data.frame ",
    "with an explicit grouping column, and say so in a comment"
  ))
}

# no_row_per_person for stacked results. The rule is about the rows of one
# simulation, so it takes `results` apart into the value of each
# simulation, and checks it against the row counts of the data of that
# simulation in manifest$n_rows_by_sim. A stacked table of 400 simulations
# with 1 row each passes when each simulation has 400 rows of data.
no_row_per_person_stacked <- function(results, m) {
  by_sim <- m[["n_rows_by_sim"]]
  sims <- if (is_count(m[["sims"]])) m[["sims"]] else max(c(0, by_sim$sim))
  slices <- unstack_value(results, sims)
  where <- character(0)
  checked <- 0L
  for (k in seq_len(sims)) {
    here <- by_sim[by_sim$sim %in% k, , drop = FALSE]
    n <- unlist(here[intersect(names(here), n_rows_columns)], use.names = FALSE)
    n <- unique(n[is.finite(n) & n >= 2])
    if (length(n) == 0) {
      next
    }
    checked <- checked + 1L
    found <- find_paths(slices[[k]], "x", function(o) {
      return(person_size(o, n))
    })
    names(found) <- paste0(names(found), recycle0 = TRUE, " in simulation ", k)
    where <- c(where, found)
  }
  return(result_row(
    "no_row_per_person",
    person_failures(where),
    if (checked == 0) {
      "n_rows_data is NA, 0 or 1 in each simulation, so nothing to check"
    } else {
      paste0(
        "in each of the ",
        checked,
        " simulations with 2 or more rows of data, no data.frame has the ",
        "rows of that data, and no vector or matrix has that many values ",
        "in one dimension"
      )
    }
  ))
}

# The columns of manifest$n_rows_by_sim with a row count.
n_rows_columns <- c(
  "n_rows_data",
  "n_rows_before_exclusions",
  "n_rows_after_exclusions"
)

# The stacked value `o` taken apart into a list with the value of each of
# the `sims` simulations. This undoes the rules of the section Stacking of
# save_results(): a data.frame with the column sim gives its rows of each
# simulation without that column, a list named sim_0001 to sim_<sims> gives
# its entries, and another plain list is taken apart element by element.
# Each other value is the same in each simulation.
unstack_value <- function(o, sims) {
  if (is.data.frame(o)) {
    if (!"sim" %in% names(o)) {
      return(rep(list(o), sims))
    }
    f <- as.data.frame(o)
    keep <- setdiff(names(f), "sim")
    rows <- split(seq_len(nrow(f)), factor(f$sim, levels = seq_len(sims)))
    return(unname(lapply(rows, function(i) {
      return(f[i, keep, drop = FALSE])
    })))
  }
  if (!is.list(o) || !is.null(oldClass(o))) {
    return(rep(list(o), sims))
  }
  if (identical(names(o), sim_folder(seq_len(sims)))) {
    return(unname(o))
  }
  parts <- lapply(o, unstack_value, sims = sims)
  return(lapply(seq_len(sims), function(k) {
    out <- lapply(parts, function(p) return(p[[k]]))
    names(out) <- names(o)
    return(out)
  }))
}

# The size of `o` when it has one value for each row of the data, for one of
# the row counts `n`: a data.frame with that many rows, an atomic vector of
# that length, or a matrix or an array with a dimension of that size. NULL
# otherwise.
person_size <- function(o, n) {
  if (is.data.frame(o)) {
    if (nrow(o) %in% n) {
      return(paste("a data.frame with", nrow(o), "rows"))
    }
    return(NULL)
  }
  if (!is.atomic(o)) {
    return(NULL)
  }
  d <- dim(o)
  if (!is.null(d) && any(d %in% n)) {
    return(paste("dimensions", paste(d, collapse = "x")))
  }
  if (is.null(d) && length(o) %in% n) {
    return(paste("length", length(o)))
  }
  return(NULL)
}

# Each element of `obj` for which `test` gives a string, named by its path.
# The walk goes into each list, each data.frame and each attribute other than
# skipped_attributes, and it does not go into a match. It also does not go
# into an object that allowed_objects reports, so the model frame of a
# fitted model is not reported again.
find_paths <- function(obj, where, test) {
  hit <- test(obj)
  if (!is.null(hit)) {
    return(stats::setNames(hit, where))
  }
  if (!is.null(disallowed_class(obj))) {
    return(character(0))
  }
  out <- character(0)
  for (a in setdiff(names(attributes(obj)), skipped_attributes)) {
    out <- c(
      out,
      find_paths(
        attr(obj, a, exact = TRUE),
        paste0("attr(", where, ", \"", a, "\")"),
        test
      )
    )
  }
  if (is.list(obj)) {
    for (i in seq_along(obj)) {
      out <- c(out, find_paths(obj[[i]], child_path(obj, i, where), test))
    }
  }
  return(out)
}

# The path of the element `i` of `obj`, when `where` is the path of `obj`.
child_path <- function(obj, i, where) {
  nm <- names(obj)
  if (!is.null(nm) && !is.na(nm[i]) && nzchar(nm[i])) {
    return(paste0(where, "$", nm[i]))
  }
  return(paste0(where, "[[", i, "]]"))
}

is_stacked <- function(x) {
  m <- if (is.list(x)) x[["manifest"]] else NULL
  return(is.list(m) && isTRUE(m[["stacked"]]))
}

rule_manifest_scenario <- function(x) {
  rule <- "manifest_scenario"
  m <- if (is.list(x)) x[["manifest"]] else NULL
  if (!is.list(m)) {
    return(result_row(rule, "manifest is not a list", ""))
  }
  failures <- character(0)
  missing <- setdiff(c("scenario", "sim", "sims"), names(m))
  if (length(missing) > 0) {
    failures <- paste("manifest has no", paste(missing, collapse = ", "))
  }
  if (!"scenario" %in% missing && !is_one_string(m[["scenario"]])) {
    failures <- c(failures, "manifest$scenario is not one string")
  }
  return(result_row(
    rule,
    failures,
    "the manifest names the scenario, the simulation and the simulations"
  ))
}

rule_manifest_data <- function(x) {
  rule <- "manifest_data"
  m <- if (is.list(x)) x[["manifest"]] else NULL
  if (!is.list(m)) {
    return(result_row(rule, "manifest is not a list", ""))
  }
  return(result_row(
    rule,
    manifest_data_failure(m),
    "the manifest has data_files and fake_real_data for \"real\", or fake_md5 for a fake scenario"
  ))
}

rule_manifest_source <- function(x) {
  rule <- "manifest_source"
  m <- if (is.list(x)) x[["manifest"]] else NULL
  if (!is.list(m)) {
    return(result_row(rule, "manifest is not a list", ""))
  }
  return(result_row(
    rule,
    source_md5_failure(m),
    "the manifest has source_md5, with the md5 of the run script"
  ))
}

# The failure of the rule manifest_source for the manifest `m`, or
# character(0). The run script is <version>_run.R when the manifest names one
# version, and any vNN_run.R otherwise.
source_md5_failure <- function(m) {
  s <- m[["source_md5"]]
  if (is.null(s)) {
    return(paste(
      "manifest has no source_md5. An older pbn wrote this file:",
      "run the scenarios again"
    ))
  }
  if (!is_data_md5(s) || length(s) == 0) {
    return(paste(
      "manifest$source_md5 is not a named character vector of md5 values,",
      "each a string of 32 lower-case hex digits"
    ))
  }
  version <- m[["version"]]
  named <- if (is_one_string(version) && grepl("^v[0-9]{2}$", version)) {
    paste0(version, "_run.R") %in% names(s)
  } else {
    any(grepl(run_script_pattern, names(s)))
  }
  if (!named) {
    return("manifest$source_md5 does not name the run script vNN_run.R")
  }
  return(character(0))
}

# The failure of the rule manifest_data for the manifest `m`, or
# character(0). When the scenario is not one string, manifest_scenario fails,
# and this rule checks only that one of the two elements is there.
manifest_data_failure <- function(m) {
  if (!is.null(m[["data_md5"]])) {
    return(paste(
      "manifest has data_md5, the md5 of the data files that an older pbn",
      "wrote. pbn now writes data_files: run the scenario again"
    ))
  }
  found <- data_element_failure(m)
  if (length(found) > 0 || !identical(m[["scenario"]], "real")) {
    return(found)
  }
  flag <- m[["fake_real_data"]]
  if (is.null(flag)) {
    return(paste(
      "manifest has no fake_real_data. An older pbn wrote this file:",
      "run the scenario again"
    ))
  }
  if (!isTRUE(flag) && !isFALSE(flag)) {
    return("manifest$fake_real_data is not TRUE or FALSE")
  }
  return(failed_fake_checks_failure(m))
}

# The failure of manifest$failed_fake_checks of the real manifest `m`, or
# character(0). pbn 2026.11.25 and newer write it. An older pbn did not, and
# a manifest without it and without a newer pbn_version passes.
failed_fake_checks_failure <- function(m) {
  x <- m[["failed_fake_checks"]]
  if (is.null(x)) {
    v <- tryCatch(
      package_version(m[["pbn_version"]]),
      error = function(e) return(NULL)
    )
    new <- length(v) == 1 && v >= package_version("2026.11.25")
    if (new) {
      return(paste(
        "manifest has no failed_fake_checks. pbn 2026.11.25 and newer write",
        "it for \"real\""
      ))
    }
    return(character(0))
  }
  ok <- is.character(x) &&
    is.null(attributes(x)) &&
    !anyNA(x) &&
    all(grepl("^[a-z0-9_]+$", x)) &&
    !"real" %in% x &&
    !anyDuplicated(x)
  if (!ok) {
    return(paste(
      "manifest$failed_fake_checks is not a character vector of names of",
      "fake scenarios. Each name has lower-case letters, digits and",
      "underscores only, is not \"real\" and comes once"
    ))
  }
  return(character(0))
}

# The failure of manifest_data on the elements data_files and fake_md5 of
# the manifest `m`, or character(0).
data_element_failure <- function(m) {
  has <- c(
    data_files = !is.null(m[["data_files"]]),
    fake_md5 = !is.null(m[["fake_md5"]])
  )
  if (all(has)) {
    return("manifest has both data_files and fake_md5")
  }
  if (!any(has)) {
    return("manifest has neither data_files nor fake_md5")
  }
  scenario <- m[["scenario"]]
  real <- is_one_string(scenario) && scenario == "real"
  if (
    is_one_string(scenario) && !has[[if (real) "data_files" else "fake_md5"]]
  ) {
    if (real) {
      return(
        "manifest has fake_md5, but the scenario \"real\" needs data_files"
      )
    }
    return(paste0(
      "manifest has data_files, but the fake scenario \"",
      scenario,
      "\" needs fake_md5"
    ))
  }
  return(md5_failure(m, has))
}

# The failure when the fingerprint element of the manifest `m` is not in
# the form that pbn writes, or character(0).
md5_failure <- function(m, has) {
  if (has[["fake_md5"]] && !is_md5(m[["fake_md5"]])) {
    return(
      "manifest$fake_md5 is not one md5: a string of 32 lower-case hex digits"
    )
  }
  if (has[["data_files"]] && !is_data_files(m[["data_files"]])) {
    return(paste(
      "manifest$data_files is not a data.frame with the columns file, size",
      "and mtime: each file once and named, each size a number of 0 or more,",
      "and each mtime a number"
    ))
  }
  return(character(0))
}

# TRUE for a fingerprint of the files of the data folders, see
# folder_files(). A folder with no file gives 0 rows.
is_data_files <- function(x) {
  if (!is.data.frame(x) || !identical(names(x), c("file", "size", "mtime"))) {
    return(FALSE)
  }
  return(
    is_file_names(x$file) &&
      is_numbers(x$size) &&
      all(x$size >= 0) &&
      is_numbers(x$mtime)
  )
}

# TRUE for a character vector of names that are not empty, each once.
is_file_names <- function(x) {
  return(
    is.character(x) && !anyNA(x) && all(nzchar(x)) && anyDuplicated(x) == 0
  )
}

# TRUE for a numeric vector of finite numbers.
is_numbers <- function(x) {
  return(is.numeric(x) && all(is.finite(x)))
}

# One md5 as tools::md5sum() writes it.
is_md5 <- function(x) {
  return(is_one_string(x) && grepl("^[0-9a-f]{32}$", x))
}

# A named character vector of md5 values, as `source_md5`.
is_data_md5 <- function(x) {
  nm <- names(x)
  return(
    is.character(x) &&
      !is.null(nm) &&
      !anyNA(nm) &&
      all(nzchar(nm)) &&
      all(grepl("^[0-9a-f]{32}$", x))
  )
}

is_one_string <- function(x) {
  return(is.character(x) && length(x) == 1 && !is.na(x))
}
