# The steps form of add_data(): raw, clean, cohort and after_cohort.
#
# run_steps() and the functions that it calls run where the data runs, also
# on a PSOCK worker without pbn. They are in worker_functions, so
# data_wrapper_env() copies them into the environment of the wrapper. That
# environment holds .pbn_clean, .pbn_cohort and .pbn_after_cohort, each
# step or NULL, and .pbn_step_args, the argument of each step.

utils::globalVariables(c(
  ".pbn_clean",
  ".pbn_cohort",
  ".pbn_after_cohort",
  ".pbn_step_args"
))

# The names of the steps, in the order that they run.
step_names <- c("raw", "clean", "cohort", "after_cohort")

# The steps that add_data() got as a named list, without the steps that are
# NULL. NULL for the free form `fn`. Stops when the arguments break a rule.
check_data_steps <- function(fn, steps) {
  given <- names(steps)[!vapply(steps, is.null, logical(1))]
  if (!is.null(fn)) {
    if (length(given) > 0) {
      stop(
        "Give `fn` or the steps, not both. add_data() got `fn` and `",
        paste(given, collapse = "`, `"),
        "`.",
        call. = FALSE
      )
    }
    if (!is.function(fn)) {
      stop("`fn` must be a function.", call. = FALSE)
    }
    return(NULL)
  }
  if (length(given) == 0) {
    stop(
      "Give the data function as `fn`, or give the steps, starting with ",
      "`raw`.",
      call. = FALSE
    )
  }
  if (!"raw" %in% given) {
    stop(
      "The steps need the step `raw`, the function(scenario, sim) that ",
      "reads or makes the data.",
      call. = FALSE
    )
  }
  if ("after_cohort" %in% given && !"cohort" %in% given) {
    stop(
      "The step `after_cohort` needs the step `cohort`, because it gets the ",
      "cohort::CohortPipeline of that step.",
      call. = FALSE
    )
  }
  steps <- steps[given]
  for (s in given) {
    check_step_function(steps[[s]], s)
  }
  return(steps)
}

# Stops when the step `step` is not a function, or when a step after raw
# does not have exactly one argument without a default.
check_step_function <- function(f, step) {
  if (!is.function(f)) {
    stop("`", step, "` must be a function.", call. = FALSE)
  }
  if (step == "raw") {
    return(invisible(NULL))
  }
  args <- key_fields(f)
  if (length(args) != 1) {
    stop(
      "The step `",
      step,
      "` must take exactly one argument without a default, the value of ",
      "the step before it. It takes ",
      if (length(args) == 0) "none" else paste(args, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# The argument without a default of each step after raw.
step_args <- function(steps) {
  steps <- steps[setdiff(names(steps), "raw")]
  return(vapply(steps, key_fields, character(1)))
}

# The code of the data of a plan, for fake_md5: each function `fn` named by
# its data, and each step named <data>$<step>. After them comes each function
# of the functions folder that a data function or a step calls, as in the
# part helpers of the cache key, named by the function and sorted in the C
# locale. A plan whose data code calls no such function has the code of
# pbn 2026.11.28.
data_code <- function(info) {
  out <- list()
  helpers <- character(0)
  for (nm in names(info$data)) {
    steps <- info$steps[[nm]]
    if (is.null(steps)) {
      out[[nm]] <- info$data[[nm]]
    } else {
      out[paste0(nm, "$", names(steps))] <- steps
    }
    helpers <- union(helpers, helper_names(info, nm))
  }
  helpers <- sort(helpers, method = "radix")
  return(c(out, project_functions()[helpers]))
}

# The part data_function of the cache key of the data `name`: one md5 of
# `fn`, or for the steps an md5 of each step, named by the step.
data_function_md5 <- function(info, name) {
  steps <- info$steps[[name]]
  if (is.null(steps)) {
    return(function_md5(info$data[name]))
  }
  return(vapply(
    names(steps),
    function(s) return(function_md5(steps[s])),
    character(1)
  ))
}

# The part helpers of the cache key of the data `name`: the md5 of each
# function of the functions folder that `fn` or a step calls, directly or
# through other such functions, named by the function and sorted in the C
# locale. A function counts when its name is a name in the code of `fn`, of
# a step or of a function that counts, see code_names(). function_md5()
# hashes the code and the arguments without the source references. A
# function that only an analysis calls is not in the part.
helper_md5 <- function(info, name) {
  fns <- project_functions()
  seen <- helper_names(info, name)
  return(vapply(
    seen,
    function(nm) return(function_md5(fns[nm])),
    character(1)
  ))
}

# The names of the functions of the part helpers of the cache key of the
# data `name`, see helper_md5(), sorted in the C locale.
helper_names <- function(info, name) {
  fns <- project_functions()
  steps <- info$steps[[name]]
  todo <- if (is.null(steps)) info$data[name] else steps
  seen <- character(0)
  while (length(todo) > 0) {
    found <- unlist(lapply(todo, code_names))
    new <- setdiff(intersect(found, names(fns)), seen)
    seen <- c(seen, new)
    todo <- fns[new]
  }
  return(sort(seen, method = "radix"))
}

# Each name in the code of the function `f`: in its arguments and in its
# body, also inside a formula such as ~ f(.x), after $ and in a nested
# function. A local variable counts too. So does each string, such as "f" in
# do.call("f", args) or get("f"), see code_strings().
code_names <- function(f) {
  if (!is.function(f) || is.primitive(f)) {
    return(character(0))
  }
  args <- as.call(c(as.name("list"), as.list(formals(f))))
  return(unique(c(
    all.names(args),
    all.names(body(f)),
    code_strings(formals(f)),
    code_strings(body(f))
  )))
}

# The names of the elements of CFG that the code of the function `f` reads,
# as CFG$x or CFG[["x"]], in its arguments and in its body. NA when the code
# uses CFG in any other way, for example CFG[[k]], cfg = CFG, f(CFG) or
# get("CFG"). Then the whole of CFG counts.
cfg_names <- function(f) {
  if (!is.function(f) || is.primitive(f)) {
    return(character(0))
  }
  return(unique(c(cfg_names_code(formals(f)), cfg_names_code(body(f)))))
}

# cfg_names() of the code `x`, a call, a pairlist of arguments, a symbol or a
# constant.
cfg_names_code <- function(x) {
  if (is.symbol(x) || is.character(x)) {
    return(cfg_names_leaf(x))
  }
  if (!is.call(x) && !is.pairlist(x)) {
    return(character(0))
  }
  if (is_cfg_element(x)) {
    return(cfg_element_name(x))
  }
  # The name after $ or @, as in d$CFG, is no use of CFG.
  if (is_member_access(x)) {
    return(cfg_names_code(x[[2]]))
  }
  return(cfg_names_parts(x))
}

# cfg_names() of the symbol or the strings `x`: NA when it is CFG or holds
# "CFG", and else nothing.
cfg_names_leaf <- function(x) {
  if (is.symbol(x)) {
    return(if (identical(as.character(x), "CFG")) NA_character_ else character(0))
  }
  return(if ("CFG" %in% x) NA_character_ else character(0))
}

# TRUE when the code `x` is CFG$<element> or CFG[[<element>]].
is_cfg_element <- function(x) {
  return(
    is.call(x) &&
      length(x) == 3 &&
      is.symbol(x[[2]]) &&
      identical(as.character(x[[2]]), "CFG") &&
      (identical(x[[1]], as.name("$")) || identical(x[[1]], as.name("[[")))
  )
}

# The name of the element of the code `x`, CFG$<element> or
# CFG[[<element>]]: the symbol after $ or one string after $ or [[. NA for
# any other element, such as CFG[[k]].
cfg_element_name <- function(x) {
  el <- x[[3]]
  if (is.symbol(el) && identical(x[[1]], as.name("$"))) {
    return(as.character(el))
  }
  if (is.character(el) && length(el) == 1) {
    return(el)
  }
  return(NA_character_)
}

# TRUE when the code `x` is a call of $ or @ with two arguments.
is_member_access <- function(x) {
  return(
    is.call(x) &&
      length(x) == 3 &&
      (identical(x[[1]], as.name("$")) || identical(x[[1]], as.name("@")))
  )
}

# cfg_names() of each part of the call or the pairlist `x`, in order.
cfg_names_parts <- function(x) {
  out <- character(0)
  for (i in seq_along(x)) {
    # The empty symbol of an argument without a default cannot be passed on.
    if (!identical(x[[i]], quote(expr = ))) {
      out <- c(out, cfg_names_code(x[[i]]))
    }
  }
  return(out)
}

# Each string constant in the code `x`, a call, a pairlist of arguments or a
# constant. A symbol, also the empty symbol of an argument without a default,
# is skipped before it is passed on, because a missing argument cannot be
# passed.
code_strings <- function(x) {
  if (is.character(x)) {
    return(x)
  }
  if (!is.call(x) && !is.pairlist(x)) {
    return(character(0))
  }
  out <- character(0)
  for (i in seq_along(x)) {
    if (!is.symbol(x[[i]])) {
      out <- c(out, code_strings(x[[i]]))
    }
  }
  return(out)
}

# The functions that the functions folder defined when initialize_project()
# sourced it, named by the function, see defined_names(). An empty list
# without initialize_project().
project_functions <- function() {
  env <- state$env
  if (!is.environment(env)) {
    return(list())
  }
  names <- intersect(state$sourced, ls(env, all.names = TRUE))
  fns <- mget(names, envir = env)
  return(fns[vapply(fns, is.function, logical(1))])
}

# The worker functions ------------------------------------------------------

# The value of the steps of the data `name`. `x` is the value of the step
# raw, a promise that run_steps() forces first. The step clean MUST keep the
# number of rows of a data.frame, and the step cohort MUST give its cohort
# every row that it gets. The value of the step after_cohort gets the
# exclusion table of the cohort from pbn. The elapsed seconds of each step
# that ran go to .pbn_clock as `steps`, also when a step fails.
run_steps <- function(x, name) {
  start <- proc.time()[["elapsed"]]
  force(x)
  seconds <- c(raw = proc.time()[["elapsed"]] - start)
  on.exit(assign("steps", seconds, envir = .pbn_clock), add = TRUE)
  if (!is.null(.pbn_clean)) {
    n <- if (is.data.frame(x)) nrow(x) else NULL
    start <- proc.time()[["elapsed"]]
    x <- call_step("clean", x)
    seconds[["clean"]] <- proc.time()[["elapsed"]] - start
    check_clean_rows(x, n, name)
  }
  if (is.null(.pbn_cohort)) {
    return(x)
  }
  n <- if (is.data.frame(x)) nrow(x) else NULL
  start <- proc.time()[["elapsed"]]
  cp <- call_step("cohort", x)
  seconds[["cohort"]] <- proc.time()[["elapsed"]] - start
  check_cohort_rows(cp, n, name)
  if (is.null(.pbn_after_cohort)) {
    return(cp)
  }
  start <- proc.time()[["elapsed"]]
  x <- call_step("after_cohort", cp)
  seconds[["after_cohort"]] <- proc.time()[["elapsed"]] - start
  return(keep_cohort_table(x, cp, name))
}

# `x`, the value of the step after_cohort, with the exclusion table of the
# cohort `cp` and its number of rows at the start, as keep_exclusions() puts
# them. They replace the attributes that the step set, so a table from
# another cohort does not stay. The cohort `cp` itself goes on as it is, and
# cohort_value() takes its table. Another cohort::CohortPipeline fails the
# data, because its table would replace that of `cp`.
keep_cohort_table <- function(x, cp, name) {
  if (is.null(x)) {
    stop(
      "The step after_cohort of the data \"",
      name,
      "\" returned NULL. It MUST return the data for the analyses.",
      call. = FALSE
    )
  }
  if (identical(x, cp)) {
    return(x)
  }
  if (inherits(x, "CohortPipeline")) {
    stop(
      "The step after_cohort of the data \"",
      name,
      "\" returned another cohort::CohortPipeline than the one it got. It ",
      "MUST return the data for the analyses, or the same cohort, so that ",
      "the exclusion table is that of the step cohort.",
      call. = FALSE
    )
  }
  lineage <- cohort_lineage(cp, name)$lineage
  x <- set_value_attr(
    x,
    "pbn_exclusions",
    cohort_exclusions(cp$consort(), lineage, name)
  )
  return(set_value_attr(x, "pbn_n_before", as.integer(cp$n_total())))
}

# Calls the step `step` with `x` as its argument without a default, by name.
# The call is .pbn_<step>(<argument> = x), so the value of `x` is not in
# the call of an error.
call_step <- function(step, x) {
  call <- as.call(c(
    list(as.name(paste0(".pbn_", step))),
    stats::setNames(list(quote(x)), .pbn_step_args[[step]])
  ))
  return(eval(call))
}

# Stops when the step clean changed the number of rows `n` of the data.frame
# of the step raw. NULL `n` is a value of raw that is not a data.frame, and
# then nothing is checked.
check_clean_rows <- function(x, n, name) {
  if (is.null(n) || (is.data.frame(x) && nrow(x) == n)) {
    return(invisible(NULL))
  }
  stop(
    "The step clean of the data \"",
    name,
    "\" returned ",
    if (is.data.frame(x)) {
      paste(nrow(x), "rows")
    } else {
      paste0("an object of class ", class(x)[1], ", not a data.frame")
    },
    ", but the step raw returned ",
    n,
    " rows. The step clean MUST return a data.frame with the same number ",
    "of rows. Put each exclusion in the step cohort, with ",
    "$exclude_and_track(), so that pbn counts it.",
    call. = FALSE
  )
}

# Stops when the step cohort did not return a cohort::CohortPipeline, or
# when its cohort starts with another number of rows than `n`, the rows that
# the step got. NULL `n` is a value that is not a data.frame, and then the
# rows are not checked.
check_cohort_rows <- function(cp, n, name) {
  if (!inherits(cp, "CohortPipeline")) {
    stop(
      "The step cohort of the data \"",
      name,
      "\" returned an object of class ",
      class(cp)[1],
      ". It MUST return a cohort::CohortPipeline.",
      call. = FALSE
    )
  }
  start <- cp$n_total()
  if (is.null(n) || identical(as.integer(start), as.integer(n))) {
    return(invisible(NULL))
  }
  stop(
    "The cohort of the step cohort of the data \"",
    name,
    "\" starts with ",
    start,
    " rows, but the step got ",
    n,
    " rows. The step cohort MUST give cohort::CohortPipeline$new() every ",
    "row that it gets. Put each exclusion in $exclude_and_track(), so that ",
    "pbn counts it.",
    call. = FALSE
  )
}
