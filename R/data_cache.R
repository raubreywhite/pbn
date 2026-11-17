# The value of a data function: a cohort turned into its data, and the disk
# cache of add_data(cache = TRUE).
#
# The functions of the first part run where the data runs, also on a PSOCK
# worker without pbn. data_wrapper_env() copies them into the environment of
# the wrapper, and sets their environment to it. They call base R, qs2, tools,
# utils and data.table only, each with `::`.

# The functions that data_wrapper_env() copies into the environment of each
# data wrapper.
worker_functions <- c(
  "data_value",
  "cohort_value",
  "cohort_lineage",
  "cohort_leaf",
  "cohort_exclusions",
  "cache_value",
  "cache_parts",
  "cache_md5",
  "cache_reason",
  "changed_parts",
  "part_change",
  "differing_names",
  "cache_read",
  "cache_save",
  "cache_line",
  "loaded_versions",
  "changed_packages",
  "set_value_attr",
  "run_steps",
  "call_step",
  "check_clean_rows",
  "check_cohort_rows",
  "keep_cohort_table"
)

# The environment of a data wrapper: that of wrapper_env(), with the
# functions of worker_functions, .pbn_cache, the cache context, and
# .pbn_plan, the key of the plan in the registry, or NULL without a cache.
# run_plan() sets .pbn_cache for a data function with cache = TRUE, and sets
# it back to NULL after the run. For the steps form, `steps` holds the steps
# that add_data() got, and the environment holds each step after raw as
# .pbn_<step>, NULL when it was not given, and .pbn_step_args.
data_wrapper_env <- function(fn, plan_key = NULL, steps = NULL) {
  e <- wrapper_env(fn)
  assign(".pbn_cache", NULL, envir = e)
  assign(".pbn_plan", plan_key, envir = e)
  for (s in setdiff(step_names, "raw")) {
    assign(paste0(".pbn_", s), steps[[s]], envir = e)
  }
  assign(".pbn_step_args", step_args(steps), envir = e)
  for (nm in worker_functions) {
    f <- get(nm, mode = "function")
    environment(f) <- e
    assign(nm, f, envir = e)
  }
  return(e)
}

# The value that the analyses get as data[[name]]. compute() sets the seed
# and calls the data function. `where` is list(scenario, sim), or NULL for a
# data function without both. `ctx` is NULL, or the cache context. `plan` is
# NULL without a cache, or the key of the plan. Outside run_plan(), for
# example in p$get_data() by hand, `ctx` is NULL, and pbn builds it from the
# state of the project at the time of the call. That happens in the main
# session only, where pbn is loaded.
data_value <- function(compute, name, where, ctx, plan = NULL) {
  if (is.null(ctx) && !is.null(plan)) {
    ctx <- get("hand_cache_context", envir = asNamespace("pbn"))(plan, name)
  }
  if (is.null(ctx)) {
    return(cohort_value(compute(), name))
  }
  return(cache_value(compute, name, where, ctx))
}

# `x`, or for a cohort::CohortPipeline the data.table of its one cohort
# without a child. The value carries the exclusion table in the attribute
# pbn_exclusions, and the number of rows given to the pipeline in the
# attribute pbn_n_before. A value of keep_exclusions() carries both already:
# its table gets the name of the data.
cohort_value <- function(x, name) {
  if (!inherits(x, "CohortPipeline")) {
    table <- attr(x, "pbn_exclusions", exact = TRUE)
    if (is.data.frame(table)) {
      table$data <- rep(name, nrow(table))
      x <- set_value_attr(x, "pbn_exclusions", table)
    }
    return(x)
  }
  where <- cohort_lineage(x, name)
  out <- x$get_included(where$leaf)
  out <- set_value_attr(
    out,
    "pbn_exclusions",
    cohort_exclusions(x$consort(), where$lineage, name)
  )
  return(set_value_attr(out, "pbn_n_before", as.integer(x$n_total())))
}

# The one cohort without a child of the pipeline `x`, `leaf`, and the
# cohorts from the root down to it, `lineage`.
cohort_lineage <- function(x, name) {
  cohorts <- as.data.frame(x$list_cohorts())
  leaf <- cohort_leaf(cohorts, name)
  lineage <- leaf
  repeat {
    up <- cohorts$parent[cohorts$name == lineage[1]]
    if (length(up) != 1 || is.na(up)) {
      break
    }
    lineage <- c(up, lineage)
  }
  return(list(leaf = leaf, lineage = lineage))
}

# The one cohort that no other cohort of the pipeline branches from. `name`
# is the name of the data, or NA outside a data function.
cohort_leaf <- function(cohorts, name) {
  leaf <- setdiff(cohorts$name, cohorts$parent)
  if (length(leaf) != 1) {
    stop(
      if (is.na(name)) {
        "The cohort::CohortPipeline has "
      } else {
        paste0(
          "The data function \"",
          name,
          "\" returned a cohort::CohortPipeline with "
        )
      },
      length(leaf),
      " cohorts that no other cohort branches from: ",
      paste(leaf, collapse = ", "),
      ". pbn needs exactly one, the cohort of the analyses.",
      call. = FALSE
    )
  }
  return(leaf)
}

# The exclusion table of the cohorts `lineage`, from the root to the cohort
# of the analyses, out of the table of $consort(), in the order of the steps.
cohort_exclusions <- function(consort, lineage, name) {
  log <- as.data.frame(consort)
  log <- log[log$branch %in% lineage, , drop = FALSE]
  log <- log[order(log$step), , drop = FALSE]
  return(data.frame(
    data = rep(name, nrow(log)),
    step = as.character(log$reason),
    n_before = log$n_excluded + log$n_remaining,
    n_excluded = log$n_excluded,
    n_after = log$n_remaining
  ))
}

# `x` with the attribute `which` set to `value`. attr<-() gives a data.table
# a new list of columns, so setalloccol() gives it a valid self-reference
# again. The object of the caller does not change.
set_value_attr <- function(x, which, value) {
  attr(x, which) <- value
  if (inherits(x, "data.table")) {
    x <- data.table::setalloccol(x)
  }
  return(x)
}

# The value of the data from the cache when the key is the same, and from
# compute() otherwise. Either way, the value carries the attribute pbn_cache:
# the name of the data, whether it came from the cache, the key and the
# printed line.
cache_value <- function(compute, name, where, ctx) {
  parts <- cache_parts(ctx, where)
  key <- cache_md5(parts)
  file <- file.path(
    ctx[["dir"]],
    where[["scenario"]],
    sprintf("sim_%04d.qs2", where[["sim"]])
  )
  key_file <- sub("\\.qs2$", "_key.qs2", file)
  reason <- cache_reason(file, key_file, parts, ctx[["memo"]])
  value <- NULL
  if (is.null(reason)) {
    got <- cache_read(file, key)
    reason <- got$reason
    value <- got$value
  }
  from_cache <- is.null(reason)
  line <- cache_line(name, where, reason, file)
  if (isTRUE(ctx[["print_now"]])) {
    cat(line, "\n", sep = "")
    utils::flush.console()
  }
  if (!from_cache) {
    value <- cohort_value(compute(), name)
    if (is.null(value)) {
      stop(
        "The data function \"",
        name,
        "\" returned NULL, and cache = TRUE needs a value.",
        call. = FALSE
      )
    }
    # The record of the packages comes after the data function, so it holds
    # each package that the data function loaded.
    packages <- loaded_versions(ctx[["skip"]])
    cache_save(list(key = key, value = value), file)
    cache_save(list(key = key, parts = parts, packages = packages), key_file)
  }
  return(set_value_attr(
    value,
    "pbn_cache",
    list(data = name, from_cache = from_cache, key = key, line = line)
  ))
}

# The parts of the cache key of one scenario and simulation, in a fixed
# order.
cache_parts <- function(ctx, where) {
  s <- where[["scenario"]]
  return(list(
    scenario = s,
    sim = where[["sim"]],
    settings = ctx[["settings"]][[s]],
    raw_data = if (identical(s, "real")) ctx[["raw_data"]] else NULL,
    data_files = ctx[["data_files"]],
    data_function = ctx[["data_function"]],
    cfg_data = ctx[["cfg_data"]],
    versions = ctx[["versions"]],
    rng = ctx[["rng"]]
  ))
}

# The md5 of the serialization of `x`.
cache_md5 <- function(x) {
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  writeBin(serialize(x, NULL, xdr = TRUE, version = 3), file)
  return(unname(tools::md5sum(file)))
}

# Why the cache of `file` cannot be used for the key parts `parts`, or NULL
# when its key file holds the same parts and no recorded package has another
# version now. `memo` keeps the installed versions that this use looked up.
cache_reason <- function(file, key_file, parts, memo) {
  if (!file.exists(file)) {
    return("there is no cache file")
  }
  got <- tryCatch(qs2::qs_read(key_file), error = function(e) return(NULL))
  old <- if (is.list(got)) got[["parts"]] else NULL
  packages <- if (is.list(got)) got[["packages"]] else NULL
  if (
    !is.list(old) ||
      !all(names(parts) %in% names(old)) ||
      !is.character(packages)
  ) {
    return("the key file of the cache is missing or misses parts of the key")
  }
  changed <- changed_parts(old, parts)
  if (length(changed) == 0) {
    changed <- changed_packages(packages, memo)
  }
  if (length(changed) > 0) {
    return(paste0(paste(changed, collapse = ", "), " changed"))
  }
  return(NULL)
}

# The version of each loaded package that is not in `skip`, named by the
# package and sorted. package_version() writes "1.1.1-1" as "1.1.1.1", the
# form of utils::packageVersion(), so the two compare.
loaded_versions <- function(skip) {
  ns <- sort(setdiff(loadedNamespaces(), skip), method = "radix")
  return(vapply(
    ns,
    function(n) {
      return(as.character(package_version(getNamespaceVersion(n))))
    },
    character(1)
  ))
}

# "the version of <pkg>" for each package of the record `packages` whose
# installed version now differs from the recorded one. A package that is not
# installed counts as changed.
changed_packages <- function(packages, memo) {
  out <- character(0)
  for (nm in names(packages)) {
    if (!exists(nm, envir = memo, inherits = FALSE)) {
      now <- tryCatch(
        as.character(utils::packageVersion(nm)),
        error = function(e) return(NA_character_)
      )
      assign(nm, now, envir = memo)
    }
    if (!identical(get(nm, envir = memo, inherits = FALSE), packages[[nm]])) {
      out <- c(out, paste("the version of", nm))
    }
  }
  return(out)
}

# The names of the parts of the key that differ between `old` and `new`. A
# changed data_*.R file, a changed version and a changed step of the data
# are each named.
changed_parts <- function(old, new) {
  out <- character(0)
  for (nm in names(new)) {
    if (!identical(old[[nm]], new[[nm]])) {
      out <- c(out, part_change(nm, old[[nm]], new[[nm]]))
    }
  }
  return(out)
}

# The name of the change of the part `nm` of the key from `a` to `b`. The
# part data_function of the steps holds one md5 for each step, named by the
# step, and the part data_function of `fn` holds one md5 without a name.
part_change <- function(nm, a, b) {
  if (nm == "data_function" && !is.null(names(a)) && !is.null(names(b))) {
    return(paste0("the function given as `", differing_names(a, b), "`"))
  }
  if (nm == "versions") {
    return(paste("the version of", differing_names(a, b)))
  }
  if (nm == "data_files") {
    return(differing_names(a, b))
  }
  labels <- c(
    scenario = "the scenario",
    sim = "the simulation",
    settings = "the settings of the scenario",
    raw_data = "the raw data",
    data_function = "the data function of add_data()",
    cfg_data = "CFG$data",
    rng = "the kind of random number generator"
  )
  return(labels[[nm]])
}

# The names of the named vectors `a` and `b` whose values differ, also a
# name that only one of them has, in the order of union().
differing_names <- function(a, b) {
  each <- union(names(a), names(b))
  same <- vapply(
    each,
    function(f) return(identical(unname(a[f]), unname(b[f]))),
    logical(1)
  )
  return(each[!same])
}

# The value in the cache file `file`, and a reason when it cannot be used:
# the file cannot be read, or it holds another key than its key file.
cache_read <- function(file, key) {
  got <- tryCatch(qs2::qs_read(file), error = function(e) return(NULL))
  if (!is.list(got) || !all(c("key", "value") %in% names(got))) {
    return(list(value = NULL, reason = "the cache file cannot be read"))
  }
  if (!identical(got[["key"]], key)) {
    return(list(
      value = NULL,
      reason = "the cache file holds another key than its key file"
    ))
  }
  return(list(value = got[["value"]], reason = NULL))
}

# Writes `x` to a temporary file in the folder of `file`, then renames it to
# `file`. A reader sees the old file or the new one, never a part of one.
cache_save <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("cache_", tmpdir = dirname(file), fileext = ".qs2.tmp")
  on.exit(unlink(tmp), add = TRUE)
  qs2::qs_save(x, tmp)
  if (!file.rename(tmp, file)) {
    stop("pbn could not write the cache file ", file, call. = FALSE)
  }
  return(invisible(file))
}

# The line that run_plan() prints for one scenario and simulation.
cache_line <- function(name, where, reason, file) {
  head <- paste0(
    "pbn: data ",
    name,
    ", ",
    where[["scenario"]],
    " ",
    sprintf("sim_%04d", where[["sim"]]),
    ": "
  )
  if (is.null(reason)) {
    return(paste0(
      head,
      "reused the cache of ",
      format(file.mtime(file), "%Y-%m-%d %H:%M"),
      "."
    ))
  }
  return(paste0(head, "computed, because ", reason, "."))
}

# The main session ----------------------------------------------------------

# Sets the cache context of each data function of the plan with
# cache = TRUE, before the run. Stops when the project has no state, or when
# git does not ignore the cache folder. Returns the names of those data
# functions.
set_cache <- function(p, info, workers) {
  cached <- names(info$cache)[vapply(info$cache, isTRUE, logical(1))]
  if (length(cached) == 0) {
    return(character(0))
  }
  check_state("run_plan")
  check_cache_ignored(cached, "pbn ran nothing.")
  check_data_code("pbn ran nothing.")
  for (nm in cached) {
    ctx <- cache_context(info, nm, print_now = workers == 1)
    assign(".pbn_cache", ctx, envir = info$data_envs[[nm]])
  }
  return(cached)
}

# The cache context of the data `name` of a plan, from the state of the
# project now. The md5 of the data_*.R files comes from initialize_project(),
# which computed it before it sourced them, and check_data_code() stops when
# a file on disk differs from it. The md5 of the raw data is that of the last
# refresh_data_md5(). `skip` names the packages that the record of the
# loaded packages leaves out, and `memo` keeps the installed version of each
# package that a use of the cache looked up.
cache_context <- function(info, name, print_now) {
  return(list(
    settings = info$scenario_settings,
    raw_data = state$data_md5,
    data_files = data_files_md5(state$source_md5),
    cfg_data = cfg_data(state$env),
    versions = cache_versions(),
    rng = RNGkind(),
    skip = record_skip(),
    memo = new.env(parent = emptyenv()),
    print_now = print_now,
    dir = file.path(
      state$home,
      paste0(state$version, "_cache_gitexclude"),
      name
    ),
    data_function = data_function_md5(info, name)
  ))
}

# The cache context of the data `name` of the plan with the registry key
# `plan`, for a call outside run_plan(), for example p$get_data() by hand. It
# prints the line of the cache at once.
hand_cache_context <- function(plan, name) {
  info <- get0(plan, envir = plans, inherits = FALSE)
  if (is.null(info)) {
    stop("pbn has no record of this plan.", call. = FALSE)
  }
  if (is.null(state$results)) {
    stop(
      "The data \"",
      name,
      "\" has cache = TRUE. Call pbn::initialize_project() before you get ",
      "the data.",
      call. = FALSE
    )
  }
  check_cache_ignored(name, "pbn computed nothing.")
  check_data_code("pbn computed nothing.")
  refresh_data_md5()
  return(cache_context(info, name, print_now = TRUE))
}

# The packages that pbn uses to run the plan and that tests and documents use
# to run R. The record of the loaded packages leaves them out, with the base
# packages, which the version of R covers. The key holds the version of pbn.
machinery_packages <- c(
  "pbn",
  "plnr",
  "foreach",
  "doParallel",
  "iterators",
  "qs2",
  "digest",
  "uuid",
  "progress",
  "testthat",
  "pkgload",
  "devtools",
  "callr",
  "processx",
  "ps",
  "knitr",
  "rmarkdown",
  "evaluate"
)

record_skip <- function() {
  base <- rownames(utils::installed.packages(
    lib.loc = .Library,
    priority = "base"
  ))
  return(c(base, machinery_packages))
}

# Stops when a data_*.R file of the functions folder is not the file that
# initialize_project() hashed and sourced: changed, added or removed. A file
# with the size and mtime of the record is not hashed again.
check_data_code <- function(done) {
  folder <- paste0(state$version, "_functions")
  inner <- list.files(
    file.path(state$home, folder),
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  now <- file.path(folder, inner, fsep = "/")
  now <- now[startsWith(basename(now), "data_")]
  recorded <- data_files_md5(state$source_md5)
  both <- intersect(now, names(recorded))
  changed <- setdiff(union(now, names(recorded)), both)
  moved <- both[!same_stat(state$home, both, state$source_stat)]
  moved <- moved[hash_files(file.path(state$home, moved)) != recorded[moved]]
  changed <- sort(c(changed, moved), method = "radix")
  if (length(changed) > 0) {
    stop(
      "These data files changed after pbn::initialize_project() ran: ",
      paste(changed, collapse = ", "),
      ". This R session may hold the old code. Run pbn::initialize_project() ",
      "again. ",
      done,
      call. = FALSE
    )
  }
  return(invisible(NULL))
}

# TRUE for each file `rel`, relative to `home`, whose size and mtime are
# those of the record `stat`.
same_stat <- function(home, rel, stat) {
  now <- file_stat(file.path(home, rel), rel)
  i <- match(rel, stat$file)
  same <- !is.na(i) & now$size == stat$size[i] & now$mtime == stat$mtime[i]
  return(same %in% TRUE)
}

# Sets state$data_md5 to the md5 of the files in data_raw now. A file with
# the size and mtime of the record keeps its md5, and each other file is
# hashed again. Nothing happens when data_raw did not exist at
# initialize_project().
refresh_data_md5 <- function() {
  if (is.null(state$data_md5)) {
    return(invisible(NULL))
  }
  files <- list.files(
    state$data_raw,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[!dir.exists(files)]
  rel <- basename(files)
  keep <- same_stat(state$data_raw, rel, state$data_stat)
  md5 <- unname(state$data_md5[rel])
  md5[!keep] <- hash_files(files[!keep])
  names(md5) <- rel
  state$data_md5 <- md5
  state$data_stat <- file_stat(files, rel)
  return(invisible(md5))
}

# Stops when git does not ignore the cache folders of the data `cached`.
check_cache_ignored <- function(cached, done) {
  folder <- paste0(state$version, "_cache_gitexclude")
  check_gitignored(
    state$home,
    file.path(folder, cached, "real", "sim_0001.qs2", fsep = "/"),
    done
  )
  return(invisible(NULL))
}

# Sets the cache context of the data functions `cached` back to NULL.
clear_cache <- function(info, cached) {
  for (nm in cached) {
    assign(".pbn_cache", NULL, envir = info$data_envs[[nm]])
  }
  return(invisible(NULL))
}

# The md5 of each file of the functions folder whose name starts with
# "data_", from the md5 that initialize_project() computed.
data_files_md5 <- function(source_md5) {
  return(source_md5[startsWith(basename(names(source_md5)), "data_")])
}

# CFG$data of the sourced environment, or NULL.
cfg_data <- function(env) {
  cfg <- get0("CFG", envir = env, inherits = FALSE)
  if (!is.list(cfg)) {
    return(NULL)
  }
  return(cfg[["data"]])
}

# The versions of R and pbn. The record beside each cache file holds the
# versions of the other loaded packages.
cache_versions <- function() {
  return(c(
    R = R.version.string,
    pbn = as.character(getNamespaceVersion("pbn"))
  ))
}

# Prints the cache line of each scenario and simulation of the run `r`, in
# the order of the plan. run_plan() calls it after a run with more than one
# worker, where a worker prints to no console.
print_cache_lines <- function(r, info) {
  for (nm in info$hidden) {
    found <- attr(r[[nm]], "cache", exact = TRUE)
    for (x in found) {
      cat(x[["line"]], "\n", sep = "")
    }
  }
  utils::flush.console()
  return(invisible(NULL))
}

# Stops when `home` is in a git repository, and git does not ignore each of
# the paths `rel`, relative to `home`. Each such path holds data for each
# person. Without git, or outside a git repository, it checks nothing.
check_gitignored <- function(home, rel, done) {
  git <- git_binary()
  if (!nzchar(git) || !in_git_repository(git, home)) {
    return(invisible(NULL))
  }
  for (f in rel) {
    status <- suppressWarnings(system2(
      git,
      c("-C", shQuote(home), "check-ignore", "-q", "--no-index", shQuote(f)),
      stdout = FALSE,
      stderr = FALSE
    ))
    if (!identical(as.integer(status), 0L)) {
      stop(
        "The project ",
        home,
        " is a git repository, and git does not ignore ",
        f,
        ". That file holds data for each person. Add the line *gitexclude* ",
        "to ",
        file.path(home, ".gitignore"),
        ". ",
        done,
        call. = FALSE
      )
    }
  }
  return(invisible(NULL))
}

# The path of git, or "" when git is not installed. In one place, so that
# the tests can mock it.
git_binary <- function() {
  return(Sys.which("git"))
}

# TRUE when the folder `path` is in the work tree of a git repository.
in_git_repository <- function(git, path) {
  inside <- suppressWarnings(system2(
    git,
    c("-C", shQuote(path), "rev-parse", "--is-inside-work-tree"),
    stdout = TRUE,
    stderr = FALSE
  ))
  return(identical(as.character(inside), "true"))
}
