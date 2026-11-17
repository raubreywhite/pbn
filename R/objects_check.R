# The check that an analysis does not change an object of the functions
# folder, for example CFG. plnr runs many analyses in one R session, so a
# change would reach each later analysis of that session, and a result would
# depend on the analyses before it.

# The address of each object of the environment `env`, named by the object.
object_addresses <- function(env) {
  objects <- ls(env, all.names = TRUE)
  return(vapply(
    objects,
    function(o) return(data.table::address(get(o, envir = env))),
    character(1)
  ))
}

# The names of the objects that sourcing the functions folder made or
# changed: each object of `env` that is new, or whose address differs from
# `before`, the value of object_addresses() before the source.
defined_names <- function(env, before) {
  after <- object_addresses(env)
  new <- is.na(before[names(after)]) | before[names(after)] != after
  return(names(after)[new])
}

# The objects that each analysis MUST NOT change: each object that the
# functions folder defined, see defined_names(), that is not a function, not
# a plnr plan and not .Random.seed. A random draw while the folder is
# sourced changes .Random.seed, and each analysis sets its seed. Empty
# without initialize_project().
checked_names <- function(env) {
  if (!is.environment(env)) {
    return(character(0))
  }
  objects <- intersect(
    setdiff(state$sourced, ".Random.seed"),
    ls(env, all.names = TRUE)
  )
  keep <- vapply(
    objects,
    function(o) {
      x <- get(o, envir = env)
      return(!is.function(x) && !inherits(x, "Plan"))
    },
    logical(1)
  )
  return(objects[keep])
}

# The fingerprint of each object `names` of `env`: its bytes from
# serialize(), named by the object. The bytes hold the values and the
# attributes, such as the index that a data.table subset adds in place.
# Two parts do not count:
# - a function inside a list. R compiles a function after some calls, and
#   that changes its bytes.
# - the contents of an environment, for example that of a formula.
objects_fingerprint <- function(env, names) {
  out <- lapply(names, function(nm) {
    x <- get0(nm, envir = env, inherits = FALSE)
    if (is.list(x)) {
      x <- rapply(
        x,
        function(f) return(NULL),
        classes = "function",
        how = "replace"
      )
    }
    return(serialize(x, NULL, xdr = FALSE, refhook = function(e) return("")))
  })
  names(out) <- names
  return(out)
}
environment(objects_fingerprint) <- baseenv()

# The fingerprint of the checked objects of the run context `run` before an
# analysis: the one after the last analysis of this R session, or a new one.
# NULL when there is nothing to check.
objects_print <- function(run) {
  if (is.null(run) || length(run$checked) == 0) {
    return(NULL)
  }
  if (!is.null(run$fp)) {
    return(run$fp)
  }
  return(run$fingerprint(run$env, run$checked))
}
environment(objects_print) <- baseenv()

# The value of an analysis after the shape check, `out`: `out`, or a
# pbn_failed_analysis when a checked object changed from the fingerprint
# `before`. The fingerprint after the analysis stays in `run`, so the next
# analysis compares with it.
objects_value <- function(out, run, before) {
  if (is.null(before)) {
    return(out)
  }
  after <- run$fingerprint(run$env, run$checked)
  run$fp <- after
  changed <- names(after)[!mapply(identical, before, after)]
  if (length(changed) == 0 || inherits(out, "pbn_failed_analysis")) {
    return(out)
  }
  return(structure(
    list(
      message = paste0(
        "The analysis changed the object '",
        paste(changed, collapse = "', '"),
        "' of the functions folder in place. Each later analysis in the same ",
        "R session would see the change, so a result would depend on the ",
        "analyses that ran before it in that session. Change a copy, for ",
        "example x <- data.table::copy(CFG$table). A data.table subset with ",
        "a logical, for example CFG$table[group == \"a\"], adds an index ",
        "to the table in place. Copy the table first, or set ",
        "options(datatable.auto.index = FALSE) in the project."
      )
    ),
    class = "pbn_failed_analysis"
  ))
}
environment(objects_value) <- baseenv()

# The data `out` of the data function `name` after the check of the
# objects: `out`, or a pbn_data_error when a checked object changed from the
# fingerprint `before`. So a data function that changes CFG fails each
# analysis that gets the data, and the change never becomes the fingerprint
# that the next analysis compares with unnoticed.
data_checked <- function(out, run, before, name) {
  if (is.null(before)) {
    return(out)
  }
  after <- run$fingerprint(run$env, run$checked)
  run$fp <- after
  changed <- names(after)[!mapply(identical, before, after)]
  if (length(changed) == 0 || inherits(out, "pbn_data_error")) {
    return(out)
  }
  return(structure(
    list(
      data = name,
      message = paste0(
        "it changed the object '",
        paste(changed, collapse = "', '"),
        "' of the functions folder in place. Each later data function and ",
        "analysis in the same R session would see the change. Change a ",
        "copy, for example x <- data.table::copy(CFG$table). ?add_analyses ",
        "gives the rules."
      )
    ),
    class = "pbn_data_error"
  ))
}
environment(data_checked) <- baseenv()

# The check that an analysis leaves no new object in the global
# environment. Without `out`, the names of the global environment before
# the analysis: those after the last analysis of this R session, or new
# ones. With `out`, the value of the analysis: `out`, or a
# pbn_failed_analysis when the global environment holds a name that was not
# in `before`. .Random.seed does not count, because each seed makes it.
new_globals <- function(run, before, out) {
  if (missing(out)) {
    if (is.null(run)) {
      return(NULL)
    }
    if (!is.null(run$gl)) {
      return(run$gl)
    }
    return(ls(globalenv(), all.names = TRUE, sorted = FALSE))
  }
  if (is.null(before)) {
    return(out)
  }
  after <- ls(globalenv(), all.names = TRUE, sorted = FALSE)
  run$gl <- after
  # The same names in the same order is the common case, and the cheapest.
  if (identical(after, before) || inherits(out, "pbn_failed_analysis")) {
    return(out)
  }
  added <- after[!after %in% before]
  added <- added[added != ".Random.seed"]
  if (length(added) == 0) {
    return(out)
  }
  return(structure(
    list(
      message = paste0(
        "The analysis left the new object '",
        paste(sort(added), collapse = "', '"),
        "' in the global environment. Each later analysis in the same R ",
        "session could find it, so a result would depend on the analyses ",
        "that ran before it there. Keep the object inside the function: do ",
        "not assign it to the global environment with <<- or assign()."
      )
    ),
    class = "pbn_failed_analysis"
  ))
}
environment(new_globals) <- baseenv()
