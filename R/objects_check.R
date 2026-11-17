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

# Where the object `name` changed first, and how, from its fingerprints
# `before` and `after`: two strings, for example "CFG$vars" and "a new
# attribute 'index'". For a list, the place is its first top-level element
# that changed, in the order of the list after the change. The fingerprints
# hold the objects, so unserialize() gives them back, with each environment
# as an empty environment on both sides. It runs only after a change was
# found.
object_change <- function(name, before, after) {
  hook <- function(x) return(emptyenv())
  old <- unserialize(before, refhook = hook)
  new <- unserialize(after, refhook = hook)
  if (!is.list(old) || !is.list(new) || !change_same_attrs(old, new)) {
    return(c(name, change_how(old, new)))
  }
  nn <- names(new)
  if (is.null(names(old)) || is.null(nn) || anyDuplicated(nn) > 0) {
    return(change_by_position(name, old, new))
  }
  return(change_by_name(name, old, new))
}

# The attributes of `x`, without names and the self-reference of a
# data.table, which both sides of object_change() hold as an empty
# environment.
change_attrs <- function(x) {
  a <- attributes(x)
  return(a[setdiff(names(a), c("names", ".internal.selfref"))])
}

change_same_attrs <- function(a, b) {
  return(identical(change_attrs(a), change_attrs(b)))
}

# How the value `b` differs from `a`: a new, a gone or a changed attribute,
# or the value.
change_how <- function(a, b) {
  at <- change_attrs(a)
  bt <- change_attrs(b)
  added <- setdiff(names(bt), names(at))
  if (length(added) > 0) {
    return(paste0("a new attribute '", added[1], "'"))
  }
  gone <- setdiff(names(at), names(bt))
  if (length(gone) > 0) {
    return(paste0("the attribute '", gone[1], "' is gone"))
  }
  for (k in names(bt)) {
    if (!identical(at[[k]], bt[[k]])) {
      return(paste0("the attribute '", k, "' changed"))
    }
  }
  return("the value changed")
}

# The first change of the list `name` from `old` to `new`, element by
# element in order, for a list without unique names.
change_by_position <- function(name, old, new) {
  for (i in seq_len(max(length(old), length(new)))) {
    at <- paste0(name, "[[", i, "]]")
    if (i > length(old)) {
      return(c(at, "a new element"))
    }
    if (i > length(new)) {
      return(c(at, "the element is gone"))
    }
    if (!identical(old[[i]], new[[i]])) {
      return(c(at, change_how(old[[i]], new[[i]])))
    }
  }
  return(c(name, "the value changed"))
}

# The first change of the named list `name` from `old` to `new`, in the
# order of `new`, then an element that is gone.
change_by_name <- function(name, old, new) {
  for (k in names(new)) {
    at <- paste0(name, "$", k)
    if (!k %in% names(old)) {
      return(c(at, "a new element"))
    }
    if (!identical(old[[k]], new[[k]])) {
      return(c(at, change_how(old[[k]], new[[k]])))
    }
  }
  gone <- setdiff(names(old), names(new))
  if (length(gone) > 0) {
    return(c(paste0(name, "$", gone[1]), "the element is gone"))
  }
  return(c(name, "the order of the elements changed"))
}

# object_change() runs on a worker without pbn. It and its helpers live in
# one environment whose parent is the base environment, so they find each
# other there.
change_env <- new.env(parent = baseenv())
for (nm in c(
  "object_change",
  "change_attrs",
  "change_same_attrs",
  "change_how",
  "change_by_position",
  "change_by_name"
)) {
  local({
    f <- get(nm)
    environment(f) <- change_env
    assign(nm, f, envir = change_env)
  })
}
object_change <- change_env$object_change
rm(nm)

# The names of the objects of the fingerprints `before` and `after` that
# changed, and the place of the first change from `describe`, which is
# object_change(). It is an argument, because on a worker without pbn this
# function finds no function of pbn. NULL when no object changed.
objects_changed <- function(before, after, describe) {
  changed <- names(after)[!mapply(identical, before, after)]
  if (length(changed) == 0) {
    return(NULL)
  }
  first <- changed[1]
  where <- tryCatch(
    describe(first, before[[first]], after[[first]]),
    error = function(e) return(c(first, "the value changed"))
  )
  return(list(names = changed, where = where[1], how = where[2]))
}
environment(objects_changed) <- baseenv()

# The words of a change `found` of objects_changed(), for example "the
# object 'CFG' of the functions folder in place, at CFG$vars: a new
# attribute 'index'".
objects_change_text <- function(found) {
  return(paste0(
    if (length(found$names) == 1) "the object '" else "the objects '",
    paste(found$names, collapse = "', '"),
    "' of the functions folder in place, at ",
    found$where,
    ": ",
    found$how
  ))
}
environment(objects_change_text) <- baseenv()

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
  if (
    inherits(out, "pbn_failed_analysis") ||
      identical(unname(before), unname(after))
  ) {
    return(out)
  }
  found <- run$changed(before, after, run$describe)
  return(structure(
    list(
      message = paste0(
        "The analysis changed ",
        run$change_text(found),
        ". Each later analysis in the same ",
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
  if (
    inherits(out, "pbn_data_error") || identical(unname(before), unname(after))
  ) {
    return(out)
  }
  found <- run$changed(before, after, run$describe)
  return(structure(
    list(
      data = name,
      message = paste0(
        "it changed ",
        run$change_text(found),
        ". Each later data function and ",
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

# The fingerprint of the objects of the functions folder in this R session,
# before a call of project code in the main session: the output function of
# make_outputs() or an expect_ function. NULL without initialize_project()
# or without such an object.
main_fingerprint <- function() {
  names <- checked_names(state$env)
  if (length(names) == 0) {
    return(NULL)
  }
  return(objects_fingerprint(state$env, names))
}

# The change of the objects of the functions folder since the fingerprint
# `before` of main_fingerprint(), see objects_changed(), or NULL.
main_change <- function(before) {
  if (is.null(before)) {
    return(NULL)
  }
  after <- objects_fingerprint(state$env, names(before))
  return(objects_changed(before, after, object_change))
}
