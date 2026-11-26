# The rules no_plot_title, no_plot_subtitle and no_plot_caption of
# check_project(): a figure has no title, no subtitle and no caption, because
# its legend belongs in the manuscript. The set house_2026_11_25 brought
# them. Each rule parses each file and runs no code of it.

# The elements of a figure that the rules fail, each the end of the name of
# its rule.
plot_text_elements <- c("title", "subtitle", "caption")

# The calls that give a figure a title, a subtitle or a caption: the package
# that a call MAY name with `::` or `:::`, and a function with the arguments
# of the call. match.call() matches the arguments of a call to them as R
# does, by name, by a part of a name and by position. A call of the same name
# from another package passes.
plot_text_calls <- list(
  ggtitle = list(
    package = "ggplot2",
    args = function(label, subtitle) return(NULL)
  ),
  labs = list(
    package = "ggplot2",
    args = function(..., title, subtitle, caption) return(NULL)
  ),
  plot_annotation = list(
    package = "patchwork",
    args = function(
      title,
      subtitle,
      caption,
      tag_levels,
      tag_prefix,
      tag_suffix,
      tag_sep,
      theme
    ) {
      return(NULL)
    }
  )
)

# The element of the figure that each argument of plot_text_calls sets.
# Another argument, such as x, tag or tag_levels, sets none.
plot_text_args <- c(
  label = "title",
  title = "title",
  subtitle = "subtitle",
  caption = "caption"
)

# The rule no_plot_title.
rule_no_plot_title <- function(path) {
  return(plot_text_findings(path, "title"))
}

# The rule no_plot_subtitle.
rule_no_plot_subtitle <- function(path) {
  return(plot_text_findings(path, "subtitle"))
}

# The rule no_plot_caption.
rule_no_plot_caption <- function(path) {
  return(plot_text_findings(path, "caption"))
}

# The findings of the rule no_plot_<element> in the .R files of code_files(),
# the files of no_absolute_paths. A file that R cannot parse fails
# loc_per_file, so the rule skips it.
plot_text_findings <- function(path, element) {
  rule <- paste0("no_plot_", element)
  out <- list()
  for (f in sort(code_files(path), method = "radix")) {
    hits <- plot_text_hits(file.path(path, f))
    hits <- hits[hits$element == element, , drop = FALSE]
    if (nrow(hits) > 0) {
      out[[f]] <- project_row(
        rule,
        "FAIL",
        rep(f, nrow(hits)),
        paste0(f, ":", hits$line, ": ", hits$shown)
      )
    }
  }
  return(bind_rows(out))
}

# The places of the file `file` that give a figure a title, a subtitle or a
# caption, as a data.frame with the columns `line`, `col`, `element` and
# `shown`, in the order of the file:
# - an argument of a call of plot_text_calls that sets an element, such as
#   labs(title = "A") or ggtitle("A", "B"),
# - an assignment to <x>$labels$title, <x>$labels$subtitle or
#   <x>$labels$caption.
# A value NULL removes the element, and it passes. Empty for a file that R
# cannot parse.
plot_text_hits <- function(file) {
  none <- data.frame(
    line = integer(0),
    col = integer(0),
    element = character(0),
    shown = character(0)
  )
  # getParseText() needs the parse data as getParseData() gives it.
  data <- utils::getParseData(parse_or_null(file, keep_source = TRUE))
  if (is.null(data) || nrow(data) == 0) {
    return(none)
  }
  tokens <- data[order(data$line1, data$col1), , drop = FALSE]
  quoted <- tokens$token == "SYMBOL_FUNCTION_CALL"
  tokens$text[quoted] <- gsub("`", "", tokens$text[quoted], fixed = TRUE)
  found <- rbind(
    none,
    plot_text_call_hits(tokens, data),
    plot_label_assign_hits(tokens, data)
  )
  return(found[order(found$line, found$col), , drop = FALSE])
}

# The arguments of the calls of plot_text_calls in the parse data `tokens`
# that set an element, as rows of plot_text_hits(). `data` is the parse data
# as getParseData() gives it.
plot_text_call_hits <- function(tokens, data) {
  out <- list()
  rows <- which(
    tokens$token == "SYMBOL_FUNCTION_CALL" &
      tokens$text %in% names(plot_text_calls)
  )
  for (i in rows) {
    name <- tokens$text[i]
    pkg <- call_package(tokens, i)
    if (!is.na(pkg) && pkg != plot_text_calls[[name]]$package) {
      next
    }
    call_id <- tokens$parent[match(tokens$parent[i], tokens$id)]
    args <- plot_text_set(
      utils::getParseText(data, call_id),
      plot_text_calls[[name]]$args
    )
    if (length(args) == 0) {
      next
    }
    row <- match(call_id, tokens$id)
    out[[length(out) + 1]] <- data.frame(
      line = tokens$line1[row],
      col = tokens$col1[row],
      element = unname(plot_text_args[args]),
      shown = paste0(
        if (is.na(pkg)) "" else paste0(pkg, "::"),
        name,
        "(",
        args,
        " = ...)"
      )
    )
  }
  return(do.call(rbind, out))
}

# The arguments of plot_text_args that the call in the text `text` gives a
# value other than NULL, with the arguments of the function `args`. A `...`
# among the arguments of the call does not count. None when R cannot match
# the call, for example with an argument that `args` does not have.
plot_text_set <- function(text, args) {
  matched <- tryCatch(
    {
      call <- str2lang(text)
      dots <- vapply(
        as.list(call)[-1],
        function(a) return(identical(a, quote(...))),
        logical(1)
      )
      as.list(match.call(args, call[c(TRUE, !dots)]))[-1]
    },
    error = function(e) return(list())
  )
  given <- names(matched)[!vapply(matched, is.null, logical(1))]
  return(intersect(
    intersect(names(formals(args)), names(plot_text_args)),
    given
  ))
}

# The assignments with <-, <<-, =, -> or ->> to <x>$labels$title,
# <x>$labels$subtitle or <x>$labels$caption in the parse data `tokens`, as
# rows of plot_text_hits(). An assignment of NULL passes. `data` is the parse
# data as getParseData() gives it.
plot_label_assign_hits <- function(tokens, data) {
  out <- list()
  ops <- which(tokens$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))
  for (i in ops) {
    sides <- tokens$id[
      tokens$parent == tokens$parent[i] & tokens$token == "expr"
    ]
    if (length(sides) != 2) {
      next
    }
    right <- tokens$token[i] == "RIGHT_ASSIGN"
    target <- if (right) sides[2] else sides[1]
    value <- if (right) sides[1] else sides[2]
    text <- utils::getParseText(data, target)
    element <- labels_element(text)
    inner <- tokens$token[tokens$parent == value]
    if (is.na(element) || identical(inner, "NULL_CONST")) {
      next
    }
    row <- match(target, tokens$id)
    out[[length(out) + 1]] <- data.frame(
      line = tokens$line1[row],
      col = tokens$col1[row],
      element = element,
      shown = if (right) {
        paste("...", tokens$text[i], gsub("[[:space:]]+", " ", text))
      } else {
        paste(gsub("[[:space:]]+", " ", text), tokens$text[i], "...")
      }
    )
  }
  return(do.call(rbind, out))
}

# The element of plot_text_elements that the target `text` of an assignment
# names as <x>$labels$<element>, or NA.
labels_element <- function(text) {
  x <- tryCatch(str2lang(text), error = function(e) return(NULL))
  is_dollar <- function(e) {
    return(is.call(e) && identical(e[[1]], as.name("$")) && length(e) == 3)
  }
  if (!is_dollar(x) || !is_dollar(x[[2]])) {
    return(NA_character_)
  }
  if (!identical(as.character(x[[2]][[3]]), "labels")) {
    return(NA_character_)
  }
  element <- as.character(x[[3]])
  if (!element %in% plot_text_elements) {
    return(NA_character_)
  }
  return(element)
}
