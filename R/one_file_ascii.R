# The ASCII text of the file of to_one_file(). The file goes through a
# clipboard, which can change a byte that is not ASCII. pbn writes each such
# character of the included code as an escape, token by token on the parse
# data, and never changes the files of the project.

# The lines of the R file `rel` as ASCII. `lines` are its lines as UTF-8,
# without CR. Each character that is not ASCII:
# - in a string becomes its escape \uXXXX, or \UXXXXXXXX beyond the BMP. pbn
#   writes the whole string again from its value, so a raw string becomes a
#   string in double quotes with the same value.
# - in a comment becomes the text <U+XXXX>.
# - in a name stops, and the error names the file and the line.
# Stops when the file does not parse.
ascii_code <- function(lines, rel) {
  if (all(vapply(lines, is_ascii, logical(1)))) {
    return(lines)
  }
  exprs <- tryCatch(
    parse(text = lines, keep.source = TRUE, encoding = "UTF-8"),
    error = function(e) return(e)
  )
  if (inherits(exprs, "error")) {
    stop(
      rel,
      " does not parse, so pbn::to_one_file() cannot write it. R says: ",
      conditionMessage(exprs),
      call. = FALSE
    )
  }
  tokens <- utils::getParseData(exprs)
  tokens <- tokens[tokens$terminal, , drop = FALSE]
  # The last token first, so that the places of the earlier tokens hold.
  tokens <- tokens[order(-tokens$line1, -tokens$col1), , drop = FALSE]
  for (i in seq_len(nrow(tokens))) {
    tok <- tokens[i, , drop = FALSE]
    place <- token_place(lines, tok)
    text <- span_text(lines, place)
    if (is_ascii(text)) {
      next
    }
    new <- switch(
      tok$token,
      STR_CONST = escaped_string(text, rel, tok$line1),
      COMMENT = escaped_comment(text),
      stop(
        rel,
        ":",
        tok$line1,
        " has the name ",
        text,
        ", which is not ASCII. pbn::to_one_file() writes an ASCII file, and ",
        "R reads a name only as it is written. Give it an ASCII name.",
        call. = FALSE
      )
    )
    lines <- replace_span(lines, place, strsplit(new, "\n", fixed = TRUE)[[1]])
  }
  bad <- which(!vapply(lines, is_ascii, logical(1)))
  if (length(bad) > 0) {
    stop(
      rel,
      ":",
      bad[1],
      " holds a character that is not ASCII outside a string, a comment ",
      "and a name. pbn::to_one_file() writes an ASCII file. Remove it.",
      call. = FALSE
    )
  }
  return(lines)
}

# The place of the token `tok` of the parse data in `lines`: the first line,
# the first character, the last line and the last character.
token_place <- function(lines, tok) {
  return(c(
    tok$line1,
    col_to_byte(lines[tok$line1], tok$col1),
    tok$line2,
    col_to_byte(lines[tok$line2], tok$col2)
  ))
}

# The text of `lines` from the place `place` of token_place(), with "\n"
# between its lines.
span_text <- function(lines, place) {
  first <- place[[1]]
  last <- place[[3]]
  if (first == last) {
    return(substr(lines[first], place[[2]], place[[4]]))
  }
  return(paste(
    c(
      substr(lines[first], place[[2]], nchar(lines[first])),
      lines[seq_len(last - first - 1) + first],
      substr(lines[last], 1, place[[4]])
    ),
    collapse = "\n"
  ))
}

# The string constant `text` of R code, written again as a string in double
# quotes with only ASCII characters and the same value. Stops when the value
# is not valid UTF-8, for example after an escape \xE9, and names the file
# `rel` and the line `line`.
escaped_string <- function(text, rel, line) {
  value <- parse(text = text, keep.source = FALSE, encoding = "UTF-8")[[1]]
  codes <- if (validUTF8(value)) utf8ToInt(enc2utf8(value)) else NA
  if (anyNA(codes)) {
    stop(
      rel,
      ":",
      line,
      " holds a string that is not valid UTF-8, for example after an ",
      "escape such as \\xE9. pbn::to_one_file() writes each string as UTF-8. ",
      "Write the character with an escape such as \\u00e9.",
      call. = FALSE
    )
  }
  out <- vapply(codes, escaped_code, character(1))
  return(paste0("\"", paste(out, collapse = ""), "\""))
}

# One code point `x` of a string as ASCII text inside double quotes.
escaped_code <- function(x) {
  special <- c(
    "92" = "\\\\",
    "34" = "\\\"",
    "10" = "\\n",
    "13" = "\\r",
    "9" = "\\t"
  )
  key <- as.character(x)
  if (key %in% names(special)) {
    return(special[[key]])
  }
  if (x >= 32 && x < 127) {
    return(intToUtf8(x))
  }
  if (x <= 0xFFFF) {
    return(sprintf("\\u%04x", x))
  }
  return(sprintf("\\U%08x", x))
}

# The comment `text` with each character that is not ASCII as <U+XXXX>.
escaped_comment <- function(text) {
  codes <- utf8ToInt(enc2utf8(text))
  out <- vapply(
    codes,
    function(x) {
      return(if (x < 128) intToUtf8(x) else sprintf("<U+%04X>", x))
    },
    character(1)
  )
  return(paste(out, collapse = ""))
}
