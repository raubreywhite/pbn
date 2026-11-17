# The display of the progress of run_plan(): one line for each scenario.
# It is a progressr handler, so the counts from the workers reach it through
# progressr and future. Its state is an environment, see new_display().

# The rules of the display, in seconds and as shares of a scenario:
# - quiet: in a log, nothing is shown before the run has lasted this long.
# - beat_seconds and beat_share: in a log, a running scenario gets a line at
#   most every beat_seconds, and only when beat_share more of it is done.
# - redraw: in a terminal, the display is drawn again at most this often.
display_rules <- function() {
  return(list(quiet = 60, beat_seconds = 60, beat_share = 0.1, redraw = 0.2))
}

# How the display shows the progress:
# - "ansi": an interactive terminal. The lines are drawn again in place, with
#   ANSI moves of the cursor.
# - "line": another interactive console, for example that of RStudio or
#   RGui. One line is drawn again in place with a carriage return.
# - "log": no interactive session, for example Rscript, knitr, testthat and
#   R CMD check. Lines are only added, see display_log().
display_mode <- function() {
  if (!console_session()) {
    return("log")
  }
  if (identical(Sys.getenv("RSTUDIO"), "1") || nzchar(Sys.getenv("POSITRON"))) {
    return("line")
  }
  if (isatty(stdout()) && ansi_terminal()) {
    return("ansi")
  }
  return("line")
}

# TRUE in an interactive session that is not knitr or testthat.
console_session <- function() {
  return(
    interactive() &&
      !isTRUE(getOption("knitr.in.progress")) &&
      !identical(Sys.getenv("TESTTHAT"), "true")
  )
}

# TRUE when the terminal reads ANSI codes: a TERM other than "dumb". On
# Windows, only Windows Terminal, which sets WT_SESSION. The old console of
# Windows may not read them.
ansi_terminal <- function() {
  if (.Platform$OS.type == "windows") {
    return(nzchar(Sys.getenv("WT_SESSION")))
  }
  term <- Sys.getenv("TERM")
  return(nzchar(term) && term != "dumb")
}

# The time now, in seconds. The workers run on this computer, so their
# Sys.time() is on the same clock.
display_now <- function() {
  return(as.numeric(Sys.time()))
}

# The state of the display of one run. `scenarios` is plan_scenarios() of
# the plan: the scenarios in the order of the plan, with their number of
# analyses that count.
new_display <- function(scenarios, workers, started, mode) {
  d <- new.env(parent = emptyenv())
  n <- nrow(scenarios)
  d$mode <- mode
  # A log goes to the output, which a sink can take. A console display goes
  # to stderr, because progressr holds back the output while it is shown.
  d$stream <- if (mode == "log") "stdout" else "stderr"
  d$scenario <- scenarios$scenario
  d$total <- scenarios$analyses
  d$done <- rep(0, n)
  d$start <- rep(NA_real_, n)
  d$finished <- rep(NA_real_, n)
  d$details <- rep(list(list()), n)
  d$logged <- rep(NA_real_, n)
  d$logged_share <- rep(0, n)
  d$logged_detail <- rep(0, n)
  d$logged_key <- rep(NA_character_, n)
  d$logged_done <- rep(FALSE, n)
  d$t_start <- display_now()
  d$header <- paste0(
    "pbn: ",
    n,
    if (n == 1) " scenario, " else " scenarios, ",
    workers,
    if (workers == 1) " worker" else " workers",
    ", started ",
    format(started, "%H:%M")
  )
  d$header_shown <- FALSE
  d$drawn <- 0
  d$last_draw <- -Inf
  d$width <- max(20, getOption("width", 80))
  return(d)
}

# The progressr handler of the display `d`. It shows every progression, so
# times and interval are fixed here and do not come from the options of
# progressr.
display_handler <- function(d) {
  reporter <- list(
    initiate = function(config, state, progression, ...) {
      return(display_show(d, display_now(), force = TRUE))
    },
    update = function(config, state, progression, ...) {
      return(display_update(d, progression))
    },
    finish = function(config, state, progression, ...) {
      return(display_finish(d))
    },
    hide = function(config, state, progression, ...) {
      return(display_hide(d))
    },
    unhide = function(config, state, progression, ...) {
      return(display_show(d, display_now(), force = TRUE))
    },
    interrupt = function(config, state, progression, ...) {
      return(display_interrupt(d))
    }
  )
  return(progressr::make_progression_handler(
    "pbn",
    reporter,
    enable = TRUE,
    times = Inf,
    interval = 0,
    intrusiveness = 1,
    clear = FALSE,
    target = "terminal"
  ))
}

# Adds the progression `p` to the display `d` and shows it:
# - pbn_done, the number of finished analyses of each scenario, and pbn_t0,
#   the time when the worker started them.
# - pbn_ended, the analyses whose text of pbn::progress() ends.
# - pbn_detail, a text of pbn::progress().
display_update <- function(d, p) {
  now <- display_now()
  done <- p[["pbn_done"]]
  i <- match(names(done), d$scenario)
  keep <- !is.na(i)
  i <- i[keep]
  d$done[i] <- d$done[i] + done[keep]
  t0 <- p[["pbn_t0"]]
  if (length(i) > 0 && !is.null(t0)) {
    d$start[i] <- pmin(d$start[i], t0, na.rm = TRUE)
  }
  newly <- i[d$done[i] >= d$total[i] & is.na(d$finished[i])]
  d$finished[newly] <- now
  d$details[newly] <- list(list())
  ended <- p[["pbn_ended"]]
  for (k in seq_along(ended)) {
    j <- match(names(ended)[k], d$scenario)
    if (!is.na(j)) {
      d$details[[j]][[ended[[k]]]] <- NULL
    }
  }
  detail <- p[["pbn_detail"]]
  j <- match(detail[["scenario"]], d$scenario)
  if (length(j) == 1 && !is.na(j) && is.na(d$finished[j])) {
    detail$at <- now
    d$details[[j]][[detail[["key"]]]] <- detail
    d$start[j] <- min(d$start[j], detail[["t0"]], na.rm = TRUE)
  }
  return(display_show(d, now, force = length(newly) > 0))
}

# Shows the display `d` at the time `now`. In a console the display is drawn
# again at most every `redraw` seconds, unless `force`.
display_show <- function(d, now, force = FALSE) {
  if (d$mode == "log") {
    return(display_log(d, now))
  }
  if (!force && now - d$last_draw < display_rules()$redraw) {
    return(invisible(NULL))
  }
  d$last_draw <- now
  if (d$mode == "ansi") {
    lines <- fit_width(display_lines(d, now), d$width)
    up <- if (d$drawn > 0) paste0("\033[", d$drawn, "A\r") else ""
    display_write(d, up, paste0("\033[2K", lines, "\n", collapse = ""))
    d$drawn <- length(lines)
  } else {
    line <- fit_width(display_one_line(d, now), d$width)
    display_write(d, "\r", formatC(line, width = -(d$width - 1)))
    d$drawn <- 1
  }
  utils::flush.console()
  return(invisible(NULL))
}

# Writes the text `...` of the display `d` to its stream, at the time of the
# call, so that a sink of that time takes the output.
display_write <- function(d, ...) {
  file <- if (identical(d$stream, "stderr")) stderr() else ""
  cat(..., sep = "", file = file)
  return(invisible(NULL))
}

# Removes the display from the console, so that other output can go there.
display_hide <- function(d) {
  if (d$drawn == 0 || d$mode == "log") {
    return(invisible(NULL))
  }
  if (d$mode == "ansi") {
    display_write(d, "\033[", d$drawn, "A\r\033[J")
  } else {
    display_write(d, "\r", strrep(" ", d$width - 1), "\r")
  }
  d$drawn <- 0
  return(invisible(NULL))
}

# Shows the last state of the display. In the "line" mode, the one line
# gives way to the lines of every scenario.
display_finish <- function(d) {
  now <- display_now()
  if (d$mode == "line") {
    display_hide(d)
    lines <- fit_width(display_lines(d, now), d$width)
    display_write(d, paste0(lines, "\n"))
    d$drawn <- 0
    return(invisible(NULL))
  }
  return(display_show(d, now, force = TRUE))
}

# After an error or an interrupt, the next output starts on a new line.
display_interrupt <- function(d) {
  if (d$mode == "line" && d$drawn > 0) {
    display_write(d, "\n")
    d$drawn <- 0
  }
  return(invisible(NULL))
}

# The number of analyses of each scenario that the display did not get,
# named by the scenario.
display_rest <- function(d) {
  return(stats::setNames(pmax(d$total - d$done, 0), d$scenario))
}

# The lines of the display at the time `now`: the header and one line for
# each scenario, with aligned columns.
display_lines <- function(d, now) {
  name <- formatC(d$scenario, width = -max(nchar(d$scenario)))
  w <- nchar(format(max(d$total)))
  count <- paste0(
    formatC(d$done, width = w),
    "/",
    formatC(d$total, width = -w),
    " analyses"
  )
  time <- vapply(
    seq_along(d$scenario),
    function(i) return(scenario_time(d, i, now)),
    character(1)
  )
  note <- vapply(
    seq_along(d$scenario),
    function(i) return(scenario_note(d, i, now)),
    character(1)
  )
  lines <- paste0("  ", name, "  ", count, "  ", formatC(time, width = 7))
  lines <- ifelse(nzchar(note), paste0(lines, "  ", note), lines)
  return(c(d$header, lines))
}

# The one line of the "line" mode: the time since the start, the count of
# each scenario, and the latest text of pbn::progress().
display_one_line <- function(d, now) {
  parts <- paste0(d$scenario, " ", d$done, "/", d$total)
  out <- paste0(
    "pbn: ",
    format_duration(now - d$t_start),
    " | ",
    paste(parts, collapse = " | ")
  )
  latest <- NULL
  for (i in seq_along(d$scenario)) {
    det <- latest_detail(d, i)
    if (!is.null(det) && (is.null(latest) || det$at > latest$at)) {
      latest <- det
    }
  }
  if (!is.null(latest)) {
    out <- paste0(out, " | ", latest$scenario, ": ", latest$text)
  }
  return(out)
}

# The elapsed time of scenario `i`: from its start to its end, or to `now`.
# Empty before it starts.
scenario_time <- function(d, i, now) {
  if (is.na(d$start[i])) {
    return("")
  }
  end <- if (is.na(d$finished[i])) now else d$finished[i]
  return(format_duration(end - d$start[i]))
}

# The note of scenario `i`: "done", the latest text of pbn::progress(),
# "waiting", or the time left. The time left comes from the finished
# analyses of the scenario, after at least 3 and at least 10% of them.
scenario_note <- function(d, i, now) {
  if (!is.na(d$finished[i])) {
    return("done")
  }
  det <- latest_detail(d, i)
  if (!is.null(det)) {
    left <- det$left - (now - det$at)
    if (is.na(left)) {
      return(det$text)
    }
    return(paste0(det$text, ", about ", format_left(left), " left"))
  }
  if (is.na(d$start[i])) {
    return("waiting")
  }
  left <- progress_left(now - d$start[i], d$done[i], d$total[i])
  if (is.na(left)) {
    return("")
  }
  return(paste0("about ", format_left(left), " left"))
}

# The text of pbn::progress() of scenario `i` that came last, or NULL.
latest_detail <- function(d, i) {
  dets <- d$details[[i]]
  if (length(dets) == 0) {
    return(NULL)
  }
  at <- vapply(dets, function(x) return(x$at), numeric(1))
  return(dets[[which.max(at)]])
}

# The lines of a log at the time `now`. Nothing before the run has lasted
# `quiet` seconds. Then the header once, and one line for each scenario
# that has started or finished. After that, one line when a scenario
# finishes, and one line for a running scenario at most every
# `beat_seconds`, when at least `beat_share` more of it is done, counted in
# analyses or in the latest pbn::progress() of the scenario.
display_log <- function(d, now) {
  rules <- display_rules()
  if (now - d$t_start < rules$quiet) {
    return(invisible(NULL))
  }
  lines <- character(0)
  first <- !d$header_shown
  if (first) {
    lines <- d$header
    d$header_shown <- TRUE
  }
  for (i in seq_along(d$scenario)) {
    lines <- c(lines, log_scenario(d, i, now, first, rules))
  }
  if (length(lines) > 0) {
    display_write(d, paste0(lines, "\n"))
  }
  return(invisible(NULL))
}

# The line of a log for scenario `i` at the time `now`, or none. `first` is
# TRUE for the lines under the header. The display remembers what it showed.
log_scenario <- function(d, i, now, first, rules) {
  if (d$logged_done[i] || (is.na(d$start[i]) && is.na(d$finished[i]))) {
    return(character(0))
  }
  if (!is.na(d$finished[i])) {
    d$logged_done[i] <- TRUE
    return(log_line(d, i, now))
  }
  det <- log_detail(d, i)
  key <- det$key
  share <- d$done[i] / d$total[i]
  detail <- det$share
  gain <- max(share - d$logged_share[i], detail - d$logged_detail[i])
  since <- if (is.na(d$logged[i])) d$start[i] else d$logged[i]
  due <- gain >= rules$beat_share && now - since >= rules$beat_seconds
  if (!first && !due) {
    return(character(0))
  }
  d$logged[i] <- now
  d$logged_share[i] <- share
  d$logged_detail[i] <- detail
  d$logged_key[i] <- key
  return(log_line(d, i, now))
}

# The latest text of pbn::progress() of scenario `i`, with its key and its
# share done, or a key NA and a share 0. A text of another analysis than the
# one of the last line of the log counts its share from 0.
log_detail <- function(d, i) {
  det <- latest_detail(d, i)
  if (is.null(det)) {
    det <- list(key = NA_character_, share = 0)
  }
  if (!identical(det$key, d$logged_key[i])) {
    d$logged_detail[i] <- 0
  }
  return(det)
}

# One line of a log for scenario `i`, for example "pbn: null 252/400
# analyses, 12m 05s, about 7m left".
log_line <- function(d, i, now) {
  head <- paste0(
    "pbn: ",
    d$scenario[i],
    " ",
    d$done[i],
    "/",
    d$total[i],
    " analyses, "
  )
  if (!is.na(d$finished[i])) {
    return(paste0(head, "done in ", scenario_time(d, i, now)))
  }
  note <- scenario_note(d, i, now)
  return(paste0(
    head,
    scenario_time(d, i, now),
    if (nzchar(note)) paste0(", ", note) else ""
  ))
}

# `lines` cut to `width` - 1 characters, so that a console does not wrap a
# line. The ANSI moves of the display count lines.
fit_width <- function(lines, width) {
  long <- nchar(lines) > width - 1
  lines[long] <- substr(lines[long], 1, width - 1)
  return(lines)
}

# A duration of `s` seconds, for example "12s", "6m 12s" or "1h 05m".
format_duration <- function(s) {
  s <- max(0, round(s))
  if (s < 60) {
    return(paste0(s, "s"))
  }
  if (s < 3600) {
    return(sprintf("%dm %02ds", s %/% 60, s %% 60))
  }
  return(sprintf("%dh %02dm", s %/% 3600, (s %% 3600) %/% 60))
}

# A time left of `s` seconds, rounded up: to 10 seconds below 1 minute, to
# whole minutes below 1 hour, for example "40s", "7m" or "1h 05m".
format_left <- function(s) {
  s <- max(0, s)
  if (s < 60) {
    return(paste0(max(10, ceiling(s / 10) * 10), "s"))
  }
  if (s < 3600) {
    return(paste0(ceiling(s / 60), "m"))
  }
  m <- ceiling(s / 60)
  return(sprintf("%dh %02dm", m %/% 60, m %% 60))
}
