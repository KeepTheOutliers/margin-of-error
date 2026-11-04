# Builds the static site into _site/: a plain web page with no R in the browser, so it
# loads in a second or two and can't be overloaded (GitHub Pages only serves files).
#
#   index.html + lib/  app.R's page layout, rendered once (Bootstrap, slider, dropdowns, Plotly;
#                      no Shiny server connection)
#   data.json          the data, plus the pieces R renders that depend only on the data
#                      (Polling Averages rows, poll rows, history tiles and chart, About figures)
#   app.js             the browser-side app (web/app.js): scenario logic, horseshoe, map,
#                      ranked chart, sliders, history tab, list filters
#
# Run from the repo root:  Rscript R/build_static.R
suppressPackageStartupMessages({
  library(jsonlite)
  library(htmltools)
})

unlink("_site", recursive = TRUE)
dir.create("_site")

# Load the app with the static-build switch on (drops the server-only Refresh button).
env <- new.env()
assign("BUILD_STATIC", TRUE, envir = env)
sys.source("app.R", envir = env, keep.source = FALSE)
d <- env$INIT

# ---- 1. The page --------------------------------------------------------------------
# Everything except Shiny's server connection and the htmlwidgets runtime (app.js draws
# the Plotly charts directly with the bundled plotly.js).
page <- tagList(env$ui, suppressDependencies("shiny", "htmlwidgets", "plotly-binding", "crosstalk"))
save_html(page, file = "_site/index.html", libdir = "lib")

html <- readLines("_site/index.html", warn = FALSE, encoding = "UTF-8")
shim <- c(
  "<script>",
  "// Stand-in for Shiny's browser API. The page's own scripts and generated buttons call",
  "// Shiny.setInputValue / Shiny.addCustomMessageHandler; app.js answers them instead of a server.",
  "window.Shiny = { _h: {}, _q: [],",
  "  setInputValue: function (n, v) { if (window.App) App.input(n, v); else this._q.push([n, v]); },",
  "  addCustomMessageHandler: function (t, f) { this._h[t] = f; },",
  "  // bslib's components subclass these and register them; nothing listens here.",
  "  InputBinding: class {}, OutputBinding: class {},",
  "  inputBindings: { register: function () {} }, outputBindings: { register: function () {} },",
  "  bindAll: function () {}, unbindAll: function () {}, initializeInputs: function () {} };",
  "</script>")
h <- grep("<head>", html, fixed = TRUE)[1]
html <- append(html, shim, after = h)
# Link-preview tags (shared with the Pages wrapper page).
meta <- grep('<meta (name="description"|property="og:|name="twitter:)', readLines("site/index.html", warn = FALSE), value = TRUE)
t <- grep("</title>", html, fixed = TRUE)[1]
html <- append(html, meta, after = t)
b <- tail(grep("</body>", html, fixed = TRUE), 1)
html <- append(html, c('<script src="lib/plotly/plotly-latest.min.js"></script>', '<script src="app.js"></script>'), after = b - 1)
# Shiny's stylesheet only (labels, slider skin, outputs); its JavaScript stays out.
dir.create("_site/lib/shiny", recursive = TRUE, showWarnings = FALSE)
file.copy(system.file("www/shared/shiny.min.css", package = "shiny"), "_site/lib/shiny/")
t2 <- grep("</title>", html, fixed = TRUE)[1]
html <- append(html, '<link href="lib/shiny/shiny.min.css" rel="stylesheet" />', after = t2)
writeLines(html, "_site/index.html", useBytes = TRUE)
# plotly.js itself, from the R plotly package (Shiny would attach it when a chart first renders).
dir.create("_site/lib/plotly", recursive = TRUE, showWarnings = FALSE)
file.copy(system.file("htmlwidgets/lib/plotlyjs/plotly-latest.min.js", package = "plotly"), "_site/lib/plotly/")
writeLines(html, "_site/index.html", useBytes = TRUE)

# ---- 2. The data ----------------------------------------------------------------------
fmt_ts <- function(x) format(as.POSIXct(x), "%Y-%m-%d %H:%M")
races <- d$seats |> dplyr::left_join(d$polls, by = "abb")

# Polling Averages: one pre-rendered row per polled race, closest first.
polled <- races |> dplyr::filter(!is.na(poll_margin)) |> dplyr::arrange(abs(poll_margin))
avg_rows <- lapply(seq_len(nrow(polled)), function(i) {
  list(abb = polled$abb[i],
       html = as.character(env$avg_row(polled[i, ], d$race_polls[d$race_polls$abb == polled$abb[i], ])))
})

# All Latest Polls: one pre-rendered row per poll, newest first.
pl <- d$race_polls |>
  dplyr::left_join(dplyr::select(d$seats, abb, state), by = "abb") |>
  dplyr::left_join(dplyr::select(d$polls, abb, opp_name, opp_party, rep_name), by = "abb") |>
  dplyr::arrange(dplyr::desc(date), state)
poll_rows <- lapply(seq_len(nrow(pl)), function(i) {
  list(abb = pl$abb[i], pollster = pl$pollster[i], in_average = isTRUE(pl$in_average[i]),
       date = format(pl$date[i]), day = env$fmt_day(pl$date[i]),
       html = as.character(env$poll_row(pl[i, ])))
})

# Data-only outputs, taken from the app's own server code.
app <- shiny::shinyApp(env$ui, env$server)
pre <- list()
shiny::testServer(app, {
  session$setInputs(miss_mode = "none", cycles = character(0), types = c("Senate", "President"), shift = 0,
                    seat_view = "horseshoe", ind_with_d = FALSE, hm_view = "rows", hm_type = "Both",
                    hm_years = as.character(env$CYCLES), hm_2026_only = TRUE)
  pre$hm_tiles <<- output$hm_tiles$html
  pre$natl_bias <<- output$natl_bias
  pre$about_unpolled <<- output$about_unpolled
  pre$about_inds <<- output$about_inds
  pre$about_bias <<- output$about_bias
  pre$about_cover <<- output$about_cover
})
nb <- fromJSON(pre$natl_bias, simplifyVector = FALSE)$x
pre$natl_bias <- toJSON(list(data = nb$data, layout = nb$layout, config = nb$config), auto_unbox = TRUE, null = "null", na = "null", digits = NA)

out <- list(
  fetched_at = fmt_ts(d$polls$fetched_at[1]),
  seats = d$seats[, c("abb", "state", "incumbent_party", "note")],
  polls = d$polls |> dplyr::mutate(latest_poll = format(latest_poll)) |>
    dplyr::select(abb, poll_margin, opp_name, opp_party, opp_pct, rep_name, rep_pct, n_polls_in_avg, latest_poll),
  errors = d$errors[, c("cycle", "abb", "race_type", "poll_margin", "actual_margin", "error")],
  # 2026 vote counts so far (the "2026 results" scenario); absent before Election Day
  results = if (is.null(d$results)) list() else d$results |> dplyr::mutate(fetched_at = fmt_ts(fetched_at)) |>
    dplyr::select(abb, opp_name, opp_votes, rep_name, rep_votes, margin, eevp, called, winner, fetched_at),
  states = data.frame(abb = state.abb, name = state.name, lon = state.center$x, lat = state.center$y),
  party_icon = as.list(env$PARTY_ICON_PATH),
  avg_header = as.character(env$avg_header()),
  avg_rows = avg_rows,
  poll_header = as.character(env$poll_header()),
  poll_rows = poll_rows,
  pollsters = sort(unique(d$race_polls$pollster)),
  hm_tiles = pre$hm_tiles,
  natl_bias = pre$natl_bias,
  about = pre[c("about_unpolled", "about_inds", "about_bias", "about_cover")],
  ev_html = as.character(env$ev_list_ui(d$seats, d$polls, d$early)),
  ev_updated = if (is.null(d$early)) "" else paste0("States reporting through ", env$fmt_short(max(d$early$updated, na.rm = TRUE)), ".")
)
write_json(out, "_site/data.json", auto_unbox = TRUE, na = "null", null = "null", digits = NA, dataframe = "rows", json_verbatim = TRUE)

# ---- 3. The app and assets ------------------------------------------------------------
file.copy("web/app.js", "_site/app.js", overwrite = TRUE)
file.copy(list.files("www", full.names = TRUE), "_site", overwrite = TRUE)   # logo and site icons
file.copy("site/og.png", "_site/og.png", overwrite = TRUE)
cat("Built _site/ (", round(sum(file.size(list.files("_site", recursive = TRUE, full.names = TRUE))) / 1e6, 1), "MB )\n")
