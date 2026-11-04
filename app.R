# 2026 Senate scenario dashboard.
#
# Not a forecast model: takes Silver Bulletin's published polling averages as given and
# asks "what if the polls miss the way they did before?" using historical
# state-level polling error. Data comes from R/fetch_data.R (cached in data/).
#
# Run:  shiny::runApp("senate_dashboard")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(plotly)
})

# In the browser build (Shinylive) the app runs on bundled data and cannot fetch;
# the fetch script is only loaded when running under regular R.
IS_BROWSER <- identical(R.version$os, "emscripten")
# Set by R/build_static.R, which renders this UI into the static (plain JavaScript) site.
IS_STATIC <- exists("BUILD_STATIC") && isTRUE(BUILD_STATIC)
# Early Vote tab (and its credits) hidden for now; set TRUE to bring it back. The data still refreshes.
SHOW_EARLY_VOTE <- FALSE
# Ranked-margins (violin) view hidden for now: the toggle is hidden, so the horseshoe always shows. TRUE restores it.
SHOW_RANKED <- FALSE
if (!IS_BROWSER) source("R/fetch_data.R", local = TRUE)
# The two constants the app needs from that script, for the browser build.
if (!exists("SEATS_NOT_UP")) SEATS_NOT_UP <- c(D = 34, R = 31)
if (!exists("state_lookup")) state_lookup <- tibble::tibble(state = state.name, abb = state.abb)

# ---- Palette ------------------------------------------------------------------
# Slate blue <-> brick with a warm-gray midpoint. Each arm is one hue stepped at
# OKLCH L .40/.52/.64/.78 with chroma easing off toward the middle, so near-even
# races (D+0-2 vs R+0-2) read as close cousins rather than opposites; the outer
# steps keep the hue and lightness separation the dataviz validator checked.
SURFACE    <- "#fbf8f1"
INK        <- "#1b1a17"
INK_2      <- "#55524b"
INK_MUTED  <- "#8a867c"
GRID       <- "#e4e1da"
COL_NOT_UP <- "#e8e6e2"
D_DARK <- "#1b4a76"; D_MID <- "#316ca5"; D_TINT <- "#e7f1fc"
R_DARK <- "#792b13"; R_MID <- "#ac4323"; R_TINT <- "#feebe6"

# Bins 1-8 run D to R; 9-10 are races an independent (who doesn't caucus with
# either party, e.g. Osborn) leads. Teal can't stay clear of all eight party
# shades for every color-vision type, so independents also get a hatch in the
# horseshoe and "Ind" in every label.
BINS <- tibble(
  bin = c("D+10 or more", "D+5 to 10", "D+2 to 5", "D+0 to 2",
          "R+0 to 2", "R+2 to 5", "R+5 to 10", "R+10 or more",
          "Ind+5 or more", "Ind+0 to 5"),
  col = c("#1b4a76", "#386ca0", "#6b90b6", "#a8b9cc",
          "#ccb0a8", "#b97a67", "#a44a30", "#792b13",
          "#2f6f66", "#8fbfb3"),
  ink = c("#ffffff", "#ffffff", INK, INK, INK, INK, "#ffffff", "#ffffff", "#ffffff", INK)
)
I_DARK <- "#2f6f66"; I_MID <- "#3f8a80"

# Index into BINS (1 = most Democratic). A margin of exactly 0 lands in an R bin,
# matching the winner rule in build_races(). Independents ahead get bins 9-10.
margin_bin <- function(m, opp_party = "D") {
  b <- 9L - cut(m, c(-Inf, -10, -5, -2, 0, 2, 5, 10, Inf), labels = FALSE)
  ifelse(!is.na(m) & m > 0 & opp_party %in% "I", ifelse(m >= 5, 9L, 10L), b)
}

fmt_margin <- function(m, opp_party = "D") {
  lead <- ifelse(opp_party %in% "I", "Ind", "D")
  ifelse(is.na(m), "n/a",
         ifelse(abs(m) < 0.05, "Even",
                ifelse(m > 0, sprintf("%s+%.1f", lead, m), sprintf("R+%.1f", -m))))
}
fmt_error <- function(e) {
  # error > 0: polls overstated the Dem/Ind; < 0: overstated the Republican
  ifelse(is.na(e), "n/a",
         ifelse(abs(e) < 0.05, "0.0",
                ifelse(e > 0, sprintf("%.1f toward R", e), sprintf("%.1f toward D", -e))))
}
# Pollster house effect (positive = leans D): "D+0.8", "R+3.6", em dash if unrated.
fmt_lean <- function(x) {
  ifelse(is.na(x), "—",
         ifelse(abs(x) < 0.05, "0.0", ifelse(x > 0, sprintf("D+%.1f", x), sprintf("R+%.1f", -x))))
}
# Grid cell fill for a polling miss: blue (Democrat beat polls) through warm gray to
# red (Republican beat polls), saturating at 12 points.
MISS_RAMP <- grDevices::colorRamp(c("#1b4a76", "#6b90b6", "#ebe8e2", "#b97a67", "#792b13"))
miss_fill <- function(e) {
  v <- MISS_RAMP((pmax(pmin(e, 12), -12) + 12) / 24)
  grDevices::rgb(v[, 1], v[, 2], v[, 3], maxColorValue = 255)
}
# Polling miss by who beat the polls (error > 0 = Republican did better): "+3.1 R".
fmt_miss <- function(e) {
  ifelse(abs(e) < 0.05, "0", paste0("+", sub("\\.0$", "", sprintf("%.1f", abs(e))), ifelse(e > 0, " R", " D")))
}
# Shift labels read like the slider: "+2 R", "0", "+4.5 D".
fmt_shift <- function(x) {
  amt <- sub("\\.0$", "", sprintf("%.1f", abs(x)))
  ifelse(abs(x) < 0.05, "0", paste0("+", amt, ifelse(x < 0, " R", " D")))
}

# ---- Font ---------------------------------------------------------------------
# Newsreader (an editorial serif with optical sizes); plotly needs it spelled out.
APP_FONT <- "'Newsreader', Georgia, serif"
GOOGLE_FONTS <- paste0(
  "https://fonts.googleapis.com/css2?",
  "family=Newsreader:ital,opsz,wght@0,6..72,400;0,6..72,500;0,6..72,600;1,6..72,400&display=swap"
)

APP_CSS <- paste0(sprintf(":root { --ink: %s; --ink-2: %s; --ink-muted: %s; --line: %s; --surface: %s; }", INK, INK_2, INK_MUTED, GRID, SURFACE), "
.chips span { font-variant-numeric: lining-nums tabular-nums; }
/* Header: pinned while scrolling, slab-serif title and tabs */
:root { --slab: 'Newsreader', Georgia, serif; --page-max: 1480px; }
.navbar { position: sticky; top: 0; z-index: 1030; background: var(--surface) !important; padding-top: 14px !important;
  border-bottom: 1.5px solid var(--ink); padding-top: 6px; padding-bottom: 0; }
.navbar > .container-fluid { max-width: var(--page-max); margin: 0 auto; align-items: flex-end; }
.navbar-header { align-self: flex-start; }
.navbar-brand { display: block; padding: 4px 0 8px; margin-right: 28px; white-space: normal; flex: 0 0 auto; width: 560px; }
.brand { display: flex; flex-direction: column; line-height: 1.1; }
.brand-head { display: inline-flex; flex-direction: column; align-self: flex-start; }
.brand-title { font-family: var(--slab); font-weight: 700; font-size: 1.6rem; line-height: 1.05; white-space: nowrap; letter-spacing: -.01em; color: var(--ink); }
.brand-title .bt-name { color: var(--accent); }
.brand-title .bt-sep { margin: 0 .4em; font-weight: 300; color: var(--ink-muted); }
/* A flat rule in the eight margin tiers, D to R, under the title */
.brand-rule { position: relative; display: flex; height: 4px; margin: 9px 0 9px; width: 100%; }
.brand-rule .tier { flex: 1 1 0; }
.brand-rule .tier:first-child { border-radius: 2px 0 0 2px; }
.brand-rule .tier:nth-child(8) { border-radius: 0 2px 2px 0; }
/* The rule as an error bar: a bracketed margin around the middle, with the estimate at Even */
.brand-rule .moe-band { position: absolute; left: 34%; width: 32%; top: -4px; bottom: -4px; border-left: 1.5px solid var(--ink-2);
  border-right: 1.5px solid var(--ink-2); background: rgba(255, 255, 255, .38); }
.brand-rule .moe-est { position: absolute; left: 50%; top: -6px; bottom: -6px; width: 3px; margin-left: -1.5px; border-radius: 2px;
  background: var(--ink); }
.brand-sub { font-size: .84rem; color: var(--ink-2); margin-top: 4px; line-height: 1.3; }
.navbar .nav-link { font-family: var(--slab); font-size: 1.08rem; color: var(--ink-2) !important;
  padding: 10px 14px 12px !important; border-bottom: 4px solid transparent; }
.navbar .nav-link:hover { color: var(--ink) !important; }
.navbar .nav-link.active { color: var(--ink) !important; font-weight: 700; border-bottom-color: var(--ink); }
.navbar-nav .nav-item + .nav-item > .nav-link[data-bs-toggle='tab'] { border-left: 1px solid var(--line); }
.navbar .nav-item .gh-link { display: inline-flex; align-items: center; gap: 6px; padding: 0;
  font-size: .9rem; color: var(--ink-2); text-decoration: none; }
.navbar .nav-item .gh-link:hover { color: var(--ink); text-decoration: underline; }
@media (max-width: 1440px) {
  .brand-title { font-size: 1.12rem; }
  .navbar .credit-row .ds-k { font-size: .62rem; letter-spacing: .1em; margin-right: 8px; }
  .navbar-brand { width: 390px; margin-right: 12px; }
  .brand-sub { font-size: .78rem; }

  .brand-sub { max-width: none; }
  .navbar .nav-link { padding-left: 7px !important; padding-right: 7px !important; font-size: .92rem; }
}
/* Credit strip under the tabs: data source ~70% | code ~30%, split by a thin rule, two lines each */
.navbar .credit-li { flex-basis: 100%; }
.navbar .credit-row { display: flex; align-items: stretch; width: 100%; max-width: 860px; padding: 4px 0 10px; }
.navbar .credit-src { flex: 7 1 0; min-width: 0; padding-right: 18px; }
.navbar .credit-code { flex: 3 1 0; min-width: 0; padding-left: 18px; border-left: 1px solid var(--line); }
.navbar .credit-row .cr-line { display: flex; align-items: center; gap: 10px; line-height: 1.5; }
.navbar .credit-row .cr-sub { font-size: .84rem; font-style: italic; color: var(--ink-2); line-height: 1.35; margin-top: 1px; }
.navbar .credit-row .gh-link { font-size: .9rem; font-weight: 600; color: var(--ink); margin-top: 1px; }
.navbar .credit-row .ds-k { font-size: .68rem; font-weight: 600; letter-spacing: .14em; text-transform: uppercase;
  color: var(--ink-muted); }
.navbar .credit-row a { color: var(--ink); font-weight: 600; text-decoration: none; border-bottom: 1px solid transparent; }
.navbar .credit-row a:hover { border-bottom-color: var(--ink); }
/* Silver Bulletin credit: their icon, and a thin underline in their brand green (too bright to use as text) */
.navbar .credit-row a.sb-link { display: inline-flex; align-items: center; gap: 6px; border-bottom: 0; text-decoration: none !important; }
.navbar .credit-row a.sb-link span { border-bottom: 2px solid #00d93a; padding-bottom: 1px; }
.navbar .credit-row a.sb-link:hover span { border-bottom-width: 3px; }
.sb-icon { width: 16px; height: 16px; border-radius: 3px; }
/* About: the methodology note the header links to; flashes when arrived at */
.about-callout { background: var(--accent-bg); border: 1px solid var(--accent-line); border-left: 4px solid var(--accent);
  border-radius: 8px; padding: 14px 18px 4px; margin: 6px 0 18px; scroll-margin-top: 190px; }
.about-callout h5 { color: var(--accent); }
.about-callout.flash { animation: callout-flash 1.6s ease-out; }
@keyframes callout-flash { 0% { box-shadow: 0 0 0 6px rgba(192, 138, 30, .45); } 100% { box-shadow: 0 0 0 0 rgba(192, 138, 30, 0); } }

/* Keep content to a readable width on very wide screens */
.tab-content > .tab-pane { max-width: var(--page-max); margin-left: auto; margin-right: auto; }

/* Beside the map: per-state shift from the polls */
.bias-panel { position: relative; padding-left: 4px; }
.bias-overlay { position: absolute; inset: 0; z-index: 5; display: flex; align-items: center; justify-content: center; padding: 24px; pointer-events: none;
  background: rgba(251, 248, 241, .8); border-radius: 8px; text-align: center; }
.bias-overlay .bias-note { font-size: .92rem; line-height: 1.45; color: var(--ink-2); text-decoration: none; max-width: 260px;
  background: rgba(255, 253, 248, .82); border: 1px solid rgba(228, 225, 218, .7); border-radius: 8px; padding: 10px 14px; }
.bias-overlay .bias-note b { color: var(--ink-2); font-weight: 600; }
.map-wrap { position: relative; }
.map-key { position: absolute; top: 0; left: 4px; z-index: 2; font-size: .78rem; color: var(--ink-2); }
.map-key .sw { display: inline-block; width: 12px; height: 12px; border-radius: 3px; margin-right: 5px; vertical-align: -1px; }
/* Uniform-shift slider in the page palette: no stock end pills, ochre value tag, neutral track */
.shift-wrap .irs--shiny .irs-min, .shift-wrap .irs--shiny .irs-max { display: none !important; }
.shift-wrap .irs--shiny .irs-single { background: var(--accent); font-size: .8rem; padding: 1px 7px; }
.shift-wrap .irs--shiny .irs-single::before { border-top-color: var(--accent); }
.shift-wrap .irs--shiny .irs-bar { background: transparent; border: 0; }
.shift-wrap .irs--shiny .irs-line { background: var(--line); border: 0; }
.shift-wrap .irs--shiny .irs-handle { background: #fff; border: 2.5px solid var(--accent); box-shadow: none; }
.shift-wrap .irs--shiny .irs-handle:hover, .shift-wrap .irs--shiny .irs-handle.state_hover { background: var(--accent-soft); }
.shift-ends { display: flex; justify-content: space-between; margin: -10px 0 14px; font-size: .8rem; }
.bslib-sidebar-layout > .sidebar hr { margin: 4px 0; }
.bias-title { font-weight: 600; font-size: .95rem; }
.bias-axis { display: flex; justify-content: space-between; font-size: .72rem; color: var(--ink-muted);
  margin: 2px 52px 4px 30px; }
.bias-row { display: grid; grid-template-columns: 26px 1fr 52px; align-items: center; gap: 6px; height: 15px; }
.bias-row .bias-track { cursor: ew-resize; touch-action: none; }
.bias-row .bias-track .dot { transition: transform .1s ease; }
.bias-row:hover .bias-track .dot, .bias-row.dragging .bias-track .dot { transform: scale(1.5); }
.bias-row.edited .bias-st { color: var(--accent); }
.bias-row.edited .bias-track .dot { box-shadow: 0 0 0 2px var(--accent-soft), 0 0 0 3px var(--accent); }
.bias-row:hover { background: rgba(27, 26, 23, .05); }
/* 2026 results: rows whose count is still provisional */
.bias-tools { flex-wrap: wrap; }
.bias-row.counting, .bk-counting { background: rgba(27, 26, 23, .07); }
.bias-key { display: flex; flex-wrap: wrap; gap: 4px 12px; width: 100%; margin-top: 3px; }
.bias-key > span { display: inline-flex; align-items: center; }
.bk-sw { display: inline-block; width: 16px; height: 11px; margin-right: 5px; border-radius: 2px; }
.hs-pending { fill: url(#hs-phatch); stroke: #1b1a17; stroke-width: 1.5; stroke-dasharray: 4 3; pointer-events: none; }
.legend .sw.sw-pending { background: repeating-linear-gradient(45deg, rgba(255, 255, 255, .6) 0 2px, transparent 2px 5px), #6b90b6;
  outline: 1.5px dashed #1b1a17; outline-offset: -1.5px; }
.bias-tools { display: flex; justify-content: space-between; align-items: baseline; font-size: .74rem; color: var(--ink-muted); margin: 2px 0 4px; }
.bias-all a { margin-left: 10px; color: var(--ink-2); text-decoration: underline; text-decoration-color: var(--line); text-underline-offset: 2px; }
.bias-all a:hover { color: var(--ink); }
.m-ctl .party-icon { margin-right: 7px; vertical-align: -3px; }
.bias-st { font-size: .7rem; font-weight: 600; color: var(--ink-2); }
.bias-track { position: relative; height: 100%; }
.bias-track .mid { position: absolute; top: 0; bottom: 0; left: 50%; width: 1px; background: var(--ink-muted); }
.bias-track .line { position: absolute; top: 50%; height: 2px; margin-top: -1px; opacity: .55; }
.bias-track .dot { position: absolute; top: 50%; width: 7px; height: 7px; margin: -3.5px 0 0 -3.5px; border-radius: 50%; }
.bias-v { font-size: .7rem; font-weight: 600; text-align: right; font-variant-numeric: lining-nums; }
.bias-empty { font-size: .86rem; color: var(--ink-2); margin-top: 8px; }
.card-header { font-weight: 600; background: transparent; }
.navbar .form-group { margin-bottom: 0; }

/* Toggle chips: a restyled inline checkboxGroupInput / radioButtons */
.chips .shiny-options-group { display: flex; flex-wrap: wrap; gap: 6px; }
.chips .shiny-options-group label.checkbox-inline,
.chips .shiny-options-group label.radio-inline { margin: 0 !important; padding: 0 !important; }
/* The real input is stretched invisibly over its whole chip, so any click on the chip hits it */
.chips label, .seg label { position: relative; }
.chips input[type=checkbox], .chips input[type=radio] { position: absolute; inset: 0; width: 100%; height: 100%; margin: 0; opacity: 0; cursor: pointer; z-index: 1; }
.chips input + span {
  display: inline-block; padding: 2px 10px; border: 1px solid var(--line); border-radius: 999px;
  font-size: .85rem; color: var(--ink-2); cursor: pointer; user-select: none;
}
.chips input:checked + span { background: var(--ink); border-color: var(--ink); color: #fff; }
.chips input:focus-visible + span { outline: 2px solid var(--ink); outline-offset: 2px; }
.chips .shiny-input-container { margin-bottom: .4rem; }
/* Scenario box at the top of the sidebar */
/* Polling-error controls carry their own accent (ochre), apart from party colors */
:root { --accent: #7a5a12; --accent-soft: #f3e6c2; --accent-line: #e4d4a6; --accent-bg: #fdf9ef; }
.scen-box { padding: 0 0 14px; margin-bottom: 4px; border-bottom: 1px solid var(--line); }
.scen-kicker { font-size: 1rem; font-weight: 600; color: var(--accent); margin-bottom: 8px; }
.scen-sub { font-size: .86rem; color: var(--ink-2); margin: 12px 0 6px; }
.scen-box .seg .shiny-options-group { border-color: var(--accent); background: #fff; }
.scen-box .seg input + span { color: var(--accent); }
.scen-box .seg label.radio-inline + label.radio-inline span { border-left-color: var(--accent); }
.scen-box .seg input:checked + span { background: var(--accent); color: #fff; }
.scen-box .chips input + span { background: #fff; border-color: var(--accent-line); }
.scen-box .chips input:checked + span { background: var(--accent); border-color: var(--accent); color: #fff; }
/* Presets: one full-width row each, years listed on the right */
.preset-list { display: flex; flex-direction: column; gap: 6px; }
.scen-box .btn.preset { display: block; width: 100%; padding: 6px 11px; border: 1px solid var(--accent-line);
  border-radius: 8px; background: #fff; color: var(--accent); text-align: left; line-height: 1.25; }
.scen-box .btn.preset .action-label { display: flex; flex-direction: column; font-size: .92rem; font-weight: 600; }
.scen-box .btn.preset .action-label > span { font-size: .76rem; font-weight: 400; color: var(--ink-muted); }
.scen-box .btn.preset:hover { background: var(--accent-soft); }
.scen-box .btn.preset.active { border-color: var(--accent); background: var(--accent); color: #fff; }
.scen-box .btn.preset.active .action-label > span { color: rgba(255, 255, 255, .8); }
/* Year chips: a compact 4 x 2 grid */
.scen-box .chips-yr .shiny-options-group { display: grid; grid-template-columns: repeat(4, 1fr); gap: 4px; }
.scen-box .chips-yr input + span { display: block; text-align: center; padding: 1px 0; font-size: .82rem; }
.scen-title-tag { font-size: .95rem; padding: 3px 10px; margin-top: 0; }
.t-tag { display: inline-block; margin-top: 2px; padding: 1px 8px; border-radius: 4px; font-size: .82rem; font-weight: 600;
  background: var(--accent-soft); color: var(--accent); }
.seg.seg-full .shiny-options-group { display: flex; flex-wrap: wrap; width: 100%; }
/* A third option (2026 results) takes its own full-width row */
.seg.seg-full .shiny-options-group label.radio-inline:nth-child(3) { flex-basis: 100%; }
.scen-box .seg.seg-full label.radio-inline:nth-child(3) span { border-left: none; border-top: 1.5px solid var(--accent); }
.seg.seg-full .shiny-options-group label.radio-inline { flex: 1 1 0; display: flex; }
.seg.seg-full input + span { flex: 1 1 auto; display: flex; align-items: center; justify-content: center; text-align: center; padding: 8px 4px; font-size: .86rem; white-space: nowrap; }
.scen-box .scenario-caption { margin-top: 10px; }

/* Seat tally strip */
.tally { display: flex; flex-wrap: wrap; align-items: center; gap: 10px 0; background: #fff; border: 1px solid var(--line);
  border-radius: 10px; padding: 10px 18px; margin-bottom: 14px; }
.t-seats { display: flex; align-items: baseline; gap: 6px; padding-right: 24px; }
.t-num { font-size: 2.1rem; font-weight: 600; line-height: 1; font-variant-numeric: lining-nums; }
.t-lab { font-size: .8rem; color: var(--ink-2); }
.t-dash { font-size: 1.4rem; color: var(--ink-muted); margin: 0 8px; }
.t-item { display: flex; flex-direction: column; line-height: 1.25; padding: 0 24px; border-left: 1px solid var(--line); }
.t-big { font-size: 1.05rem; font-weight: 600; }
.t-small { font-size: .8rem; color: var(--ink-2); }
.chips-lg input + span { padding: 4px 12px; font-size: .92rem; }
.scen-box .scenario-caption { margin-bottom: .8rem; }

/* Historical Polling Misses */
.hm-d { color: #316ca5; font-weight: 600; }
.hm-r { color: #ac4323; font-weight: 600; }
.hm-h { font-size: 1.35rem; font-weight: 600; margin: 30px 0 2px; }
.hm-sub { font-size: .9rem; color: var(--ink-2); margin: 0 0 10px; max-width: 900px; }
.hm-tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(210px, 1fr)); gap: 12px; }
.hm-tile { background: #fff; border: 1px solid var(--line); border-radius: 10px; padding: 12px 16px; }
.hm-tile b { display: block; font-size: 1.7rem; line-height: 1.15; font-variant-numeric: lining-nums; }
.hm-tile span { font-size: .84rem; color: var(--ink-2); }
.hm-cols, .hm-row { display: grid; align-items: center; gap: 16px; grid-template-columns: 150px minmax(320px, 1fr) 90px 60px; }
.hm-cols > *, .hm-row > * { min-width: 0; }
.hm-row { padding: 5px 0; border-bottom: 1px solid var(--line); }
.hm-row:hover { background: rgba(27, 26, 23, .03); }
.hm-st { font-weight: 600; font-size: .92rem; }
.hm-track { position: relative; height: 32px; }
.hm-track .tick { position: absolute; top: -3px; bottom: -3px; width: 1px; background: var(--line); }
.hm-track .mid { position: absolute; top: -5px; bottom: -5px; left: 50%; width: 1.5px; margin-left: -.75px; background: var(--ink); }
.hm-track .fill { position: absolute; top: 11px; bottom: 11px; opacity: .3; border-radius: 2px; }
.hm-track .dot { position: absolute; width: 6px; height: 6px; margin: -3px 0 0 -3px; border-radius: 50%; }
.hm-track .dot.sen { background: var(--c); opacity: .75; }
.hm-track .dot.pres { border: 1.2px solid var(--c); opacity: .6; }
.hm-track .avg { position: absolute; top: 4px; bottom: 4px; width: 3px; margin-left: -1.5px; border-radius: 2px; }
.hm-filters { display: flex; flex-wrap: wrap; align-items: center; gap: 12px 28px; margin: 14px 0 10px; }
.hm-fgroup { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 8px; }
.hm-fgroup .shiny-input-container { margin-bottom: 0; }
.seg.seg-sm input + span { padding: 4px 14px; font-size: .88rem; }
.btn.hm-preset { padding: 2px 11px; font-size: .82rem; border: 1px solid var(--line); border-radius: 999px; background: #fff; color: var(--ink-2); }
.btn.hm-preset.active { background: var(--ink); border-color: var(--ink); color: #fff; }
.hm-legend { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; font-size: .8rem; color: var(--ink-2); margin: 4px 0 10px; }
.hm-legend .ramp { width: 200px; height: 10px; border-radius: 3px;
  background: linear-gradient(to right, #1b4a76, #6b90b6, #ebe8e2, #b97a67, #792b13); }
.hm-legend-note { margin-left: 12px; color: var(--ink-muted); }
.hm-grid-wrap { overflow-x: auto; }
.hm-table { border-collapse: separate; border-spacing: 2px; font-variant-numeric: lining-nums tabular-nums; }
.hm-table th { font-size: .74rem; font-weight: 600; color: var(--ink-2); text-align: center; padding: 2px 0; }
.hm-table th.yr { border-bottom: 1.5px solid var(--ink); }
.hm-table th.ty { font-weight: 400; color: var(--ink-muted); }
.hm-table td { width: 44px; height: 26px; text-align: center; font-size: .8rem; border-radius: 3px; }
.hm-table td.st { width: 150px; text-align: left; font-weight: 600; font-size: .9rem; padding-right: 10px; white-space: nowrap; }
.hm-table td.empty { background: repeating-linear-gradient(45deg, transparent 0 3px, var(--line) 3px 4px); opacity: .5; }
.hm-table td.av, .hm-table th.av { width: 80px; text-align: right; font-weight: 600; padding-left: 10px; }
.hm-table tbody tr:hover td.st { text-decoration: underline; }
.hm-avg { font-weight: 600; font-size: .9rem; font-variant-numeric: lining-nums; }
.hm-n { font-size: .82rem; color: var(--ink-muted); }
.hm-key { display: inline-block; width: 8px; height: 8px; border-radius: 50%; vertical-align: 0; }
.hm-key.sen { background: var(--ink-2); }
.hm-key.pres { border: 1.5px solid var(--ink-2); }

/* Ranked-margins legend: an annotated example column */
.rk-legend { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 28px; margin-top: 4px; }
.rk-legend svg { width: 100%; max-width: 720px; height: auto; }
.rk-legend text { font-size: 12px; fill: var(--ink-2); dominant-baseline: central; }
.rk-legend .lead { stroke: var(--ink-muted); stroke-width: 1; }
.rk-legend .sub { font-size: 11px; fill: var(--ink-muted); }
.rk-note { flex: 1 1 240px; font-size: .82rem; color: var(--ink-2); }
.scenario-caption { font-size: .82rem; color: var(--ink-2); line-height: 1.35; margin-bottom: 1.1rem; }

/* Seat horseshoe (SVG; text scales with the drawing so labels never overflow) */
.hs { display: block; width: 100%; max-width: 1100px; height: auto; margin: 12px auto 0; }
.hs text { pointer-events: none; }
.hs .hs-cell:hover { stroke: var(--ink); stroke-width: 2; }
.hs-cell-label { font-size: 10.5px; font-weight: 600; text-anchor: middle; dominant-baseline: central; }
.hs-chunk-label { font-size: 15px; fill: #fff; text-anchor: middle; dominant-baseline: central; }
.hs-ctl { stroke: var(--ink); stroke-width: 2.5; }
.hs-key { fill: rgba(192, 138, 30, .16); pointer-events: none; }
.hs-key-label { font-size: 15px; fill: var(--ink); dominant-baseline: central; }
.hs-break-gap { fill: var(--surface); }
.hs-break { stroke: var(--ink-2); stroke-width: 1.2; }
.hs-ctl-label { font-size: 14px; fill: var(--ink); dominant-baseline: central; }
.hs-tally { font-size: 50px; font-weight: 600; text-anchor: middle; font-variant-numeric: lining-nums; }
.hs-tally-ctl { font-size: 19px; font-weight: 600; text-anchor: middle; }
.hs-prov { font-size: 13px; font-style: italic; letter-spacing: .06em; text-anchor: middle; fill: #6e6a60; }
.m-prov { font-size: .82rem; font-style: italic; letter-spacing: .05em; color: var(--ink-muted); margin-top: 2px; }
.bias-rows.res .bias-row { grid-template-columns: 26px 1fr 46px 48px; }
.bias-row.bias-head { height: 16px; margin-bottom: 2px; font-size: .72rem; font-style: italic; color: var(--ink-muted); white-space: nowrap; pointer-events: none; }
.bias-row.bias-head span:nth-child(n+3) { text-align: right; }
.bias-pct { font-size: .68rem; color: var(--ink-muted); text-align: right; font-variant-numeric: tabular-nums; }
.seat-note { font-size: .88rem; color: var(--ink-2); margin: 6px 0 4px; }
.view-toggle { margin-left: auto; }
.view-toggle .shiny-input-container { margin-bottom: 0; }

/* Shared hover tooltip for any [data-tip] element (horseshoe cells) */
#app-tip { position: absolute; z-index: 2000; display: none; pointer-events: none; max-width: 340px;
  white-space: pre-line; font-size: .8rem; line-height: 1.35;
  color: var(--ink); background: #fff; border: 1px solid var(--line); border-radius: 6px;
  padding: 6px 9px; box-shadow: 0 2px 10px rgba(0, 0, 0, .08); }

/* Latest Polls */
.lp-head { margin: 6px 0 20px; }
.lp-title { font-size: 2.7rem; font-weight: 600; letter-spacing: -.015em; margin: 0 0 6px; }
.lp-dek { font-size: 1.15rem; color: var(--ink-2); margin: 0 0 8px; max-width: 760px; }
.lp-updated { font-size: .85rem; color: var(--ink-muted); }
.lp-unpolled { margin: 16px 0 0; font-size: .88rem; color: var(--ink-2); }
/* Sidebar: final pre-election update notice */
.freshness-final { padding: 9px 12px; background: var(--accent-bg); border: 1px solid var(--accent-line);
  border-left: 4px solid var(--accent); border-radius: 6px; line-height: 1.35; }
.freshness-final { font-size: .86rem; color: var(--ink-2); }
.freshness-final b { color: var(--accent); }
/* Early Vote tab */
.ev-note { max-width: 920px; margin: 2px 0 20px; padding: 10px 14px; background: var(--accent-bg); border-left: 3px solid var(--accent);
  border-radius: 6px; font-size: .9rem; line-height: 1.45; color: var(--ink-2); }
.ev-note b { color: var(--ink); }
.ev-cols, .ev-row { display: grid; align-items: center; gap: 16px; grid-template-columns: 170px 170px minmax(240px, 1fr) 250px 64px; }
.ev-cols > *, .ev-row > * { min-width: 0; }
.ev-row { padding: 8px 0; border-bottom: 1px solid var(--line); }
.ev-n b { display: block; font-size: 1rem; font-variant-numeric: lining-nums tabular-nums; }
.ev-bar { position: relative; height: 22px; }
.ev-bar .tick { position: absolute; top: 0; bottom: 0; width: 1px; background: var(--line); }
.ev-bar .full { position: absolute; top: -3px; bottom: -3px; width: 1.5px; margin-left: -.75px; background: var(--ink); }
.ev-bar .fill { position: absolute; left: 0; top: 5px; height: 12px; border-radius: 0 2px 2px 0; background: var(--accent); opacity: .55; }
.ev-pct { position: absolute; top: 2px; margin-left: 6px; font-size: .82rem; font-weight: 600; color: var(--ink); font-variant-numeric: lining-nums; }
.ev-pbar { display: flex; height: 10px; border-radius: 2px; overflow: hidden; }
.ev-pnum { display: flex; gap: 12px; margin-top: 3px; font-size: .78rem; color: var(--ink-2); font-variant-numeric: lining-nums; }
.ev-none { font-style: italic; }
.navbar .credit-row .cr-ev { margin-top: 3px; }
@media (max-width: 767.98px) {
  .ev-cols { display: none; }
  .ev-row { grid-template-columns: 1fr 1fr 48px; gap: 6px 10px; }
  .ev-row .ev-bar, .ev-row .ev-party, .ev-row .ev-none { grid-column: 1 / -1; }
}
.lp-foot { margin: 16px 0 6px; font-size: .8rem; color: var(--ink-muted); max-width: 900px; }

/* View toggle: a segmented control (restyled radioButtons) */
.seg .shiny-input-container { margin-bottom: 0; }
.seg .shiny-options-group { display: inline-flex; gap: 0; border: 1.5px solid var(--ink); border-radius: 8px; overflow: hidden; }
.seg .shiny-options-group label.radio-inline { margin: 0 !important; padding: 0 !important; }
.seg input[type=radio] { position: absolute; inset: 0; width: 100%; height: 100%; margin: 0; opacity: 0; cursor: pointer; z-index: 1; }
.seg input + span { display: block; padding: 8px 24px; font-size: 1.08rem; font-weight: 600; color: var(--ink-2); cursor: pointer; }
.seg label.radio-inline + label.radio-inline span { border-left: 1.5px solid var(--ink); }
.seg input:checked + span { background: var(--ink); color: #fff; }
.seg input:focus-visible + span { outline: 2px solid var(--surface); outline-offset: -5px; }

/* Filters under the toggle: compact dropdowns side by side */
.lp-filters { display: flex; flex-wrap: wrap; align-items: center; gap: 10px 26px; margin: 16px 0 4px; }
.lp-filters > div { display: flex; align-items: center; gap: 8px; }
.lp-filters .shiny-input-container { margin-bottom: 0; }
.lp-filters .form-check { margin-bottom: 0; }
.lp-lab { font-size: .72rem; letter-spacing: .08em; text-transform: uppercase; color: var(--ink-muted); }

/* Shared row pieces */
.lp-cols { font-size: .68rem; letter-spacing: .08em; text-transform: uppercase; color: var(--ink-muted);
  padding: 14px 0 6px; border-bottom: 1.5px solid var(--ink); }
.lp-cols > *, .av-row > *, .pr-row > * { min-width: 0; }
.lp-held { font-size: .68rem; letter-spacing: .05em; text-transform: uppercase; color: var(--ink-muted); }
.lp-meta { font-size: .82rem; color: var(--ink-2); font-variant-numeric: lining-nums; }
.lp-head-txt { font-weight: 600; font-variant-numeric: lining-nums; white-space: nowrap; }
.lp-bar { position: relative; display: flex; height: 8px; border-radius: 4px; overflow: hidden; flex: 1 1 auto; }
.lp-bar span { display: block; height: 100%; }
.lp-bar::after { content: ''; position: absolute; left: 50%; top: 0; bottom: 0; width: 2px; margin-left: -1px;
  background: var(--surface); }

/* State averages: one line per race, margin drawn as a bar from Even */
.av-cols, .av-row { display: grid; align-items: center; gap: 18px;
  grid-template-columns: 160px 120px minmax(300px, 2.2fr) minmax(200px, 1fr) 140px 150px; }
.av-row { padding: 5px 0; border-bottom: 1px solid var(--line); }
.av-row:hover { background: rgba(27, 26, 23, .03); }
.av-state { display: flex; flex-direction: column; line-height: 1.15; cursor: pointer; }
.av-name { font-weight: 600; text-decoration: underline; text-decoration-color: var(--line); text-underline-offset: 3px; }
.av-row:hover .av-name { text-decoration-color: var(--ink); }
.av-axis { position: relative; height: 1em; }
.av-axis span { position: absolute; transform: translateX(-50%); white-space: nowrap; }
.av-bar { position: relative; height: 32px; cursor: pointer; }
.av-bar .tick { position: absolute; top: -3px; bottom: -3px; width: 1px; background: var(--line); }
.av-bar .mid { position: absolute; top: -5px; bottom: -5px; left: 50%; width: 1.5px; margin-left: -.75px; background: var(--ink); }
.av-bar .fill { position: absolute; top: 11px; bottom: 11px; opacity: .3; border-radius: 2px; }
.av-bar .avg { position: absolute; top: 4px; bottom: 4px; width: 3px; margin-left: -1.5px; border-radius: 2px; }
.av-bar .dot { position: absolute; width: 6px; height: 6px; margin: -3px 0 0 -3px; border-radius: 50%; }
.av-bar .dot.in { background: var(--c); opacity: .8; }
.av-bar .dot.old { border: 1px solid var(--c); opacity: .35; }
.av-match { font-size: .86rem; color: var(--ink-2); font-variant-numeric: lining-nums; white-space: nowrap;
  overflow: hidden; text-overflow: ellipsis; }

/* All polls: one dense line per poll */
.pr-cols, .pr-row { display: grid; align-items: center; gap: 16px;
  grid-template-columns: 130px 130px minmax(200px, 1.4fr) 82px 92px minmax(190px, 1.2fr) 52px; }
.pr-row { padding: 6px 0; border-bottom: 1px solid var(--line); font-size: .92rem; }
.pr-row:hover { background: rgba(27, 26, 23, .03); }
.pr-day { display: flex; align-items: baseline; gap: 10px; padding: 14px 0 4px; border-bottom: 1.5px solid var(--ink); }
.pr-day b { font-size: 1rem; }
.pr-day span { font-size: .85rem; color: var(--ink-2); }
.pr-pollster { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.pr-pollster a { color: var(--ink); text-underline-offset: 3px; text-decoration-color: var(--line); }
.pr-result { display: flex; align-items: center; gap: 8px; font-variant-numeric: lining-nums tabular-nums; }
.pr-result b { width: 2.2em; font-weight: 600; }
.pr-result b:last-child { text-align: right; }
.pr-in { text-align: center; color: var(--ink); }
.lp-grade { display: inline-block; margin-left: 6px; padding: 0 5px; border: 1px solid var(--ink); border-radius: 4px;
  font-size: .7rem; line-height: 1.45; vertical-align: 1px; }
.lp-grade.g-a { background: var(--ink); color: #fff; }
.lp-grade.g-c { border-color: var(--line); color: var(--ink-muted); }
.lp-sponsor { display: inline-block; margin-left: 6px; font-size: .74rem; font-style: italic; color: var(--ink-muted); white-space: nowrap; }

@media (max-width: 1000px) {
  .av-cols, .pr-cols { display: none; }
  .av-row { grid-template-columns: 1fr 1fr; gap: 6px 14px; padding: 10px 0; }
  .av-row .av-bar { grid-column: 1 / -1; }
  .pr-row { grid-template-columns: 1fr 1fr; gap: 4px 14px; padding: 10px 0; }
}

.legend { display: flex; flex-wrap: wrap; gap: 4px 14px; font-size: .78rem; color: var(--ink-2); }
/* Uniform shift: label row with a Reset link, and a tick at 0 (the range is symmetric, so 0 is the middle of the track) */
.shift-head { display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 2px; }
.shift-head label { margin: 0; }
.shift-reset { font-size: .8rem; color: var(--ink-2); text-decoration: underline; text-decoration-color: var(--line); text-underline-offset: 3px; }
.shift-reset:hover { color: var(--ink); text-decoration-color: var(--ink); }
.shift-wrap .irs-line { overflow: visible !important; }
.shift-wrap .irs-line::after { content: ''; position: absolute; left: 50%; top: -5px; width: 1.5px; height: calc(100% + 10px); margin-left: -.75px; background: var(--ink-2); }
.how-link { margin: -6px 0 6px; }
.how-link a { font-size: .95rem; font-weight: 600; color: var(--accent); line-height: 1.35; text-decoration: underline; text-decoration-color: var(--accent-line); text-underline-offset: 3px; }
.how-link a .fa, .how-link a svg { margin-right: 6px; }
.how-link a:hover { text-decoration-color: var(--accent); }
.switch-help { font-size: .78rem; color: var(--ink-muted); line-height: 1.35; margin: -14px 0 18px; }
.legend .spec { display: inline-flex; align-items: flex-start; gap: 6px; margin-right: 10px; }
.legend .spec-end { font-size: .8rem; line-height: 12px; }
.legend .spec-bar { position: relative; display: flex; width: 320px; height: 12px; margin-bottom: 17px;
  border-radius: 3px; overflow: visible; }
.legend .spec-seg { flex: 1 1 0; height: 12px; }
.legend .spec-seg:first-child { border-radius: 3px 0 0 3px; }
.legend .spec-seg:nth-child(8) { border-radius: 0 3px 3px 0; }
.legend .spec-cut { position: absolute; top: 18px; transform: translateX(-50%); font-size: .8rem; line-height: 1;
  color: var(--ink-muted); font-variant-numeric: lining-nums; }
.legend .spec-cut::before { content: ''; position: absolute; left: 50%; top: -6px; width: 1px; height: 4px; background: var(--ink-muted); }
.legend .sw-ind { width: 18px !important; }
/* One-year replays above the seat views */
.yr-wrap { margin: 14px 0 10px; }
.yr-title { font-weight: 600; font-size: .95rem; margin-bottom: 8px; }
.yr-strip { display: flex; flex-wrap: wrap; align-items: stretch; gap: 6px; }
.yr-cell { display: flex; flex-direction: column; align-items: center; min-width: 64px; padding: 3px 8px 4px; border: 1px solid var(--line);
  border-radius: 6px; background: #fff; line-height: 1.15; cursor: pointer; font-family: inherit; }
.yr-cell:focus-visible { outline: 2px solid var(--ink); outline-offset: 2px; }
.yr-cell.d { background: #a8b9cc; border-color: #a8b9cc; }
.yr-cell.r { background: #ccb0a8; border-color: #ccb0a8; }
.yr-cell.split { background: #e4e1da; border-color: #e4e1da; }
.yr-cell { transition: transform .12s ease, box-shadow .12s ease; }
.yr-cell:hover { transform: translateY(-2px); box-shadow: 0 3px 8px rgba(27, 26, 23, .15); border-color: var(--ink-2); }
.yr-cell.active { border-color: var(--ink); box-shadow: 0 0 0 2px var(--ink); }
.yr-cell.yr-actual { margin-left: 10px; font-weight: 700; }
.yr-y { font-size: .74rem; color: var(--ink-2); font-variant-numeric: lining-nums; }
.yr-s { font-size: .95rem; font-weight: 600; color: var(--ink); font-variant-numeric: lining-nums tabular-nums; }
.legend .sw { display: inline-block; width: 12px; height: 12px; border-radius: 3px; margin-right: 5px; vertical-align: -1px; }
.legend .flip-ring { display: inline-block; width: 13px; height: 13px; border-radius: 50%; border: 1.3px solid var(--ink); margin-right: 5px; vertical-align: -2px; }
.hs-flip { fill: none; stroke-width: 1.3; pointer-events: none; }
.filter-row { display: flex; align-items: center; gap: 10px; margin-bottom: 14px; }
.filter-row .form-group { margin-bottom: 0; }
.m-tally, .swipe-hint { display: none; }
/* ---- Phones and small tablets ---- */
@media (max-width: 767.98px) {
  /* header: let the title wrap so the menu button stays on screen */
  .navbar-brand { width: auto !important; flex: 1 1 0; min-width: 0; margin-right: 8px; }
  .brand-title { white-space: normal; font-size: 1.15rem !important; line-height: 1.2; }
  .brand-title .bt-sep { margin: 0 .25em; }
  .brand-sub { font-size: .76rem; }
  .navbar .nav-link { border-left: 0 !important; padding: 8px 4px !important; }
  .navbar .credit-row { flex-direction: column; gap: 8px; max-width: none; }
  .navbar .credit-code { padding-left: 0; border-left: 0; }
  .navbar .credit-row .cr-line { flex-wrap: wrap; white-space: normal; }
  /* seat views: swipe sideways rather than shrink */
  .seat-scroll { overflow-x: auto; -webkit-overflow-scrolling: touch; }
  .seat-scroll .hs { min-width: 720px; }
  .seat-scroll-rk > div { min-width: 760px; }
  /* legend and year strip fit the screen */
  .legend .spec-bar { width: min(320px, 62vw); }
  .yr-cell { min-width: 0; flex: 1 1 calc(33% - 6px); }
  .yr-s { white-space: nowrap; font-size: .9rem; }
  .m-tally { display: block; text-align: center; margin: 4px 0 8px; }
  .m-nums { font-family: var(--slab); font-size: 2.4rem; font-weight: 600; line-height: 1; font-variant-numeric: lining-nums; }
  .m-dash { color: var(--ink-muted); margin: 0 .25em; }
  .m-ctl { font-size: 1.05rem; font-weight: 600; margin-top: 4px; }
  .swipe-hint { display: block; font-size: .76rem; color: var(--ink-muted); text-align: right; margin: 2px 0 6px; }
  .av-axis .minor { display: none; }
  /* map: shorter, so it isn't lost in empty space */
  #map { height: 260px !important; }
  .map-key { position: static; margin-bottom: 4px; }
  /* Historical Polling Misses: rows fit the width */
  .hm-cols, .hm-row { grid-template-columns: 78px minmax(0, 1fr) 58px 30px; gap: 8px; }
  .hm-st { font-size: .82rem; }
  #natl_bias { overflow-x: auto; }
}
")

# Shiny's slider can't format labels itself; swap in ion.rangeSlider's prettify.
APP_JS <- "
$(document).on('shiny:sessioninitialized', function() {
  var s = $('#shift').data('ionRangeSlider');
  if (!s) return;
  s.update({
    prettify_enabled: true,
    prettify: function(n) {
      var a = Math.round(Math.abs(n) * 10) / 10;
      return a === 0 ? '0' : '+' + a + (n < 0 ? ' D' : ' R');
    }
  });
});

// One floating tooltip for every [data-tip] element (the horseshoe is plain SVG).
$(document).on('mousemove', '[data-tip]', function(e) {
  var tip = document.getElementById('app-tip');
  if (!tip) { tip = document.createElement('div'); tip.id = 'app-tip'; document.body.appendChild(tip); }
  var esc = function(s) { return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
  var col = { 'D': '#316ca5', 'R': '#ac4323', 'Ind': '#3f8a80' };
  tip.innerHTML = esc(this.getAttribute('data-tip')).replace(/^Projected (D|R|Ind)[+]([0-9.]+)$/m, function(m, p, v) {
    return 'Projected <b style=\"color:' + col[p] + '\">' + p + '+' + v + '</b>';
  }).replace(/^Projected Even$/m, 'Projected <b>Even</b>');
  tip.style.display = 'block';
  var x = e.pageX + 14;
  if (x + tip.offsetWidth > window.scrollX + window.innerWidth - 8) x = e.pageX - tip.offsetWidth - 14;
  tip.style.left = x + 'px';
  tip.style.top = (e.pageY + 14) + 'px';
});
// Assumed-bias panel: each state's row is a slider. Drag (or click) to set that state's
// bias, +/-10 points in half-point steps; double-click a row to reset it.
(function() {
  var CAP = 10;
  var fmt = function(v) { var a = Math.abs(v); return a < 0.05 ? '0' : '+' + (Math.round(a * 10) / 10) + (v > 0 ? ' R' : ' D'); };
  var colr = function(v) { return v > 0.04 ? '#ac4323' : (v < -0.04 ? '#316ca5' : '#8a867c'); };
  var valAt = function(track, x) {
    var r = track.getBoundingClientRect(), f = (x - r.left) / r.width;
    return Math.max(-CAP, Math.min(CAP, Math.round((f - 0.5) * 2 * CAP * 2) / 2));
  };
  var paint = function(row, v) {
    var p = 50 + 50 * v / CAP, c = colr(v);
    row.find('.dot').css({ left: p + '%', background: c });
    row.find('.line').css({ left: Math.min(50, p) + '%', width: Math.abs(p - 50) + '%', background: c });
    row.find('.bias-v').text(fmt(v)).css('color', c);
  };
  $(document).on('pointerdown', '.bias-row .bias-track', function(e) {
    var track = this, row = $(track).closest('.bias-row'), abb = row.data('abb'), v = valAt(track, e.clientX);
    e.preventDefault(); row.addClass('dragging'); paint(row, v);
    try { track.setPointerCapture(e.pointerId); } catch (err) {}
    var move = function(ev) { v = valAt(track, ev.clientX); paint(row, v); };
    var up = function() {
      track.removeEventListener('pointermove', move); track.removeEventListener('pointerup', up); track.removeEventListener('pointercancel', up);
      row.removeClass('dragging');
      Shiny.setInputValue('bias_set', { abb: abb, v: v, t: Date.now() });
    };
    track.addEventListener('pointermove', move); track.addEventListener('pointerup', up); track.addEventListener('pointercancel', up);
  });
  $(document).on('dblclick', '.bias-row', function() {
    Shiny.setInputValue('bias_reset', { abb: $(this).data('abb'), t: Date.now() });
  });
})();
// Reset the uniform shift to 0 (Reset link or double-click on the slider). Done in the
// browser rather than with updateSliderInput, which would drop the '+2 R' labels.
window.resetShift = function() {
  var s = $('#shift').data('ionRangeSlider');
  if (s) { s.update({ from: 0 }); $('#shift').trigger('change'); }
  return false;
};
$(document).on('dblclick', '.shift-wrap .irs', resetShift);
// Header link: open About the Data and bring the methodology note into view.
window.goToErrors = function() {
  var tab = $('.navbar .nav-link').filter(function() { return $(this).text().trim() === 'About the Data'; });
  if (tab.length) tab[0].click();
  setTimeout(function() {
    var el = document.getElementById('about-errors');
    if (!el) return;
    el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    el.classList.remove('flash'); void el.offsetWidth; el.classList.add('flash');
  }, 200);
  return false;
};
// Light up whichever quick-pick button matches the selected cycles.
$(function() {
  // Phones: open the swipeable horseshoe at its right end, where the races and the 51st seat are.
  $(document).on('shiny:value', function(e) {
    if (e.name !== 'horseshoe') return;
    setTimeout(function() { var el = document.querySelector('.seat-scroll'); if (el) el.scrollLeft = el.scrollWidth; }, 50);
  });
  // Plotly sizes itself to zero inside a hidden panel; refit charts when the seat view changes.
  $(document).on('change', 'input[name=seat_view]', function() {
    setTimeout(function() {
      document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
        if (el.offsetParent !== null) Plotly.Plots.resize(el);
      });
    }, 30);
  });
  Shiny.addCustomMessageHandler('preset-active', function(id) {
    $('.preset').removeClass('active');
    if (id) $('#' + id).addClass('active');
  });
});

$(function() {
  Shiny.addCustomMessageHandler('hm-preset-active', function(id) {
    $('.hm-preset').removeClass('active');
    if (id) $('#' + id).addClass('active');
  });
});

// State averages: clicking a state's name or bar opens its polls.
$(document).on('click', '.av-row[data-abb] .av-state, .av-row[data-abb] .av-bar', function() {
  Shiny.setInputValue('drill_state', $(this).closest('.av-row').data('abb'), {priority: 'event'});
  window.scrollTo({top: 0, behavior: 'smooth'});
});

$(document).on('mouseleave', '[data-tip]', function() {
  var tip = document.getElementById('app-tip');
  if (tip) tip.style.display = 'none';
});
"

# ---- Data ---------------------------------------------------------------------
load_data <- function() {
  needed <- file.path("data", c("race_polls.csv", "poll_errors.csv", "pollster_ratings.csv"))
  if (!all(file.exists(needed)) && !IS_BROWSER) refresh_all(history = FALSE)
  race_polls <- read_csv("data/race_polls.csv", show_col_types = FALSE)
  if (!"sb_grade" %in% names(race_polls)) {   # older cache: match pollsters to ratings here
    ratings <- read_csv("data/pollster_ratings.csv", show_col_types = FALSE)
    hit <- match_pollsters(race_polls$pollster, ratings)
    race_polls <- mutate(race_polls, sb_grade = ratings$sb_grade[hit], sb_house = ratings$sb_house[hit])
  }
  list(
    seats = read_csv("data/seats_2026.csv", show_col_types = FALSE),
    polls = read_csv("data/current_polls.csv", show_col_types = FALSE),
    race_polls = race_polls,
    errors = read_csv("data/poll_errors.csv", show_col_types = FALSE),
    early = if (file.exists("data/early_vote.csv")) read_csv("data/early_vote.csv", show_col_types = FALSE),
    results = if (file.exists("data/results_2026.csv")) read_csv("data/results_2026.csv", show_col_types = FALSE)
  )
}
INIT <- load_data()

CYCLES <- c(2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)
MIDTERMS <- CYCLES[CYCLES %% 4 == 2]
PRES_YEARS <- CYCLES[CYCLES %% 4 == 0]
TRUMP_ERA <- CYCLES[CYCLES >= 2016]
TRUMP_MID <- c(2018, 2022)
# Races Democrats must win for 51 seats; at 50-50 VP Vance breaks ties for R.
NEED_FOR_CONTROL <- 51 - SEATS_NOT_UP[["D"]]

# Per-state assumed polling error under the chosen cycles/race types. States
# with no polled race in the selection get the selection's national average.
assumed_errors <- function(errors, abbs, cycles, types) {
  if (length(cycles) == 0 || length(types) == 0) {
    return(tibble(abb = abbs, err = 0, err_src = "polls taken at face value"))
  }
  sel <- errors |> filter(cycle %in% cycles, race_type %in% types)
  natl <- if (nrow(sel)) mean(sel$error) else 0
  by_state <- sel |>
    group_by(abb) |>
    summarise(err = mean(error), n_races = n(), .groups = "drop")
  tibble(abb = abbs) |>
    left_join(by_state, by = "abb") |>
    mutate(
      err_src = ifelse(is.na(err), "national avg (no state history)",
                       sprintf("state history (%d race%s)", n_races, ifelse(n_races == 1, "", "s"))),
      err = coalesce(err, natl)
    ) |>
    select(abb, err, err_src)
}

# The 2026 results as a scenario: each polled race's assumed error is its actual 2026
# polling miss (final average minus the margin counted so far), so it lands on its result.
result_errors <- function(polls, results, abbs) {
  tibble(abb = abbs) |>
    left_join(select(polls, abb, poll_margin), by = "abb") |>
    left_join(select(results, abb, result_margin = margin), by = "abb") |>
    mutate(err = coalesce(poll_margin - result_margin, 0), err_src = "2026 result") |>
    select(abb, err, err_src)
}

# One race's count so far, for hover text.
fmt_votes <- function(n) formatC(n, format = "d", big.mark = ",")
result_detail <- function(res, opp_party) {
  if (is.null(res) || !nrow(res)) return("No results yet")
  lead <- abs(res$opp_votes - res$rep_votes)
  leader <- if (res$opp_votes >= res$rep_votes) res$opp_name else res$rep_name
  sprintf("Result so far: %s%s, %.0f%% of expected vote counted\n%s",
          fmt_margin(res$margin, opp_party), if (abs(res$margin) < 0.5) sprintf(" (%s leads by %s votes)", leader, fmt_votes(lead)) else "",
          res$eevp, if (isTRUE(res$called)) sprintf("Called for %s", res$winner) else "Not yet called")
}

# States still being called, e.g. "AK, MS".
uncalled <- function(results) if (is.null(results)) character(0) else sort(results$abb[!results$called])
# Provisional results: "pending" = not yet called (marked on the horseshoe and map);
# "counting" = under COUNTING_EEVP% of the expected vote is in (shaded in the bias panel).
# eevp = the share of the expected vote counted (shown in the bias panel).
COUNTING_EEVP <- 95
count_status <- function(races, results) {
  if (is.null(results)) return(mutate(races, pending = FALSE, counting = FALSE, eevp = NA_real_))
  st <- transmute(results, abb, pending = !called, counting = eevp < COUNTING_EEVP, eevp)
  races |> select(-any_of(c("pending", "counting", "eevp"))) |> left_join(st, by = "abb") |>
    mutate(across(c(pending, counting), function(v) coalesce(v, FALSE)))
}
# The sidebar's one-line data status: when the votes were counted and the polls they're
# compared with (or, before results, when the polls were last updated). Times are UTC.
fmt_day_time <- function(x, time = TRUE) {
  x <- as.POSIXct(x, tz = "UTC")
  paste0(sub(" 0", " ", format(x, "%b %d", tz = "UTC")), if (time) format(x, ", %H:%M UTC", tz = "UTC"))
}
data_status <- function(polls, results) {
  if (is.null(results)) {
    return(sprintf("<b>Final pre-election update:</b> polls updated %s. Results will be added once they're in.",
                   fmt_day_time(polls$fetched_at[1])))
  }
  out <- uncalled(results)
  sprintf("<b>Votes counted as of %s</b>, compared with the final pre-election polling averages (%s); %s.",
          fmt_day_time(results$fetched_at[1]), fmt_day_time(polls$fetched_at[1], time = FALSE),
          if (length(out)) paste("not yet called:", paste(out, collapse = ", ")) else "all races called")
}

# Hover text for one state's assumed miss: the past races behind it, so a reader
# can tell a lasting pattern from one unusual race. In the results scenario, the count instead.
miss_detail <- function(errors, abb, cycles, types, err, shift, result = NULL, opp_party = "D") {
  shift_txt <- if (abs(shift) >= 0.05) sprintf("\nPlus uniform shift: %s", fmt_shift(shift)) else ""
  if (!is.null(result)) return(paste0(result_detail(result, opp_party), shift_txt))
  if (length(cycles) == 0 || length(types) == 0) return(paste0("Polls taken at face value", shift_txt))
  g <- errors |> filter(abb == !!abb, cycle %in% cycles, race_type %in% types) |> arrange(cycle, race_type)
  if (!nrow(g)) {
    return(sprintf("No polled race here in these years; uses the national average miss (%s)%s", fmt_miss(err), shift_txt))
  }
  races <- sprintf("%d %s: polls %s, result %s (%s vs. polls)", g$cycle,
                   ifelse(g$race_type == "Senate", "Senate", "president"),
                   fmt_margin(g$poll_margin), fmt_margin(g$actual_margin), fmt_miss(g$error))
  paste0(paste(races, collapse = "\n"),
         if (nrow(g) > 1) sprintf("\nAverage: %s", fmt_miss(err)) else "", shift_txt)
}

scenario_caption <- function(cycles, types, results = NULL) {
  if (!is.null(results)) {
    out <- uncalled(results)
    return(paste0("Each race is placed at its actual 2026 margin in the votes counted so far (AP, via The New York Times), ",
                  "so the bias panel shows how far each state's final polling average missed.",
                  if (length(out)) sprintf(" Not yet called: %s.", paste(out, collapse = ", ")) else ""))
  }
  adj <- ""
  if (length(cycles) == 0 || length(types) == 0) {
    return(paste0("Each race is projected at its current polling average as published, with no polling-error adjustment.", adj))
  }
  kinds <- paste(c(Senate = "Senate", President = "presidential")[types], collapse = " and ")
  yrs <- sort(cycles)
  yr_txt <- if (length(yrs) == 1) yrs else paste(paste(head(yrs, -1), collapse = ", "), "and", tail(yrs, 1))
  what <- if (length(yrs) == 1) {
    sprintf("Each state's polling average is shifted by its %s polling error in %s races.", yr_txt, kinds)
  } else {
    sprintf("Each state's polling average is shifted by its mean polling error in %s races across %s.", kinds, yr_txt)
  }
  paste0(what, " States with no polled race in those years take the national average error.", adj)
}

# Short name of the active assumption, for the seat strip.
scenario_tag <- function(cycles, types, results = NULL) {
  if (!is.null(results)) return(if (length(uncalled(results))) "2026 results (count ongoing)" else "2026 results")
  if (length(cycles) == 0 || length(types) == 0) return("Polls as published")
  yrs <- sort(cycles)
  if (length(yrs) == 1) return(sprintf("%s polling error", yrs))
  if (setequal(yrs, MIDTERMS)) return("Midterm-year polling error")
  if (setequal(yrs, PRES_YEARS)) return("Presidential-year polling error")
  if (setequal(yrs, TRUMP_ERA)) return("Trump-era polling error (2016-2024)")
  if (setequal(yrs, TRUMP_MID)) return("Trump-era midterm polling error (2018, 2022)")
  sprintf("Mean polling error, %s", paste(yrs, collapse = ", "))
}

build_races <- function(d, err_tbl, shift) {
  d$seats |>
    left_join(d$polls, by = "abb") |>
    left_join(err_tbl, by = "abb") |>
    mutate(
      polled = !is.na(poll_margin),
      # custom_move: a bias the reader set for this state with its slider (NA = use the scenario)
      proj_margin = ifelse(polled, poll_margin - ifelse(is.na(custom_move), err - shift, custom_move), NA_real_),
      winner = case_when(
        !polled ~ incumbent_party,            # unpolled = safe hold
        proj_margin > 0 ~ ifelse(opp_party == "I", "I", "D"),
        TRUE ~ "R"
      ),
      flip = winner != incumbent_party,       # an independent taking a seat is a flip too
      bin = ifelse(polled, margin_bin(proj_margin, opp_party), ifelse(incumbent_party == "R", 8L, 1L))
    )
}

# Seats by caucus. D includes the two independents not up (King, Sanders), who
# caucus with Democrats; I = 2026 independents who say they'll caucus with neither.
seat_totals <- function(races) {
  c(D = SEATS_NOT_UP[["D"]] + sum(races$winner == "D"),
    I = sum(races$winner == "I"),
    R = SEATS_NOT_UP[["R"]] + sum(races$winner == "R"))
}

# Who controls: "d", "r" or "split". Republicans hold the chamber at 50 (the Vice
# President breaks ties). Democrats need 51 of their own, or 51 with the 2026
# independents if the reader assumes those caucus with them; otherwise the
# independents hold the balance and control is split.
control_side <- function(totals, ind_with_d = FALSE) {
  dem <- totals[["D"]] + if (isTRUE(ind_with_d)) totals[["I"]] else 0
  if (totals[["R"]] >= 50) "r" else if (dem >= 51) "d" else "split"
}
CONTROL_LABEL <- c(d = "Democrats control", r = "Republicans control", split = "Split control")
CONTROL_COL <- c(d = D_DARK, r = R_DARK, split = INK)
# Font Awesome's donkey and elephant (bundled with Shiny), for the control line.
PARTY_ICON_PATH <- vapply(c(d = "democrat", r = "republican"), function(n) {
  sub('.*<path d="([^"]+)".*', "\\1", as.character(fontawesome::fa(n)))
}, character(1))
party_icon_html <- function(side, size = "1em") {
  if (!side %in% names(PARTY_ICON_PATH)) return(NULL)
  HTML(sprintf('<svg class="party-icon" viewBox="0 0 640 512" style="height:%s;width:auto" aria-hidden="true"><path fill="%s" d="%s"/></svg>',
               size, CONTROL_COL[[side]], PARTY_ICON_PATH[[side]]))
}

# Races in seat order: unpolled D holds, polled races from most Democratic to
# most Republican, then unpolled R holds. Race NEED_FOR_CONTROL is the 51st seat.
race_order <- function(r, results = NULL) {
  r <- r |>
    mutate(key = case_when(polled ~ proj_margin, incumbent_party == "D" ~ Inf, TRUE ~ -Inf)) |>
    arrange(desc(key), abb)
  if (!is.null(results)) {   # the results scenario: the count so far in place of the projection
    res <- transmute(results, abb, res_margin = margin, eevp, called,
                     res_lead = ifelse(abs(margin) < 0.5, sprintf(", %s ahead by %s votes",
                                       ifelse(opp_votes >= rep_votes, opp_name, rep_name), fmt_votes(abs(opp_votes - rep_votes))), ""))
    return(r |> select(-any_of("eevp")) |> left_join(res, by = "abb") |> mutate(tip = ifelse(
      polled,
      sprintf("%s\n%s (%s) v %s (R)\n%s %s, result vs. polls %s\nResult %s (%.0f%% counted%s, %s)",
              state, opp_name, opp_party, rep_name, avg_label, fmt_margin(poll_margin, opp_party), fmt_error(err),
              fmt_margin(proj_margin, opp_party), eevp, res_lead, ifelse(called, "called", "not yet called")),
      sprintf("%s\nNo general-election polls\nResult %s (%.0f%% counted, %s)", state,
              fmt_margin(res_margin), eevp, ifelse(called, "called", "not yet called"))
    )) |> select(-res_margin, -eevp, -called, -res_lead))
  }
  r |>
    mutate(tip = ifelse(
      polled,
      sprintf("%s\n%s (%s) v %s (R)\n%s %s, assumed miss %s\nProjected %s",
              state, opp_name, opp_party, rep_name, avg_label, fmt_margin(poll_margin, opp_party), fmt_error(err),
              fmt_margin(proj_margin, opp_party)),
      sprintf("%s\nNo general-election polls\nAssumed safe %s hold", state, incumbent_party)
    ))
}

# Spread (SD, points) of single-race polling misses, Senate and presidential,
# 2010-2024: the width of the ranked view's bell curves around each polling
# average. Fixed: the scenario picks a point on the curve, it doesn't reshape it.
miss_sd <- function(errors) sd(errors$error)

# ---- Seat horseshoe -------------------------------------------------------------
# All 100 seats along a sideways-U band (SVG viewBox units): Dem/Ind seats not up
# start top-left, the 35 races wrap around the curve from most Democratic to most
# Republican, and Republican seats not up end bottom-left. Races get a fixed,
# roomy length; the not-up blocks are shortened (drawn with break marks) and sized
# so that seat 50|51 lands exactly on the apex.
HS <- list(w = 1100, h = 300, x0 = 30, xc = 790, ymid = 150, r = 118, t = 40, gap = 2.5, race = 38)
HS$arm <- HS$xc - HS$x0
HS$len <- 2 * HS$arm + pi * HS$r

# Centerline point and outward unit normal at distance s along the band.
hs_point <- function(s) {
  if (s <= HS$arm) return(c(HS$x0 + s, HS$ymid - HS$r, 0, -1))
  if (s <= HS$arm + pi * HS$r) {
    th <- -pi / 2 + (s - HS$arm) / HS$r
    return(c(HS$xc + HS$r * cos(th), HS$ymid + HS$r * sin(th), cos(th), sin(th)))
  }
  c(HS$xc - (s - HS$arm - pi * HS$r), HS$ymid + HS$r, 0, 1)
}

# SVG path outlining the stretch of band between s0 and s1 (thickness t).
hs_band <- function(s0, s1, t = HS$t) {
  p <- t(vapply(seq(s0, s1, length.out = max(2, ceiling((s1 - s0) / 5))), hs_point, numeric(4)))
  edge <- function(side) p[, 1:2] + side * t / 2 * p[, 3:4]
  xy <- rbind(edge(1), edge(-1)[nrow(p):1, ])
  paste0("M", paste(sprintf("%.1f,%.1f", xy[, 1], xy[, 2]), collapse = "L"), "Z")
}

horseshoe_svg <- function(ordered, totals, ind_with_d = FALSE) {
  nd <- SEATS_NOT_UP[["D"]]
  nr <- SEATS_NOT_UP[["R"]]
  w <- HS$race
  half <- HS$len / 2
  race_start <- function(i) half - (50 - nd) * w + (i - 1) * w   # race 50 - nd ends at the apex
  esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

  # A not-up block, with a break mark near its outer end since it isn't to scale.
  # `outer` is where the break goes (on a straight arm); the label centers on the
  # stretch between the break and the races.
  block <- function(s0, s1, outer, inner, fill, label) {
    band <- hs_band(s0, s1)
    b <- hs_point(outer)
    y0 <- b[2] - HS$t / 2 - 4
    y1 <- b[2] + HS$t / 2 + 4
    k <- 5   # slant
    mid <- hs_point((outer + inner) / 2)
    sprintf(paste0(
      '<path d="%s" fill="%s"/><path d="%s" fill="url(#hs-hatch)"/>',
      '<path class="hs-break-gap" d="M%.1f,%.1fL%.1f,%.1fL%.1f,%.1fL%.1f,%.1fZ"/>',
      '<line class="hs-break" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
      '<line class="hs-break" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
      '<text class="hs-chunk-label" x="%.1f" y="%.1f">%s</text>'),
      band, fill, band,
      b[1] - 3.5 + k, y0, b[1] + 3.5 + k, y0, b[1] + 3.5 - k, y1, b[1] - 3.5 - k, y1,
      b[1] - 3.5 + k, y0, b[1] - 3.5 - k, y1,
      b[1] + 3.5 + k, y0, b[1] + 3.5 - k, y1,
      mid[1], mid[2], label)
  }
  cells <- vapply(seq_len(nrow(ordered)), function(i) {
    s0 <- race_start(i) + HS$gap / 2
    s1 <- race_start(i + 1) - HS$gap / 2
    mid <- hs_point((s0 + s1) / 2)
    b <- ordered$bin[i]
    tip <- ordered$tip[i]
    if (i == NEED_FOR_CONTROL) tip <- paste0(tip, "\nThe 51st seat: whoever wins it controls the Senate")
    if (isTRUE(ordered$flip[i])) tip <- paste0(tip, "\nFlips party in this scenario")
    ring <- if (isTRUE(ordered$flip[i])) {
      sprintf('<circle class="hs-flip" cx="%.1f" cy="%.1f" r="13" stroke="%s"/>', mid[1], mid[2], BINS$ink[b])
    } else ""
    hatch <- if (b %in% 9:10) sprintf('<path d="%s" fill="url(#hs-ihatch)" pointer-events="none"/>', hs_band(s0, s1)) else ""
    # not yet called: pale stripes and a dashed outline
    if (isTRUE(ordered$pending[i])) hatch <- paste0(hatch, sprintf('<path class="hs-pending" d="%s"/>', hs_band(s0, s1)))
    sprintf(paste0('<path class="hs-cell" d="%s" fill="%s" data-tip="%s"/>%s%s',
                   '<text class="hs-cell-label" x="%.1f" y="%.1f" fill="%s">%s</text>'),
            hs_band(s0, s1), BINS$col[b], esc(tip), hatch, ring, mid[1], mid[2], BINS$ink[b], ordered$abb[i])
  }, "")

  # 50-seat line across the apex.
  apex <- hs_point(half)
  ends <- rbind(apex[1:2] - (HS$t / 2 + 8) * apex[3:4], apex[1:2] + (HS$t / 2 + 12) * apex[3:4])
  lab <- apex[1:2] + (HS$t / 2 + 17) * apex[3:4] - c(0, 9)
  # The 51st seat (first race past the line): a soft shaded wedge behind it that
  # reaches past both edges of the band, as in the ranked view, labeled at its tip.
  k0 <- race_start(NEED_FOR_CONTROL)
  k1 <- race_start(NEED_FOR_CONTROL + 1)
  km <- hs_point((k0 + k1) / 2)
  key_lab <- km[1:2] + (HS$t / 2 + 26) * km[3:4]
  cx <- 500
  HTML(paste0(
    sprintf('<svg class="hs" viewBox="0 0 %d %d" role="img" aria-label="Senate seats: %d Democratic caucus, %d independent, %d Republican">',
            HS$w, HS$h, totals[["D"]], totals[["I"]], totals[["R"]]),
    '<defs><pattern id="hs-hatch" width="7" height="7" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">',
    '<line x1="0" y1="0" x2="0" y2="7" stroke="#ffffff" stroke-opacity="0.16" stroke-width="2.5"/></pattern>',
    '<pattern id="hs-ihatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(-45)">',
    '<line x1="0" y1="0" x2="0" y2="5" stroke="#1b1a17" stroke-opacity="0.22" stroke-width="1.2"/></pattern>',
    '<pattern id="hs-phatch" width="8" height="8" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">',
    '<line x1="0" y1="0" x2="0" y2="8" stroke="#ffffff" stroke-opacity="0.55" stroke-width="3.2"/></pattern></defs>',
    sprintf('<path class="hs-key" d="%s"/>', hs_band(k0, k1, t = HS$t + 36)),
    block(0, race_start(1) - HS$gap / 2, 34, race_start(1), D_DARK, sprintf("%d Dem-caucus seats not up", nd)),
    paste(cells, collapse = ""),
    block(race_start(nrow(ordered) + 1) + HS$gap / 2, HS$len, HS$len - 34, race_start(nrow(ordered) + 1),
          R_DARK, sprintf("%d Republican seats not up", nr)),
    sprintf(paste0('<text class="hs-key-label" x="%.1f" y="%.1f"><tspan font-weight="600">51st seat</tspan>',
                   '<tspan x="%.1f" dy="15"> decides control</tspan></text>'),
            key_lab[1], key_lab[2] + 2, key_lab[1]),
    sprintf('<line class="hs-ctl" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>', ends[1, 1], ends[1, 2], ends[2, 1], ends[2, 2]),
    sprintf('<text class="hs-ctl-label" x="%.1f" y="%.1f">50 seats</text>', lab[1], lab[2]),
    if (totals[["I"]] > 0) {
      paste0(sprintf(paste0('<text class="hs-tally" x="%d" y="%d"><tspan fill="%s">%d</tspan>',
                            '<tspan fill="%s" dx="12">–</tspan><tspan fill="%s" dx="12">%d</tspan>',
                            '<tspan fill="%s" dx="12">–</tspan><tspan fill="%s" dx="12">%d</tspan></text>'),
                     cx, HS$ymid - 8, D_DARK, totals[["D"]], INK_MUTED, I_DARK, totals[["I"]], INK_MUTED, R_DARK, totals[["R"]]))
    } else {
      paste0(sprintf(paste0('<text class="hs-tally" x="%d" y="%d"><tspan fill="%s">%d</tspan>',
                            '<tspan fill="%s" dx="14">–</tspan><tspan fill="%s" dx="14">%d</tspan></text>'),
                     cx, HS$ymid - 8, D_DARK, totals[["D"]], INK_MUTED, R_DARK, totals[["R"]]))
    },
    {
      side <- control_side(totals, ind_with_d)
      lab <- CONTROL_LABEL[[side]]
      icon_w <- 30; half <- nchar(lab) * 4.9   # approx. half the label's width at 19px
      shift <- if (side %in% names(PARTY_ICON_PATH)) (icon_w + 6) / 2 else 0
      paste0(
        if (shift > 0) sprintf('<svg x="%.1f" y="%.1f" width="%d" height="24" viewBox="0 0 640 512"><path fill="%s" d="%s"/></svg>',
                               cx + shift - half - icon_w - 6, HS$ymid + 34 - 18, icon_w, CONTROL_COL[[side]], PARTY_ICON_PATH[[side]]),
        sprintf('<text class="hs-tally-ctl" x="%.1f" y="%d" fill="%s">%s</text>', cx + shift, HS$ymid + 34,
                CONTROL_COL[[side]], lab),
        # results scenario while any race is still being counted
        if (any(ordered$pending | ordered$counting)) sprintf('<text class="hs-prov" x="%d" y="%d">Provisional</text>', cx, HS$ymid + 58) else "")
    },
    "</svg>"
  ))
}

margin_axis_labels <- function(v) ifelse(v == 0, "Even", ifelse(v > 0, paste0("D+", v), paste0("R+", -v)))

# Map states too small to hold a label: anchor point inside the state, and where
# the label sits out in the Atlantic.
OFFSHORE_LABELS <- tribble(
  ~abb, ~lon,   ~lat,  ~lab_lon, ~lab_lat,
  "NH", -71.55, 43.40, -68.2,    43.10,
  "MA", -71.90, 42.35, -68.6,    42.00,
  "RI", -71.52, 41.68, -69.4,    40.90,
  "NJ", -74.45, 40.00, -71.2,    39.50,
  "DE", -75.45, 39.00, -72.6,    38.30
)

# Ranked-margins violins: each race's bell curve, drawn from -Z to +Z standard
# deviations (95% of outcomes) with a darker core over the middle 50%, and
# half-width VIOLIN_W (x units) at the peak.
VIOLIN_Z <- qnorm(0.975)
CORE_Z <- qnorm(0.75)
VIOLIN_W <- 0.42
rgba <- function(hex, a) { v <- col2rgb(hex)[, 1]; sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }
VIOLIN_FILL <- list(outer = c(D = rgba(D_MID, 0.26), R = rgba(R_MID, 0.26), I = rgba(I_MID, 0.26)),
                    core = c(D = rgba(D_MID, 0.70), R = rgba(R_MID, 0.70), I = rgba(I_MID, 0.70)),
                    edge = c(D = rgba(D_MID, 0.80), R = rgba(R_MID, 0.80), I = rgba(I_MID, 0.80)))

# Outline of one bell curve centred on m (spread s) at column x, from -z to +z
# SDs, split at zero. Returns list(D = polygon, R = polygon), each a data frame
# of x/y (or NULL).
violin_parts <- function(x, m, s, cap, z = VIOLIN_Z) {
  ys <- seq(m - z * s, m + z * s, length.out = 31)
  ys <- ys[ys >= -cap & ys <= cap]
  piece <- function(keep) {
    y <- ys[keep]
    if (length(y) < 2) return(NULL)
    w <- VIOLIN_W * exp(-(y - m)^2 / (2 * s^2))
    data.frame(x = c(x - w, rev(x + w)), y = c(y, rev(y)))
  }
  # include the zero crossing in both pieces so they meet cleanly
  if (m - z * s < 0 && m + z * s > 0) ys <- sort(c(ys, 0))
  list(D = piece(ys >= 0), R = piece(ys <= 0))
}

# Legend for the ranked view: one example violin with each part labeled.
ranked_legend <- function() {
  cx <- 290; mid <- 70; zero <- 102; sp <- 27.5   # sp = one SD in px
  ys <- seq(mid - VIOLIN_Z * sp, mid + VIOLIN_Z * sp, length.out = 61)
  shape <- function(keep, fill, z = VIOLIN_Z, edge = "none") {
    y <- ys[keep & abs(ys - mid) <= z * sp + 1e-9]
    w <- 26 * exp(-(y - mid)^2 / (2 * sp^2))
    pts <- paste(sprintf("%.1f,%.1f", c(cx - w, rev(cx + w)), c(y, rev(y))), collapse = " ")
    sprintf('<polygon points="%s" fill="%s" stroke="%s" stroke-width="0.8"/>', pts, fill, edge)
  }
  ys <- sort(c(ys, zero, mid - CORE_Z * sp, mid + CORE_Z * sp))
  lead <- function(x1, y1, x2, y2) sprintf('<line class="lead" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>', x1, y1, x2, y2)
  left <- function(y_from, x_from, y, label) {
    paste0(lead(cx - 66, y, x_from, y_from), sprintf('<text x="%.1f" y="%.1f" text-anchor="end">%s</text>', cx - 72, y, label))
  }
  right <- function(y_from, x_from, y, label) {
    paste0(lead(x_from, y_from, cx + 70, y), sprintf('<text x="%.1f" y="%.1f">%s</text>', cx + 76, y, label))
  }
  top <- mid - VIOLIN_Z * sp
  HTML(paste0(
    '<svg viewBox="0 0 760 150" role="img" aria-label="How to read the ranked margins chart">',
    shape(ys <= zero, VIOLIN_FILL$outer[["D"]], edge = VIOLIN_FILL$edge[["D"]]),
    shape(ys >= zero, VIOLIN_FILL$outer[["R"]], edge = VIOLIN_FILL$edge[["R"]]),
    shape(ys <= zero, VIOLIN_FILL$core[["D"]], z = CORE_Z),
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="2"/>', cx - 44, zero, cx + 44, zero, INK),
    sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="3"/>', cx - 22, mid, cx + 22, mid, INK),
    sprintf('<circle cx="%.1f" cy="%.1f" r="4.5" fill="#ffffff" stroke="%s" stroke-width="1.5"/>', cx, mid + 18, INK),
    left(top + 2, cx, 18, "Full shape: range of past misses*"),
    left(mid + 12, cx - 23, 62, "Dark core: middle 50%"),
    sprintf('<text x="%.1f" y="%.1f" text-anchor="end" class="sub">wider = more common</text>', cx - 72, 77),
    right(mid, cx + 22, 30, "Polling average (center of the shape)"),
    right(mid + 18, cx + 5, 66, "Where this scenario lands"),
    right(zero, cx + 44, 102, "Even: blue above = Dem/Ind ahead,"),
    sprintf('<text x="%.1f" y="%.1f">red below = Republican ahead</text>', cx + 76, 117),
    "</svg>"
  ))
}

# ---- Latest Polls tab pieces -----------------------------------------------------
fmt_short <- function(d) paste0(format(d, "%b. "), as.integer(format(d, "%d")))
fmt_day <- function(d) paste0(format(d, "%A, %b. "), as.integer(format(d, "%d")))
party_col <- function(m) if (abs(m) < 0.05) INK else if (m > 0) D_MID else R_MID
dot_col <- function(m) if (abs(m) < 0.05) INK_MUTED else party_col(m)   # ties stay quiet in dot plots
headline <- function(m, opp_name, rep_name) {
  if (abs(m) < 0.05) return("Even")
  paste0(if (m > 0) opp_name else rep_name, " +", format(round(abs(m), 1)))
}

# Vote shares as one bar: Dem/Ind from the left, Republican from the right,
# undecided/other between; the notch marks 50%.
share_bar <- function(opp, rep) {
  div(class = "lp-bar",
      span(style = sprintf("width:%.1f%%;background:%s", opp, D_MID)),
      span(style = sprintf("width:%.1f%%;background:%s", max(0, 100 - opp - rep), COL_NOT_UP)),
      span(style = sprintf("width:%.1f%%;background:%s", rep, R_MID)))
}

# State averages: one line per race. The margin is drawn as a bar from the Even
# line, toward the left for a Dem/Ind lead and the right for a Republican lead,
# on a scale capped at AV_CAP points (the headline carries the exact number).
AV_CAP <- 20

avg_header <- function() {
  ticks <- c(-AV_CAP, -AV_CAP / 2, 0, AV_CAP / 2, AV_CAP)
  div(class = "lp-cols av-cols",
      span("State"), span("Leader"),
      div(class = "av-axis", lapply(ticks, function(v) {
        pos <- 50 - 50 * v / AV_CAP
        shift <- if (pos == 0) "none" else if (pos == 100) "translateX(-100%)" else "translateX(-50%)"
        span(style = sprintf("left:%.0f%%;transform:%s", pos, shift),
             if (v == 0) "Even" else paste0(if (v > 0) "D+" else "R+", abs(v)))
      })),
      span("Dem / Ind vs Republican"), span("Polls in average"), span("Partisan-sponsored"))
}

# x: one row of seats + polls; polls: that race's individual polls,
# drawn as dots on the same scale (filled = counted in the average, hollow = older).
avg_row <- function(x, polls) {
  m <- x$poll_margin
  w <- 50 * min(abs(m), AV_CAP) / AV_CAP
  pos <- function(v) 50 - 50 * pmax(pmin(v, AV_CAP), -AV_CAP) / AV_CAP
  note <- sub(" \\(.*", "", coalesce(x$note, ""))
  polls <- polls[order(polls$in_average, polls$date), ]          # current polls drawn on top
  jit <- 18 + 64 * ((seq_len(nrow(polls)) * 0.618034) %% 1)       # steady vertical jitter, % of track
  dots <- lapply(seq_len(nrow(polls)), function(i) {
    p <- polls[i, ]
    span(class = paste("dot", if (isTRUE(p$in_average)) "in" else "old"),
         style = sprintf("left:%.1f%%;top:%.0f%%;--c:%s", pos(p$margin), jit[i], dot_col(p$margin)),
         title = sprintf("%s, %s: %s%s", p$pollster, fmt_short(p$date), fmt_margin(p$margin),
                         if (isTRUE(p$in_average)) " (in average)" else ""))
  })
  div(class = "av-row", `data-abb` = x$abb,
      div(class = "av-state", title = "Show this state's polls",
          span(class = "av-name", x$state),
          span(class = "lp-held", paste0(x$incumbent_party, "-held", if (nzchar(note)) paste0(", ", note)))),
      span(class = "lp-head-txt", style = sprintf("color:%s", party_col(m)), headline(m, x$opp_name, x$rep_name)),
      div(class = "av-bar", title = "Show this state's polls",
          span(class = "tick", style = "left:25%"), span(class = "tick", style = "left:75%"),
          span(class = "fill", style = sprintf("%s:%.1f%%;width:%.1f%%;background:%s",
                                               if (m > 0) "right" else "left", 50, w, party_col(m))),
          span(class = "mid"),
          dots,
          span(class = "avg", style = sprintf("left:%.1f%%;background:%s", pos(m), party_col(m)))),
      div(class = "av-match", title = sprintf("%.1f%% %s (%s), %.1f%% %s (R)", x$opp_pct, x$opp_name, x$opp_party,
                                              x$rep_pct, x$rep_name),
          sprintf("%s (%s) %.1f  –  %.1f %s (R)", x$opp_name, x$opp_party, x$opp_pct, x$rep_pct, x$rep_name)),
      span(class = "lp-meta", sprintf("%d poll%s, latest %s", x$n_polls_in_avg, if (x$n_polls_in_avg == 1) "" else "s", fmt_short(x$latest_poll))),
      span(class = "lp-meta", {
        cur <- polls[polls$in_average, ]
        n_part <- sum(!is.na(cur$partisan))
        if (n_part == 0) "—" else sprintf("%d of %d", n_part, nrow(cur))
      }))
}

# Short label for a poll's partisan sponsor (Silver Bulletin's flag).
sponsor_label <- function(x) {
  if (is.na(x)) return(NULL)
  switch(x, D = "Dem sponsor", R = "Rep sponsor", IND = "Ind sponsor", paste(x, "sponsor"))
}

# All polls: one dense line per poll (x: one row of race_polls joined to the race).
poll_header <- function() {
  div(class = "lp-cols pr-cols",
      span("State"), span("Margin"), span("Pollster"), span("Sample"), span("House effect"),
      span("Result (Dem / Ind – Rep)"), span(class = "pr-in", "In avg"))
}

poll_row <- function(x) {
  grade <- if (is.na(x$sb_grade)) "" else if (startsWith(x$sb_grade, "A")) "g-a" else if (startsWith(x$sb_grade, "B")) "g-b" else "g-c"
  name <- if (is.na(x$poll_url)) x$pollster else a(href = x$poll_url, target = "_blank", rel = "noopener", x$pollster)
  div(class = "pr-row",
      span(x$state),
      span(class = "lp-head-txt", style = sprintf("color:%s", party_col(x$margin)),
           headline(x$margin, x$opp_name, x$rep_name)),
      div(class = "pr-pollster", name,
          if (!is.na(x$sb_grade)) span(class = paste("lp-grade", grade), title = "Silver Bulletin grade", x$sb_grade),
          if (!is.na(x$partisan)) span(class = "lp-sponsor", title = "Conducted for a partisan sponsor (per Silver Bulletin)",
                                       sponsor_label(x$partisan))),
      span(class = "lp-meta", if (is.na(x$sample_n)) "" else trimws(paste(format(x$sample_n, big.mark = ","),
                                                                            coalesce(x$population, "")))),
      span(class = "lp-meta",
           if (is.na(x$sb_house)) span(title = "Not rated by Silver Bulletin", "—")
           else span(style = sprintf("color:%s", party_col(x$sb_house)), fmt_lean(x$sb_house))),
      div(class = "pr-result",
          title = sprintf("%g%% %s (%s), %g%% %s (R)", x$opp_pct, x$opp_name, x$opp_party, x$rep_pct, x$rep_name),
          tags$b(style = sprintf("color:%s", D_MID), sprintf("%g", x$opp_pct)),
          share_bar(x$opp_pct, x$rep_pct),
          tags$b(style = sprintf("color:%s", R_MID), sprintf("%g", x$rep_pct))),
      span(class = "pr-in", title = "Carries weight in Silver Bulletin's current average", if (isTRUE(x$in_average)) "✓"))
}

# ---- Early Vote tab pieces -------------------------------------------------------
# UF Election Lab figures per Senate state: early votes cast so far, as a share of ALL
# 2022 votes (early and Election Day), and the registered party of early voters where
# the state records it for most of them. Context only; nothing here feeds the scenarios.
EV_CAP <- 1.2   # bar scale runs to 120% of 2022 turnout
fmt_big <- function(n) ifelse(n >= 1e6, sprintf("%.1fM", n / 1e6), ifelse(n >= 1e3, sprintf("%.0fK", n / 1e3), format(n)))
ev_party_ok <- function(x) {
  tot <- x$dem + x$rep + x$other
  isTRUE(x$voted > 0 && tot >= 0.95 * x$voted && (x$dem + x$rep) >= 0.5 * x$voted)
}
ev_header <- function() {
  div(class = "lp-cols ev-cols",
      span("State"), span("Early votes cast"),
      div(class = "av-axis", lapply(c(0, 0.5, 1), function(v) {
        pos <- 100 * v / EV_CAP
        span(style = sprintf("left:%.1f%%;transform:%s", pos, if (v == 0) "none" else "translateX(-50%)"),
             if (v == 1) "100% of 2022" else sprintf("%.0f%%", 100 * v))
      })),
      span("Registered party of early voters"), span("Updated"))
}
ev_row <- function(x) {
  share <- x$voted / x$turn_2022
  w <- 100 * min(share, EV_CAP) / EV_CAP
  note <- sub(" \\(.*", "", coalesce(x$note, ""))
  party <- if (ev_party_ok(x)) {
    tot <- x$dem + x$rep + x$other
    p <- 100 * c(x$dem, x$rep, x$other) / tot
    div(class = "ev-party", title = "Registered party of the voters who have cast early ballots, not how they voted",
        div(class = "ev-pbar",
            span(style = sprintf("width:%.1f%%;background:%s", p[1], D_MID)),
            span(style = sprintf("width:%.1f%%;background:%s", p[2], R_MID)),
            span(style = sprintf("width:%.1f%%;background:%s", p[3], COL_NOT_UP))),
        div(class = "ev-pnum", span(class = "hm-d", sprintf("D %.0f%%", p[1])), span(class = "hm-r", sprintf("R %.0f%%", p[2])),
            span(sprintf("Other %.0f%%", p[3]))))
  } else {
    span(class = "lp-meta ev-none", title = "This state doesn't record party for most early voters", "Not recorded")
  }
  div(class = "ev-row",
      div(class = "av-state", span(class = "av-name", x$state),
          span(class = "lp-held", paste0(x$incumbent_party, "-held", if (nzchar(note)) paste0(", ", note)))),
      div(class = "ev-n", tags$b(format(x$voted, big.mark = ",")),
          # the split only where the state reports both parts
          if (isTRUE(x$mail > 0 && x$inperson > 0)) span(class = "lp-meta", sprintf("%s mail, %s in person", fmt_big(x$mail), fmt_big(x$inperson)))),
      div(class = "ev-bar", title = sprintf("%s early votes so far, %.0f%% of the %s votes cast in all of 2022",
                                            format(x$voted, big.mark = ","), 100 * share, format(x$turn_2022, big.mark = ",")),
          span(class = "tick", style = sprintf("left:%.1f%%", 100 * 0.5 / EV_CAP)),
          span(class = "full", style = sprintf("left:%.1f%%", 100 / EV_CAP)),
          span(class = "fill", style = sprintf("width:%.1f%%", w)),
          span(class = "ev-pct", style = sprintf("left:%.1f%%", w), sprintf("%.0f%%", 100 * share))),
      party,
      span(class = "lp-meta", if (is.na(x$updated)) "" else fmt_short(x$updated)))
}
# The whole list: closest polling races first (as on Polling Averages), then the rest.
ev_list_ui <- function(seats, polls, ev) {
  if (is.null(ev) || !nrow(ev)) return(p(class = "lp-unpolled", "Early-vote data isn't available right now."))
  a <- seats |> left_join(select(polls, abb, poll_margin), by = "abb") |> left_join(ev, by = "abb") |>
    arrange(is.na(poll_margin), abs(poll_margin), state)
  have <- filter(a, !is.na(voted))
  missing <- filter(a, is.na(voted))
  tagList(
    ev_header(),
    lapply(seq_len(nrow(have)), function(i) ev_row(have[i, ])),
    if (nrow(missing)) p(class = "lp-unpolled", tags$b("No early-vote data reported: "),
                         paste(missing$state, collapse = ", "), ".")
  )
}

# ---- UI -----------------------------------------------------------------------
theme <- bs_theme(
  version = 5, bg = SURFACE, fg = INK, primary = INK,
  base_font = font_collection("Newsreader", "Georgia", "serif"),
  heading_font = font_collection("Newsreader", "Georgia", "serif"),
  "border-color" = GRID
)

scenario_sidebar <- sidebar(
  width = 310, open = list(desktop = "open", mobile = "always-above"),
  div(class = "scen-box",
      div(class = "scen-kicker", "Polling-error assumption"),
      div(class = "seg seg-full",
          radioButtons("miss_mode", NULL, inline = TRUE,
                       choices = c("Polls as published" = "none", "Historical error" = "past",
                                   if (!is.null(INIT$results)) c("2026 results" = "actual")))),
      conditionalPanel(
        "input.miss_mode == 'past'",
        div(class = "scen-sub", "Shift each state's polling average by its polling error in"),
        div(class = "preset-list",
            actionButton("cy_mid", tagList("Midterm years", span("2010, 2014, 2018, 2022")), class = "preset"),
            actionButton("cy_pres", tagList("Presidential years", span("2012, 2016, 2020, 2024")), class = "preset"),
            actionButton("cy_trump", tagList("Trump-era years", span("2016, 2018, 2020, 2022, 2024")), class = "preset"),
            actionButton("cy_tmid", tagList("Trump-era midterms", span("2018, 2022")), class = "preset"),
            actionButton("cy_all", tagList("All years", span("2010–2024")), class = "preset")),
        div(class = "scen-sub", "or specific years"),
        div(class = "chips chips-yr",
            checkboxGroupInput("cycles", NULL, choices = CYCLES, selected = MIDTERMS, inline = TRUE)),
        div(class = "chips",
            checkboxGroupInput("types", "Error measured in", choices = c("Senate" = "Senate", "Presidential" = "President"),
                               selected = c("Senate", "President"), inline = TRUE))
      ),
      div(class = "scenario-caption", textOutput("scenario_caption", inline = TRUE))),
  div(class = "shift-wrap", title = "Double-click the slider to reset to 0",
      div(class = "shift-head",
          tags$label(`for` = "shift", "Extra uniform shift"),
          conditionalPanel("input.shift != 0", tags$a(href = "#", class = "shift-reset", onclick = "return resetShift();", "Reset to 0"))),
      sliderInput("shift", NULL, min = -10, max = 10, value = 0, step = 0.5, ticks = FALSE),
      div(class = "shift-ends", span(class = "hm-d", "← D"), span(class = "hm-r", "R →"))),
  input_switch("ind_with_d", "Assume independents caucus with Democrats", value = FALSE),
  div(class = "how-link", a(href = "#about-errors", onclick = "return goToErrors();", icon("circle-question"),
                            "How are historical polling errors measured?")),
  hr(),
  # Data status: the vote count's time and the final polls it is compared with.
  div(class = "freshness-final", role = "status", uiOutput("freshness")),
  if (!IS_BROWSER && !IS_STATIC) actionButton("refresh", "Refresh polls", icon = icon("rotate"), class = "btn-sm btn-outline-secondary mt-2")
)

# Margin key: the eight tiers as one stepped bar, cut points labeled underneath.
legend_html <- div(
  class = "legend",
  span(class = "spec",
       span(class = "spec-end hm-d", "D"),
       span(class = "spec-bar",
            lapply(1:8, function(i) span(class = "spec-seg", title = BINS$bin[i],
                                                         style = sprintf("background:%s", BINS$col[i]))),
            lapply(seq_along(c(10, 5, 2, 0, 2, 5, 10)), function(j) {
              span(class = "spec-cut", style = sprintf("left:%.3f%%", 100 * j / 8),
                   c("10", "5", "2", "Even", "2", "5", "10")[j])
            })),
       span(class = "spec-end hm-r", "R")),
  span(title = "An independent who has said they will caucus with neither party leads (Ind+0 to 5, Ind+5 or more)",
       span(class = "sw sw-ind", style = sprintf("background:linear-gradient(90deg,%s 50%%,%s 50%%)", BINS$col[10], BINS$col[9])),
       "Independent ahead"),
  span(span(class = "flip-ring"), "Flips party"),
  conditionalPanel("input.miss_mode == 'actual'", inline = TRUE, class = "lg-pending",
                   title = "Race not yet called; the margin is the count so far", span(class = "sw sw-pending"), "Not yet called")
)

state_choices <- with(arrange(INIT$seats, state), setNames(abb, state))

ui <- page_navbar(
  id = "main_nav",
  # Title, then a flat rule in the eight D-to-R margin tiers, then the subtitle.
  title = div(class = "brand",
              span(class = "brand-head",
                   span(class = "brand-title", span(class = "bt-name", "Margin of Error"), span(class = "bt-sep", "|"),
                        "The 2026 Senate Stress Test"),
                   span(class = "brand-rule", title = "An estimate and its margin of error",
                        lapply(BINS$col[1:8], function(c) span(class = "tier", style = sprintf("background:%s", c))),
                        span(class = "moe-band"), span(class = "moe-est"))),
              span(class = "brand-sub", "How the 2026 Senate outlook changes when today's polling averages are adjusted by past state-level polling errors.")),
  window_title = "Margin of Error | The 2026 Senate Stress Test",
  theme = theme,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = GOOGLE_FONTS),
    # Site icon: the seat horseshoe (www/favicon.svg; PNGs for browsers and phones that need them)
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$style(HTML(APP_CSS)),
    tags$script(HTML(APP_JS))
  ),
  nav_panel(
    "Scenario Explorer",
    layout_sidebar(border = FALSE, border_radius = FALSE, bg = SURFACE,
      sidebar = scenario_sidebar,
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              uiOutput("scenario_title", inline = TRUE),
              div(class = "chips view-toggle", style = if (!SHOW_RANKED) "display: none;",
                  radioButtons("seat_view", NULL, inline = TRUE,
                               choices = c("Horseshoe" = "horseshoe", "Ranked margins" = "ranked"))))
        ),
        card_body(
          fillable = FALSE,
          legend_html,
          conditionalPanel(
            "input.seat_view == 'horseshoe'",
            uiOutput("m_tally"),
            div(class = "seat-scroll", uiOutput("horseshoe")),
            div(class = "swipe-hint", "← Swipe the chart to see every seat")
          ),
          conditionalPanel(
            "input.seat_view == 'ranked'",
            # Browser build: the chart is created only when shown (a hidden one can initialize before
            # the charting library has loaded there and stay blank).
            if (IS_BROWSER) uiOutput("ranked_wrap") else div(class = "seat-scroll seat-scroll-rk", plotlyOutput("ranked", height = "440px")),
            div(class = "swipe-hint", "Swipe the chart to see every race →"),
            div(class = "rk-legend", ranked_legend(), div(class = "rk-note", textOutput("ranked_caption", inline = TRUE)))
          ),
          uiOutput("year_strip"),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(8, 4)),
            div(class = "map-wrap",
                div(class = "map-key", span(class = "sw", style = sprintf("background:%s", COL_NOT_UP)), "Not up in 2026"),
                plotlyOutput("map", height = "420px")),
            div(class = "bias-panel",
                conditionalPanel("input.miss_mode == 'none' && input.shift == 0 && !output.bias_is_custom",
                  div(class = "bias-overlay",
                      div(class = "bias-note",
                        tags$b("No polling error applied."), tags$br(),
                        "Select a year or scenario to apply historical error, or drag a state to set its own."))),
                div(class = "bias-title", textOutput("bias_title", inline = TRUE)),
                div(class = "bias-axis", span("← toward Dem"), span("toward Rep →")),
                uiOutput("bias_list"))
          )
        )
      )
    )
  ),
  nav_panel(
    "Historical Polling Misses",
    div(class = "lp-head",
        h1(class = "lp-title", "Historical Polling Misses"),
        p(class = "lp-dek",
          "How far each state's final polling average landed from the actual result, Senate and presidential races ",
          "2010-2024. ", span(class = "hm-r", "Red"), " means the Republican did better than the polls; ",
          span(class = "hm-d", "blue"), " means the Democrat (or a Democratic-aligned independent) did."),
        div(class = "lp-updated",
            "Polling average = all polls in the final 21 days. Source: Silver Bulletin raw polls, the continuation of 538's archive.")),
    uiOutput("hm_tiles"),
    h3(class = "hm-h", "Election by election"),
    p(class = "hm-sub", "Average miss across every state with a polled race, by how far and toward which party."),
    plotlyOutput("natl_bias", height = "270px"),
    h3(class = "hm-h", "State by state"),
    div(class = "seg", radioButtons("hm_view", NULL, inline = TRUE,
                                    choices = c("State averages" = "rows", "Year-by-year grid" = "grid"))),
    div(class = "hm-filters",
        div(class = "hm-fgroup",
            span(class = "lp-lab", "Races"),
            div(class = "seg seg-sm", radioButtons("hm_type", NULL, inline = TRUE,
                                                   choices = c("Both" = "Both", "Senate" = "Senate", "Presidential" = "President")))),
        div(class = "hm-fgroup",
            span(class = "lp-lab", "Elections"),
            actionButton("hm_all", "All", class = "hm-preset"),
            actionButton("hm_mid", "Midterms", class = "hm-preset"),
            actionButton("hm_pres", "Presidential years", class = "hm-preset"),
            actionButton("hm_trump", "Trump-era years", class = "hm-preset"),
            actionButton("hm_tmid", "Trump-era midterms", class = "hm-preset"),
            div(class = "chips", checkboxGroupInput("hm_years", NULL, choices = CYCLES, selected = CYCLES, inline = TRUE))),
        input_switch("hm_2026_only", "Only states with a 2026 Senate race", TRUE)),
    conditionalPanel(
      "input.hm_view == 'rows'",
      p(class = "hm-sub",
        "The bar and colored tick show each state's average miss across the selected races; dots are the individual ",
        "races (", span(class = "hm-key sen"), " Senate, ", span(class = "hm-key pres"), " presidential). Sorted from ",
        "where polls most overstated Democrats (top) to where they most overstated Republicans. Hover a dot for details."),
      uiOutput("hm_rows")
    ),
    conditionalPanel(
      "input.hm_view == 'grid'",
      p(class = "hm-sub",
        "Every state and election at once. Each cell is the miss in points, colored by who beat the polls; ",
        "hover a cell for the polling average and result. Same order as the state averages."),
      uiOutput("hm_grid")
    )
  ),
  nav_panel(
    "Polling Averages",
    div(class = "lp-head",
        h1(class = "lp-title", "State Polling Averages"),
        p(class = "lp-dek", "Silver Bulletin's polling average for every 2026 Senate race, closest first, with each poll behind it."),
        div(class = "lp-updated", textOutput("avg_updated", inline = TRUE))),
    div(class = "lp-filters",
        div(span(class = "lp-lab", "State"),
            selectizeInput("avg_state", NULL, choices = c("All states" = "all", state_choices), width = "210px"))),
    div(
      uiOutput("avg_list"),
      p(class = "lp-foot",
        "Closest races first; click a state or its bar to see its polls. The shaded bar and tick show the average, ",
        "running from Even toward the leader. Dots are individual polls: solid = carries weight in Silver Bulletin's ",
        "average today; faint = an older poll that no longer counts. Scale capped at 20 points. Silver Bulletin's average ",
        "weights more reliable polls more heavily, adjusts for house effects and likely- vs. registered-voter samples, and ",
        "updates older polls for later national trends, so it can sit away from the raw dots. Partisan-sponsored = polls ",
        "in today's average conducted for a party, candidate or allied group.")
    )
  ),
  nav_panel(
    "All Latest Polls",
    div(class = "lp-head",
        h1(class = "lp-title", "All Latest Polls"),
        p(class = "lp-dek", "Every general-election Senate poll in Silver Bulletin's averages, newest first, with ",
          "pollster grades and house effects."),
        div(class = "lp-updated", textOutput("lp_updated", inline = TRUE))),
    div(class = "lp-filters",
        div(span(class = "lp-lab", "State"),
            selectizeInput("lp_state", NULL, choices = c("All states" = "all", state_choices), width = "210px")),
        div(span(class = "lp-lab", "Pollster"),
            selectizeInput("lp_pollster", NULL, choices = c("All pollsters" = "all"), width = "250px")),
        input_switch("lp_in_avg", "Only polls counted in today's averages", FALSE)),
    div(
      uiOutput("poll_list"),
      div(class = "text-center my-3", uiOutput("poll_more")),
      p(class = "lp-foot",
        "Polls largely collected by ", a("The New York Times", href = "https://www.nytimes.com/interactive/polls/senate-polls-2026.html", target = "_blank", .noWS = "after"), " and averaged by Silver Bulletin. ",
        "✓ = carries weight in Silver Bulletin's current average for that race. Sponsor notes mark polls conducted for a ",
        "party, candidate or allied group. Grade and house effect: Silver Bulletin's January 2026 pollster ",
        "ratings (filled badge = A range); house effect is how a firm's polls have run against other polls of the same ",
        "races, so R+3.6 means 3.6 points more Republican. Hover a result for candidate names.")
    )
  ),
  if (SHOW_EARLY_VOTE) nav_panel(
    "Early Vote",
    div(class = "lp-head",
        h1(class = "lp-title", "Early Vote"),
        p(class = "lp-dek", "Ballots already cast in each 2026 Senate state, by mail and in person, with each state's ",
          "total 2022 turnout for scale."),
        div(class = "lp-updated", "Early vote data: ",
            a(href = "https://election.lab.ufl.edu/early-vote/2026-early-voting/", target = "_blank", rel = "noopener", "UF Election Lab", .noWS = "after"),
            ". ", textOutput("ev_updated", inline = TRUE))),
    div(class = "ev-note",
        tags$b("Turnout, not vote choice."), " These are ballots cast, not counted results. The party column is voters' ",
        "registration, not how they voted, and only appears where a state records it. Republicans increasingly vote on ",
        "Election Day, so an early Democratic edge is common and says little on its own. This tab is context only: it ",
        "doesn't feed the scenarios."),
    div(
      uiOutput("ev_list"),
      p(class = "lp-foot",
        "Closest polling races first. The bar shows early votes so far as a share of all votes cast in the state in 2022 ",
        "(early and Election Day); the mark is 100%. Figures are the UF Election Lab's, compiled from state and county ",
        "election offices; states update on different schedules (see Updated).")
    )
  ),
  nav_panel(
    "About the Data",
    card(
      card_body(
        class = "about",
        h5("What this is"),
        p(tags$b("A scenario explorer, not a forecast."), " It asks one question: if 2026's polls miss the way polls ",
          "have missed before, who controls the Senate?"),
        p("All polling averages, poll weights and pollster ratings are Silver Bulletin's. This site builds no averages or ",
          "forecasts of its own: it takes Silver Bulletin's 2026 averages as published and applies past polling errors to them."),

        h5("Where the numbers come from"),
        tags$ul(
          tags$li(tags$b("2026 polling averages and polls:"), " Silver Bulletin, which updates them daily. This site pulls ",
                  "them on refresh; the time of the last pull is shown in the sidebar. Silver Bulletin draws most of ",
                  "its 2026 election polls from ", a("The New York Times", href = "https://www.nytimes.com/interactive/polls/senate-polls-2026.html", target = "_blank", .noWS = "after"), "'s poll database, so the individual polls are ",
                  "largely collected by The Times; the averaging, weighting and adjustments are Silver Bulletin's."),
          tags$li("Silver Bulletin's averages weight more reliable polls more heavily and adjust for house effects, ",
                  "likely- vs. registered-voter samples and later national trends. Polls conducted for a party, candidate ",
                  "or allied group are labeled as partisan-sponsored."),
          tags$li(tags$b("Pollster grades and house effects:"), " Silver Bulletin's January 2026 pollster ratings."),
          tags$li(tags$b("Races without a Silver Bulletin average"), " are treated as safe holds for the incumbent party: ",
                  textOutput("about_unpolled", inline = TRUE))
        ),

        div(id = "about-errors", class = "about-callout",
          h5("How historical polling errors are measured"),
          tags$ul(
            tags$li(tags$b("Source:"), " Silver Bulletin's raw-polls archive (the continuation of FiveThirtyEight's), which ",
                    "holds every poll of each Senate and presidential general election taken in the final 21 days of the race, ",
                    "with the certified result. This site uses 2010–2024."),
            tags$li(tags$b("The miss:"), " for each state, year and race, a simple, unweighted average of those polls' margins, ",
                    "minus the certified margin. Positive means the polls overstated the Democrat (or leading non-Republican); ",
                    "negative means they overstated the Republican."),
            tags$li(tags$b("What's included:"), " a special election in the same state and year is averaged with the regular ",
                    "race. Runoffs, and races without exactly one Republican among the top two candidates, are left out."),
            tags$li(tags$b("In a scenario:"), " a state's assumed miss is the average of its misses across the selected years ",
                    "and race types, each race counting once (so in a presidential year, a state with a Senate race too ",
                    "contributes two). A state with no polled race in those years takes the average of all selected races nationally."),
            tags$li(tags$b("Overall:"), " ", textOutput("about_bias", inline = TRUE), " The Historical Polling Misses tab ",
                    "breaks this down by year and state."),
            tags$li("This calculation is this site's own, not Silver Bulletin's: a plain average of the final polls, not a ",
                    "weighted polling average like the ones used for 2026.")
          )
        ),

        h5("How a scenario is built"),
        tags$ol(
          tags$li("Start from each race's 2026 polling average."),
          tags$li("Subtract the assumed miss. ", tags$i("Polls as published"), " assumes none; a preset, a single year or any ",
                  "mix of years uses each state's average miss in those years and race types (Senate, presidential or both)."),
          tags$li("Apply the extra uniform shift, which moves every race the same number of points toward one party."),
          tags$li("The leader in the projected margin wins the seat. Margin = the leading non-Republican (a Democrat, or an ",
                  "independent shown as Ind+) minus the Republican, in points.")
        ),
        p(tags$b("Control:"), " 34 Democratic-caucus seats (including independents King and Sanders) and 31 Republican seats ",
          "are not up. Republicans control at 50 seats, since the Vice President breaks ties; Democrats need 51 of their own. ",
          "Independents running in 2026 ", textOutput("about_inds", inline = TRUE), " are counted separately. If neither ",
          "party reaches its mark, control is shown as split, unless the sidebar switch assumes independents caucus with Democrats."),

        h5("Reading the views"),
        tags$ul(
          tags$li(tags$b("Horseshoe:"), " all 100 seats, with races ordered from most Democratic to most Republican. The line ",
                  "marks 50 seats; the shaded race is the 51st seat counted from the Democratic end, which decides control ",
                  "unless independents win seats. A ring marks a race that flips party."),
          if (SHOW_RANKED) tags$li(tags$b("Ranked margins:"), " each race's bell curve is centered on its polling average (black tick) and ",
                  "extends above and below it as far as single-race polling misses have run, 2010–2024. The full shape spans ±1.96 SD of those past misses ",
                  "and the dark core the middle 50%; ", textOutput("about_cover", inline = TRUE), " The white dot is where ",
                  "the current scenario puts the race. The shapes describe past misses, not a 2026 probability, and the ",
                  "normal curve is a simplification: there are too few races per state to draw each state's own shape."),
          tags$li(tags$b("Year strip:"), " the seat count if 2026's polls miss exactly as each single year's did, shaded by ",
                  "who would control (a 50–50 Senate is Republican, through the Vice President's tiebreak). Click a year to apply it."),
          tags$li(tags$b("Map and assumed bias:"), " the map colors each state by projected margin; the panel beside it shows ",
                  "how far the scenario moves each race from its polls. Hover a state in either to see the past races behind its number.")
        ),

        h5("Limitations"),
        tags$ul(
          tags$li(tags$b("Raw misses, adjusted averages."), " Past misses are measured on simple averages of raw polls, while ",
                  "Silver Bulletin's 2026 averages are adjusted. Some of a past miss may already be corrected for in today's ",
                  "averages, so the scenarios may overstate how far the polls move."),
          tags$li(tags$b("Few races per state."), " Each state has only a handful of past races, so its average miss partly ",
                  "reflects who was running: Kansas in 2014 (Roberts vs. Orman), or Maine's Susan Collins, who is on the ",
                  "ballot again. That may be a lasting pattern or a one-off; the hover lists let you judge."),
          tags$li(tags$b("The past need not repeat."), " Misses have run in both directions and varied from year to year. ",
                  "No scenario here is more likely than another."),
          tags$li(tags$b("Assumptions at the edges."), " Unpolled races are assumed safe holds, and how independents would ",
                  "caucus is an assumption either way.")
        ),

        h5("Sources"),
        tags$ul(
          tags$li(a("Silver Bulletin 2026 Senate forecast", href = "https://www.natesilver.net/p/nate-silver-2026-midterm-election-senate-model", target = "_blank", .noWS = "after"),
                  ": the 2026 polling averages and the polls behind them."),
          tags$li(a("Silver Bulletin pollster ratings", href = "https://www.natesilver.net/p/pollster-ratings-silver-bulletin", target = "_blank", .noWS = "after"),
                  ": pollster grades and house effects, and the raw-polls archive behind the historical misses."),
          tags$li(a("The New York Times poll database", href = "https://www.nytimes.com/interactive/polls/senate-polls-2026.html", target = "_blank", .noWS = "after"),
                  ": the original source of most of the 2026 polls in Silver Bulletin's averages."),
          tags$li(a("The New York Times 2026 Senate results", href = "https://www.nytimes.com/interactive/2026/11/03/us/elections/results-senate.html", target = "_blank", .noWS = "after"),
                  ": the 2026 vote counts behind the 2026 results scenario (Associated Press data). Margins are of the votes counted so far."),
          if (SHOW_EARLY_VOTE) tags$li(a("UF Election Lab, 2026 early vote", href = "https://election.lab.ufl.edu/early-vote/2026-early-voting/", target = "_blank", .noWS = "after"),
                  ": early votes cast by state, early voters' party registration, and 2022 turnout (the Early Vote tab).")
        )
      )
    )
  ),
  nav_spacer(),
  # Credit strip under the tabs: data source (left, ~70%) | code (right, ~30%), two lines each.
  nav_item(class = "credit-li", div(class = "credit-row",
    div(class = "credit-src",
      div(class = "cr-line",
          span(class = "ds-k", "Polling data & averages"),
          a(class = "sb-link", href = "https://www.natesilver.net/p/nate-silver-2026-midterm-election-senate-model", target = "_blank", rel = "noopener",
            img(class = "sb-icon", src = "silver-bulletin.png", alt = ""), span("Silver Bulletin"))),
      div(class = "cr-sub", "Polling averages and poll weights, pollster ratings, historical polls"),
      if (!is.null(INIT$results)) div(class = "cr-line cr-res", span(class = "ds-k", "2026 results"),
          a(href = "https://www.nytimes.com/interactive/2026/11/03/us/elections/results-senate.html", target = "_blank", rel = "noopener",
            "The New York Times / AP")),
      if (SHOW_EARLY_VOTE) div(class = "cr-line cr-ev", span(class = "ds-k", "Early vote"),
          a(href = "https://election.lab.ufl.edu/early-vote/2026-early-voting/", target = "_blank", rel = "noopener", "UF Election Lab"))),
    div(class = "credit-code",
      div(class = "cr-line", span(class = "ds-k", "Code")),
      a(class = "gh-link", href = "https://github.com/KeepTheOutliers/margin-of-error", target = "_blank", rel = "noopener",
        icon("github"), "KeepTheOutliers"))))
)

# ---- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  d <- reactiveVal(INIT)

  observeEvent(input$refresh, {
    withProgress(message = "Pulling Silver Bulletin averages...", value = 0.3, {
      ok <- tryCatch({ refresh_all(history = FALSE); TRUE }, error = function(e) conditionMessage(e))
      if (isTRUE(ok)) {
        d(load_data())
        showNotification("Polls refreshed.", type = "message")
      } else {
        showNotification(paste("Refresh failed:", ok), type = "error", duration = 10)
      }
    })
  })

  observeEvent(input$cy_mid, updateCheckboxGroupInput(session, "cycles", selected = MIDTERMS))
  observeEvent(input$cy_pres, updateCheckboxGroupInput(session, "cycles", selected = PRES_YEARS))
  observeEvent(input$cy_trump, updateCheckboxGroupInput(session, "cycles", selected = TRUMP_ERA))
  observeEvent(input$cy_tmid, updateCheckboxGroupInput(session, "cycles", selected = TRUMP_MID))
  observeEvent(input$cy_all, updateCheckboxGroupInput(session, "cycles", selected = CYCLES))

  style_plot <- function(p, ...) {
    p <- p |>
      layout(
        font = list(family = APP_FONT, color = INK_2, size = 13),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
        hoverlabel = list(bgcolor = "#ffffff", bordercolor = GRID,
                          font = list(family = APP_FONT, color = INK)),
        ...
      ) |>
      config(displayModeBar = FALSE) |>
      plotly_build()
    # plotly R expands hoverinfo = "skip" into one value per point, and plotly.js
    # only ignores a trace when hoverinfo is the single string "skip"; otherwise
    # label and outline traces steal hover from the shapes underneath them.
    p$x$data <- lapply(p$x$data, function(tr) {
      if (length(tr$hoverinfo) > 1 && identical(unique(tr$hoverinfo), "skip")) tr$hoverinfo <- "skip"
      tr
    })
    p
  }

  # "No bias" ignores the year chips (they keep their selection for next time).
  sel_cycles <- reactive({
    if (!identical(input$miss_mode, "past")) return(numeric(0))
    as.numeric(input$cycles %||% character(0))
  })
  sel_types <- reactive(input$types %||% character(0))

  # Data the scenarios run on: Silver Bulletin's averages as published.
  scen <- reactive({
    x <- d()
    x$polls <- mutate(x$polls, avg_label = "Poll avg")
    x
  })

  # Per-state biases the reader set by dragging a state's slider in the assumed-bias panel
  # (points; > 0 = toward Republicans). They replace that state's scenario move until reset.
  bias_custom <- reactiveVal(setNames(numeric(0), character(0)))
  observeEvent(input$bias_set, {
    b <- bias_custom(); b[[input$bias_set$abb]] <- as.numeric(input$bias_set$v); bias_custom(b)
  })
  observeEvent(input$bias_reset, {
    a <- input$bias_reset$abb
    bias_custom(if (identical(a, "__all__")) setNames(numeric(0), character(0)) else bias_custom()[names(bias_custom()) != a])
  })
  with_custom <- function(et) mutate(et, custom_move = unname(bias_custom()[abb]))
  custom <- reactive(length(bias_custom()) > 0)
  output$bias_is_custom <- reactive(custom())
  outputOptions(output, "bias_is_custom", suspendWhenHidden = FALSE)
  # The 2026 results scenario ("2026 results"): the actual count in place of a historical miss.
  res_on <- reactive(if (identical(input$miss_mode, "actual") && !is.null(d()$results)) d()$results)
  res_row <- function(abb) { r <- res_on(); if (is.null(r)) NULL else r[r$abb == abb, ] }
  err_tbl <- reactive(with_custom(if (!is.null(res_on())) result_errors(d()$polls, res_on(), d()$seats$abb)
                                  else assumed_errors(d()$errors, d()$seats$abb, sel_cycles(), sel_types())))
  # Slider runs D (left, negative) to R (right, positive); the model counts toward D as positive.
  shift_d <- reactive(-input$shift)
  races <- reactive(count_status(build_races(scen(), err_tbl(), shift_d()), res_on()))
  totals <- reactive(seat_totals(races()))

  output$scenario_caption <- renderText({
    if (identical(input$miss_mode, "past") && !length(sel_cycles())) return("Select at least one election year.")
    if (identical(input$miss_mode, "past") && !length(sel_types())) return("Select Senate, presidential, or both.")
    scenario_caption(sel_cycles(), sel_types(), res_on())
  })
  observe({
    cy <- as.numeric(input$cycles %||% character(0))
    session$sendCustomMessage("preset-active", if (setequal(cy, MIDTERMS)) "cy_mid"
                              else if (setequal(cy, PRES_YEARS)) "cy_pres"
                              else if (setequal(cy, TRUMP_ERA)) "cy_trump"
                              else if (setequal(cy, TRUMP_MID)) "cy_tmid"
                              else if (setequal(cy, CYCLES)) "cy_all"
                              else "")
  })
  output$bias_title <- renderText(if (is.null(res_on())) "Assumed bias: polling average vs. result" else "Observed bias: polling average vs. result")
  output$freshness <- renderUI(HTML(data_status(d()$polls, d()$results)))

  # Beside the map: how far the scenario moves each race from its polls
  # (assumed miss plus the extra uniform shift), toward D on the left, R on the right.
  output$bias_list <- renderUI({
    cap <- 10
    is_res <- !is.null(res_on())
    r <- races() |>
      filter(polled) |>
      mutate(edited = !is.na(custom_move),
             move = ifelse(edited, custom_move, err - shift_d())) |>     # > 0 = moved toward Republicans
      arrange(move)
    pos <- function(v) 50 + 50 * pmax(pmin(v, cap), -cap) / cap
    n_set <- sum(r$edited)
    tagList(
    div(class = "bias-tools",
        span(class = "bias-hint", if (n_set) sprintf("%d state%s set by you", n_set, if (n_set == 1) "" else "s")
             else "Drag a state's dot to set its own bias"),
        if (is_res && any(r$counting)) span(class = "bias-key",
          span(span(class = "bk-sw bk-counting"), sprintf("Under %d%% of expected vote counted", COUNTING_EEVP))),
        if (n_set) span(class = "bias-all", tags$a(href = "#", onclick = "Shiny.setInputValue('bias_reset', {abb: '__all__', t: Date.now()}); return false;", "Reset all"))),
    div(class = paste("bias-rows", if (is_res) "res"),
        if (is_res) div(class = "bias-row bias-head", title = "Share of the expected vote counted so far",
                        span(), span(), span("Miss"), span("Counted")),
        lapply(seq_len(nrow(r)), function(i) {
          v <- r$move[i]
          col <- dot_col(-v)
          lo <- min(pos(0), pos(v)); hi <- max(pos(0), pos(v))
          div(class = paste("bias-row", if (r$edited[i]) "edited", if (r$counting[i]) "counting"), `data-abb` = r$abb[i],
              title = if (r$edited[i]) sprintf("%s: you set this to %s. Double-click to reset.", r$state[i], fmt_miss(v)) else
                sprintf("%s: %s %s from the polls\n%s\nDrag to set your own.", r$state[i],
                        if (is.null(res_on())) "projection moved" else "result landed", fmt_miss(v),
                        miss_detail(d()$errors, r$abb[i], sel_cycles(), sel_types(), r$err[i], shift_d(), res_row(r$abb[i]), r$opp_party[i])),
              span(class = "bias-st", r$abb[i]),
              div(class = "bias-track",
                  span(class = "mid"),
                  span(class = "line", style = sprintf("left:%.1f%%;width:%.1f%%;background:%s", lo, hi - lo, col)),
                  span(class = "dot", style = sprintf("left:%.1f%%;background:%s", pos(v), col))),
              span(class = "bias-v", style = sprintf("color:%s", col), fmt_miss(v)),
              if (is_res) span(class = "bias-pct", if (is.na(r$eevp[i])) "" else sprintf("%.0f%%", r$eevp[i])))
        })))
  })

  # One year at a time: the seat result if 2026's polls miss exactly as each past
  # year's did (same race types and uniform shift as the current scenario).
  # Clicking a year selects it.
  output$year_strip <- renderUI({
    types <- if (length(sel_types())) sel_types() else c("Senate", "President")
    one_year <- identical(input$miss_mode, "past") && length(sel_cycles()) == 1
    cells <- lapply(CYCLES, function(y) {
      r <- build_races(scen(), with_custom(assumed_errors(d()$errors, d()$seats$abb, y, types)), shift_d())
      # Shaded by the same control rule as the headline, including the independents switch.
      t <- seat_totals(r)
      rep <- t[["R"]]
      side <- control_side(t, input$ind_with_d)
      ctl <- CONTROL_LABEL[[side]]
      tags$button(
        type = "button", class = paste("yr-cell", side, if (one_year && sel_cycles() == y) "active"),
        title = sprintf("If 2026's polls miss as %d's did: %d Dem, %d Ind, %d Rep. %s.", y, t[["D"]], t[["I"]], rep, ctl),
        onclick = sprintf("Shiny.setInputValue('yr_pick', %d, {priority: 'event'})", y),
        span(class = "yr-y", y),
        span(class = "yr-s",
             if (t[["I"]] > 0) sprintf("%d–%d–%d", t[["D"]], t[["I"]], rep) else sprintf("%d–%d", t[["D"]], rep)))
    })
    # Last cell: the actual 2026 miss, i.e. the seats as counted so far.
    if (!is.null(d()$results)) {
      t <- seat_totals(build_races(scen(), with_custom(result_errors(d()$polls, d()$results, d()$seats$abb)), shift_d()))
      side <- control_side(t, input$ind_with_d)
      out <- uncalled(d()$results)
      cells <- c(cells, list(tags$button(
        type = "button", class = paste("yr-cell yr-actual", side, if (!is.null(res_on())) "active"),
        title = sprintf("2026 as counted so far: %d Dem, %d Ind, %d Rep. %s.%s", t[["D"]], t[["I"]], t[["R"]], CONTROL_LABEL[[side]],
                        if (length(out)) sprintf(" Not yet called: %s.", paste(out, collapse = ", ")) else ""),
        onclick = "Shiny.setInputValue('yr_pick', 2026, {priority: 'event'})",
        span(class = "yr-y", "2026 result"),
        span(class = "yr-s",
             if (t[["I"]] > 0) sprintf("%d–%d–%d", t[["D"]], t[["I"]], t[["R"]]) else sprintf("%d–%d", t[["D"]], t[["R"]])))))
    }
    div(class = "yr-wrap",
        div(class = "yr-title", "Projected 2026 seats assuming each single year's polling error"),
        div(class = "yr-strip", cells))
  })

  observeEvent(input$yr_pick, {
    if (input$yr_pick == 2026) return(updateRadioButtons(session, "miss_mode", selected = "actual"))
    updateRadioButtons(session, "miss_mode", selected = "past")
    updateCheckboxGroupInput(session, "cycles", selected = as.character(input$yr_pick))
  })

  # ---- About the Data: figures that come from the data, so they never go stale ----
  output$about_unpolled <- renderText({
    abbs <- sort(setdiff(d()$seats$abb, d()$polls$abb))
    if (length(abbs)) paste0(paste(abbs, collapse = ", "), ".") else "none at present."
  })
  output$about_inds <- renderText({
    p <- d()$polls |> filter(opp_party == "I") |> arrange(abb)
    if (nrow(p)) paste0("(", paste(sprintf("%s in %s", p$opp_name, p$abb), collapse = ", "), ")") else "(none at present)"
  })
  output$about_bias <- renderText({
    m <- mean(d()$errors$error)
    sprintf("across %d races in 2010–2024, the polls overstated %s by %.1f points on average, but the direction varied by year and state.",
            nrow(d()$errors), if (m > 0) "Democrats" else "Republicans", abs(m)) |>
      (\(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2)))()
  })
  output$about_cover <- renderText({
    e <- d()$errors$error; s <- sd(e)
    sprintf("%.0f%% of past races fell within the full shape (SD %.1f points).", 100 * mean(abs(e) <= VIOLIN_Z * s), s)
  })

  # ---- Seat views (horseshoe / ranked margins) ----
  ordered <- reactive(race_order(races(), res_on()))
  output$scenario_title <- renderUI(span(class = "t-tag scen-title-tag", if (custom()) "Custom polling error" else paste0(scenario_tag(sel_cycles(), sel_types(), res_on()),
    if (shift_d() != 0) paste0(", shift ", fmt_shift(shift_d())))))
  output$horseshoe <- renderUI(horseshoe_svg(ordered(), totals(), isTRUE(input$ind_with_d)))
  # Phones only (hidden by CSS on wider screens): the horseshoe's tally, shown above the swipeable chart.
  output$m_tally <- renderUI({
    t <- totals()
    side <- control_side(t, isTRUE(input$ind_with_d))
    div(class = "m-tally",
        div(class = "m-nums", span(style = sprintf("color:%s", D_DARK), t[["D"]]), span(class = "m-dash", "–"),
            if (t[["I"]] > 0) tagList(span(style = sprintf("color:%s", I_DARK), t[["I"]]), span(class = "m-dash", "–")),
            span(style = sprintf("color:%s", R_DARK), t[["R"]])),
        div(class = "m-ctl", style = sprintf("color:%s", CONTROL_COL[[side]]), party_icon_html(side, "1.1em"), CONTROL_LABEL[[side]]),
        if (any(races()$pending | races()$counting)) div(class = "m-prov", "Provisional"))
  })

  spread <- reactive(miss_sd(d()$errors))
  output$ranked_caption <- renderText(sprintf(
    "*±%.1f pts around each polling average; %.0f%% of single-race polling misses in 2010–2024 fell within it.",
    VIOLIN_Z * spread(), 100 * mean(abs(d()$errors$error) <= VIOLIN_Z * spread())))

  output$ranked <- renderPlotly({
    r <- ordered()
    x <- seq_len(nrow(r))
    pol <- r$polled
    s <- spread()
    # Each shape is centred on the polling average (house effects removed if that
    # switch is on); the tick is where this scenario puts the race on that curve.
    ctr <- r$poll_margin
    m <- r$proj_margin
    cap <- 30
    clip <- function(v) pmax(pmin(v, cap), -cap)
    ticks <- seq(-cap, cap, by = 10)
    lo <- clip(pmin(ctr - VIOLIN_Z * s, m))
    hi <- clip(pmax(ctr + VIOLIN_Z * s, m))
    hover <- with(r, sprintf(paste0(
      "<b>%s</b><br>%s (%s) v %s (R)<br>Polling average: <b>%s</b>",
      "<br>Range of past misses (95%%): %s to %s<br>This scenario: %s (assumed miss %s)"),
      state, opp_name, opp_party, rep_name, fmt_margin(ctr, opp_party),
      fmt_margin(ctr - VIOLIN_Z * s, opp_party), fmt_margin(ctr + VIOLIN_Z * s, opp_party), fmt_margin(m, opp_party), fmt_error(err)
    ))

    # One bell-curve shape per race, blue above Even and red below. All the D
    # pieces go in one trace and all the R pieces in another (NA-separated).
    # Light full shape with a thin edge, then the darker middle-50% core on top.
    p <- plot_ly()
    for (layer in c("outer", "core")) {
      parts <- lapply(which(pol), function(i) {
        pt <- violin_parts(x[i], ctr[i], s, cap, z = if (layer == "outer") VIOLIN_Z else CORE_Z)
        # an independent's half above Even is teal, not blue
        list(D = if (!identical(r$opp_party[i], "I")) pt$D, I = if (identical(r$opp_party[i], "I")) pt$D, R = pt$R)
      })
      for (side in c("D", "I", "R")) {
        polys <- Filter(Negate(is.null), lapply(parts, `[[`, side))
        if (!length(polys)) next
        xy <- do.call(rbind, lapply(polys, function(pg) rbind(pg, data.frame(x = NA, y = NA))))
        p <- p |> add_trace(x = xy$x, y = xy$y, type = "scatter", mode = "lines", fill = "toself",
                            fillcolor = VIOLIN_FILL[[layer]][[side]],
                            line = if (layer == "outer") list(width = 0.8, color = VIOLIN_FILL$edge[[side]]) else list(width = 0),
                            hoverinfo = "skip")
      }
    }
    # Invisible full-height column per race so hovering anywhere on it works.
    p <- p |> add_bars(x = x[pol], y = (hi - lo)[pol], base = lo[pol], width = 0.9,
                       marker = list(color = "rgba(0,0,0,0)"), text = hover[pol], hoverinfo = "text",
                       textposition = "none")

    # Faint party tints above/below Even, a soft column behind the 51st seat,
    # the 50-seat line, and unpolled holds shaded at the ends.
    k51 <- NEED_FOR_CONTROL
    shapes <- list(
      list(type = "rect", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = cap + 1, fillcolor = D_TINT, opacity = 0.7,
           line = list(width = 0), layer = "below"),
      list(type = "rect", xref = "paper", x0 = 0, x1 = 1, y0 = -cap - 1, y1 = 0, fillcolor = R_TINT, opacity = 0.7,
           line = list(width = 0), layer = "below"),
      list(type = "rect", x0 = k51 - 0.5, x1 = k51 + 0.5, yref = "paper", y0 = 0, y1 = 1,
           fillcolor = "rgba(192,138,30,0.12)", line = list(width = 0), layer = "below"),
      list(type = "line", x0 = k51 - 0.5, x1 = k51 - 0.5, yref = "paper", y0 = 0, y1 = 1,
           line = list(color = INK, width = 2))
    )
    tick_txt <- ifelse(seq_along(r$abb) == k51, paste0("<b>", r$abb, "</b>"), r$abb)
    notes <- list(
      list(x = k51 - 0.5, y = 1, yref = "paper", xanchor = "right", yanchor = "bottom", showarrow = FALSE,
           text = "50 seats ", font = list(size = 12, color = INK)),
      list(x = k51 - 0.5, y = 1, yref = "paper", xanchor = "left", yanchor = "bottom", showarrow = FALSE,
           text = " <b>51st seat</b> decides control", font = list(size = 12, color = INK))
    )
    for (side in c("D", "R")) {
      idx <- which(!pol & r$incumbent_party == side)
      if (!length(idx)) next
      shapes <- c(shapes, list(list(type = "rect", x0 = min(idx) - 0.5, x1 = max(idx) + 0.5, yref = "paper",
                                    y0 = 0, y1 = 1, fillcolor = COL_NOT_UP, opacity = 0.95,
                                    line = list(width = 0), layer = "below")))
      notes <- c(notes, list(list(x = mean(range(idx)), y = 0.5, yref = "paper", showarrow = FALSE, textangle = -90,
                                  text = paste("Safe", side, "(no polls)"),
                                  font = list(size = 12, color = if (side == "D") D_MID else R_MID))))
    }

    p |>
      # Tick = polling average (the center of its curve); white dot = where this scenario lands.
      add_trace(x = x[pol], y = clip(ctr[pol]), type = "scatter", mode = "markers",
                marker = list(symbol = "line-ew", size = 22, line = list(color = INK, width = 3)),
                text = hover[pol], hoverinfo = "text") |>
      add_trace(x = x[pol], y = clip(m[pol]), type = "scatter", mode = "markers", text = hover[pol], hoverinfo = "text",
                marker = list(symbol = "circle", size = 8, color = "#ffffff", line = list(color = INK, width = 1.5))) |>
      # Flipped races: the same thin ring as the map, at the foot of the column.
      add_trace(x = x[pol & r$flip], y = rep(-cap + 1.8, sum(pol & r$flip)), type = "scatter", mode = "markers",
                marker = list(symbol = "circle-open", size = 15, color = INK, line = list(width = 1.3)),
                hoverinfo = "skip") |>
      style_plot(
        barmode = "overlay", showlegend = FALSE, shapes = shapes, annotations = notes,
        margin = list(t = 28, l = 56, r = 10, b = 30),
        xaxis = list(tickvals = x, ticktext = tick_txt, range = c(0.4, nrow(r) + 0.6), showgrid = FALSE,
                     tickfont = list(size = 11), color = INK_2, fixedrange = TRUE),
        yaxis = list(range = c(-cap - 1, cap + 1), tickvals = ticks, ticktext = margin_axis_labels(ticks),
                     gridcolor = "#ffffff", gridwidth = 1.5, zeroline = TRUE, zerolinecolor = INK, zerolinewidth = 2,
                     color = INK_MUTED, fixedrange = TRUE)
      )
  })

  # Draw the ranked view even while it is hidden, so switching to it is instant.
  # (Not in the browser build: there it can run before the charting library has loaded.)
  if (!IS_BROWSER) outputOptions(output, "ranked", suspendWhenHidden = FALSE)
  output$ranked_wrap <- renderUI({
    req(identical(input$seat_view, "ranked"))
    div(class = "seat-scroll seat-scroll-rk", plotlyOutput("ranked", height = "440px"))
  })

  # ---- Map ----
  output$map <- renderPlotly({
    r <- races()
    # the past races behind each state's assumed miss
    detail <- vapply(seq_len(nrow(r)), function(i) {
      if (!r$polled[i]) return("")
      gsub("\n", "<br>", miss_detail(d()$errors, r$abb[i], sel_cycles(), sel_types(), r$err[i], 0, res_row(r$abb[i]), r$opp_party[i]), fixed = TRUE)
    }, "")
    is_res <- !is.null(res_on())
    unpolled_txt <- vapply(seq_len(nrow(r)), function(i) {
      if (is_res) gsub("\n", "<br>", result_detail(res_row(r$abb[i]), "D"), fixed = TRUE)
      else sprintf("Assumed safe %s hold", r$incumbent_party[i])
    }, "")
    hover <- with(r, ifelse(
      polled,
      sprintf("<b>%s</b>%s<br>%s (%s) vs %s (R)<br>%s: %s  (%d poll%s, latest %s)<br>%s: %s<br><i>%s</i><br><b>%s: %s</b>",
              state, ifelse(flip, " (flip)", ""), opp_name, opp_party, rep_name,
              avg_label, fmt_margin(poll_margin, opp_party), n_polls_in_avg, ifelse(n_polls_in_avg == 1, "", "s"), format(latest_poll, "%b %d"),
              if (is_res) "Result vs. polls" else "Assumed miss", fmt_error(err), detail,
              if (is_res) "Result" else "Projected", fmt_margin(proj_margin, opp_party)),
      sprintf("<b>%s</b><br>No general-election polling<br>%s", state, unpolled_txt)
    ))

    not_up <- setdiff(state.abb, r$abb)
    p <- plot_geo(locationmode = "USA-states") |>
      add_trace(type = "choropleth", locations = not_up, z = rep(1, length(not_up)),
                colorscale = list(c(0, COL_NOT_UP), c(1, COL_NOT_UP)), showscale = FALSE,
                marker = list(line = list(color = SURFACE, width = 1)),
                text = paste0("<b>", state.name[match(not_up, state.abb)], "</b><br>No Senate race in 2026"),
                hoverinfo = "text")
    for (b in sort(unique(r$bin))) for (pend in c(FALSE, TRUE)) {   # not yet called: faded
      rb <- r$bin == b & r$pending == pend
      if (!any(rb)) next
      p <- p |> add_trace(
        type = "choropleth", locations = r$abb[rb], z = rep(1, sum(rb)),
        colorscale = list(c(0, BINS$col[b]), c(1, BINS$col[b])), showscale = FALSE,
        marker = list(line = list(color = SURFACE, width = 1), opacity = if (pend) 0.45 else 1),
        text = hover[rb], hoverinfo = "text"
      )
    }
    centers <- tibble(abb = state.abb, lon = state.center$x, lat = state.center$y) |>
      filter(abb %in% r$abb, !abb %in% OFFSHORE_LABELS$abb) |>
      # state.center puts AK where old inset maps drew it; use its real centre
      mutate(lon = ifelse(abb == "MI", -84.6, lon), lat = ifelse(abb == "MI", 43.3, lat),
             lon = ifelse(abb == "FL", -81.6, lon),
             lon = ifelse(abb == "AK", -152.5, lon), lat = ifelse(abb == "AK", 64.2, lat))
    lbl_ink <- BINS$ink[r$bin[match(centers$abb, r$abb)]]
    # Small Northeast states: label out in the Atlantic with a leader line.
    off <- filter(OFFSHORE_LABELS, abb %in% r$abb)
    leaders <- do.call(rbind, lapply(seq_len(nrow(off)), function(i) {
      data.frame(lon = c(off$lon[i], off$lab_lon[i] - 0.25, NA), lat = c(off$lat[i], off$lab_lat[i], NA))
    }))
    # A thin ring around the label of each state that flips party, in the label's own ink.
    flips <- r$abb[r$flip]
    rings <- bind_rows(
      centers |> filter(abb %in% flips) |> transmute(abb, lon, lat, col = BINS$ink[r$bin[match(abb, r$abb)]]),
      off |> filter(abb %in% flips) |> transmute(abb, lon = lab_lon + 0.75, lat = lab_lat, col = INK)
    )
    if (nrow(rings)) {
      p <- p |>
        add_trace(type = "scattergeo", mode = "markers", lon = rings$lon, lat = rings$lat,
                  marker = list(symbol = "circle-open", size = 26, color = rings$col, line = list(width = 1.3)),
                  hoverinfo = "skip", showlegend = FALSE)
    }
    p |>
      add_trace(type = "scattergeo", mode = "text", lon = centers$lon, lat = centers$lat,
                text = centers$abb, hoverinfo = "skip", showlegend = FALSE,
                textfont = list(size = 11, color = lbl_ink, family = APP_FONT)) |>
      add_trace(type = "scattergeo", mode = "lines", lon = leaders$lon, lat = leaders$lat,
                line = list(color = INK_MUTED, width = 1), hoverinfo = "skip", showlegend = FALSE) |>
      add_trace(type = "scattergeo", mode = "text", lon = off$lab_lon, lat = off$lab_lat, text = off$abb,
                textposition = "middle right", hoverinfo = "skip", showlegend = FALSE,
                textfont = list(size = 11, color = INK, family = APP_FONT)) |>
      style_plot(
        geo = list(scope = "usa", projection = list(type = "albers usa"),
                   showlakes = FALSE, bgcolor = "rgba(0,0,0,0)"),
        showlegend = FALSE, dragmode = FALSE, margin = list(l = 0, r = 0, t = 0, b = 0)
      ) |>
      config(scrollZoom = FALSE, doubleClick = FALSE)
  })

  # ---- History tab ----
  # y ticks labelled by who beat the polls: "+4 D" ... "0" ... "+4 R"
  miss_ticks <- function(lim, by) {
    v <- seq(-lim, lim, by = by)
    list(tickvals = v, ticktext = ifelse(v == 0, "0", paste0("+", abs(v), ifelse(v > 0, " R", " D"))))
  }

  # Key numbers across all state-level races in the history.
  output$hm_tiles <- renderUI({
    e <- d()$errors
    by_cycle <- e |> group_by(cycle) |> summarise(err = mean(error), .groups = "drop")
    by_ct <- e |> group_by(cycle, race_type) |> summarise(err = mean(error), .groups = "drop")
    worst <- by_ct[which.max(abs(by_ct$err)), ]
    recent <- mean(e$error[e$cycle >= 2016])
    tile <- function(big, col, label) div(class = "hm-tile", tags$b(style = sprintf("color:%s", col), big), span(label))
    div(class = "hm-tiles",
        tile(sprintf("%d of %d", sum(by_cycle$err > 0), nrow(by_cycle)), INK,
             sprintf("elections since %d where polls overstated Democrats on average", min(by_cycle$cycle))),
        tile(fmt_miss(recent), party_col(-recent), "average miss since 2016, all polled Senate and presidential races"),
        tile(fmt_miss(worst$err), party_col(-worst$err),
             sprintf("largest national miss: %d %s races", worst$cycle, if (worst$race_type == "Senate") "Senate" else "presidential")),
        tile(sprintf("%.1f pts", mean(abs(e$error))), INK, "typical miss in a single race, in either direction"))
  })

  output$natl_bias <- renderPlotly({
    nb <- d()$errors |>
      group_by(cycle, race_type) |>
      summarise(err = mean(error), states = n(), .groups = "drop")
    lim <- 2 * ceiling((max(abs(nb$err)) + 1.5) / 2)
    panel <- function(t) {
      x <- filter(nb, race_type == t)
      plot_ly(
        x = factor(x$cycle), y = x$err, type = "bar",
        marker = list(color = ifelse(x$err > 0, R_MID, D_MID)),
        text = fmt_miss(x$err), textposition = "outside", cliponaxis = FALSE,
        textfont = list(size = 11, color = INK_2),
        customdata = sprintf("%s beat their polls by %.1f on average (%d states)",
                             ifelse(x$err > 0, "Republicans", "Democrats"), abs(x$err), x$states),
        hovertemplate = paste0("<b>%{x} ", t, "</b><br>%{customdata}<extra></extra>")
      ) |>
        layout(annotations = list(list(
          text = if (t == "Senate") "<b>Senate races</b>" else "<b>Presidential races</b>",
          x = 0, xref = "paper", xanchor = "left", y = 1.12, yref = "paper",
          showarrow = FALSE, font = list(size = 13, color = INK)
        )))
    }
    subplot(panel("Senate"), panel("President"), shareY = TRUE, widths = c(0.64, 0.36), margin = 0.05) |>
      style_plot(
        showlegend = FALSE, bargap = 0.45, margin = list(t = 36, b = 24),
        xaxis = list(color = INK_2, fixedrange = TRUE), xaxis2 = list(color = INK_2, fixedrange = TRUE),
        yaxis = c(list(range = c(-lim, lim), gridcolor = GRID, zerolinecolor = INK, zerolinewidth = 1.5,
                       color = INK_MUTED, fixedrange = TRUE), miss_ticks(lim, lim / 2))
      )
  })

  # State by state: the selected races, and each state's average (most R-favoring first).
  observeEvent(input$hm_all, updateCheckboxGroupInput(session, "hm_years", selected = CYCLES))
  observeEvent(input$hm_mid, updateCheckboxGroupInput(session, "hm_years", selected = MIDTERMS))
  observeEvent(input$hm_pres, updateCheckboxGroupInput(session, "hm_years", selected = PRES_YEARS))
  observeEvent(input$hm_trump, updateCheckboxGroupInput(session, "hm_years", selected = TRUMP_ERA))
  observeEvent(input$hm_tmid, updateCheckboxGroupInput(session, "hm_years", selected = TRUMP_MID))
  observe({
    yrs <- as.numeric(input$hm_years %||% character(0))
    session$sendCustomMessage("hm-preset-active", if (setequal(yrs, CYCLES)) "hm_all"
                              else if (setequal(yrs, MIDTERMS)) "hm_mid"
                              else if (setequal(yrs, PRES_YEARS)) "hm_pres"
                              else if (setequal(yrs, TRUMP_ERA)) "hm_trump"
                              else if (setequal(yrs, TRUMP_MID)) "hm_tmid"
                              else "")
  })
  hm_err <- reactive({
    e <- d()$errors |> filter(cycle %in% as.numeric(input$hm_years %||% character(0)))
    if (input$hm_type != "Both") e <- filter(e, race_type == input$hm_type)
    if (isTRUE(input$hm_2026_only)) e <- filter(e, abb %in% d()$seats$abb)
    e
  })
  hm_states <- reactive({
    hm_err() |> group_by(abb) |> summarise(avg = mean(error), n = n(), .groups = "drop") |>
      arrange(desc(avg)) |> left_join(state_lookup, by = "abb")
  })
  race_tip <- function(g) {
    sprintf("%d %s: polls %s, result %s (%s)", g$cycle, ifelse(g$race_type == "Senate", "Senate", "president"),
            fmt_margin(g$poll_margin), fmt_margin(g$actual_margin),
            ifelse(abs(g$error) < 0.05, "polls exact",
                   sprintf("%s beat the polls by %.1f", ifelse(g$error > 0, "Republican", "Democrat"), abs(g$error))))
  }

  output$hm_rows <- renderUI({
    cap <- 16
    e <- hm_err()
    if (!nrow(e)) return(p(class = "lp-unpolled", "No races match these filters."))
    pos <- function(v) 50 + 50 * pmax(pmin(v, cap), -cap) / cap
    st <- hm_states()
    ticks <- c(-15, -10, -5, 0, 5, 10, 15)
    header <- div(class = "lp-cols hm-cols",
                  span("State"),
                  div(class = "av-axis", lapply(ticks, function(v) {
                    span(class = if (abs(v) %in% c(5, 15)) "minor", style = sprintf("left:%.1f%%", pos(v)), if (v == 0) "Even" else fmt_miss(v))
                  })),
                  span("Average"), span("Races"))
    rows <- lapply(seq_len(nrow(st)), function(i) {
      g <- e[e$abb == st$abb[i], ]
      g <- g[order(g$cycle), ]
      jit <- 22 + 56 * ((seq_len(nrow(g)) * 0.618034) %% 1)
      tips <- race_tip(g)
      dots <- lapply(seq_len(nrow(g)), function(j) {
        span(class = paste("dot", if (g$race_type[j] == "Senate") "sen" else "pres"),
             style = sprintf("left:%.1f%%;top:%.0f%%;--c:%s", pos(g$error[j]), jit[j], dot_col(-g$error[j])),
             title = tips[j])
      })
      a <- st$avg[i]
      col <- dot_col(-a)
      div(class = "hm-row",
          span(class = "hm-st", st$state[i]),
          div(class = "hm-track",
              span(class = "tick", style = sprintf("left:%.1f%%", pos(-10))),
              span(class = "tick", style = sprintf("left:%.1f%%", pos(10))),
              span(class = "fill", style = sprintf("%s:50%%;width:%.1f%%;background:%s",
                                                   if (a > 0) "left" else "right", abs(pos(a) - 50), col)),
              span(class = "mid"), dots,
              span(class = "avg", style = sprintf("left:%.1f%%;background:%s", pos(a), col))),
          span(class = "hm-avg", style = sprintf("color:%s", col), fmt_miss(a)),
          span(class = "hm-n", st$n[i]))
    })
    tagList(header, rows)
  })

  # Year-by-year grid: states down, elections across, each cell the miss in points.
  output$hm_grid <- renderUI({
    e <- hm_err()
    if (!nrow(e)) return(p(class = "lp-unpolled", "No races match these filters."))
    st <- hm_states()
    cols <- e |> distinct(cycle, race_type) |> arrange(cycle, race_type != "Senate")
    two_rows <- length(unique(cols$race_type)) > 1
    yrs <- unique(cols$cycle)
    head1 <- tags$tr(tags$th(class = "st", ""),
                     lapply(yrs, function(y) tags$th(class = "yr", colspan = sum(cols$cycle == y), y)),
                     tags$th(class = "av", "Average"))
    head2 <- if (two_rows) {
      tags$tr(tags$th(""), lapply(cols$race_type, function(t) tags$th(class = "ty", if (t == "Senate") "Sen" else "Pres")),
              tags$th(""))
    }
    body <- lapply(seq_len(nrow(st)), function(i) {
      g <- e[e$abb == st$abb[i], ]
      cells <- lapply(seq_len(nrow(cols)), function(k) {
        hit <- g[g$cycle == cols$cycle[k] & g$race_type == cols$race_type[k], ]
        if (!nrow(hit)) return(tags$td(class = "empty"))
        v <- hit$error[1]
        tags$td(style = sprintf("background:%s;color:%s", miss_fill(v), if (abs(v) > 7) "#ffffff" else INK),
                title = race_tip(hit[1, ]), if (abs(v) < 0.5) "0" else sprintf("%.0f", abs(v)))
      })
      tags$tr(tags$td(class = "st", st$state[i]), cells,
              tags$td(class = "av", style = sprintf("color:%s", dot_col(-st$avg[i])), fmt_miss(st$avg[i])))
    })
    tagList(
      div(class = "hm-legend",
          span(fmt_miss(-12)), span(class = "ramp"), span(fmt_miss(12)),
          span(class = "hm-legend-note", "Numbers are the miss in points; the color says who beat the polls.")),
      div(class = "hm-grid-wrap", tags$table(class = "hm-table", tags$thead(head1, head2), tags$tbody(body)))
    )
  })

  # ---- Latest polls tab ----
  picked <- function(x) if (is.null(x) || x %in% c("", "all")) NULL else x
  observeEvent(input$drill_state, {
    updateSelectizeInput(session, "lp_state", selected = input$drill_state)
    updateSelectizeInput(session, "lp_pollster", selected = "all")
    nav_select("main_nav", "All Latest Polls")
  })
  observeEvent(d(), updateSelectizeInput(session, "lp_pollster",
                                         choices = c("All pollsters" = "all", sort(unique(d()$race_polls$pollster))),
                                         selected = isolate(input$lp_pollster) %||% "all"))
  output$lp_updated <- renderText(paste("Updated", format(as.POSIXct(d()$polls$fetched_at[1]), "%b %d, %Y at %H:%M")))
  output$avg_updated <- renderText(paste("Updated", format(as.POSIXct(d()$polls$fetched_at[1]), "%b %d, %Y at %H:%M")))
  output$ev_list <- renderUI(ev_list_ui(d()$seats, d()$polls, d()$early))
  output$ev_updated <- renderText({
    u <- d()$early$updated
    if (is.null(u) || all(is.na(u))) "" else paste0("States reporting through ", fmt_short(max(u, na.rm = TRUE)), ".")
  })

  output$avg_list <- renderUI({
    a <- d()$seats |> left_join(d()$polls, by = "abb")
    if (!is.null(picked(input$avg_state))) a <- filter(a, abb == input$avg_state)
    polled <- a |> filter(!is.na(poll_margin)) |> arrange(abs(poll_margin))
    unpolled <- a |> filter(is.na(poll_margin))
    tagList(
      if (nrow(polled)) {
        tagList(avg_header(), lapply(seq_len(nrow(polled)), function(i) {
          avg_row(polled[i, ], d()$race_polls[d()$race_polls$abb == polled$abb[i], ])
        }))
      },
      if (nrow(unpolled)) {
        p(class = "lp-unpolled", tags$b("No general-election polls"), " (treated as safe holds): ",
          paste(sprintf("%s (%s)", unpolled$state, unpolled$incumbent_party), collapse = ", "))
      }
    )
  })

  # The poll list grows 60 at a time; changing a filter starts it over.
  poll_n <- reactiveVal(60)
  observeEvent(list(input$lp_state, input$lp_pollster, input$lp_in_avg), poll_n(60), ignoreInit = TRUE)
  observeEvent(input$poll_more_btn, poll_n(poll_n() + 60))

  filtered_polls <- reactive({
    pl <- d()$race_polls |>
      left_join(select(d()$seats, abb, state), by = "abb") |>
      left_join(select(d()$polls, abb, opp_name, opp_party, rep_name), by = "abb") |>
      arrange(desc(date), state)
    if (!is.null(picked(input$lp_state))) pl <- filter(pl, abb == input$lp_state)
    if (!is.null(picked(input$lp_pollster))) pl <- filter(pl, pollster == input$lp_pollster)
    if (isTRUE(input$lp_in_avg)) pl <- filter(pl, in_average)
    pl
  })

  output$poll_list <- renderUI({
    pl <- filtered_polls()
    if (!nrow(pl)) return(p(class = "lp-unpolled", "No polls match these filters."))
    shown <- head(pl, poll_n())
    days <- unique(shown$date)
    tagList(
      poll_header(),
      lapply(seq_along(days), function(j) {
        g <- shown[shown$date == days[j], ]
        n_day <- sum(pl$date == days[j])
        tagList(
          div(class = "pr-day", tags$b(fmt_day(days[j])), span(sprintf("%d poll%s", n_day, if (n_day == 1) "" else "s"))),
          lapply(seq_len(nrow(g)), function(i) poll_row(g[i, ]))
        )
      })
    )
  })

  output$poll_more <- renderUI({
    n <- nrow(filtered_polls())
    if (n > poll_n()) {
      actionButton("poll_more_btn", sprintf("Show more polls (%d of %d shown)", poll_n(), n),
                   class = "btn-sm btn-outline-secondary")
    }
  })
}

shinyApp(ui, server)
