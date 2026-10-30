# Data pipeline for the 2026 Senate scenario dashboard.
#
# Writes five CSVs into data/:
#   seats_2026.csv       - the 35 seats up in 2026 + incumbent party (static fact table)
#   current_polls.csv    - Silver Bulletin's polling average per race (latest day)
#   race_polls.csv       - every general-election poll behind those averages, with its weight
#   poll_errors.csv      - historical polling error per state/cycle/race type, 2010-2024
#                          (Silver Bulletin raw polls, the continuation of 538's archive)
#   pollster_ratings.csv - Silver Bulletin pollster grades and house effects
#
# Convention everywhere: margin = (Dem or D-aligned Ind) minus Rep, in points.
# error = poll margin - actual margin, so positive error = polls overstated Dems.
#
# Run standalone:  Rscript R/fetch_data.R   (from the senate_dashboard folder)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
DATA_DIR <- "data"

state_lookup <- tibble(state = c(state.name), abb = c(state.abb)) |>
  mutate(slug = gsub(" ", "-", tolower(state)))

# ---- 2026 seats -------------------------------------------------------------
# Class 2 seats plus the Ohio (Vance) and Florida (Rubio) specials.
# Senate going in: 53 R / 47 D+I. Seats NOT up: 31 R, 34 D+I.
seats_2026 <- tribble(
  ~abb, ~incumbent_party, ~note,
  "AL", "R", "Open (Tuberville running for gov)",
  "AK", "R", "",
  "AR", "R", "",
  "CO", "D", "",
  "DE", "D", "",
  "FL", "R", "Special",
  "GA", "D", "",
  "ID", "R", "",
  "IL", "D", "Open",
  "IA", "R", "Open",
  "KS", "R", "",
  "KY", "R", "Open",
  "LA", "R", "",
  "ME", "R", "",
  "MA", "D", "",
  "MI", "D", "Open",
  "MN", "D", "Open",
  "MS", "R", "",
  "MT", "R", "",
  "NE", "R", "",
  "NH", "D", "Open",
  "NJ", "D", "",
  "NM", "D", "",
  "NC", "R", "Open",
  "OH", "R", "Special",
  "OK", "R", "",
  "OR", "D", "",
  "RI", "D", "",
  "SC", "R", "",
  "SD", "R", "",
  "TN", "R", "",
  "TX", "R", "",
  "VA", "D", "",
  "WV", "R", "",
  "WY", "R", ""
) |>
  left_join(state_lookup, by = "abb")

SEATS_NOT_UP <- c(D = 34, R = 31)

# ---- Current polls: Silver Bulletin averages ----------------------------------
# Silver Bulletin's Senate forecast page carries a free embed (a table of every
# race's polling average and the polls behind it) that reads two Google Sheets
# published as CSV. Find the embed and its sheet links on each refresh rather
# than hard-coding them, so a swapped sheet is picked up automatically.
SB_SENATE_PAGE <- "https://www.natesilver.net/p/nate-silver-2026-midterm-election-senate-model"

sb_get <- function(url) {
  res <- httr::GET(url, httr::user_agent(UA), httr::timeout(60))
  httr::stop_for_status(res)
  httr::content(res, as = "text", encoding = "UTF-8")
}

# Returns list(averages = daily averages per race, polls = every poll).
sb_embed_tables <- function() {
  page <- sb_get(SB_SENATE_PAGE)
  embeds <- unique(regmatches(page, gregexpr(
    "https://[a-z0-9-]+\\.substackcode\\.com/api/v1/code-embeds/render/[^\"\\\\]+", page, perl = TRUE))[[1]])
  sheets <- unique(unlist(lapply(embeds, function(u) {
    js <- tryCatch(sb_get(gsub("&amp;", "&", u)), error = function(e) "")
    regmatches(js, gregexpr("https://docs\\.google\\.com/spreadsheets/d/e/[^\"' )]+output=csv", js, perl = TRUE))[[1]]
  })))
  if (!length(sheets)) stop("Couldn't find the polling-average sheets on ", SB_SENATE_PAGE)
  tables <- lapply(sheets, function(u) read_csv(I(sb_get(u)), show_col_types = FALSE, guess_max = 10000))
  is_avg <- vapply(tables, function(t) all(c("group", "avg_R") %in% names(t)), logical(1))
  is_polls <- vapply(tables, function(t) all(c("pollster", "influence", "value1") %in% names(t)), logical(1))
  if (!any(is_avg) || !any(is_polls)) stop("Silver Bulletin's sheets have changed shape; keeping cached data.")
  list(averages = tables[[which(is_avg)[1]]], polls = tables[[which(is_polls)[1]]])
}

# Long candidate table from the wide averages sheet (cand_D/party_D/avg_D, ...).
sb_avg_candidates <- function(avgs) {
  slots <- sub("^cand_", "", grep("^cand_", names(avgs), value = TRUE))
  bind_rows(lapply(slots, function(s) {
    tibble(id = avgs$id, place = avgs$place, date = as.Date(avgs$date),
           name = avgs[[paste0("cand_", s)]], party = avgs[[paste0("party_", s)]],
           pct = suppressWarnings(as.numeric(avgs[[paste0("avg_", s)]])))
  })) |>
    filter(!is.na(name), name != "", !is.na(pct))
}

# Returns list(avg = one row per race, polls = every general-election poll).
# Margin = strongest non-Republican (D or independent) minus strongest Republican.
fetch_current_polls <- function() {
  sb <- sb_embed_tables()
  senate <- filter(sb$averages, group == "Senate")
  latest <- senate |> mutate(date = as.Date(date)) |> group_by(id) |> filter(date == max(date)) |> ungroup()
  cands <- sb_avg_candidates(latest) |>
    left_join(select(state_lookup, place = state, abb), by = "place") |>
    filter(abb %in% seats_2026$abb)
  rep <- cands |> filter(party == "R") |> group_by(id) |> slice_max(pct, n = 1, with_ties = FALSE) |> ungroup()
  opp <- cands |> filter(party != "R") |> group_by(id) |> slice_max(pct, n = 1, with_ties = FALSE) |> ungroup()
  races <- inner_join(select(opp, id, abb, date, opp_name = name, opp_party = party, opp_pct = pct),
                      select(rep, id, rep_name = name, rep_pct = pct), by = "id")

  # Polls: one row per poll, read at the same two candidates as the average.
  p <- sb$polls |> filter(id %in% races$id)
  slot_val <- function(k, who) {
    vapply(seq_len(nrow(p)), function(i) {
      nm <- races[[who]][match(p$id[i], races$id)]
      hit <- vapply(1:k, function(j) identical(p[[paste0("name", j)]][i], nm), logical(1))
      if (any(hit)) suppressWarnings(as.numeric(p[[paste0("value", which(hit)[1])]][i])) else NA_real_
    }, numeric(1))
  }
  k <- length(grep("^name[0-9]+$", names(p)))
  polls <- tibble(
    abb = races$abb[match(p$id, races$id)],
    pollster = p$pollster,
    poll_url = p$url,
    date = as.Date(p$endDate),
    sample_n = suppressWarnings(as.integer(round(as.numeric(p$sample)))),
    population = p$population,
    moe = NA_real_,
    opp_pct = slot_val(k, "opp_name"),
    rep_pct = slot_val(k, "rep_name"),
    margin = opp_pct - rep_pct,
    weight = suppressWarnings(as.numeric(p$influence)),
    partisan = ifelse(is.na(p$partisan) | p$partisan == "", NA_character_, toupper(p$partisan)),
    in_average = !is.na(weight) & weight > 0
  ) |>
    filter(!is.na(margin)) |>
    arrange(abb, desc(date))

  counts <- polls |> group_by(abb) |>
    summarise(n_polls_in_avg = sum(in_average), latest_poll = max(date), .groups = "drop")
  avg <- races |>
    transmute(abb, poll_margin = round(opp_pct - rep_pct, 1), opp_name, opp_party, opp_pct = round(opp_pct, 1),
              rep_name, rep_pct = round(rep_pct, 1), avg_date = date) |>
    left_join(counts, by = "abb") |>
    mutate(source_url = SB_SENATE_PAGE, fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M"))
  list(avg = avg, polls = polls)
}

# ---- Silver Bulletin files ----------------------------------------------------
# The pollster-ratings post links its current spreadsheets ("Pollster Stats" and
# "Rawpolls"). Scrape the links rather than hard-coding them so new releases are
# picked up automatically.
SB_RATINGS_PAGE <- "https://www.natesilver.net/p/pollster-ratings-silver-bulletin"

read_sb_file <- function(label) {
  page <- httr::content(httr::GET(SB_RATINGS_PAGE, httr::user_agent(UA), httr::timeout(30)),
                        as = "text", encoding = "UTF-8")
  link <- regmatches(page, regexpr(paste0(
    label, '[^<]*</div>[\\s\\S]{0,800}?href="https://www\\.natesilver\\.net/api/v1/file/[^"]+\\.xlsx"'
  ), page, perl = TRUE))
  if (!length(link)) stop("Couldn't find the '", label, "' file on ", SB_RATINGS_PAGE)
  url <- sub('.*href="([^"]+)"$', "\\1", link)
  xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(xlsx))
  httr::GET(url, httr::user_agent(UA), httr::write_disk(xlsx, overwrite = TRUE), httr::timeout(120))
  out <- readxl::read_excel(xlsx, sheet = 1, .name_repair = "unique_quiet")
  attr(out, "source_url") <- url
  out
}

# ---- Historical polling error -----------------------------------------------
# Silver Bulletin's raw polls (538's pollster-ratings database, continued): every
# poll in the final 21 days of each race since 1998, with the certified result.
fetch_poll_errors <- function() {
  read_sb_file("Rawpolls") |>
    filter(type_simple %in% c("Pres-G", "Sen-G"),
           year >= 2010, year %% 2 == 0,
           format(as.Date(electiondate), "%m") == "11",  # drop runoffs
           nchar(location) == 2, location != "US") |>
    mutate(
      # orient to (non-Rep) - Rep; drop races without exactly one Republican
      sign = case_when(
        cand2_party == "REP" & cand1_party != "REP" ~ 1,
        cand1_party == "REP" & cand2_party != "REP" ~ -1,
        TRUE ~ NA_real_
      )
    ) |>
    filter(!is.na(sign)) |>
    group_by(cycle = year, abb = location, race_type = ifelse(type_simple == "Pres-G", "President", "Senate"),
             race, type_detail) |>
    summarise(
      n_polls = n(),
      poll_margin = mean(sign * margin_poll),
      actual_margin = first(sign * margin_actual),
      .groups = "drop"
    ) |>
    # a state can hold two Senate races in a cycle (specials); average them
    group_by(cycle, abb, race_type) |>
    summarise(
      n_polls = sum(n_polls),
      poll_margin = mean(poll_margin),
      actual_margin = mean(actual_margin),
      .groups = "drop"
    ) |>
    mutate(error = poll_margin - actual_margin, midterm = cycle %% 4 == 2) |>
    arrange(cycle, race_type, abb)
}

# ---- Pollster ratings: Silver Bulletin --------------------------------------
# House effect / bias: positive = leans Democratic, negative = leans Republican.
fetch_pollster_ratings <- function() {
  raw <- read_sb_file("Pollster Stats")
  url <- attr(raw, "source_url")
  tibble(
    sb_name = raw[[2]],                      # column 1 is the rank
    sb_polls = raw[["# of Polls"]],
    sb_grade = raw[["SB Grade"]],
    sb_bias = raw[["Bias"]],
    sb_house = raw[["House Effect"]],
    sb_banned = raw[["Banned by Silver Bulletin"]]
  ) |>
    filter(!is.na(sb_name)) |>
    mutate(source_url = url)
}

# 270toWin names that don't normalize to Silver Bulletin's name for the same firm.
POLLSTER_ALIASES <- c(
  "Fox News" = "Beacon Research/Shaw & Co. Research",
  "Marist Poll" = "Marist College",
  "AARP" = "Fabrizio Lee & Associates/Impact Research",
  "UMass Lowell" = "University of Massachusetts Lowell",
  "UMass Amherst" = "University of Massachusetts (Amherst)",
  "Mason-Dixon" = "Mason-Dixon Polling & Strategy",
  "GQR" = "Greenberg Quinlan Rosner",
  "TIPP" = "TIPP Insights",
  "Wash. Post-Schar School" = "The Washington Post/George Mason University Schar School of Policy and Government",
  "Rutgers-Eagleton" = "Rutgers University",
  "Univ. of Texas - Tyler" = "University of Texas at Tyler",
  "New River / GSG" = "Global Strategy Group",
  "Rosetta Stone" = "Landmark Communications/Rosetta Stone Communications",
  "Mitchell Research" = "Mitchell Research & Communications",
  "Susquehanna Polling" = "Susquehanna Polling & Research Inc.",
  "MassINC" = "MassINC Polling Group",
  "American Pulse" = "American Pulse Research & Polling"
)

pollster_key <- function(x) {
  x <- gsub("\\buniv\\.?(?=\\s|$)", "university", tolower(x), perl = TRUE)
  words <- strsplit(trimws(gsub("[^a-z0-9]+", " ", x)), " ")
  vapply(words, function(w) paste(w[!w %in% c("the", "of", "and", "inc", "llc", "co")], collapse = " "), "")
}

# Row of `ratings` for each 270toWin pollster name (NA = unrated). Tries an alias,
# then the whole name, then each "/"-separated part from the right (270toWin lists
# the sponsor before the pollster, e.g. "CNN / SSRS"). Exact matches only: loose
# matching paired Tennessee's "Beacon Poll" with the unrelated Beacon Research.
match_pollsters <- function(pollster, ratings) {
  keys <- pollster_key(ratings$sb_name)
  find <- function(p) {
    if (p %in% names(POLLSTER_ALIASES)) return(match(POLLSTER_ALIASES[[p]], ratings$sb_name))
    for (part in c(p, rev(trimws(strsplit(p, "/", fixed = TRUE)[[1]])))) {
      hit <- which(keys == pollster_key(part) & keys != "")
      if (length(hit)) return(hit[which.max(ratings$sb_polls[hit])])
    }
    NA_integer_
  }
  vapply(pollster, find, integer(1), USE.NAMES = FALSE)
}

# ---- Main -------------------------------------------------------------------
# history = FALSE refreshes only the 270toWin polls (the in-app Refresh button);
# the historical errors and pollster ratings are refetched only if missing.
refresh_all <- function(data_dir = DATA_DIR, history = TRUE) {
  dir.create(data_dir, showWarnings = FALSE)
  write_csv(seats_2026, file.path(data_dir, "seats_2026.csv"))
  current <- fetch_current_polls()
  if (history || !file.exists(file.path(data_dir, "poll_errors.csv"))) {
    write_csv(fetch_poll_errors(), file.path(data_dir, "poll_errors.csv"))
  }
  if (history || !file.exists(file.path(data_dir, "pollster_ratings.csv"))) {
    write_csv(fetch_pollster_ratings(), file.path(data_dir, "pollster_ratings.csv"))
  }
  # Attach each poll's Silver Bulletin grade and house effect here, so the app
  # itself never needs the name-matching code (it also runs in the browser).
  ratings <- read_csv(file.path(data_dir, "pollster_ratings.csv"), show_col_types = FALSE)
  hit <- match_pollsters(current$polls$pollster, ratings)
  polls <- mutate(current$polls, sb_grade = ratings$sb_grade[hit], sb_house = ratings$sb_house[hit])
  write_csv(current$avg, file.path(data_dir, "current_polls.csv"))
  write_csv(polls, file.path(data_dir, "race_polls.csv"))
  invisible(TRUE)
}

if (sys.nframe() == 0) refresh_all()
