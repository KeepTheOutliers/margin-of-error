# Parity check for the static site: computes a large set of scenarios with app.R's own
# functions and writes them, with R's answers, to _site/check_scenarios.json. The page's
# App.compute() is then run on the same scenarios in a browser and compared (seat totals,
# control, and every race's projected margin).
#
# Run from the repo root after R/build_static.R:  Rscript R/check_static.R
suppressPackageStartupMessages(library(jsonlite))
env <- new.env()
assign("BUILD_STATIC", TRUE, envir = env)
sys.source("app.R", envir = env, keep.source = FALSE)
d <- env$INIT
scen_d <- list(seats = d$seats, polls = dplyr::mutate(d$polls, avg_label = "Poll avg"))

set.seed(2026)
cyc_sets <- c(list(numeric(0)), as.list(env$CYCLES),
              list(env$MIDTERMS, env$PRES_YEARS, env$TRUMP_ERA, env$TRUMP_MID, env$CYCLES),
              replicate(10, sort(sample(env$CYCLES, sample(2:6, 1))), simplify = FALSE))
type_sets <- list(c("Senate", "President"), "Senate", "President")
shifts <- c(0, 2.5, -3, 7)
customs <- list(list(), list(KS = -3, ME = 5, TX = 0.5))

grid <- list()
for (cy in cyc_sets) for (ty in type_sets) for (sh in shifts) for (cu in customs) for (ind in c(FALSE, TRUE)) {
  err <- env$assumed_errors(d$errors, d$seats$abb, cy, ty)
  err$custom_move <- vapply(err$abb, function(a) if (is.null(cu[[a]])) NA_real_ else cu[[a]], numeric(1))
  races <- env$build_races(scen_d, err, sh)
  t <- env$seat_totals(races)
  grid[[length(grid) + 1]] <- list(
    cfg = list(cycles = I(cy), types = I(ty), shift_d = sh, custom = if (length(cu)) cu else setNames(list(), character(0)), ind_with_d = ind),
    want = list(D = t[["D"]], I = t[["I"]], R = t[["R"]], side = env$control_side(t, ind),
                proj = as.list(setNames(round(races$proj_margin, 6), races$abb))))
}
write_json(grid, "_site/check_scenarios.json", auto_unbox = TRUE, na = "null", digits = NA)
cat(length(grid), "scenarios written\n")
