# Margin of Error | The 2026 Senate Stress Test

How the 2026 Senate outlook changes when today's polling averages are adjusted by past state-level polling errors.

**Live site:** https://keeptheoutliers.github.io/margin-of-error/

A scenario explorer, not a forecast. The 2026 polling averages, poll weights and pollster ratings are
[Silver Bulletin](https://www.natesilver.net/p/nate-silver-2026-midterm-election-senate-model)'s, taken as published;
most of the underlying 2026 polls were collected by The New York Times. Historical polling errors are measured here
from Silver Bulletin's raw-polls archive (2010–2024). See the app's "About the Data" tab for the full method.

## How it runs

The live site is a plain static page (HTML + JavaScript), so it loads in a second or two and can't be overloaded.

- `R/fetch_data.R`: pulls the latest data from Silver Bulletin into `data/`.
- `app.R`: the Shiny app. It defines the page layout and the reference logic, and runs locally with `shiny::runApp()`.
- `R/build_static.R`: renders `app.R`'s layout to static HTML and writes `data.json` (data plus pieces that depend
  only on the data) into `_site/`.
- `web/app.js`: the browser-side app: scenario logic, horseshoe, map, ranked chart, sliders, history tab, list filters.
  It mirrors the server code in `app.R`; keep the two in step.
- `R/check_static.R`: parity check. Writes ~1,150 scenarios with R's answers so the page's `App.compute()` can be
  compared against them in a browser.
- `.github/workflows/deploy.yml`: refreshes the data daily, builds `_site/`, and publishes it to GitHub Pages.

Build locally: `Rscript R/build_static.R`, then serve `_site/` (for example `httpuv::runStaticServer("_site")`).
