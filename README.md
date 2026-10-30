# Margin of Error | The 2026 Senate Stress Test

How the 2026 Senate outlook changes when today's polling averages are adjusted by past state-level polling errors.

**Live site:** https://keeptheoutliers.github.io/margin-of-error/

A scenario explorer, not a forecast. The 2026 polling averages, poll weights and pollster ratings are
[Silver Bulletin](https://www.natesilver.net/p/nate-silver-2026-midterm-election-senate-model)'s, taken as published;
most of the underlying 2026 polls were collected by The New York Times. Historical polling errors are measured here
from Silver Bulletin's raw-polls archive (2010–2024). See the app's "About the Data" tab for the full method.

## How it runs

- `app.R`: the Shiny app, hosted on [Posit Connect Cloud](https://connect.posit.cloud), which republishes it from
  this repo on every push (`manifest.json` lists its R packages; regenerate with `rsconnect::writeManifest()` if they change).
- `R/fetch_data.R`: pulls the latest data from Silver Bulletin into `data/`.
- `.github/workflows/deploy.yml`: refreshes the data daily and commits it (which republishes the app), and publishes
  `site/index.html` to GitHub Pages, a page that frames the app at the address above.

Run locally: `shiny::runApp()` from this folder.
