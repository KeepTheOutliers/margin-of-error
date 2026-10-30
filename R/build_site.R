# Builds the browser-only (Shinylive) version of the app into _site/ for GitHub Pages:
# every visitor runs the app in their own browser, so traffic can't overload a server.
# Adds the page title, the link-preview tags from site/index.html, and a loading note
# that shows while R starts up in the browser.
unlink(c("_stage", "_site"), recursive = TRUE)
dir.create("_stage")
file.copy(c("app.R", "data", "www"), "_stage", recursive = TRUE)
shinylive::export("_stage", "_site")
file.copy("site/og.png", "_site/og.png", overwrite = TRUE)

idx <- readLines("_site/index.html", warn = FALSE)
wrapper <- readLines("site/index.html", warn = FALSE)
meta <- grep('<meta (name="description"|property="og:|name="twitter:)', wrapper, value = TRUE)
idx <- sub("<title>[^<]*</title>", "<title>Margin of Error | The 2026 Senate Stress Test</title>", idx)
t <- grep("</title>", idx)[1]
loading <- c(
  '<style>',
  '  body { background: #fbf8f1; }',
  '  .mo-loading { position: fixed; inset: 0; z-index: -1; display: flex; flex-direction: column; align-items: center;',
  '    justify-content: center; gap: 12px; padding: 24px; text-align: center; font-family: Georgia, serif; color: #1b1a17; }',
  '  .mo-loading b { font-size: 1.5rem; } .mo-loading span { color: #55524b; max-width: 30em; line-height: 1.4; }',
  '  .mo-rule { display: flex; width: min(420px, 80vw); height: 5px; border-radius: 3px; overflow: hidden; }',
  '  .mo-rule i { flex: 1; }',
  '</style>')
idx <- append(idx, c(meta, loading), after = t)
b <- grep("<body[^>]*>", idx)[1]
idx <- append(idx, c(
  '<div class="mo-loading">',
  '  <b>Margin of Error | The 2026 Senate Stress Test</b>',
  '  <div class="mo-rule"><i style="background:#1b4a76"></i><i style="background:#386ca0"></i><i style="background:#6b90b6"></i><i style="background:#a8b9cc"></i><i style="background:#ccb0a8"></i><i style="background:#b97a67"></i><i style="background:#a44a30"></i><i style="background:#792b13"></i></div>',
  '  <span>Loading. The app runs entirely in your browser, so the first visit takes about 20&ndash;30 seconds; after that it&rsquo;s quick.</span>',
  '</div>'), after = b)
writeLines(idx, "_site/index.html")
cat("Built _site/\n")
