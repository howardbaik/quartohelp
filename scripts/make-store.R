#!/usr/bin/env Rscript

library(ragnar)
library(stringr)
library(dotty)
library(purrr)
library(dplyr)

if (!dir.exists("~/github/quarto-dev/quarto-web")) {
  fs::dir_create("~/github/quarto-dev")
  withr::with_dir("~/github/quarto-dev", {
    system("git clone https://github.com/quarto-dev/quarto-web --depth 1")
  })
}

if (!dir.exists("~/github/quarto-dev/quarto-web/_site/")) {
  withr::with_dir("~/github/quarto-dev/quarto-web", {
    system("git pull")
    system("quarto render")
  })
}

urls <- ragnar_find_links("~/github/quarto-dev/quarto-web/_site/sitemap.xml")
local_paths <- str_replace(
  urls,
  "^https://quarto.org/",
  path.expand("~/github/quarto-dev/quarto-web/_site/")
)

sitemap <- tibble(urls, local_paths) |> rename_with(\(nms) sub("s$", "", nms))


# sanity check
stopifnot(file.exists(sitemap$local_path))

store_location <- "quarto.ragnar.store"

store <- ragnar_store_create(
  store_location,
  name = "quarto_docs",
  title = "Search Quarto Docs",
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
  overwrite = TRUE
)


for (r in seq_len(nrow(sitemap))) {
  .[local_path = local_path, url = url, ..] <- sitemap[r, ]
  message(sprintf("[% 3i/%i] ingesting: %s", r, nrow(sitemap), url))

  doc <- read_as_markdown(local_path, origin = url)
  chunks <- markdown_chunk(doc)

  ragnar_store_insert(store, chunks)
}

DBI::dbDisconnect(store@con)

if (require("quartohelp")) {
  dest <- quartohelp:::quartohelp_store_path()
  fs::dir_create(dirname(dest))
  fs::file_copy(store_location, dest, overwrite = TRUE)
}
