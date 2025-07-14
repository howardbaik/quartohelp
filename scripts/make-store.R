#!/usr/bin/env Rscript

library(ragnar)
library(stringr)
library(dotty)
library(purrr)
library(dplyr)
library(quartohelp)

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

sitemap <- xml2::read_xml("~/github/quarto-dev/quarto-web/_site/sitemap.xml") |>
  xml2::as_list() |>
  _$urlset |>
  lapply(\(u) vctrs::new_data_frame(unlist(u, recursive = FALSE))) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    url = loc,
    lastmod = anytime::anytime(lastmod),
    path = str_replace(
      url,
      "^https://quarto.org/",
      path.expand("~/github/quarto-dev/quarto-web/_site/")
    ),
    .keep = "none"
  )

# sanity check
stopifnot(file.exists(sitemap$path))

store_location <- "quarto.ragnar.store"

store <- ragnar_store_create(
  store_location,
  name = "quarto_docs",
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
  overwrite = TRUE
)


for (r in seq_len(nrow(sitemap))) {
  .[path = path, url = url, ..] <- sitemap[r, ]
  message(sprintf("[%i/%i] ingesting: %s", r, nrow(sitemap), url))

  doc <- read_as_markdown(path)
  doc@origin <- url
  chunks <- markdown_chunk(doc)

  ragnar_store_insert(store, chunks)
}

ragnar_store_build_index(store)

DBI::dbDisconnect(store@con)

if (require("quartohelp")) {
  fs::file_copy(
    store_location,
    quartohelp:::quarto_store_path(),
    overwrite = TRUE
  )
}
