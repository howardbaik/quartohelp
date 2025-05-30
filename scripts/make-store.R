#!/usr/bin/env Rscript

library(ragnar)
library(stringr)
library(dotty)
library(purrr)
library(dplyr)

pandoc::pandoc_install("3.4")
message(paste0("Pandoc from: ", pandoc::pandoc_bin("3.4")))
message(paste0("Pandoc version: ", pandoc::pandoc_version("3.4")))

if (!dir.exists("~/github/quarto-dev/quarto-web")) {
  fs::dir_create("~/github/quarto-dev")
  withr::with_dir("~/github/quarto-dev", {
    system("git clone https://github.com/quarto-dev/quarto-web")
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
  # embed = \(x) ragnar::embed_ollama(x, model = "snowflake-arctic-embed2:latest"),
  overwrite = TRUE
)

for (r in seq_len(nrow(sitemap))) {
  .[path = path, url = url, ..] <- sitemap[r, ]
  # break
  message(sprintf("[%i/%i] ingesting: %s", r, nrow(sitemap), url))

  withr::with_dir(dirname(path), {
    output <- pandoc::pandoc_convert(
      # text = readLines(path),
      file = path,
      output = fs::path_ext_set(path, ".md"),
      from = "html",
      to = "markdown",
      version = "3.4"
    )
  })

  text <- readLines(output) |> paste0(collapse = "\n")
  text <- str_replace_all(
    text,
    '(src="data:image/.*;base64,)[^"]+"',
    "\\1--removed--"
  )
  hash <- rlang::hash(text)
  frame <- markdown_frame(text, c("h1", "h2", "h3"))

  chunks <- frame |>
    mutate(hash = hash, origin = url) |>
    ragnar_chunk(boundaries = c("paragraph", "sentence", "line_break")) |>
    tidyr::drop_na(text) |>
    rowwise() |>
    dplyr::mutate(
      text = glue::glue(
        r"---(
        > excerpt from: {origin}
        {str_c("> ", na.omit(c(h1, h2, h3)), collapse = "\n")}
        ---
        {text}
        )---"
      )
    ) |>
    dplyr::ungroup()

  ragnar_store_insert(store, chunks)
}

ragnar_store_build_index(store)

