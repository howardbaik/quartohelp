#!/usr/bin/env Rscript

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  Sys.setenv(OPENAI_API_KEY = "dummy-key")
}

suppressPackageStartupMessages({
  library(shinytest2)
  library(webshot2)
})

app <- AppDriver$new(
  app_dir = "inst/quartohelp-app",
  load_timeout = 30 * 1000
)

on.exit(app$stop(), add = TRUE)

app$set_window_size(1280, 720)
app$wait_for_idle()
Sys.sleep(2)

app_url <- app$get_url()

webshot2::webshot(
  url = app_url,
  file = "man/figures/app-screenshot.png",
  vwidth = 1280,
  vheight = 720,
  selector = "body"
)
