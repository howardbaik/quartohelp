# quartohelp

<!-- badges: start -->

<!-- badges: end -->

A focused chat app for quick, authoritative answers from Quarto
documentation.

`quartohelp` launches an interactive chat interface that finds and
summarizes relevant excerpts from Quarto documentation using both
semantic and keyword-based search methods (via
[ragnar](https://github.com/tidyverse/ragnar)). Chat responses include
links to official sources and are designed to help users quickly
accomplish specific Quarto tasks.

## Screenshot

![Screenshot of quartohelp app](man/figures/app-screenshot.png)

## Usage

``` r
# Launch with a blank chat
quartohelp::ask()

# Start with a specific question
quartohelp::ask("How do I make a two column layout?")
```

## Installation

``` r
remotes::install_github("t-kalinowski/quartohelp")
```

## Requirements

-   An OpenAI API key must be set in `Sys.getenv("OPENAI_API_KEY")`.
