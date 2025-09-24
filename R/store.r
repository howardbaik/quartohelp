quartohelp_cache_dir <- function(...) {
  root <- tools::R_user_dir("quartohelp", "cache")
  normalizePath(file.path(root, ...), mustWork = FALSE)
}

quartohelp_store_path <- store_path <- function() {
  quartohelp_cache_dir("quarto.ragnar.store")
}

#' Open the Quarto knowledge store
#'
#' Returns a `ragnar::RagnarStore` containing the Quarto documentation.
#'
#' @return A `RagnarStore` object.
#' @keywords internal
quartohelp_ragnar_store <- function() {
  path <- quartohelp_store_path()
  if (!file.exists(path)) {
    update_store()
  }
  ragnar::ragnar_store_connect(path, read_only = TRUE)
}

#' Updates the Quarto knowledge store
#'
#' Downloads the latest version of the store from the quartohelp GitHub
#' repository and builds the search index locally.
#'
#' The download location can be configured with a few environment variables:
#' - `QUARTOHELP_STORE_URL`: a custom URL to download the store from. The default is to download the latest store
#' from the `t-kalinowski/quartohelp` repository in the `store-v2` release.
#' - `QUARTOHELP_STORE_RELEASE`: the release tag to download the store from. Defaults to `store-v2`.
#' - `QUARTOHELP_STORE_REPOSITORY`: the repository to download the store from. Defaults to `t-kalinowski/quartohelp`.
#'
#' @return `NULL` invisibly.
#' @export
update_store <- function() {
  path <- quartohelp_store_path()
  fs::dir_create(dirname(path))

  tmp <- withr::local_tempfile()
  download.file(quartohelp_store_url(), destfile = tmp, mode = "wb")

  fs::file_move(tmp, path)

  store <- ragnar::ragnar_store_connect(path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(store@con), add = TRUE)
  ragnar::ragnar_store_build_index(store)
  invisible(NULL)
}

quartohelp_store_url <- function() {
  url <- Sys.getenv("QUARTOHELP_STORE_URL", "")
  if (nzchar(url)) {
    return(url)
  }
  release <- Sys.getenv("QUARTOHELP_STORE_RELEASE", "store-v2")
  repository <- Sys.getenv(
    "QUARTOHELP_STORE_REPOSITORY",
    "t-kalinowski/quartohelp"
  )
  commit_hash <- readLines(
    sprintf(
      "https://github.com/%s/releases/download/%s/LATEST",
      repository,
      release
    ),
    n = 1L
  )
  sprintf(
    "https://github.com/%s/releases/download/%s/quarto.ragnar.store-%s",
    repository,
    release,
    commit_hash
  )
}

#' Serve the Quarto knowledge store via MCP
#'
#' This is a thin wrapper around [ragnar::mcp_serve_store()] that serves the
#' bundled Quarto knowledge store connection.
#'
#' @param top_k Number of excerpts to return for each request. Passed to
#'   [ragnar::mcp_serve_store()].
#' @param ... Additional arguments forwarded to [ragnar::mcp_serve_store()],
#'   such as `store_description`, `name`, or `extra_tools`.
#' @return Nothing; called for its side effect.
#' @export
serve_store <- function(top_k = 8, ...) {
  ragnar::mcp_serve_store(store = quartohelp_ragnar_store(), top_k = top_k, ...)
}
