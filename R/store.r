quarto_ragnar_store <- function() {
  path <- quarto_store_path()
  if (!file.exists(path)) {
    update_store()
  }
  ragnar::ragnar_store_connect(
    path,
    read_only = TRUE
  )
}

#' Updates the Quarto knowledge store
#' 
#' It always downloads the latest version of the store from the
#' quartohelp GitHub repository.
#' 
#' The download location can be configured with a few environment variables:
#' - `QUARTOHELP_STORE_URL`: a custom URL to download the store from. The default is to downlaod the latest store
#' from the `t-kalinowski/quartohelp` repository in the `store` release.
#' - `QUARTOHELP_STORE_RELEASE`: the release tag to download the store from. Defaults to `store`.
#' - `QUARTOHELP_STORE_REPOSITORY`: the repository to download the store from. Defaults to `t-kalinowski/quartohelp`.
#' 
#' @return `NULL` invisibly.
#' @export
update_store <- function() {
  path <- quarto_store_path()
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  download.file(quarto_store_url(), destfile = tmp)
  file.rename(tmp, path)
  invisible(NULL)
}

quarto_store_url <- function() {
  url <- Sys.getenv("QUARTOHELP_STORE_URL", "")
  if (nzchar(url)) {
    return(url)
  }
  release <- Sys.getenv("QUARTOHELP_STORE_RELEASE", "store-v2")
  repository <- Sys.getenv("QUARTOHELP_STORE_REPOSITORY", "t-kalinowski/quartohelp")
  commit_hash <- readLines(
    sprintf("https://github.com/%s/releases/download/%s/LATEST", repository, release),
    n = 1L
  )
  sprintf(
    "https://github.com/t-kalinowski/quartohelp/releases/download/store/quarto.ragnar.store-%s",
    commit_hash
  )
}

quarto_store_path <- function() {
  cache_dir <- tools::R_user_dir("quartohelp", which = "cache")
  store_path <- file.path(cache_dir, "quarto.ragnar.store")
  store_path
}