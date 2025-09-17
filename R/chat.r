.require_api_key <- function() {
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    stop("No OPENAI_API_KEY found in the environment.", call. = FALSE)
  }
}

.normalize_question <- function(question) {
  rlang:::check_string(question, allow_null = TRUE)
  if (is.null(question)) {
    return(NULL)
  }
  question <- trimws(question)
  if (!nzchar(question)) {
    return(NULL)
  }
  question
}

#' Launch the Quarto Help chat app
#'
#' Starts an interactive chat interface backed by the Quarto knowledge store.
#' The app uses [shinychat::chat_mod_ui()] and [shinychat::chat_mod_server()] to
#' wire a single chat instance and an embedded documentation browser. By
#' default a fresh chat is created with [quartohelp::chat_client()].
#'
#' @param question Optional character string to send as the first user turn.
#' @param client An `ellmer::Chat` instance. Defaults to the value returned by
#'   [quartohelp::chat_client()].
#'
#' @return Invisibly returns the `client` object.
#' @seealso [quartohelp::chat_client()], [quartohelp::configure_chat()]
#' @export
#' @examples
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   quartohelp::launch_app()
#' }
launch_app <- function(
  question = NULL,
  client = chat_client()
) {
  .require_api_key()
  question <- .normalize_question(question)

  factory <- attr(client, "chat_factory")
  if (!is.function(factory)) {
    factory <- chat_client
  }

  initial_chat <- client

  app <- shiny::shinyApp(
    ui = quartohelp_app_ui(),
    server = quartohelp_app_server(
      initial_chat = initial_chat,
      chat_factory = factory,
      initial_question = question
    )
  )

  # runApp() blocks until the window is closed.
  shiny::runApp(app)
  invisible(initial_chat)
}

#' Ask a single question with Quarto Help
#'
#' Convenience wrapper that either launches [launch_app()] (the default) or
#' runs a single turn against the provided chat client when `interactive = FALSE`.
#'
#' @inheritParams launch_app
#' @param interactive When `FALSE`, return the result of `client$chat(question)`
#'   instead of launching the app. If `question` is `NULL`, the configured
#'   `client` is returned so you can continue the conversation manually.
#' @return When `interactive = TRUE`, invisibly returns the `client`. Otherwise
#'   returns the chat response or `client`.
#' @export
#' @examples
#'
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   # Launch the app
#'   quartohelp::ask()
#'
#'   # Or answer a question programmatically
#'   quartohelp::ask("How can I make a two column layout?", interactive = FALSE)
#' }
ask <- function(
  question = NULL,
  client = chat_client(),
  interactive = TRUE
) {
  if (!interactive) {
    .require_api_key()
    question <- .normalize_question(question)
    if (is.null(question)) {
      return(client)
    }
    return(client$chat(question))
  }

  launch_app(question = question, client = client)
}
