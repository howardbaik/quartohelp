#' Launch the Quarto Help chat app
#'
#' Starts an interactive chat interface backed by the Quarto knowledge store.
#' The app uses [shinychat::chat_mod_ui()] and [shinychat::chat_mod_server()] to
#' wire a single chat instance and an embedded documentation browser. By
#' default a fresh chat is created with [quartohelp::configure_chat()].
#'
#' @param question Optional character string to send as the first user turn.
#' @param client An `ellmer::Chat` instance. Defaults to the value returned by
#'   [quartohelp::configure_chat()].
#' @param interactive If `TRUE` (default) launch the Shiny app. If `FALSE`,
#'   the `question` (when supplied) is answered immediately and the response is
#'   returned.
#'
#' @return Invisibly returns the `client` object. When `interactive = FALSE`
#'   the chat response is returned directly.
#' @seealso [quartohelp::configure_chat()]
#' @export
#' @examples
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   quartohelp::ask("How can I make a two column layout?")
#' }
ask <- function(
  question = NULL,
  client = configure_chat(),
  interactive = TRUE
) {
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    stop("No OPENAI_API_KEY found in the environment.", call. = FALSE)
  }

  rlang:::check_string(question, allow_null = TRUE)

  if (!interactive) {
    if (is.null(question)) {
      return(client)
    }
    return(client$chat(question))
  }

  factory <- attr(client, "quartohelp_factory")
  if (!is.function(factory)) {
    factory <- configure_chat
  }

  initial_chat <- client
  if (!is.null(question) && length(initial_chat$get_turns()) == 0) {
    initial_chat$chat(question)
  }

  app <- shiny::shinyApp(
    ui = quartohelp_app_ui(),
    server = quartohelp_app_server(
      initial_chat = initial_chat,
      chat_factory = factory
    )
  )

  # runApp() blocks until the window is closed.
  shiny::runApp(app)
  invisible(initial_chat)
}
