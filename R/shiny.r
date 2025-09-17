#' Shiny UI for QuartoHelp Chat
#' @noRd
quartohelp_chat_ui <- function(question) {
  bslib::page_fillable(
    style = "display: flex; flex-direction: column; height: 100vh; padding: 0.5rem;",
    shiny::h1(
      "Quarto Help",
      style = "margin-bottom: 0.5rem; text-align: center;"
    ),
    shinychat::chat_mod_ui("quartohelp_chat", height = "100%"), #, messages = question),
    shiny::actionButton(
      "close_btn",
      label = "",
      class = "btn-close",
      style = "position: fixed; top: 6px; right: 6px;"
    )
  )
}


#' Shiny Server for QuartoHelp Chat (with Initial Stream)
#' @noRd
quartohelp_chat_server <- function(
  store,
  client,
  close_action = c("stop", "clear"),
  ...
) {
  close_action <- match.arg(close_action)
  force(client)
  force(store)

  function(input, output, session) {
    if (!inherits(client, "Chat")) {
      client <- client()
    }
    shinychat::chat_mod_server("quartohelp_chat", client)

    shiny::observeEvent(input$close_btn, {
      switch(
        close_action,
        "stop" = {
          shiny::stopApp()
        },
        "clear" = {
          client$set_turns(list())
          shinychat::chat_clear("quartohelp_chat")
        }
      )
    })
  }
}
