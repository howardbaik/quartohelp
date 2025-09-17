if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("No OPENAI_API_KEY found in the environment.", call. = FALSE)
}

shiny::shinyApp(
  ui = quartohelp:::quartohelp_app_ui(),
  server = quartohelp:::quartohelp_app_server()
)
