#' Launch an Interactive Quarto Documentation Chat App
#'
#' Starts an interactive chat interface for asking questions about Quarto
#' documentation, powered by an OpenAI-based assistant and a hybrid search (RAG)
#' retrieval system over an embedded Quarto knowledge store.
#'
#' This app combines semantic and text-based search, returning authoritative
#' excerpts from Quarto documentation.
#'
#' @param question A character string with the user's question (optional). If
#'   not provided, app opens with a blank chat.
#' @param client An `ellmer::Chat` object. Defaults to openai 'gpt-4.1'. Note
#'   that if a different chat provider is used for chat, an `OPENAI_API_KEY`
#'   must still be set for embedding vector search.
#' @param interactive Logical; whether to launch the interactive Shiny app
#'   (default `TRUE`). If `FALSE`, returns chat response directly if `question`
#'   is provided, otherwise, the `client` is returned with the retrieval tool
#'   registered and system prompt set.
#'
#' @return Invisibly returns the `client` object for further use or inspection.
#'
#' @seealso [ellmer::chat_openai], [ragnar::ragnar_retrieve]
#' @importFrom rlang .data
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
  # Early check for OpenAI API Key
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    stop(
      "No OpenAI API key found in Sys.getenv('OPENAI_API_KEY').",
      call. = FALSE
    )
  }

  # Validate user input
  check_string(question, allow_null = TRUE)

  if (!interactive) {
    if (is.null(client)) {
      return(client)
    } else {
      return(quartohelp_complete(client, store, question, async = FALSE))
    }
  }

  ui <- quartohelp_chat_ui(question)
  server <- shinychat::chat_mod_server("quartohelp", client)
  server <- quartohelp_chat_server(store, client, question)

  tryCatch(shiny::runGadget(ui, server), interrupt = function(cnd) NULL)
  invisible(client)
}


# Creates a stream of chat results but instead of directly passing the user input
# to the model, it first generates a query using a different model, extracts excerpts
# and then inject those into the turns for the chat model.
quartohelp_complete <- function(client, store, question, async = TRUE) {
  # only for small questions.
  # also don't do it for follow up questions
  if (nchar(question) < 500 && length(client$get_turns()) < 2) {
    # temporary chat for making the tool call.
    chat <- ellmer::chat_openai(model = "gpt-5-nano")

    # TODO: use a json schema to bound the number of queries in the response?
    queries <- chat$chat_structured(
      echo = FALSE,
      type = ellmer::type_array(
        "search queries",
        items = ellmer::type_string()
      ),
      glue::trim(glue::glue(
        "
        To help answer the question below, generate up to 3 search queries for the Quarto Knowledge Store.
        You don't always need to generate 3 queries. Be wise.

        {question}
        "
      ))
    )

    # using a fixed retrieve tool for all requests already avoids repeated
    # documents to appear in the output.
    retrieve_tool <- client$get_tools()$rag_retrieve_quarto_excerpts
    tool_requests <- lapply(queries, function(query) {
      ellmer::ContentToolRequest(
        id = rlang::hash(query),
        name = "rag_retrieve_quarto_excerpts",
        arguments = list(text = query),
        # we're faking the request so we don't care about the function
        tool = retrieve_tool
      )
    })

    client$add_turn(
      ellmer::Turn("user", contents = list(ellmer::ContentText(question))),
      ellmer::Turn("assistant", contents = tool_requests)
    )

    question <- lapply(tool_requests, function(req) {
      ellmer::ContentToolResult(
        request = req,
        value = do.call(req@tool, req@arguments)
      )
    })
  } else {
    # we need it to be a list for later
    question <- list(question)
  }

  get_answer <- if (async) client$stream_async else client$chat
  get_answer(!!!question)
}

#
# quartohelp_retrieve_tool <- function(store) {
#   retrieved_ids <- integer()
#   rag_retrieve_quarto_excerpts <- function(text) {
#     # Retrieve relevant chunks using hybrid (vector/BM25) search,
#     # excluding previously returned IDs in this session.
#     chunks <- ragnar::ragnar_retrieve(
#       store,
#       text,
#       top_k = 10,
#       filter = !.data$chunk_id %in% retrieved_ids
#     )
#
#     retrieved_ids <<- unique(unlist(c(retrieved_ids, chunks$chunk_id)))
#     chunks
#   }
#
#   ellmer::tool(
#     rag_retrieve_quarto_excerpts,
#     glue::trim(
#       "
#       Use this tool to retrieve the most relevant excerpts from the Quarto
#       knowledge store for a given text input. This function:
#       - uses both vector (semantic) similarity and BM25 text search.
#       - never returns the same excerpt twice in the same session; it always excludes recently retrieved IDs.
#       - returns the results as plain text wrapped in <excerpt> tags.
#       "
#     ),
#     arguments = list(text = ellmer::type_string())
#   )
# }
