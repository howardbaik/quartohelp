#' Launch an Interactive Quarto Documentation Chat App
#'
#' Starts an interactive chat interface for asking questions about Quarto documentation,
#' powered by an OpenAI-based assistant and a hybrid search (RAG) retrieval system over an embedded Quarto knowledge store.
#'
#' This app combines semantic and text-based search, returning authoritative excerpts from Quarto documentation.
#'
#' @param question A character string with the user's question (optional). If not provided, app opens with a blank chat.
#' @param client An `ellmer::chat_openai` chat client instance. Defaults to a GPT-4.1-mini model.
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
  client = ellmer::chat_openai(model = "gpt-4.1-mini")
) {
  client$set_system_prompt(
    "You are an expert in Quarto documentation. You are concise.
    You help users quickly accomplish specific Quarto-related tasks.
    You directly address the user's specific needs.

ALWAYS do the following for every user request:
- Before replying, perform at least one search across the Quarto knowledge store for relevant information.
- Include in every response links to authoritative resources for learning more.
- If the user's request is ambiguous, first perform a preliminary search, then ask a clarifying question before further searching.
- If you cannot access the docs or search produces an error, inform the user and do NOT attempt to answer.

To display literal code block that include code and prose withint them, wrap the block
in a doubled-up code fence with a `markdown` tag, like:

````` markdown
PROSE HERE
```{r}
CODE HERE
```
`````

"
  )

  store <- ragnar::ragnar_store_connect(
    system.file("quarto.ragnar.store", package = "quartohelp"),
    read_only = TRUE
  )

  retrieved_ids <- integer()
  rag_retrieve_from_quarto_store <- function(text) {
    chunks <- dplyr::tbl(store) |>
      dplyr::filter(!.data$id %in% retrieved_ids) |>
      ragnar::ragnar_retrieve(text, top_k = 10)

    retrieved_ids <<- unique(c(retrieved_ids, chunks$id))
    stringi::stri_c(
      "<excerp>",
      chunks$text,
      "</excerpt>",
      sep = "\n",
      collapse = "\n"
    )
  }
  retrieve_tool <- ellmer::tool(
    rag_retrieve_from_quarto_store,
    "Use this tool to retrieve the most relevant excerpts from the Quarto knowledge store for a given text input. This function:
- uses both vector (semantic) similarity and BM25 text search.
- never returns the same excerpt twice in the same sesion; it always excludes recently retrieved IDs.
- returns the results as plain text wrapped in <excerpt> tags.",
    text = ellmer::type_string()
  )
  client$register_tool(retrieve_tool)
  # client$register_tool(btw::btw_tool_ide_read_current_editor)

  if (!is.null(question)) {
    if (nchar(question) < 500) {
      initial_tool_request <- ellmer::ContentToolRequest(
        id = "init",
        name = "rag_retrieve_from_quarto_store",
        arguments = list(text = question),
        tool = retrieve_tool
      )
      initial_tool_result <- asNamespace("ellmer")$invoke_tool(
        initial_tool_request
      )
      client$add_turn(
        ellmer::Turn("user", contents = list(ellmer::ContentText(question))),
        ellmer::Turn(
          "assistant",
          contents = list(initial_tool_request)
        )
      )
      initial_stream <- client$stream_async(initial_tool_result)
      initial_tool_result <- initial_tool_request <- NULL
    } else {
      initial_stream <- client$stream_async(question)
    }
  } else {
    initial_stream <- NULL
  }

  ui <- bslib::page_fillable(
    style = "display: flex; flex-direction: column; height: 100vh; padding: 0.5rem;",
    shiny::h1(
      "Quarto Help",
      style = "margin-bottom: 0.5rem; text-align: center;"
    ),
    shinychat::chat_mod_ui(
      "chat",
      client = client,
      height = "100%"
    ),
    shiny::actionButton(
      "close_btn",
      label = "",
      class = "btn-close",
      style = "position: fixed; top: 6px; right: 6px;"
    )
  )

  server <- function(input, output, session) {
    chat_mod_server2(
      "chat",
      client,
      initial_stream = initial_stream
    )

    shiny::observeEvent(input$close_btn, {
      shiny::stopApp()
    })
  }

  tryCatch(shiny::runGadget(ui, server), interrupt = function(cnd) NULL)
  invisible(client)
}


## slightly modified version of shinychat::chat_mod_server() that
## accepts an initial_stream arg and allows us to launch the app already streaming.
chat_mod_server2 <- function(
  id,
  client,
  initial_stream = NULL
) {
  append_stream_task <- shiny::ExtendedTask$new(
    function(client, ui_id, stream) {
      promises::then(
        promises::promise_resolve(stream),
        function(stream) {
          shinychat::chat_append(ui_id, stream)
        }
      )
    }
  )

  initial_stream # force

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      input$chat_user_input,
      {
        if (is.null(input$chat_user_input)) {
          stream <- initial_stream
          initial_stream <<- NULL
        } else {
          stream <- client$stream_async(input$chat_user_input)
        }

        append_stream_task$invoke(client, "chat", stream)
      },
      ignoreNULL = is.null(initial_stream)
    )

    shiny::reactive({
      if (append_stream_task$status() == "success") {
        client$last_turn()
      }
    })
  })
}
