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
  client = ellmer::chat_openai(model = "gpt-4.1"),
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
  if (!is.null(question)) {
    if (!is.character(question) || length(question) != 1 || is.na(question)) {
      stop("question must be a single, non-NA character string.")
    }
  }

  store <- quarto_ragnar_store()
  client <- quartohelp_setup_client(client, store)

  if (!interactive) {
    if (is.null(client)) {
      return(client)
    } else {
      return(quartohelp_complete(client, store, question, async = FALSE))
    }
  }

  ui <- quartohelp_chat_ui(question)
  server <- quartohelp_chat_server(store, client, question)

  tryCatch(shiny::runGadget(ui, server), interrupt = function(cnd) NULL)
  invisible(client)
}


#' Shiny UI for QuartoHelp Chat
#' @noRd
quartohelp_chat_ui <- function(question) {
  if (!is.null(question)) {
    question <- list(list(role = "user", content = question))
  }

  bslib::page_fillable(
    style = "display: flex; flex-direction: column; height: 100vh; padding: 0.5rem;",
    shiny::h1(
      "Quarto Help",
      style = "margin-bottom: 0.5rem; text-align: center;"
    ),
    shinychat::chat_ui(
      "chat",
      height = "100%",
      messages = question
    ),
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
  client = ellmer::chat_openai(model = "gpt-4.1"),
  question = NULL,
  close_action = c("stop", "clear"),
  ...
) {
  store <- quarto_ragnar_store()
  close_action <- match.arg(close_action)
  force(client)

  function(input, output, session) {

    if (!inherits(client, "Chat")) {
      # client can be a function returning a ellmer Chat.
      # This is used for apps that need one client per session
      # eg: when hosting this app for multiple users.
      client <- client()
    }

    complete_task <- shiny::ExtendedTask$new(function(client, store, question) {
      value <- quartohelp_complete(client, store, question)
      promises::then(
        promises::promise_resolve(value),
        function(stream) {
          shinychat::chat_append("chat", stream)
        }
      )
    })

    if (!is.null(question)) {
      complete_task$invoke(client, store, question)
    }

    shiny::observeEvent(input$chat_user_input, {
      complete_task$invoke(client, store, input$chat_user_input)
    })

    shiny::observeEvent(input$close_btn, {
      if (close_action == "stop") {
        shiny::stopApp()
      } else if (close_action == "clear") {
        # Do not allow clearing while task is executing.
        while(complete_task$status() == "running") {
          Sys.sleep(0.5)
        }

        # clear the front-end and backend.
        client$set_turns(list())
        shinychat::chat_clear("chat")
      }
    })

    shiny::observeEvent(complete_task$status(), {
      if (complete_task$status() == "error") {
        complete_task$result() # reraise error
      }
    })
  }
}

# Creates a stream of chat results but instead of directly passing the user input
# to the model, it first generates a query using a different model, extracts excerpts
# and then inject those into the turns for the chat model.
quartohelp_complete <- function(client, store, question, async = TRUE) {
  # only for small questions.
  # also don't do it for follow up questions
  if (nchar(question) < 500 && length(client$get_turns()) < 2) {
    # temporary chat for making the tool call.
    chat <- ellmer::chat_openai("gpt-4.1-nano") |>
      quartohelp_setup_client(store)

    queries <- chat$chat_structured(
      echo = FALSE,
      type = ellmer::type_array(
        "queries",
        items = ellmer::type_string("a query. escaped if needed")
      ),
      glue::trim(glue::glue(
        "
        You are going to search on the Quarto Knowledge store. First generate up to
        3 search queries related to the question below. You don't always need to
        generate 3 queries. Be wise.

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

    question <-lapply(tool_requests, function(req) {
      ellmer::ContentToolResult(
        request = req,
        value = req@tool@fun(req@arguments$text)
      )
    })
  } else {
    # we need it to be a list for later
    question <- list(question)
  }

  if (async) {
    client$stream_async(!!!question)
  } else {
    client$chat(!!!question)
  }
}


quartohelp_setup_client <- function(client, store) {
  client$set_system_prompt(glue::trim(
    "
    You are an expert in Quarto documentation. You are concise.
    Always perform a search of the Quarto knowledge store for each user request.
    Every response must cite links to official documentation sources.
    If the request is ambiguous, search first, then ask a clarifying question.
    If docs are unavailable or search fails, inform the user and do NOT answer the question.

    Always give answers that include a minimal fully self-contained quarto document.

    To display quarto code blocks, use oversized markdown fences, like this:

    ````` markdown
    PROSE HERE
    ```{r}
    CODE HERE
    ```
    ```{python}
    CODE HERE
    ```
    `````
    "
  ))

  retrieve_tool <- quartohelp_retrieve_tool(store)

  client$register_tool(retrieve_tool)
  client
}


quartohelp_retrieve_tool <- function(store) {
  retrieved_ids <- integer()
  rag_retrieve_quarto_excerpts <- function(text) {
    # Retrieve relevant chunks using hybrid (vector/BM25) search,
    # excluding previously returned IDs in this session.
    chunks <- dplyr::tbl(store) |>
      dplyr::filter(!.data$id %in% retrieved_ids) |>
      ragnar::ragnar_retrieve(text, top_k = 10)

    retrieved_ids <<- unique(c(retrieved_ids, chunks$id))

    stringi::stri_c(
      "<excerpt>",
      chunks$text,
      "</excerpt>",
      sep = "\n",
      collapse = "\n"
    )
  }

  ellmer::tool(
    rag_retrieve_quarto_excerpts,
    glue::trim(
      "
      Use this tool to retrieve the most relevant excerpts from the Quarto
      knowledge store for a given text input. This function:
      - uses both vector (semantic) similarity and BM25 text search.
      - never returns the same excerpt twice in the same session; it always excludes recently retrieved IDs.
      - returns the results as plain text wrapped in <excerpt> tags.
      "
    ),
    text = ellmer::type_string()
  )
}
