#' Configure the default chat client
#'
#' Attaches the Quarto knowledge store retrieval tool and system prompt to a
#' chat instance. By default this creates a fresh OpenAI chat via
#' [ellmer::chat_openai()] and registers [ragnar::ragnar_register_tool_retrieve]
#' so that every response can cite relevant Quarto documentation.
#'
#' @param chat An `ellmer::Chat` object to configure.
#' @param top_k Number of excerpts to request from the knowledge store for each
#'   retrieval.
#' @param store A connection returned by `quartohelp_ragnar_store()`.
#'
#' @return The configured `chat` object.
#' @export
configure_chat <- function(
  chat = ellmer::chat_openai(
    model = "gpt-5-nano",
    params = ellmer::params(reasoning_effort = "low", verbosity = "low"),
    echo = FALSE
  ),
  top_k = 8,
  store = quartohelp_ragnar_store()
) {
  stopifnot(inherits(chat, "Chat"))
  chat$set_system_prompt(c(
    chat$get_system_prompt(),
    glue::trim(
      "
      You are an expert in Quarto documentation. You are concise.
      Always perform a search of the Quarto knowledge store for each user request.
      Every response must include links to official documentation sources.
      If the request is ambiguous, search first, then ask a clarifying question.
      If docs are unavailable, or if search fails, or if docs do not contain an answer
      to the question, inform the user and do NOT answer the question.

      Always give answers that include a minimal fully self-contained quarto document.

      To display Quarto code blocks, use oversized markdown fences, like this:

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
    )
  ))

  ragnar::ragnar_register_tool_retrieve(chat, store, top_k = top_k)

  attr(chat, "quartohelp_factory") <- function() {
    configure_chat(top_k = top_k, store = store)
  }

  chat
}
