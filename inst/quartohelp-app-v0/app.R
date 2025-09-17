# Early check for OpenAI API Key
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("No OpenAI API key found", call. = FALSE)
}


if (FALSE) {
  #' Shiny UI for QuartoHelp Chat
  #' @noRd
  quartohelp_chat_ui <- function() {
    bslib::page_sidebar(
      title = "Quarto Help",
      sidebar = bslib::sidebar(
        open = TRUE,
        shiny::div(
          class = "btn-toolbar",
          shiny::div(
            class = "btn-group btn-group-sm",
            shiny::actionButton(
              "new_chat",
              label = "New",
              icon = shiny::icon("plus")
            ),
            shiny::actionButton(
              "delete_chat",
              label = "Delete",
              icon = shiny::icon("trash")
            )
          )
        ),
        chatListUI("chat_list")
      ),
      shinychat::chat_ui("chat", height = "100%"),
      shiny::actionButton(
        "close_btn",
        label = "",
        class = "btn-close",
        style = "position: fixed; top: 6px; right: 6px;"
      ),
      open_chat_links_in_new_window()
    )
  }

  #' Shiny Server for QuartoHelp Chat (with Initial Stream)
  #'
  #' By default, history is stored in the package cache directory.
  #' You can customize the location and the functionality of saving and loading
  #' chats with the options:
  #'
  #' - `quartohelp.history_dir`: A function that takes a Shiny session and returns
  #'   the directory where chat history should be stored.
  #' - `quartohelp.load_state`: A function that takes a Shiny session and returns
  #'   the initial state of the chat history. Called when the app starts to load
  #'   the current state.
  #' - `quartohelp.save_state`: A function that takes a Shiny session and the
  #'  current chat history, and saves it to the specified directory. Called at the
  #'  end of the session to save the current state.
  #'
  #' For example use `option(``quartohelp.history_dir = function(session) "path/to/dir")`
  #' to set the history directory to a custom location.
  #'
  #' @noRd
  quartohelp_chat_server <- function(
    store,
    client = \() ellmer::chat_openai(model = "gpt-4.1"),
    question = NULL,
    close_action = c("stop", "clear"),
    ...
  ) {
    force(client)
    store <- quarto_ragnar_store()
    close_action <- match.arg(close_action)

    function(input, output, session) {
      chat <- reactiveVal(NULL, "currently active chat")
      selected <- shiny::reactiveVal(NULL, "id of the currently selected chat")
      client <- quartohelp_setup_client(client, store)

      complete_task <- shiny::ExtendedTask$new(function(turns, question) {
        client$set_turns(turns)
        value <- client$stream_async(!!!question)
        value |>
          promises::promise_resolve() |>
          promises::then(
            function(stream) {
              shinychat::chat_append("chat", stream)
            }
          )
      })

      loaded_chats <- quartohelp_load_state(session)
      if (!is.null(question)) {
        ch <- new_chat()
        loaded_chats <- append(list(ch), loaded_chats)
      }

      chats <- shiny::reactiveVal(loaded_chats %||% list(new_chat()))

      chatListServer(
        "chat_list",
        chats = chats,
        selected = selected
      )

      progress <- reactiveVal(NULL)
      observe({
        current_chats <- isolate(chats())
        current_chat <- isolate(chat())
        p <- shiny::isolate(progress())
        task_status <- isolate(complete_task$status())

        sel <- selected() %||% current_chats[[1]]$id

        # if a completion is running, we don't switch chat just yet
        # instead, we show a progress bar and wait a little for completion
        if (task_status == "running") {
          if (is.null(p)) {
            p <- shiny::Progress$new()
            progress(p)
          }
          p$inc(message = "Wait for completion", amount = 0.05)
          invalidateLater(500, session = session)
          return()
        } else {
          # cleanup the progress bar when the task is not running
          if (!is.null(p)) {
            p$close()
            progress(NULL)
          }
        }

        # save current turns in the chat object
        if (!is.null(current_chat)) {
          chats(lapply(current_chats, function(ch) {
            if (ch$id == current_chat$id) {
              ch$turns <- client$get_turns()
            }
            ch
          }))
        }

        for (ch in current_chats) {
          if (ch$id == sel) {
            chat(ch)
            break
          }
        }
      })

      observeEvent(chat(), {
        # Clear the current chat
        shinychat::chat_clear("chat")
        client$set_turns(chat()$turns)

        # Append messages of the new chat
        lapply(chat()$turns, function(x) {
          msg <- list(
            role = x@role,
            content = ellmer::contents_markdown(x)
          )

          shinychat::chat_append_message("chat", msg, chunk = FALSE)
        })

        # Invoke question if it's not NULL and reset
        if (!is.null(question)) {
          shinychat::chat_append_message(
            "chat",
            list(role = "user", content = question),
            chunk = FALSE
          )
          complete_task$invoke(chat()$turns, question)
          question <<- NULL
        }
      })

      observeEvent(input$new_chat, {
        ch <- new_chat()
        chats(append(list(ch), chats()))
        selected(ch$id)
      })

      observeEvent(input$delete_chat, {
        current_chats <- chats()
        sel <- selected()

        filtered_chats <- Filter(
          function(x) x$id != sel,
          current_chats
        )

        if (length(filtered_chats) == 0) {
          filtered_chats <- list(new_chat())
        }
        selected(filtered_chats[[1]]$id)
        chats(filtered_chats)
      })

      shiny::observeEvent(input$chat_user_input, {
        complete_task$invoke(chat()$turns, input$chat_user_input)
      })

      shiny::observeEvent(input$close_btn, {
        if (close_action == "stop") {
          shiny::stopApp()
        } else if (close_action == "clear") {
          # Do not allow clearing while task is executing.
          while (complete_task$status() == "running") {
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
          return()
        }

        if (complete_task$status() != "success") {
          return()
        }

        # After a successful task complete, we update the chat list
        # with the current chat's turns and title.
        chats() |>
          lapply(function(ch) {
            if (ch$id == chat()$id) {
              ch$turns <- client$get_turns()
              ch$last_message <- Sys.time()
            }
            if (is.null(ch$title) && length(ch$turns) > 0) {
              ch$title <- generate_chat_title(ch$turns)
            }
            ch
          }) |>
          chats()
      })

      session$onSessionEnded(function() {
        chats <- isolate(chats())
        quartohelp_save_state(session, chats)
      })
    }
  }

  #' @param client a closure that returns an ellmer::Chat
  #' @noRd
  quartohelp_setup_client <- function(client, store) {
    ragnar:::chat_ragnar(
      function() {
        client <- client()
        # Don't override the system prompt if it is already set.
        if (is.null(client$get_system_prompt())) {
          client$set_system_prompt(paste(
            readLines(
              system.file(
                "prompt",
                "quartohelp_system.txt",
                package = "quartohelp"
              )
            ),
            collapse = "\n"
          ))
        }
        client
      },
      store = store
    )
  }

  chatListUI <- function(id) {
    uiOutput(shiny::NS(id, "chats"))
  }

  chatListServer <- function(id, chats, selected) {
    stopifnot(is.reactive(chats))
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      output$chats <- shiny::renderUI({
        chats <- chats()

        # reorder chats by last message time
        if (length(chats)) {
          chats <- chats[order(
            sapply(chats, function(x) x$last_message),
            decreasing = TRUE
          )]
        }

        sel <- selected() %||% chats[[1]]$id

        shiny::tags$ul(
          class = "list-group list-group-flush",
          !!!lapply(
            chats,
            function(x) {
              class <- "list-group-item list-group-item-action"
              if (x$id == sel) {
                class <- paste(class, "active disabled")
              }

              unclassedActionButton(
                inputId = ns(paste0("chat-", x$id)),
                label = x$title %||% "Untitled chat",
                class = class
              )
            }
          )
        )
      })

      shiny::observe({
        lapply(chats(), function(x) {
          observeEvent(
            input[[paste0("chat-", x$id)]],
            {
              selected(x$id)
            },
            ignoreInit = TRUE
          )
        })
      })
    })
  }

  unclassedActionButton <- function(inputId, label, class = NULL, ...) {
    shiny::tags$button(
      id = inputId,
      class = paste("action-button", class, collapse = " "),
      type = "button",
      label,
      ...
    )
  }

  generate_chat_title <- function(turns) {
    cli <- ellmer::chat_openai(model = "gpt-4.1-nano")
    cli$set_turns(turns)
    cli$chat_structured(
      "Create a title for this conversation. No more than 4 words.",
      type = ellmer::type_string()
    )
  }

  new_chat <- function() {
    list(
      id = timestamp(),
      turns = list(),
      title = NULL,
      last_message = Sys.time()
    )
  }

  timestamp <- function() {
    paste0("chat_", format(Sys.time(), "%Y-%m-%d_%H-%M-%OS6"))
  }

  quartohelp_history_dir <- function(session) {
    if (is.function(history <- getOption("quartohelp.history_dir"))) {
      history(session)
    }

    dir <- file.path(tools::R_user_dir("quartohelp", which = "data"), "history")
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    dir
  }

  quartohelp_load_state <- function(session) {
    if (is.function(load_state <- getOption("quartohelp.load_state"))) {
      load_state(session)
    }

    res <- list.files(quartohelp_history_dir(session), full.names = TRUE) |>
      sort(decreasing = TRUE) |>
      lapply(readRDS)
    if (length(res) == 0) {
      return(NULL)
    }
    res
  }

  quartohelp_save_state <- function(session, chats) {
    if (is.function(save_state <- getOption("quartohelp.save_state"))) {
      save_state(session, chats)
    }

    dir <- quartohelp_history_dir(session)
    unlink(dir, recursive = TRUE)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    lapply(chats, function(chat) {
      # Do not save empty chats
      if (length(chat$turns) == 0) {
        return(NULL)
      }
      saveRDS(chat, file.path(dir, paste0(chat$id, ".rds")))
    })
  }

  quartohelp_clean_history <- function(session) {
    dir <- quartohelp_history_dir()
    files <- list.files(dir, full.names = TRUE)
    file.remove(files)
    invisible(NULL)
  }

  open_chat_links_in_new_window <- function() {
    tags$script(
      HTML(
        r"---(
  document.getElementById('chat').addEventListener('click', function(event) {
    // Find the closest link element (in case user clicks on nested elements)
    const link = event.target.closest('a');

    if (link) {
        const href = link.href;

        // Check if the link starts with http:// or https://
        if (href && /^https?:\/\//.test(href)) {
            // Prevent the default link behavior
            event.preventDefault();

            // Open the link in a new window
            window.open(href, '_blank');
        }
    }
    });
      )---"
      )
    )
  }
}
### self-standing app

store <- quartohelp:::quartohelp_ragnar_store()

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

ui <- quartohelp_chat_ui()
server <- quartohelp_chat_server(
  store,
  client = function() {
    quartohelp:::configure_chat()
  },
  close_action = "clear"
)

shiny::shinyApp(ui, server)
