## Quarto Help – Chat + Browser (v2)
## Single-file Shiny app consolidating chat history, LLM chat, and embedded preview.

## Dependencies
library(shiny)
library(bslib)
library(shinychat)
library(ellmer)
library(ragnar)
library(glue)
library(promises)

## Early check for OpenAI API Key
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("No OPENAI_API_KEY found in the environment.", call. = FALSE)
}

## ----------------------------------------------------------------------------
## Core helpers: store + chat configuration
## ----------------------------------------------------------------------------

# Minimal copy of the package's store helpers to be self-contained here.
quartohelp_cache_dir <- function(...) {
  root <- tools::R_user_dir("quartohelp", "cache")
  normalizePath(file.path(root, ...), mustWork = FALSE)
}

quartohelp_store_path <- function() {
  quartohelp_cache_dir("quarto.ragnar.store")
}

quartohelp_ragnar_store <- function() {
  path <- quartohelp_store_path()
  if (!file.exists(path)) {
    # Try to use the package-supplied helper if available to populate the store.
    if ("quartohelp" %in% rownames(installed.packages())) {
      try(quartohelp::update_store(), silent = TRUE)
    }
  }
  ragnar::ragnar_store_connect(path, read_only = TRUE)
}

# Configure a fresh Chat object with system prompt + Quarto RAG tool.
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
}

## ----------------------------------------------------------------------------
## Persistence of chats (Chat objects are stateful; we persist whole objects)
## ----------------------------------------------------------------------------

history_dir <- function() {
  dir <- file.path(
    tools::R_user_dir("quartohelp", which = "data"),
    "history_v2"
  )
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

load_chats <- function() {
  files <- list.files(history_dir(), pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) {
    return(list())
  }
  res <- lapply(files, function(f) {
    # Be defensive in case Chat is not serializable or schema changed
    tryCatch(readRDS(f), error = function(e) NULL)
  })
  # Drop failed loads
  Filter(Negate(is.null), res)
}

save_chats <- function(chats) {
  dir <- history_dir()
  # Replace atomically by clearing and re-writing
  unlink(list.files(dir, full.names = TRUE, pattern = "\\.rds$"))
  invisible(lapply(chats, function(ch) {
    # Persist only if chat exists and has at least one turn
    if (!is.null(ch$chat) && length(ch$chat$get_turns()) > 0) {
      saveRDS(ch, file.path(dir, paste0(ch$id, ".rds")))
    }
    NULL
  }))
  invisible(NULL)
}

new_chat_entry <- function() {
  list(
    id = paste0("chat_", format(Sys.time(), "%Y-%m-%d_%H-%M-%OS6")),
    chat = configure_chat(),
    title = NULL,
    last_message = Sys.time()
  )
}

derive_title <- function(chat_obj) {
  # Simple, dependency-free title from first user message
  turns <- chat_obj$get_turns()
  user_msgs <- Filter(function(t) t@role == "user", turns)
  if (!length(user_msgs)) {
    return(NULL)
  }
  txt <- ellmer::contents_text(user_msgs[[1]])
  if (!nzchar(txt)) {
    return(NULL)
  }
  words <- strsplit(txt, "\\s+")[[1]]
  paste(utils::head(words, 6), collapse = " ")
}

## ----------------------------------------------------------------------------
## Sidebar: chat list module (UI + server)
## ----------------------------------------------------------------------------

chat_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "btn-toolbar mb-2",
      div(
        class = "btn-group btn-group-sm",
        actionButton(ns("new_chat"), "New", icon = icon("plus")),
        actionButton(ns("delete_chat"), "Delete", icon = icon("trash"))
      )
    ),
    uiOutput(ns("list"))
  )
}

chat_list_server <- function(id, chats, selected, busy) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$list <- renderUI({
      chs <- chats()
      # Order by most recent
      if (length(chs)) {
        chs <- chs[order(
          sapply(chs, function(x) x$last_message),
          decreasing = TRUE
        )]
      }
      sel <- selected() %||% if (length(chs)) chs[[1]]$id else NULL

      tags$ul(
        class = "list-group list-group-flush",
        !!!lapply(chs, function(x) {
          classes <- "list-group-item list-group-item-action"
          if (identical(x$id, sel)) {
            classes <- paste(classes, "active disabled")
          }
          tags$button(
            id = ns(paste0("chat-", x$id)),
            type = "button",
            class = paste("action-button", classes),
            style = if (isTRUE(busy())) {
              "pointer-events:none; opacity:0.7;"
            } else {
              NULL
            },
            x$title %||% "Untitled chat"
          )
        })
      )
    })

    observe({
      lapply(chats(), function(x) {
        observeEvent(input[[paste0("chat-", x$id)]], ignoreInit = TRUE, {
          if (!isTRUE(busy())) selected(x$id)
        })
      })
    })

    observeEvent(input$new_chat, {
      chs <- chats()
      ch <- new_chat_entry()
      chats(append(list(ch), chs))
      selected(ch$id)
    })

    observeEvent(input$delete_chat, {
      chs <- chats()
      if (!length(chs)) {
        return()
      }
      sel <- selected()
      chs <- Filter(function(x) x$id != sel, chs)
      if (!length(chs)) {
        chs <- list(new_chat_entry())
      }
      selected(chs[[1]]$id)
      chats(chs)
    })
  })
}

## ----------------------------------------------------------------------------
## UI
## ----------------------------------------------------------------------------

app_ui <- function() {
  page_sidebar(
    title = "Quarto Help",
    theme = bs_theme(version = 5),
    sidebar = sidebar(
      open = TRUE,
      chat_list_ui("chats")
    ),
    # Main content: two panels side-by-side
    tagList(
      tags$style(HTML(
        "
        html, body, .bslib-page { height: 100%; }
        .content-split { display: flex; gap: 1rem; height: calc(100vh - 4.5rem); }
        .left-pane, .right-pane { flex: 1; display: flex; flex-direction: column; }
        .card { display: flex; flex-direction: column; height: 100%; }
        .card-body { flex: 1; overflow: auto; }
        .iframe-wrap { flex: 1; }
        iframe { width: 100%; height: 100%; border: none; }
        "
      )),

      div(
        class = "content-split",
        # Left: chat
        div(
          class = "left-pane",
          card(
            card_header("Chat"),
            card_body(
              div(
                id = "chat-pane",
                shinychat::chat_ui("chat", height = "100%")
              )
            )
          )
        ),
        # Right: embedded browser
        div(
          class = "right-pane",
          card(
            card_header(
              div(
                class = "d-flex align-items-center justify-content-between gap-3",
                tags$span("Preview"),
                div(
                  class = "d-flex align-items-center gap-3",
                  # Toggle: open links in preview vs external tab
                  tags$div(
                    class = "form-check form-switch m-0",
                    tags$input(
                      id = "open-in-pane-toggle",
                      type = "checkbox",
                      class = "form-check-input",
                      checked = "checked"
                    ),
                    tags$label(
                      `for` = "open-in-pane-toggle",
                      class = "form-check-label",
                      "Open links here"
                    )
                  ),
                  # Button: open current preview in a new tab
                  tags$button(
                    id = "open-preview-external",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    "Open preview in new tab"
                  )
                )
              )
            ),
            card_body(
              div(
                id = "iframe-container",
                class = "iframe-wrap",
                p(
                  id = "iframe-placeholder",
                  "Click a link in chat to preview here."
                ),
                tags$iframe(
                  id = "content-iframe",
                  name = "content-iframe",
                  src = "about:blank",
                  style = "width:100%; height:100%; border:0; display:none;"
                )
              )
            )
          )
        )
      ),

      # JS: simple in-chat link hijack -> right pane iframe
      tags$script(HTML(
        r"---(
        // Preference: open in right pane (true) or new tab (false)
        window.APP_OPEN_IN_PANE = true;

        function ensureIframeShown(){
          var iframe = document.getElementById('content-iframe');
          if (!iframe) return;
          try { iframe.removeEventListener('load', iframe._onload || function(){}); } catch(e) {}
          iframe._onload = function(){ var ph = document.getElementById('iframe-placeholder'); if (ph) ph.style.display = 'none'; iframe.style.display = 'block'; };
          iframe.addEventListener('load', iframe._onload);
        }
        ensureIframeShown();

        function openInNewTab(url) { try { window.open(url, '_blank', 'noopener'); } catch (e) {} }

        // Toggle: update preference from the header switch
        document.addEventListener('change', function(ev){
          var el = ev.target.closest('#open-in-pane-toggle');
          if (!el) return;
          window.APP_OPEN_IN_PANE = !!el.checked;
        }, true);

        // Button: open current preview iframe in a new tab
        document.addEventListener('click', function(ev){
          var btn = ev.target.closest('#open-preview-external');
          if (!btn) return;
          var iframe = document.getElementById('content-iframe');
          var url = iframe && iframe.src;
          if (url) openInNewTab(url);
        }, true);

        // Simplified handlers: no link normalization

        // Simplified: no separate handler function

        // Simplified: rely on single document-level delegated click handler

        // Fallback: also listen on document in capture mode
        function findAnchor(ev){
          var path = (typeof ev.composedPath === 'function') ? ev.composedPath() : null;
          var a = null;
          if (path && path.length){ for (var i=0;i<path.length;i++){ var el = path[i]; if (el && el.tagName === 'A') { a = el; break; } } }
          if (!a) { a = ev.target && ev.target.closest ? ev.target.closest('a') : null; }
          if (!a) return null;
          // Only anchors inside our chat pane
          var inPane = false;
          if (path && path.length){ for (var j=0;j<path.length;j++){ var el2 = path[j]; if (el2 && el2.id === 'chat-pane') { inPane = true; break; } } }
          if (!inPane && !(a.closest && a.closest('#chat-pane'))) return null;
          return a;
        }

        // On mousedown, neutralize href/target early to prevent native nav
        document.addEventListener('mousedown', function(ev){
          var a = findAnchor(ev); if (!a) return;
          var href = a.getAttribute('href'); if (!href) return;
          if (!/^https?:\/\//i.test(href)) return;
          a.setAttribute('data-orig-href', href);
          a.setAttribute('href', '#');
          a.removeAttribute('target');
        }, true);

        // On click, route to iframe or new tab
        document.addEventListener('click', function(ev){
          var a = findAnchor(ev); if (!a) return;
          var href = a.getAttribute('data-orig-href') || a.getAttribute('href'); if (!href) return;
          if (!/^https?:\/\//i.test(href)) return;
          ev.preventDefault(); ev.stopPropagation(); if (ev.stopImmediatePropagation) ev.stopImmediatePropagation();
          if (window.APP_OPEN_IN_PANE) {
            ensureIframeShown();
            var iframe = document.getElementById('content-iframe'); if (iframe) iframe.src = href;
          } else {
            try { window.open(href, '_blank', 'noopener'); } catch(e) {}
          }
          return false;
        }, true);
        )---"
      ))
    )
  )
}

## ----------------------------------------------------------------------------
## Server
## ----------------------------------------------------------------------------

app_server <- function(input, output, session) {
  # State: list of chat entries (id, chat, title, last_message)
  chats <- reactiveVal({
    loaded <- load_chats()
    if (!length(loaded)) list(new_chat_entry()) else loaded
  })
  # Do NOT read chats() here; no reactive context yet
  selected <- reactiveVal(NULL)
  busy <- reactiveVal(FALSE)

  # Sidebar module
  chat_list_server("chats", chats = chats, selected = selected, busy = busy)

  # Initialize selection once chats are available
  observeEvent(chats(), {
    chs <- chats()
    if (is.null(selected()) && length(chs)) {
      selected(chs[[1]]$id)
    }
  }, ignoreInit = FALSE)

  # When selection changes, paint that chat's history into UI
  observeEvent(
    selected(),
    {
      chs <- chats()
      sel <- selected()
      current <- NULL
      for (ch in chs) {
        if (identical(ch$id, sel)) {
          current <- ch
          break
        }
      }
      if (is.null(current)) {
        return()
      }
      shinychat::chat_clear("chat")
      lapply(current$chat$get_turns(), function(tn) {
        msg <- list(role = tn@role, content = ellmer::contents_markdown(tn))
        shinychat::chat_append_message("chat", msg, chunk = FALSE)
      })
    },
    ignoreInit = TRUE
  )

  # Handle user input -> stream to the selected Chat object
  observeEvent(input$chat_user_input, {
    chs <- chats()
    sel <- selected()
    current_idx <- NULL
    for (i in seq_along(chs)) {
      if (identical(chs[[i]]$id, sel)) {
        current_idx <- i
        break
      }
    }
    if (is.null(current_idx)) {
      return()
    }

    cli <- chs[[current_idx]]$chat
    busy(TRUE)
    promises::promise_resolve(cli$stream_async(input$chat_user_input)) |>
      promises::then(function(stream) {
        shinychat::chat_append("chat", stream)
      }) |>
      promises::finally(function(value) {
        # Update metadata: last_message + maybe title
        chs <- chats()
        if (!is.null(current_idx) && current_idx <= length(chs)) {
          chs[[current_idx]]$last_message <- Sys.time()
          if (is.null(chs[[current_idx]]$title)) {
            chs[[current_idx]]$title <- derive_title(
              chs[[current_idx]]$chat
            ) %||%
              "Untitled chat"
          }
          chats(chs)
          save_chats(chs)
        }
        busy(FALSE)
      })
  })

  # Save on session end
  session$onSessionEnded(function() {
    save_chats(isolate(chats()))
  })
}

## ----------------------------------------------------------------------------
## Run app
## ----------------------------------------------------------------------------

shinyApp(app_ui(), app_server)
