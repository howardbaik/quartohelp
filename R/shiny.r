# Keeps track of whether we've mounted the app's static assets
.register_assets <- local({
  added <- FALSE
  function() {
    if (added) {
      return(invisible(NULL))
    }
    dir <- system.file("quartohelp-app", "www", package = "quartohelp")
    if (!nzchar(dir) || !dir.exists(dir)) {
      warning(
        "quartohelp assets directory not found; app resources may be missing.",
        call. = FALSE
      )
      return(invisible(NULL))
    }
    shiny::addResourcePath("quartohelp", dir)
    added <<- TRUE
    invisible(NULL)
  }
})

#' Internal UI for the Quarto Help app
#' @noRd
quartohelp_app_ui <- function() {
  .register_assets()

  bslib::page_fillable(
    title = "Quarto Help",
    theme = bslib::bs_theme(version = 5),
    shiny::tags$head(
      shiny::singleton(
        shiny::tags$link(
          rel = "icon",
          type = "image/png",
          href = "quartohelp/favicon.png"
        )
      ),
      shiny::singleton(
        shiny::tags$script(src = "quartohelp/quartohelp-app.js")
      )
    ),
    shiny::tagList(
      shiny::tags$style(
        shiny::HTML(
          "
          html, body, .bslib-page { height: 100%; }
          body, .bslib-page { margin: 0; }
          .bslib-page .main, [data-bslib-main] { padding: 1rem; }
          .content-split { display: flex; gap: 0; height: calc(100vh - 2rem); position: relative; }
          .left-pane { flex: 0 0 var(--left-pane-width, 40%); min-width: 240px; max-width: 80%; display: flex; flex-direction: column; }
          .right-pane { flex: 1 1 auto; display: flex; flex-direction: column; min-width: 20%; }
          .left-pane, .right-pane { min-height: 0; }
          .split-resizer { width: 6px; cursor: col-resize; background: transparent; position: relative; }
          .split-resizer::after { content: ''; position: absolute; top: 0; bottom: 0; left: 2px; width: 2px; background: var(--bs-border-color, #dee2e6); }
          #toggle-chat-show { display: none; }
          .collapsed-left #toggle-chat-show { display: inline-flex; }
          .card { display: flex; flex-direction: column; height: 100%; }
          .card-body { flex: 1; overflow: auto; }
          .iframe-wrap { flex: 1; }
          iframe { width: 100%; height: 100%; border: none; }
          .collapsed-left .left-pane, .collapsed-left .split-resizer { display: none !important; }
          .collapsed-left .split-reveal { display: flex !important; }
          .collapsed-left .right-pane { flex: 1 1 auto; }
          .resizing iframe { pointer-events: none !important; }
          .resizing, .resizing * { cursor: col-resize !important; }
          "
        )
      ),
      shiny::div(
        class = "content-split",
        shiny::div(
          class = "left-pane",
          bslib::card(
            bslib::card_header(
              shiny::div(
                class = "d-flex align-items-center justify-content-between gap-2",
                shiny::tags$span("Chat"),
                shiny::div(
                  class = "btn-group btn-group-sm",
                  shiny::actionButton(
                    inputId = "clear_chat",
                    label = "Clear",
                    icon = shiny::icon("broom"),
                    class = "btn-outline-secondary"
                  ),
                  shiny::tags$button(
                    id = "toggle-chat",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Collapse chat",
                    `aria-label` = "Collapse chat",
                    shiny::icon("chevron-left")
                  )
                )
              )
            ),
            bslib::card_body(
              shiny::div(
                id = "chat-pane",
                shiny::uiOutput("chat_panel")
              )
            )
          )
        ),
        shiny::div(id = "split-resizer", class = "split-resizer"),
        shiny::div(
          class = "right-pane",
          bslib::card(
            bslib::card_header(
              shiny::div(
                class = "d-flex align-items-center justify-content-between gap-3",
                shiny::div(
                  class = "d-flex align-items-center gap-2",
                  shiny::tags$button(
                    id = "toggle-chat-show",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Show chat",
                    `aria-label` = "Show chat",
                    shiny::icon("chevron-right")
                  ),
                  shiny::tags$button(
                    id = "iframe-back",
                    type = "button",
                    class = "btn btn-sm btn-secondary",
                    title = "Back",
                    `aria-label` = "Back",
                    shiny::HTML("&#8592;")
                  ),
                  shiny::tags$button(
                    id = "iframe-forward",
                    type = "button",
                    class = "btn btn-sm btn-secondary",
                    title = "Forward",
                    `aria-label` = "Forward",
                    shiny::HTML("&#8594;")
                  ),
                  shiny::tags$span("Docs")
                ),
                shiny::div(
                  class = "d-flex align-items-center gap-3",
                  shiny::tags$button(
                    id = "open-preview-external",
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary",
                    title = "Open in new tab",
                    `aria-label` = "Open in new tab",
                    shiny::icon("external-link-alt")
                  )
                )
              )
            ),
            bslib::card_body(
              shiny::div(
                id = "iframe-container",
                class = "iframe-wrap",
                shiny::p(
                  id = "iframe-placeholder",
                  "Loading documentation..."
                ),
                shiny::tags$iframe(
                  id = "content-iframe",
                  name = "content-iframe",
                  src = "https://quarto.org",
                  style = "width:100%; height:100%; border:0;"
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Internal server for the Quarto Help app
#' @noRd
quartohelp_app_server <- function(
  initial_chat = NULL,
  chat_factory = configure_chat,
  initial_question = NULL
) {
  force(chat_factory)

  function(input, output, session) {
    first <- TRUE
    initial <- initial_chat
    pending_question <- shiny::reactiveVal(initial_question)

    make_chat <- function() {
      if (first) {
        first <<- FALSE
        if (!is.null(initial)) {
          chat_obj <- initial
          initial <<- NULL
          return(chat_obj)
        }
      }
      chat_factory()
    }

    chat <- shiny::reactiveVal(make_chat())
    chat_gen <- shiny::reactiveVal(1L)

    output$chat_panel <- shiny::renderUI({
      shinychat::chat_mod_ui(paste0("chat_", chat_gen()), height = "100%")
    })

    shiny::observeEvent(
      chat_gen(),
      {
        module_id <- paste0("chat_", chat_gen())
        module <- shinychat::chat_mod_server(module_id, chat())

        question <- pending_question()
        if (!is.null(question)) {
          pending_question(NULL)
          session$onFlushed(
            function() {
              module$update_user_input(value = question, submit = TRUE)
            },
            once = TRUE
          )
        }
      },
      ignoreInit = FALSE
    )

    shiny::observeEvent(input$clear_chat, {
      chat(make_chat())
      chat_gen(shiny::isolate(chat_gen()) + 1L)
    })
  }
}
